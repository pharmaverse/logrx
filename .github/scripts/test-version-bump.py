import os
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).with_name("version-bump.sh").resolve()


class VersionBumpTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        root = Path(self.tmp.name)
        self.env = os.environ.copy()
        self.env["HOME"] = str(root)
        self.env["GIT_CONFIG_GLOBAL"] = str(root / ".gitconfig")
        self.env["GIT_CONFIG_COUNT"] = "0"
        for key in ("GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL"):
            self.env.pop(key, None)
        self.env["PATH"] = f"{root / 'bin'}:{self.env['PATH']}"
        (root / "bin").mkdir()
        rscript = root / "bin" / "Rscript"
        rscript.write_text(
            """#!/usr/bin/env python3
import pathlib, re, sys
path = pathlib.Path("DESCRIPTION")
text = path.read_text()
version = re.search(r"^Version: (.+)$", text, re.M).group(1)
if "desc_bump_version" in sys.argv[2]:
    parts = version.split(".")
    if len(parts) == 3:
        parts.append("9000")
    else:
        parts[-1] = str(int(parts[-1]) + 1)
    path.write_text(text.replace("Version: " + version, "Version: " + ".".join(parts)))
else:
    print(version, end="")
"""
        )
        rscript.chmod(0o755)
        self.remote = root / "remote.git"
        self.repo = root / "repo"
        self.git(root, "init", "--bare", "--initial-branch=main", str(self.remote))
        self.git(root, "clone", str(self.remote), str(self.repo))
        self.git(self.repo, "config", "user.name", "Test")
        self.git(self.repo, "config", "user.email", "test@example.com")
        (self.repo / "DESCRIPTION").write_text("Package: example\nVersion: 0.4.0.9001\n")
        (self.repo / "NEWS.md").write_text("# example (development version)\n")
        self.git(self.repo, "add", ".")
        self.git(self.repo, "commit", "-m", "Initial commit")
        self.git(self.repo, "push", "origin", "main")
        self.sha = self.git(self.repo, "rev-parse", "HEAD").stdout.strip()

    def git(self, cwd, *args):
        return subprocess.run(
            ["git", *args], cwd=cwd, env=self.env, check=True, capture_output=True, text=True
        )

    def bump(self, sha=None):
        return subprocess.run(
            ["bash", str(SCRIPT)], cwd=self.repo, env={**self.env, "PUSH_SHA": sha or self.sha},
            capture_output=True, text=True
        )

    def commit(self, message):
        self.git(self.repo, "commit", "--allow-empty", "-m", message)
        self.git(self.repo, "push", "origin", "main")
        return self.git(self.repo, "rev-parse", "HEAD").stdout.strip()

    def test_bump_and_rerun_only_change_description_once(self):
        result = self.bump()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0.9002", (self.repo / "DESCRIPTION").read_text())
        self.assertEqual((self.repo / "NEWS.md").read_text(), "# example (development version)\n")
        bump_sha = self.git(self.repo, "rev-parse", "HEAD").stdout.strip()
        result = self.bump()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.git(self.repo, "rev-parse", "HEAD").stdout.strip(), bump_sha)
        self.assertIn("already been bumped", result.stdout)
        result = self.bump(bump_sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.git(self.repo, "rev-parse", "HEAD").stdout.strip(), bump_sha)

    def test_each_of_two_pushes_is_bumped_even_if_processed_out_of_order(self):
        second = self.commit("Documentation only")
        for sha in (second, self.sha):
            result = self.bump(sha)
            self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0.9003", (self.repo / "DESCRIPTION").read_text())

    def test_skip_preserves_release_and_blocks_delayed_bump(self):
        (self.repo / "DESCRIPTION").write_text("Package: example\nVersion: 0.4.0\n")
        self.git(self.repo, "add", "DESCRIPTION")
        self.git(self.repo, "commit", "-m", "[skip vbump] Release")
        self.git(self.repo, "push", "origin", "main")
        release = self.git(self.repo, "rev-parse", "HEAD").stdout.strip()
        for sha in (self.sha, release):
            result = self.bump(sha)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("Version: 0.4.0\n", (self.repo / "DESCRIPTION").read_text())
        next_sha = self.commit("Start development")
        result = self.bump(next_sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0.9000", (self.repo / "DESCRIPTION").read_text())

    def test_push_rejection_is_reported(self):
        hook = self.remote / "hooks" / "pre-receive"
        hook.write_text("#!/bin/sh\nexit 1\n")
        hook.chmod(0o755)
        result = self.bump()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("bypass access", result.stderr)

    def test_retries_after_concurrent_push(self):
        other = self.repo.parent / "other"
        self.git(self.repo.parent, "clone", str(self.remote), str(other))
        self.git(other, "config", "user.name", "Test")
        self.git(other, "config", "user.email", "test@example.com")
        hook = self.repo / ".git" / "hooks" / "pre-push"
        hook.write_text(
            "#!/bin/sh\n"
            "unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE\n"
            f"if [ ! -f '{other / 'raced'}' ]; then\n"
            f"  echo change > '{other / 'raced'}'\n"
            f"  git -C '{other}' add raced\n"
            f"  git -C '{other}' commit -m concurrent\n"
            f"  git -C '{other}' push origin main\n"
            "fi\n"
        )
        hook.chmod(0o755)
        result = self.bump()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("retrying", result.stdout)
        self.assertIn("Version: 0.4.0.9002", (self.repo / "DESCRIPTION").read_text())
        self.assertTrue((self.repo / "raced").exists())

    def test_release_during_retry_is_not_overwritten(self):
        other = self.repo.parent / "other"
        self.git(self.repo.parent, "clone", str(self.remote), str(other))
        self.git(other, "config", "user.name", "Test")
        self.git(other, "config", "user.email", "test@example.com")
        hook = self.repo / ".git" / "hooks" / "pre-push"
        hook.write_text(
            "#!/bin/sh\n"
            "unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE\n"
            f"echo 'Package: example\nVersion: 0.4.0' > '{other / 'DESCRIPTION'}'\n"
            f"git -C '{other}' add DESCRIPTION\n"
            f"git -C '{other}' commit -m '[skip vbump] Release'\n"
            f"git -C '{other}' push origin main\n"
        )
        hook.chmod(0o755)
        result = self.bump()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("preserving its version", result.stdout)
        self.assertIn("Version: 0.4.0\n", (self.repo / "DESCRIPTION").read_text())

    def test_missing_trigger_is_an_error(self):
        other = self.repo.parent / "other"
        self.git(self.repo.parent, "clone", str(self.remote), str(other))
        self.git(other, "config", "user.name", "Test")
        self.git(other, "config", "user.email", "test@example.com")
        self.git(other, "checkout", "--orphan", "other")
        self.git(other, "commit", "--allow-empty", "-m", "Unrelated")
        unrelated = self.git(other, "rev-parse", "HEAD").stdout.strip()
        self.git(other, "push", "origin", "HEAD:refs/heads/other")
        self.git(self.repo, "fetch", "origin", "other")
        result = self.bump(unrelated)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no longer in main's history", result.stderr)


if __name__ == "__main__":
    unittest.main()
