import os
from pathlib import Path
import subprocess
import tempfile
import unittest


WORKFLOW = Path(__file__).resolve().parents[1] / "workflows" / "version-bump.yaml"


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
        self.script = root / "version-bump.sh"
        script = subprocess.run(
            ["ruby", "-ryaml", "-e",
             'puts YAML.load_file(ARGV[0])["jobs"]["vbump"]["steps"].last["run"]',
             str(WORKFLOW)],
            check=True, capture_output=True, text=True
        ).stdout
        self.script.write_text(script)
        (root / "bin").mkdir()
        rscript = root / "bin" / "Rscript"
        rscript.write_text(
            """#!/usr/bin/env python3
import pathlib, re, sys
path = pathlib.Path(sys.argv[3] if len(sys.argv) > 3 else "DESCRIPTION")
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

    def bump(self, **env):
        return subprocess.run(
            ["bash", str(self.script)], cwd=self.repo, env={**self.env, **env},
            capture_output=True, text=True
        )

    def test_pushes_bump_and_keeps_development_news(self):
        result = self.bump(PUSH_SHA=self.sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0.9002", (self.repo / "DESCRIPTION").read_text())
        self.assertEqual((self.repo / "NEWS.md").read_text(), "# example (development version)\n")
        self.assertEqual(self.git(self.remote, "rev-parse", "main").stdout.strip(),
                         self.git(self.repo, "rev-parse", "HEAD").stdout.strip())

    def test_old_push_does_not_bump_after_release_skip(self):
        (self.repo / "DESCRIPTION").write_text("Package: example\nVersion: 0.4.0\n")
        self.git(self.repo, "add", "DESCRIPTION")
        self.git(self.repo, "commit", "-m", "[skip vbump] Release")
        self.git(self.repo, "push", "origin", "main")
        result = self.bump(PUSH_SHA=self.sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0\n", (self.repo / "DESCRIPTION").read_text())
        self.assertIn("release skip", result.stdout)

    def test_push_rejection_is_reported(self):
        hook = self.remote / "hooks" / "pre-receive"
        hook.write_text("#!/bin/sh\nexit 1\n")
        hook.chmod(0o755)
        result = self.bump(PUSH_SHA=self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("branch protection", result.stderr)

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
        result = self.bump(PUSH_SHA=self.sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("retrying", result.stdout)
        self.assertIn("Version: 0.4.0.9002", (self.repo / "DESCRIPTION").read_text())
        self.assertTrue((self.repo / "raced").exists())

    def test_after_release_creates_development_news_section(self):
        (self.repo / "DESCRIPTION").write_text("Package: example\nVersion: 0.4.0\n")
        (self.repo / "NEWS.md").write_text("# example 0.4.0\n\nRelease notes\n")
        self.git(self.repo, "add", ".")
        self.git(self.repo, "commit", "-m", "[skip vbump] Release")
        self.git(self.repo, "tag", "v0.4.0")
        self.git(self.repo, "push", "origin", "main", "v0.4.0")
        result = self.bump(VBUMP_AFTER_RELEASE="true", RELEASE_REF="refs/tags/v0.4.0")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Version: 0.4.0.9000", (self.repo / "DESCRIPTION").read_text())
        self.assertEqual((self.repo / "NEWS.md").read_text(),
                         "# example 0.4.0.9000\n\n# example 0.4.0\n\nRelease notes\n")


if __name__ == "__main__":
    unittest.main()
