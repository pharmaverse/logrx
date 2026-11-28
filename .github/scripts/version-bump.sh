#!/usr/bin/env bash
set -euo pipefail

git config --global --add safe.directory "$PWD"
git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"

for attempt in {1..20}; do
  git fetch origin main
  git checkout -B main FETCH_HEAD
  base=$(git rev-parse HEAD)

  if ! git merge-base --is-ancestor "$PUSH_SHA" "$base"; then
    echo "The triggering push is no longer in main's history; refusing to bump." >&2
    exit 1
  fi
  message=$(git log -1 --format=%B "$PUSH_SHA")
  if [[ "$message" == *'[skip vbump]'* ]]; then
    echo "The triggering commit requested a version bump skip."
    exit 0
  fi
  if [[ "$message" == *'[automated vbump]'* ]]; then
    echo "The triggering commit is an automated version bump."
    exit 0
  fi
  if [[ -n $(git log --format=%H --fixed-strings --grep="Version-bump-for: $PUSH_SHA") ]]; then
    echo "This push has already been bumped."
    exit 0
  fi
  if [[ -n $(git log --format=%H --fixed-strings --grep='[skip vbump]' "$PUSH_SHA..$base") ]]; then
    echo "A later commit requested a version bump skip; preserving its version."
    exit 0
  fi

  Rscript -e 'desc::desc_bump_version("dev", normalize = TRUE)'
  version=$(Rscript -e 'cat(as.character(desc::desc_get_version()))')
  git add DESCRIPTION
  git commit -m "Bump development version to $version [automated vbump]" \
    -m "Version-bump-for: $PUSH_SHA"

  if git push origin HEAD:refs/heads/main; then
    exit 0
  fi

  git fetch origin main
  if [[ $(git rev-parse FETCH_HEAD) == "$base" ]]; then
    echo "Push failed; check the GitHub App's contents permission and bypass access to main." >&2
    exit 1
  fi
  echo "Main changed during the bump; retrying against its latest version."
done

echo "Could not push the version bump after 20 attempts; rerun this workflow." >&2
exit 1
