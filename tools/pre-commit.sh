#!/bin/bash

shopt -s nullglob

# Skip in case there is nothing staged
if git diff --staged --quiet
then
  exit 0
fi

pushd $(git rev-parse --show-toplevel) > /dev/null

# If we are a merge commit, save the merge status since the stash command will break it
if [ -f .git/MERGE_HEAD ]
then
  MERGE_FILES=(MERGE_*)
  if [ ${#MERGE_FILES[@]} -gt 0 ]
  then
    echo "Saved merge is already present in the worktree" > /dev/stderr
    echo "Need to remove files: ${MERGE_FILES[@]}" > /dev/stderr
    exit 1
  fi
  mv .git/MERGE_* .
fi

STASH_NAME="Stash before pre-commit ($(date))"
git stash push --quiet --keep-index --include-untracked -m "${STASH_NAME}"

./tester run
TESTER_RESULT=$?

if git stash list | head -n 1 | grep -q "${STASH_NAME}"
then
  git restore --worktree -s stash@{0} :/
  # The untracked part is somehow not included in the main stash but in stash^3
  git restore --worktree --overlay -s stash@{0}^3 :/
  git stash drop --quiet

  # Restore the merge status
  if [ -f MERGE_HEAD ]
  then
    mv MERGE_* .git
  fi
fi

popd > /dev/null

if [ $TESTER_RESULT != 0 ]
then
  echo "Commit aborted because tests are failing." > /dev/stderr
  echo "Add the failing tests to ignore list if you want to proceed." > /dev/stderr
  echo "Otherwise, rerun with --no-verify to force the commit." > /dev/stderr
  if ! git diff --quiet
  then
    echo "Be careful, some changes in the work tree were not included in the commit." > /dev/stderr
  fi
fi
exit $TESTER_RESULT
