#!/bin/bash

# Skip in case there is nothing staged
if git diff --staged --quiet
then
  exit 0
fi

STASH_NAME="pre-commit-$(date +%s)"
git stash push --quiet --keep-index --include-untracked -m $STASH_NAME

pushd $(git rev-parse --show-toplevel) > /dev/null

./tester run
TESTER_RESULT=$?

popd > /dev/null

if git stash list | head -n 1 | grep -q ${STASH_NAME}
then
  git restore --worktree -s stash@{0} :/
  git stash drop --quiet
fi

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
