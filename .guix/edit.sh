#! /usr/bin/env bash

PRJ="$1"

if test -d $PRJ/.guix && test -f $PRJ/.emacs && test -f $PRJ/.bashrc;
then
  if test -f $PRJ/.guix/channels.scm && test -f $PRJ/.guix/manifest.scm;
  then
    guix time-machine --channels=$PRJ/.guix/channels.scm -- \
    shell --pure --manifest=$PRJ/.guix/manifest.scm -- \
    bash --norc -c "source .bashrc && emacs -q -l .emacs"
    exit 0
  fi
fi

echo "CHYBA: Tento priečinok neobsahuje platné určenie Guix prostredia! Končím."
exit 1
