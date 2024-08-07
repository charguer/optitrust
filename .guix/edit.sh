#! /usr/bin/env bash

DEVHOME="$1"

if test -d $DEVHOME/.guix && test -f $DEVHOME/.emacs;
then
  if test -f $DEVHOME/.guix/channels.scm && test -f $DEVHOME/.guix/manifest.scm;
  then
    guix time-machine --channels=$DEVHOME/.guix/channels.scm -- \
    shell --pure --manifest=$DEVHOME/.guix/manifest.scm -- \
    emacs -q -l .emacs
    exit 0
  fi
fi

echo "CHYBA: Tento priečinok neobsahuje platné určenie Guix prostredia! Končím."
exit 1
