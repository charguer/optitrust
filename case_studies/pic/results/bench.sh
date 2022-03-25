#!/bin/bash

# usage: ./bench.sh  machine action
# - where machine is the machine name, for which config_machine.sh must be available

MACHINE=$1
ACTION=$2

# The environment variable FAST=1 can be used to request a smaller simulation

if [ -z "${MACHINE}" ]; then
  echo "Bench: missing machine name"
  exit 1
fi

if [ -z "${ACTION}" ]; then
  ACTION="all"
fi

#--------------------------------------------------------------------------------
# Parameters

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "params" ] ; then


// nb_particles  = 10000000;


lstopo --of pdf > lstopo.pdf
lstopo-no-graphics > lstopo.txt



Save all your experimental results in a file in the git, in a folder  pic/results, with one subfolder per machine, each time you save the contents of "your_configuration"



-cat /proc/cpuinfo
-


if ! command -v <the_command> &> /dev/null
then
    echo "<the_command> could not be found"
    exit
fi