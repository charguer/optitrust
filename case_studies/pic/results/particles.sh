#!/bin/bash

# Reports the number of million particles that can be fit on a given RAM size.
#
# usage: ./particles.sh ram_in_gb
# usage: V=1 ./particles.sh
#
# - where `ram_in_gb` denotes the number of GB of RAM available on the machine
#   if not provided, it will be reported automatically
#
#  - V=1 option is to display computation details, and pretty printed output

MEMBOUND=$1

# LATER: take as arguments these values
GRIDSIDE="64"
CHUNKSIZE="256"
# PARTICLESIZE in bytes
PARTICLESIZE="36"
MARGIN="0.9"

if [ -z "${MEMBOUND}" ]; then
  MEMBOUND=`grep MemTotal /proc/meminfo | awk '{print $2 / 1024 / 1024}'`
  if [ ! -z "${v}" ]; then
    echo "Memory bound not provided; reading it from /proc/meminfo:"
    echo "   Total memory available: ${MEMBOUND} GB"
  fi
fi


NBCELLS=`echo "${GRIDSIDE} * ${GRIDSIDE} * ${GRIDSIDE}" | bc`
# CHUNKMEM in bytes
CHUNKMEM=`echo "${CHUNKSIZE} * ${PARTICLESIZE}" | bc`
# GRIDEXTRAMEM in GB
GRIDEXTRAMEM=`echo "scale=3; 4 * ${NBCELLS} * ${CHUNKMEM} / 1024 / 1024 / 1024" | bc`
# PARTICLESMEM in GB
PARTICLESMEM=`echo "scale=3; ${MEMBOUND} - ${GRIDEXTRAMEM}" | bc`
# PARTICLES in millions
PARTICLESMILLIONS=`echo "${MARGIN} * 1000 * ${PARTICLESMEM} / ${PARTICLESIZE}" | bc`
# PARTICLES in units
PARTICLES=`echo "${PARTICLESMILLIONS} * 1000 * 1000" | bc`

if [ ! -z "${v}" ]; then
  echo "NBCELLS = ${NBCELLS}"
  echo "CHUNKMEM = ${CHUNKMEM}"
  echo "GRIDEXTRAMEM = ${GRIDEXTRAMEM}"
  echo "PARTICLESMEM = ${PARTICLESMEM}"
  echo "PARTICLESMILLIONS = ${PARTICLESMILLIONS}"
  echo "PARTICLES = ${PARTICLES}"
  echo "For ${MEMBOUND} GB of RAM, Maximal number of particles: ${PARTICLES} million"
fi

echo ${PARTICLES}

