#!/bin/bash

# Generate "config_`hostname`.sh" with parameters

# usage: ./autoconfig.sh [machine]

MACHINE=`hostname`
OUTPUT="config_${MACHINE}.sh"

#---------------------------------------------
# Number of cores

# number of cores, including hyperthreading
# grep -c ^processor /proc/cpuinfo

# number of cores, excluding hyperthreading

# this command does not seem to work, we inline it later
NBCORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq | awk '{print $4}')

#---------------------------------------------
# Number of particles

NBPARTICLES=$(./particles.sh)

#---------------------------------------------
# COMPILERS AVAILABLE

COMPILERS=""
if command -v gcc &> /dev/null; then
  COMPILERS+="gcc "
fi
if command -v icc &> /dev/null; then
  COMPILERS+="icc "
fi

#---------------------------------------------
# Generated output

echo "#!/bin/bash" > ${OUTPUT}
echo "nb_cores=\"${NBCORES}\"" >> ${OUTPUT}
#grep ^cpu\\scores /proc/cpuinfo | uniq | awk '{print nb_cores=$4}' >> ${OUTPUT}
echo "compilers=\"${COMPILERS}\"" >> ${OUTPUT}
echo "nb_particles=${NBPARTICLES}" >> ${OUTPUT}


#---------------------------------------------
# Final message

chmod +x ${OUTPUT}
echo "Generated ${OUTPUT} with contents:"
cat ${OUTPUT}
echo "==> Don't forget to run:   git add ${OUTPUT}"

