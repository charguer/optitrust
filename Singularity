Bootstrap: library
From: ubuntu:18.04

%post:
apt-get update && apt-get -y upgrade
apt -y install software-properties-common rsync wget gpg bc

wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
| gpg --dearmor | tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list

add-apt-repository universe
apt-get update && apt-get -y upgrade
apt-get install intel-basekit intel-hpckit libfftw3-dev libjemalloc-dev hwloc

%runscript





