Bootstrap: library
From: ubuntu:20.04

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
apt-get install opam
opam init
opam update && opam upgrade && eval $(opam env)
opam switch install 4.12.0
eval $(opam env)
opam pin add menhirLib 20210419 -y
opam pin add pprint 20220103 -y
opam install dune clangml pprint menhir menhirLib base64 ocamlbuild

%runscript





