Bootstrap: library
From: ubuntu:20.04

%environment
  export LC_ALL=C
  HOME=/container
  export HOME
  export OPTITRUST=`pwd`/optitrust

%files
  /optitrust/ /container/optitrust

%post
  chmod 777 /container
  HOME=/container
  export HOME
  apt-get update && apt-get -y upgrade
  apt -y install software-properties-common rsync wget gpg bc

  wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
  | gpg --dearmor | tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

  echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
  https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list

  add-apt-repository universe
  apt-get update && apt-get -y upgrade
  apt install debconf-utils -y
  echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
  apt-get install -y binutils binutils-common binutils-x86-64-linux-gnu build-essential cmake  \
  cmake-data cpp cpp-9 dpkg-dev fakeroot g++ g++-9 gcc gcc-9 gcc-9-base  \
  intel-hpckit-getting-started  \
  intel-oneapi-common-licensing intel-oneapi-common-licensing-2022.0.0  \
  intel-oneapi-common-licensing-2022.0.1 intel-oneapi-common-vars  \
  intel-oneapi-compiler-cpp-eclipse-cfg intel-oneapi-compiler-dpcpp-cpp  \
  intel-oneapi-compiler-shared-2022.0.2  \
  intel-oneapi-compiler-shared-common-2022.0.2  \
  intel-oneapi-compiler-shared-runtime-2022.0.2 intel-oneapi-condaindex  \
  intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic \
  intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic-2022.0.2 \
  intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic-common-2022.0.2 \
  intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic-runtime-2022.0.2 \
  intel-oneapi-compiler-dpcpp-cpp-classic-fortran-shared-runtime-2022.0.2 \
  intel-oneapi-dev-utilities-2021.5.2 intel-oneapi-dev-utilities-eclipse-cfg  \
  intel-oneapi-icc-eclipse-plugin-cpp-2022.0.2 intel-oneapi-inspector  \
  intel-oneapi-libdpstd-devel-2021.6.0 intel-oneapi-mkl-2022.0.2  \
  intel-oneapi-mpi-2021.5.1 intel-oneapi-mpi-devel  \
  intel-oneapi-mpi-devel-2021.5.1 intel-oneapi-onevpl-2022.0.0  \
  intel-oneapi-openmp-2022.0.2 intel-oneapi-openmp-common-2022.0.2  \
  libalgorithm-diff-xs-perl libalgorithm-merge-perl libarchive13 libasan5  \
  libatomic1 libbinutils libbsd0 libc-dev-bin libc6-dev libcc1-0 libcrypt-dev  \
  libctf-nobfd0 libctf0 libcurl4 libdpkg-perl libdrm-common libdrm2  \
  libfakeroot libfile-fcntllock-perl libgcc-9-dev libgdbm-compat4 libgdbm6  \
  libgomp1 libisl22 libitm1 libjsoncpp1 liblocale-gettext-perl liblsan0  \
  libmpc3 libmpfr6 libnghttp2-14 libpciaccess0 libperl5.30 libquadmath0  \
  librhash0 librtmp1 libssh-4 libssl-dev libstdc++-9-dev libtsan0 libubsan1  \
  libuv1 libx11-6 libx11-data libxau6 libxcb1 libxdmcp6 libxext6 libxfixes3  \
  linux-libc-dev make manpages manpages-dev netbase patch perl  \
  perl-modules-5.30 libfftw3-dev libjemalloc-dev hwloc clang-format meld libclang-dev llvm-dev libomp-dev pkg-config zlib1g-dev 
  apt-get install opam -y
  opam init -y --auto-setup --root /container/.opam
  opam update 
  opam switch create 4.12.0 -y
  opam pin add menhirLib 20210419 -y
  opam pin add pprint 20220103 -y
  eval `opam config env`
  opam install dune clangml pprint menhir menhirLib base64 ocamlbuild -y
  cd $HOME/optitrust && make install
  
%runscript
	eval `opam config env`
	if [ $# = 0 ] ; then exec bash; else exec "$@"; fi

%help
  This container contains everything you need to be able to run OptiTrust and its benchmarks.
