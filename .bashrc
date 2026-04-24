export OPTITRUST=$(pwd)

LIBCLANG=$(clang --version | head -n 1 | cut -d ' ' -f 3)
LIBCLANG=$GUIX_ENVIRONMENT/lib/clang/$LIBCLANG/include
export C_INCLUDE_PATH=$C_INCLUDE_PATH:$LIBCLANG
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$LIBCLANG

export XDG_DATA_DIRS=/usr/share/xfce4:/usr/local/share/:/usr/share/:/usr/share
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME=/home/marek/.config

export FREETYPE_PROPERTIES=truetype:interpreter-version=35

export PATH=$PATH:$OPTITRUST/_build/install/default/bin/
