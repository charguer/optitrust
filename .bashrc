export OPTITRUST=$(pwd)

LIBCLANG=$(clang --version | head -n 1 | cut -d ' ' -f 3)
LIBCLANG=$GUIX_ENVIRONMENT/lib/clang/$LIBCLANG/include
export C_INCLUDE_PATH=$C_INCLUDE_PATH:$LIBCLANG
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$LIBCLANG

XDG_DATA_DIRS=/usr/share/xubuntu:/usr/share/xfce4
export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
export XDG_DATA_DIRS=$XDG_DATA_DIRS:/var/lib/snapd/desktop:/usr/share
export XDG_CONFIG_DIRS=/etc/xdg/xdg-xubuntu:/etc/xdg:/etc/xdg

export FREETYPE_PROPERTIES=truetype:interpreter-version=35

export PATH=$PATH:$OPTITRUST/_build/install/default/bin/
