#!/bin/bash
#
# Script to display the list of OptiTrust shorcuts in use
# Usage: ./shortcuts.sh
#
# Disclaimer: assumes the key binding file to be located in ~/.config/Code/User/keybindings.json

SOURCE=~/.config/Code/User/keybindings.json

cat ${SOURCE} \
 | tr '\n' ' ' \
 | sed 's/\"key\":/\n/g' \
 | grep '"when": "config.optitrust.enableKeybindings"' \
 | sed 's/\"\([^"]*\)\".*\"args\":[^"]*\"\([^"]*\)\".*$/\2 ===> \1/g' \
 | sort


