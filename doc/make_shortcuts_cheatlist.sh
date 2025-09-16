#!/bin/bash

# Usage: in the current folder, type "./make_shortcut_cheatlist.sh"

# Extract the key-args keybindings pairs from the INSTALL.md file using sed.
# Then use awk to add some padding to the output.

FILEIN="../INSTALL.md"
FILEOUT="shortcut_cheatlist.txt"

grep -E '"(key|args)"' "${FILEIN}" \
  | sed -E 's/.*"(key|args)"[[:space:]]*:[[:space:]]*"([^"]*)".*/\2/' \
  | awk 'NR % 2 { printf "%-19s", $0; next } { print $0 }' > ${FILEOUT}

echo "Generated ${FILEOUT} by extracting information from ${FILEIN}."
