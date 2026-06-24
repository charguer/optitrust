#!/usr/bin/env bash
# Compile the OptiTrust VS Code extension and open it for manual testing.
#
# Preferred path: use VS Code's Extension Development Host when the installed
# CLI supports --extensionDevelopmentPath.
#
# Fallback path: some remote/reduced "code" CLIs cannot open an Extension
# Development Host. For those, build a local VSIX, install/update it, and open
# the OptiTrust workspace in a normal window.
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
extension_dir="$(cd "$script_dir/.." && pwd)"
repo_root="$(cd "$extension_dir/../.." && pwd)"
code_cmd="${CODE_CMD:-code}"
vsix_path="$extension_dir/.optitrust-dev.vsix"

cd "$extension_dir"
npm run compile

if "$code_cmd" --help 2>&1 | grep -q -- "--extensionDevelopmentPath"; then
  echo "Opening VS Code Extension Development Host..."
  "$code_cmd" --new-window --extensionDevelopmentPath="$extension_dir" "$repo_root"
else
  echo "The '$code_cmd' CLI does not support --extensionDevelopmentPath."
  echo "Packaging and installing the OptiTrust extension instead..."
  ./node_modules/.bin/vsce package --out "$vsix_path" --no-dependencies
  "$code_cmd" --install-extension "$vsix_path" --force
  echo "Opening OptiTrust with the installed extension..."
  "$code_cmd" --new-window "$repo_root"
fi
