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

run_code() {
  local candidate
  while IFS= read -r candidate; do
    if [[ "$candidate" == *"/.vscode-server/"*"/remote-cli/code" ]]; then
      if [[ -n "${VSCODE_IPC_HOOK_CLI:-}" && -S "${VSCODE_IPC_HOOK_CLI}" ]]; then
        if "$candidate" "$@"; then
          return 0
        fi
      fi
      continue
    fi

    if env -u VSCODE_IPC_HOOK_CLI "$candidate" "$@"; then
      return 0
    fi
  done < <(type -P -a "$code_cmd" 2>/dev/null || printf '%s\n' "$code_cmd")

  return 1
}

install_dev_vsix() {
  echo "Packaging and installing the OptiTrust extension..."
  ./node_modules/.bin/vsce package --out "$vsix_path" --no-dependencies
  run_code --install-extension "$vsix_path" --force
}

cd "$extension_dir"
npm run compile

if run_code --help 2>&1 | grep -q -- "--extensionDevelopmentPath"; then
  echo "Opening VS Code Extension Development Host..."
  run_code --new-window --extensionDevelopmentPath="$extension_dir" "$repo_root"
  install_dev_vsix
else
  echo "The '$code_cmd' CLI does not support --extensionDevelopmentPath."
  install_dev_vsix
  echo "Opening OptiTrust with the installed extension..."
  run_code --new-window "$repo_root"
fi
