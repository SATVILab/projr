#!/usr/bin/env bash
# Last modified: 2023 Dec 13

# 1. Adds the `config-r.sh` file to be sourced if it's not already present.
# 2. Sources the `config-r.sh` file.
# 3. Sources the `config-r-vscode.sh` file if the environment is GitPod.
# 4. Adds the `config-r-vscode.sh` file to be sourced if it's not already present in the case of CodeSpaces.
# 5. Sources the `config-r-vscode.sh` file in the case of CodeSpaces.

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

echo "set up further R config"
# add config-r.sh
if ! [ -d "$HOME/.bashrc.d" ]; then mkdir -p "$HOME/.bashrc.d"; fi
echo "copying config-r.sh to $HOME/.bashrc.d"
cp "$project_root/.devcontainer/scripts/config-r.sh" "$HOME/.bashrc.d/" || exit 1
chmod 755 "$HOME/.bashrc.d/config-r.sh"
echo "copied config-r.sh to $HOME/.bashrc.d"
"$HOME/.bashrc.d/config-r.sh"
echo "Sourced config-r.sh"

# add config-r-vscode.sh
if [ -n "$(env | grep -E "^GITPOD")" ]; then
  # On GitPod, only source
  echo "-------------------"
  echo "Sourcing config-r-vscode.sh"
  "$project_root/.devcontainer/scripts/config-r-vscode.sh" 
  echo "Sourced config-r-vscode.sh"
elif [ -n "$(env | grep -E "^CODESPACES")" ]; then
  echo "-------------------"
  # add config-r-vscode.sh to be sourced if 
  # it's not already present
  if ! [ -d "$HOME/.bashrc.d" ]; then mkdir -p "$HOME/.bashrc.d"; fi
  echo "copying config-r-vscode.sh to $HOME/.bashrc.d"
  cp "$project_root/.devcontainer/scripts/config-r-vscode.sh" "$HOME/.bashrc.d/" || exit 1
  chmod 755 "$HOME/.bashrc.d/config-r-vscode.sh"
  echo "copied config-r-vscode.sh to $HOME/.bashrc.d"
  "$HOME/.bashrc.d/config-r-vscode.sh"
  echo "Sourced config-r-vscode.sh"
fi

echo "completed setting up further R config"
echo "-------------------"
