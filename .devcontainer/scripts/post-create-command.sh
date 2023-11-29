#!/usr/bin/env bash
# Last modified: 2023 Nov 26

# This script is executed after the creation of the project's development container.
# It performs the following tasks:
# 1. Ensures that `$HOME/.bashrc.d` files are sourced.
# 2. Sets up the `bashrc_d` configuration.
# 3. Adds the `config-r.sh` file to be sourced if it's not already present.
# 4. Sources the `config-r.sh` file.
# 5. Sources the `config-r-vscode.sh` file if the environment is GitPod.
# 6. Adds the `config-r-vscode.sh` file to be sourced if it's not already present in the case of CodeSpaces.
# 7. Sources the `config-r-vscode.sh` file in the case of CodeSpaces.

# ensure that `$HOME/.bashrc.d` files are sourced
echo " "
echo "==================="
echo "run post-create-command.sh"
echo "-------------------"

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

echo "set up bashrc_d"
"$project_root/.devcontainer/scripts/config-bashrc-d.sh" || exit 1
echo "completed setting up bashrc_d"
echo "-------------------"

# add config-r.sh to be sourced if 
# it's not already present
echo "add config-r.sh to be sourced if it's not already present"
if ! [ -e "$HOME/.bashrc.d/config-r.sh" ]; then
  echo "config-r.sh not found, adding it"
  if ! [ -d "$HOME/.bashrc.d" ]; then mkdir -p "$HOME/.bashrc.d"; fi
  echo "copying config-r.sh to $HOME/.bashrc.d"
  cp "$project_root/.devcontainer/scripts/config-r.sh" "$HOME/.bashrc.d/" || exit 1
  chmod 755 "$HOME/.bashrc.d/config-r.sh"
fi
echo "completed adding config-r.sh to be sourced if it's not already present"
"$HOME/.bashrc.d/config-r.sh"
echo "Sourced config-r.sh"

# source config-r-vscode.sh if on GitPod
if [ -n "$(env | grep -E "^GITPOD")" ]; then
  echo "-------------------"
  echo "Sourcing config-r-vscode.sh"
  "$project_root/.devcontainer/scripts/config-r-vscode.sh" 
  echo "Sourced config-r-vscode.sh"
elif [ -n "$(env | grep -E "^CODESPACES")" ]; then
  echo "-------------------"
  # add config-r-vscode.sh to be sourced if 
  # it's not already present
  echo "add config-r-vscode.sh to be sourced if it's not already present"
  if ! [ -e "$HOME/.bashrc.d/config-r-vscode.sh" ]; then
    echo "config-r-vscode.sh not found, adding it"
    if ! [ -d "$HOME/.bashrc.d" ]; then mkdir -p "$HOME/.bashrc.d"; fi
    echo "copying config-r-vscode.sh to $HOME/.bashrc.d"
    cp "$project_root/.devcontainer/scripts/config-r-vscode.sh" "$HOME/.bashrc.d/" || exit 1
    chmod 755 "$HOME/.bashrc.d/config-r-vscode.sh"
  fi
  echo "completed adding config-r-vscode.sh to be sourced if it's not already present"
  "$HOME/.bashrc.d/config-r-vscode.sh"
  echo "Sourced config-r-vscode.sh"
fi
