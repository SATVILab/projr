#!/usr/bin/env bash
# Last modified: 2023 Nov 30

# This script is executed each time the container starts.
# 1. Ensures that `$HOME/.bashrc.d` files are sourced.
# 2. Sets up the `bashrc_d` configuration.
# 3. Adds the `config-r.sh` file to be sourced if it's not already present.
# 4. Sources the `config-r.sh` file.
# 5. Sources the `config-r-vscode.sh` file if the environment is GitPod.
# 6. Adds the `config-r-vscode.sh` file to be sourced if it's not already present in the case of CodeSpaces.
# 7. Sources the `config-r-vscode.sh` file in the case of CodeSpaces.
# 8. Clones all repositories in repos-to-clone.list.
# 9. Adds all repositories in repos-to-clone.list to the workspace file (EntireProject.code-workspace).

# ensure that `$HOME/.bashrc.d` files are sourced
echo " "
echo "==================="
echo "run post-start-command.sh"
echo "-------------------"
project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

# ensure that `$HOME/.bashrc.d` files are sourced
echo "set up bashrc_d"
"$project_root/.devcontainer/scripts/config-bashrc-d.sh" || exit 1
echo "completed setting up bashrc_d"
echo "-------------------"

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

# clone all repos
echo "Cloning all repos in repos-to-clone.list"
"$project_root/.devcontainer/scripts/clone-repos.sh"
echo "Cloned all repos in repos-to-clone.list"
echo "-------------------"

# add all repos to workspace
echo "Adding all repos in repos-to-clone.list to the workspace file (EntireProject.code-workspace)"
"$project_root/.devcontainer/scripts/add-repos.sh"
echo "Added all repos"
echo "-------------------"
