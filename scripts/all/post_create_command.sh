#!/usr/bin/env bash
# ensure that `$HOME/.bashrc.d` files are sourced
project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 
"$project_root/scripts/all/setup_bashrc_d.sh" || exit 1

# add config_r.sh to be sourced if 
# it's not already present
if ! [ -e "$HOME/.bashrc.d/config_r.sh" ]; then
  if ! [ -d "$HOME/.bashrc.d" ]; then mkdir -p "$HOME/.bashrc.d"; fi
  cp "$project_root/scripts/all/config_r.sh" "$HOME/.bashrc.d/" || exit 1
  chmod 755 "$HOME/.bashrc.d/config_r.sh"
fi

"$project_root/scripts/ubuntu/install_apptainer.sh" || exit 1
"$project_root/scripts/ubuntu/install_gh.sh" || exit 1

if [ -n "$(env | grep -E "^GITPOD")" ]; then
  # install tools to run and download containers
  sudo "$project_root/scripts/all/install_r.sh"
  sudo "$project_root/scripts/all/install_quarto.sh"
fi

# clone all repos
"$project_root/clone-repos.sh"
