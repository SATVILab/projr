#!/usr/bin/env bash
# Last modified: 2023 Dec 13

# This script is executed each time the container starts.
# 1. Sets up bashrc
# 2. Sets up R
# 3. Ensure that latest versions of key R packages are installed

# ensure that `$HOME/.bashrc.d` files are sourced
echo " "
echo "==================="
echo "run post-create-command.sh"
echo "-------------------"
project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

# set up bashrc
"$project_root/.devcontainer/scripts/post-command-bashrc.sh"

# set up r
"$project_root/.devcontainer/scripts/post-command-r.sh"

# install latest versions of R packages
"$project_root/.devcontainer/scripts/post-create-command-r.sh"
