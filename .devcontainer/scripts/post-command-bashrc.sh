#!/usr/bin/env bash
# Last modified: 2023 Dec 13

# 1. Ensures that `$HOME/.bashrc.d` files are sourced.
# 2. Sets up the `bashrc_d` configuration.

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

echo "set up bashrc_d"
"$project_root/.devcontainer/scripts/config-bashrc-d.sh" || exit 1
echo "completed setting up bashrc_d"
echo "-------------------"
