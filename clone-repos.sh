#!/usr/bin/env bash

# Get the absolute path of the script folder
script_folder="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"

# Get the absolute path of the workspaces folder
workspaces_folder="$(cd "${script_folder}/.." && pwd)"

# Function to clone a repository
clone-repo()
{
    cd "${workspaces_folder}"
    if [ ! -d "${1#*/}" ]; then
        git clone "https://github.com/$1"
    else 
        echo "Already cloned $1"
    fi
}

# If running in a Codespace, set up Git credentials
if [ "${CODESPACES}" = "true" ]; then
    # Remove the default credential helper
    sudo sed -i -E 's/helper =.*//' /etc/gitconfig

    # Add one that just uses secrets available in the Codespace
    git config --global credential.helper '!f() { sleep 1; echo "username=${GITHUB_USER}"; echo "password=${GH_TOKEN}"; }; f'
fi

# If there is a list of repositories to clone, clone them
if [ -f "${script_folder}/repos-to-clone.list" ]; then
    while IFS= read -r repository; do
        clone-repo "$repository"
    done < "${script_folder}/repos-to-clone.list"
fi

# Unset GPG signing configuration
git config --global --unset commit.gpgsign
git config --unset commit.gpgsign
