#!/usr/bin/env bash
# Last modified: 2023 Nov 24

# Clones all repos in repos-to-clone.list
# into the parent directory of the current
# working directory.

# Get the absolute path of the current working directory
current_dir="$(pwd)"

# Determine the parent directory of the current directory
parent_dir="$(cd "${current_dir}/.." && pwd)"


# Function to clone a repository
clone-repo()
{
    cd "${parent_dir}"
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
if [ -f "./repos-to-clone.list" ]; then
    while IFS= read -r repository || [ -n "$repository" ]; do
        # Skip lines that are empty or contain only whitespace
        if [[ -z "$repository" || "$repository" =~ ^[[:space:]]*$ || "$repository" =~ ^[[:space:]]*# ]]; then
            continue
        fi

        clone-repo "$repository"
    done < "./repos-to-clone.list"
fi