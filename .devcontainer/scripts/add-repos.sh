#!/usr/bin/env bash
# Last modified: 2023 Nov 24

# This script is used to add repositories to a workspace JSON file,
# which can be used to open all repos in a multi-root VS Code workspace.
# It reads a list of repositories from "repos-to-clone.list"
# and adds them to the workspace JSON file.

# Get the absolute path of the current working directory
current_dir="$(pwd)"

# Define the path to the workspace JSON file
workspace_file="${current_dir}/EntireProject.code-workspace"

# Create the workspace file if it does not exist
if [ ! -f "$workspace_file" ]; then
  echo "Workspace file does not exist. Creating it now..."
  echo '{"folders": [{"path": "."}]}' > "$workspace_file"
fi

# Read and process each line from repos-to-clone.list
while IFS= read -r repo || [ -n "$repo" ]; do

  # Skip lines that are empty, contain only whitespace, or start with a hash
  if [[ -z "$repo" || "$repo" =~ ^[[:space:]]*# || "$repo" =~ ^[[:space:]]+$ ]]; then
    continue
  fi

  # Extract the repository name and create the path
  repo_name="${repo##*/}"
  repo_path="../$repo_name"

  # Check if the path is already in the workspace file
  if jq -e --arg path "$repo_path" '.folders[] | select(.path == $path) | length > 0' "$workspace_file" > /dev/null; then
    continue
  fi

  # Add the path to the workspace JSON file
  jq --arg path "$repo_path" '.folders += [{"path": $path}]' "$workspace_file" > temp.json && mv temp.json "$workspace_file"
done < "./repos-to-clone.list"

