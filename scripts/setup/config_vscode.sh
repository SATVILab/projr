#!/usr/bin/env bash

# exit if not on GitPod or Codespace
if [ -z "$(env | grep -E "^GITPOD|^CODESPACE")" ]; then
  exit 0
fi

# Define the path to your JSON file
path_rel=".vscode-remote/data/Machine/settings.json"
if [ -n "$(env | grep -E "^GITPOD")" ]; then
  path_file_json="/workspace/$path_rel"
else 
  path_file_json="/home/codespace/$path_rel"
fi

# Define the new key and value you want to add
new_key="r.libPaths"
new_array=$(Rscript -e "cat(.libPaths(), sep = '\n')" | sed ':a;N;$!ba;s/\n/", "/g' | sed 's/^/["/' | sed 's/$/"]/')

# Check if r.libPaths already exists in the JSON file
if jq -e ". | has(\"$new_key\")" "$path_file_json" > /dev/null; then
    exit 0
fi

# Add the key-value pair to the JSON file
jq --arg key "$new_key" --argjson value "$new_array" '. + {($key): $value}' $path_file_json > temp.json && mv temp.json $path_file_json