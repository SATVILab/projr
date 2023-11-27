#!/usr/bin/env bash
# Last modified: 2023 Nov 24

# This script is used to configure R settings in Visual Studio Code (VSCode) for GitPod or Codespace environments.
# It sets the `r.libPaths` VS Code settings to the default `.libPaths()` output
# when not using `renv`, as this is where the R packages
# that the VS Code extensions depend on (e.g. languageserver) are installed.
# If you do not do this, then you might get many warnings about these packages
# not being installed, even though you know the are.
# It checks if the script is running in a GitPod or Codespace environment and defines the path to the JSON file accordingly.
# Then, it defines a new key and value to be added to the JSON file.
# If the key already exists in the JSON file, the script exits.
# Otherwise, it adds the key-value pair to the JSON file using the 'jq' command.

# exit if not on GitPod or Codespace
if [ -z "$(env | grep -E "^GITPOD|^CODESPACE")" ]; then
    exit 0
fi

# Define the path to your JSON file
path_rel=".vscode-remote/data/Machine/settings.json"
if [ -n "$(env | grep -E "^GITPOD")" ]; then
    path_file_json="/workspace/$path_rel"
else 
    path_file_json="/home/$USER/$path_rel"
fi

# Create the JSON file if it does not exist
if [ ! -f "$path_file_json" ]; then
    echo "{}" > "$path_file_json"
fi

# Define the new key and value you want to add
new_key="r.libPaths"

# only run if  r.libPaths does not already exist in the JSON file
if ! jq -e ". | has(\"$new_key\")" "$path_file_json" > /dev/null; then
    # get the value(s) to add.
    # --vanilla to avoid `renv` settings coming in
    new_array=$(Rscript --vanilla -e "setwd(tempdir()); cat(.libPaths(), sep = '\n')" | sed ':a;N;$!ba;s/\n/", "/g' | sed 's/^/["/' | sed 's/$/"]/')
    # Add the key-value pair to the JSON file
    jq --arg key "$new_key" --argjson value "$new_array" '. + {($key): $value}' $path_file_json > temp.json && mv temp.json $path_file_json
fi
