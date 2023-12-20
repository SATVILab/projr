#!/usr/bin/env bash
# Last modified: 2023 Nov 30

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

# Get the current R version prepended with a forward slash
r_version=$(R --version | grep 'R version' | awk '{print $3}' | sed 's/^/\//')

# Define the new key and value you want to add
new_key="r.libPaths"

# Define a regular expression for matching version strings
version_regex="[0-9]+\.[0-9]+\.[0-9]+"

# Function to process and check the JSON file
update_json_if_needed() {
    local has_version_path=false
    local paths=( $(jq -r ".\"$new_key\"[]?" "$path_file_json") )

    for path in "${paths[@]}"; do
        if [[ "$path" =~ $version_regex ]]; then
            has_version_path=true
            if [[ "$path" =~ $r_version ]]; then
                # The current version is already in the paths, no need to update
                return 0
            fi
            break
        fi
    done

    if [[ "$has_version_path" == true ]]; then
        echo "Update r.libPaths key"
        update_json
    fi
}

update_json() {
    local new_array=$(Rscript --vanilla -e "cat(.libPaths(), sep = '\n')" | sed ':a;N;$!ba;s/\n/", "/g' | sed 's/^/["/' | sed 's/$/"]/')
    jq --arg key "$new_key" --argjson value "$new_array" '. + {($key): $value}' $path_file_json > temp.json && mv temp.json $path_file_json
}

# Check if r.libPaths exists and if the current R version is not in its values
if jq -e ". | has(\"$new_key\")" "$path_file_json" > /dev/null; then
    update_json_if_needed
else
    echo "Add r.libPaths key"
    # r.libPaths key doesn't exist, so proceed to add it
    update_json
fi
