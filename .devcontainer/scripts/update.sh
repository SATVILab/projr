#!/usr/bin/env bash
# Last modified: 2023 Nov 30
# Creating a Bash script to update specific files from a template repository to other repositories involves a few steps. The script will:

# 1. Clone the template repository or pull the latest changes if it's already cloned.
# 2. Copy the specified files and folders (devcontainer.json folder and .gitpod.yml) to the target repository.
# 3. Ensure file permissions are set correctly.

echo "Update .devcontainer folder and .gitpod.yml file to latest MiguelRodo/CompTemplate settings"
echo "Note: it overwrites any files MiguelRodo/CompTemplate has, but does not delete files MiguelRodo/CompTemplate doesn't"

# Define paths
TEMPLATE_REPO_PATH="/tmp/update_template_settings/CompTemplate"
TARGET_REPO_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

# Clone or update the template repository
if [ ! -d "$TEMPLATE_REPO_PATH" ]; then
    git clone https://github.com/MiguelRodo/CompTemplate.git "$TEMPLATE_REPO_PATH"
else
    git -C "$TEMPLATE_REPO_PATH" pull
fi

# Functions to copy and set permissions
# executable files
copy_and_set_755_permissions() {
    src=$1
    dest=$2
    # Copy file or directory
    cp -R "$src" "$dest"
    # Set executable permissions for everyone and write permission only for the user
    chmod -R u+rwX,go+rX,go-w "$dest"
}
# non-executable files
copy_and_set_644_permissions() {
    src=$1
    dest=$2
    # Copy file or directory
    cp -R "$src" "$dest"
    # Set executable permissions for everyone and write permission only for the user
    chmod -R u+rw,go+r,go-w "$dest"
}

# Update .devcontainer with scripts
copy_and_set_755_permissions "$TEMPLATE_REPO_PATH/.devcontainer/scripts" "$TARGET_REPO_PATH/.devcontainer"

# Update .devcontainer/devcontainer.json
copy_and_set_644_permissions "$TEMPLATE_REPO_PATH/.devcontainer/devcontainer.json" "$TARGET_REPO_PATH/.devcontainer"

# Update .gitpod.yml
copy_and_set_644_permissions "$TEMPLATE_REPO_PATH/.gitpod.yml" "$TARGET_REPO_PATH"

echo "Update complete."
