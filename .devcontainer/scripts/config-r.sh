#!/usr/bin/env bash
# Last modified: 2023 Nov 24

# Script for configuring the R environment in GitHub Codespaces or GitPod
# - Ensures GH_TOKEN, GITHUB_TOKEN and GITHUB_PAT are all set
#   for GitHub API access.
# - In GitPod/Codespace, stores R packages in a workspace directory to
#   persist them across sessions. Only really necessary for GitPod
# - Ensures radian works in GitPod/Codespace (without 
#   turning off auto_match, multiline interactive code does not run
#   correctly)
# - Configures R_LIBS directory for package installations
#   outside of container environments.

#!/usr/bin/env bash
# github token
if [ -n "$GH_TOKEN" ]; then 
  export GITHUB_PAT="${GITHUB_PAT:-"$GH_TOKEN"}"
  export GITHUB_TOKEN="${GITHUB_PAT:-"$GH_TOKEN"}"
elif [ -n "$GITHUB_PAT" ]; then 
  export GH_TOKEN="${GH_TOKEN:-"$GITHUB_PAT"}"
  export GITHUB_TOKEN="${GITHUB_TOKEN:-"$GITHUB_PAT"}"
elif [ -n "$GITHUB_TOKEN" ]; then 
  export GH_TOKEN="${GH_TOKEN:-"$GITHUB_TOKEN"}"
  export GITHUB_PAT="${GITHUB_PAT:-"$GITHUB_TOKEN"}"
fi

# save all R packages to /workspace directories.
# Especially important on GitPod to avoid having to
# reinstall upon container restarts
if [ -n "$(env | grep -E "^GITPOD|^CODESPACE")" ]; then
    if [ -n "$(env | grep -E "^GITPOD")" ]; then
      workspace_dir="/workspace"
    else
      workspace_dir="/workspaces"
    fi
    export R_LIBS=${R_LIBS:="$workspace_dir/.local/lib/R"}
    export RENV_PATHS_CACHE=${RENV_PATHS_CACHE:="$workspace_dir/.local/R/lib/renv"}
    export RENV_PATHS_LIBRARY_ROOT=${RENV_PATHS_LIBRARY_ROOT:="$workspace_dir/.local/.cache/R/renv"}
    export RENV_PATHS_LIBRARY=${RENV_PATHS_LIBRARY:="$workspace_dir/.local/.cache/R/renv"}
    export RENV_PREFIX_AUTO=${RENV_PREFIX_AUTO:=TRUE}
    export RENV_CONFIG_PAK_ENABLED=${RENV_CONFIG_PAK_ENABLED:=TRUE}
fi

# ensure that radian works (at least on ephemeral dev
# environments)
if [ -n "$(env | grep -E "^GITPOD|^CODESPACE")" ]; then
  if ! [ -e "$HOME/.radian_profile" ]; then touch "$HOME/.radian_profile"; fi
  if [ "$GITHUB_USER" = "MiguelRodo" ] || [ "$GITPOD_GIT_USER_NAME" = "Miguel Rodo" ]; then 
    if [ -z "$(cat "$HOME/.radian_profile" | grep -E 'options\(\s*radian\.editing_mode')" ]; then 
      echo 'options(radian.editing_mode = "vi")' >> "$HOME/.radian_profile"
    fi
  fi
  if [ -z "$(cat "$HOME/.radian_profile" | grep -E 'options\(\s*radian\.auto_match')" ]; then 
    echo 'options(radian.auto_match = FALSE)' >> "$HOME/.radian_profile"
  fi
fi

# ensure R_LIBS is set and created (so
# that one never tries to install packages
# into a singularity/apptainer container)
R_LIBS=${R_LIBS:="$HOME/.local/lib/R"}
mkdir -p "$R_LIBS"

# set linting settings
# light: just don't warn about snake case / camel case
# (which it often gets wrong) and object name
# length (which I often want to make very long)
if [ ! -f "$HOME/.lintr" ]; then
  echo "linters: with_defaults(
  object_length_linter = NULL,
  object_name_linter = NULL)
" > "$HOME/.lintr"
fi
