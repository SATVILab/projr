#!/bin/bash
# Description: This is a Bash script that attempts to run a
# containerized application using either Apptainer or
# Singularity container runtimes.
# The script first attempts
# to locate a Singularity Image Format (SIF) file in the
#./sif directory.
# If it is not found, it attempts to locate
# it in a directory named Comp* in the parent directory.
# If it is still not found, it attempts to download the
# SIF file from a GitHub release.
# If it is still not found, it attempts to build and upload
# the SIF file to a GitHub release.
# If none of these attempts are successful, the script
# exits with an error.
# Finally, the script runs the containerized application
# using either Apptainer or Singularity,
# depending on which one is available.

# Function to check if sif is in there and then return the path if it is
get_path_sif() {
  # Check if directory exists
  if [ -d "$1" ]; then
    # If it does exist, then check if it has a sif file
    if [ -n "$(ls "$1" | grep -E 'sif$')" ]; then
      echo "$1/$(ls "$1" | grep -E 'sif$')"
    else
      echo "File not found" >&2
    fi
  else
    echo "Directory does not exist" >&2
  fi
}

# Attempt to get path to sif in ./sif
echo "----------------"
echo "Attempting to get path to sif in ./sif"
path_sif="$(get_path_sif "sif")"

# If path_sif is empty, attempt to get path to sif in ../$comp_dir/sif

if [ -z "$path_sif" ]; then
  echo "No sif file found in ./sif."
  echo "----------------"
  comp_dir="$(ls .. | grep -E "^Comp")"
  if [ -n "$comp_dir" ]; then
    echo Attempting to get path to sif in a compendium workspace: "../$comp_dir/sif"
    sif_dir="../$comp_dir/sif"
    scripts_dir="../$comp_dir/scripts"
    path_sif="$(get_path_sif "$sif_dir")"
    if [ -z "$path_sif" ]; then
      echo "No sif file found in ../$comp_dir/sif."
      echo "----------------"
    else
      echo "sif file created on compendium workspace"
      echo "----------------"
    fi
  else 
    sif_dir=./sif
    scripts_dir=./scripts
  fi
else
  echo "sif file found in .sif"
  echo "----------------"
fi

if [ -z "$path_sif" ]; then
  echo "Attempting to download sif file from a GitHub release"
  "$scripts_dir"/ubuntu/download_apptainer.sh
  path_sif="$(get_path_sif "$sif_dir")"

fi

if [ -z "$path_sif" ]; then
  echo "Could not download sif from from a GitHub release"
  echo "----------------"
  echo "Attempting to create sif file and then upload as a GitHub release"
  "$scripts_dir"/ubuntu/build_and_upload_apptainer.sh
  path_sif="$(get_path_sif "$sif_dir")"
  if [ -z "$path_sif" ]; then
    echo "Could not build and upload a sif file"
    echo "sif file not found and could not be downloaded or built"
    echo "----------------"
    exit 1
  else
    echo "Sif file successfully built"
    echo "----------------"
  fi
fi

echo "Path to sif file: $path_sif"
echo "----------------"

if [ -n "$(env | grep -E "^GITPOD|^CODESPACE")" ]; then
  export RENV_CONFIG_PAK_ENABLED=${RENV_CONFIG_PAK_ENABLED:=TRUE}
fi

# Run apptainer if it is found, and singularity if apptainer is not.
# Exit with an error if neither is found
if command -v apptainer > /dev/null 2>&1; then
  echo "Running apptainer"
  apptainer run "$path_sif" "$1"
elif command -v singularity > /dev/null 2>&1; then
  echo "Running singularity"
  singularity run "$path_sif" "$1"
else
  echo "Neither singularity nor apptainer container runtime detected"
  exit 1
fi
