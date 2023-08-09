#!/bin/bash
GH_R_VERSION=423
comp_dir=$(ls .. | grep -E "^Comp")
if [ -z "$comp_dir" ]; then
  sif_dir=./sif
else
  sif_dir=../"$comp_dir"/sif
fi
mkdir -p "$sif_dir"
cd $sif_dir
gh release create r${GH_R_VERSION} r${GH_R_VERSION}.sif --title "r${GH_R_VERSION}" --notes "Apptainer/Singularity container for R${GH_R_VERSION}"
cd $OLDPWD
