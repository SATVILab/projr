comp_dir=$(ls .. | grep -E "^Comp")
if [ -z "$comp_dir" ]; then
  sif_dir=./sif
  script_dir=.
else
  sif_dir=../"$comp_dir"/sif
  scripts_dir=../"$comp_dir"
fi
SIF_R_VERSION=423
apptainer build -F "$sif_dir"/r${SIF_R_VERSION}.sif "$scripts_dir"/scripts/def/r${SIF_R_VERSION}.def
