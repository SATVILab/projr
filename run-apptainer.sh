#!/bin/bash
test -e sif/r430.sif || scripts/ubuntu/download_apptainer.sh
R_LIBS_USER=$HOME/.local/.cache/R
mkdir -p $R_LIBS_USER
apptainer run --env R_LIBS=$R_LIBS_USER,RENV_CONFIG_PAK_ENABLED=TRUE,GITHUB_PAT=${GITHUB_PAT:=$GH_TOKEN} sif/r430.sif ${1:-code-insiders tunnel --accept-server-license-terms}
