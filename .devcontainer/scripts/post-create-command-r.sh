#!/usr/bin/env bash
# Last modified: 2023 Dec 13

# 1. ensure that key VS Code packages are up to date.
# and does not take long to install.

# Notes:
# 1. install dev version of `renv` as it's pretty reliable

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" 

pushd "$HOME"
Rscript -e 'install.packages(c("jsonlite", "languageserver", "pak"))' \
  -e 'remotes::install_github("rstudio/renv")'
popd
