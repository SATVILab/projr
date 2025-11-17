## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # Once on CRAN:
# # install.packages("projr")
# 
# # For now, install from GitHub:
# remotes::install_github("SATVILab/projr")

## ----eval=FALSE---------------------------------------------------------------
# library(projr)
# 
# # Create core directories
# dir.create("_raw_data", showWarnings = FALSE)
# dir.create("_output",   showWarnings = FALSE)
# 
# # Run your first build
# projr_build()

## ----eval=FALSE---------------------------------------------------------------
# projr_init()  # Uses sensible defaults

## ----eval=FALSE---------------------------------------------------------------
# projr_init_prompt()  # Guides you through each option

## ----eval=FALSE---------------------------------------------------------------
# projr_init_full()  # Includes additional features

## ----eval=FALSE---------------------------------------------------------------
# projr_build_dev()

## ----eval=FALSE---------------------------------------------------------------
# # After a dev build, find your outputs in:
# # _tmp/projr/v0.1.0/output/
# # _tmp/projr/v0.1.0/docs/
# 
# # You can specify which docs to render:
# projr_build_dev("analysis.Rmd")

## ----eval=FALSE---------------------------------------------------------------
# # Patch version bump (0.1.0 -> 0.1.1)
# projr_build()
# # or explicitly:
# projr_build_patch()
# 
# # Minor version bump (0.1.0 -> 0.2.0)
# projr_build_minor()
# 
# # Major version bump (0.1.0 -> 1.0.0)
# projr_build_major()

## ----eval=FALSE---------------------------------------------------------------
# projr_restore_repo("owner/repo")

