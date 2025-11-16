## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# projr_build()  # That's it

## ----eval=FALSE---------------------------------------------------------------
# # Accidentally overwrite yesterday's figures
# render("analysis.Rmd")  # Oh no, the new plot is worse!
# # Now you've lost the good version

## ----eval=FALSE---------------------------------------------------------------
# # Safe iteration
# projr_build_dev()  # Outputs to _tmp/
# # Check results, if bad, just run again
# # If good:
# projr_build()  # Now commit to _output

## ----eval=FALSE---------------------------------------------------------------
# projr_restore_repo("owner/repo")
# renv::restore()
# projr_build()

