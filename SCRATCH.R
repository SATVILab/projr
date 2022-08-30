library(testthat)
setwd("/workspaces//projr")

devtools::load_all()
devtools::test()

debugonce(projr_set_up_dir)
