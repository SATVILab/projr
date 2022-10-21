library(testthat)
devtools::load_all()

dir_temp <- file.path(tempdir(), "test")
.projr_init_prompt(dir_temp)
