library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# dir_temp <- file.path(tempdir(), "test")
.projr_init_prompt(getwd())
