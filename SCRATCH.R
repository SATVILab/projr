library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# dir_temp <- file.path(tempdir(), "test")
.projr_init_prompt(getwd())

projr_dir_get("data_raw", "abc")
projr_dir_get("output", "abc")
projr_dir_get("archive", "abc")
projr_dir_get("cache", "abc")
