library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
devtools::test_active_file("tests/testthat/test-path.R")

.test_set()
.test_set_fast()
.test_set_select()
.test_unset_select()

.file_ls(path_dir_from) |>
  .path_filter_spec(dir_exc) |>
  .path_filter_spec_add_back_file(path_dir_from, dir_exc)
