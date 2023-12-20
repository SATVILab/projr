library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-build.R")
.test_unset_select()
.test_set()

.test_set_fast()


.projr_build_copy_pkg(output_run)
