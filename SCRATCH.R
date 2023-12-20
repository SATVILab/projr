library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
devtools::test_active_file("tests/testthat/test-path.R")

.test_set()
.test_set_fast()
.test_set_select()
.test_unset_select()
