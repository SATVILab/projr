library(testthat)
devtools::load_all()
.test_set_select()
devtools::test_active_file(
  "tests/testthat/test-build.R"
)

library(testthat)
devtools::load_all()
.test_unset_select()
devtools::test()

library(testthat)
devtools::load_all()
.test_unset_select()
.test_unset_fast()
devtools::load_all()
.projr_test_coverage()
