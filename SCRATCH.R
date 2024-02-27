library(testthat)
devtools::load_all()
.test_set_select()
# devtools::test_active_file("tests/testthat/test-remote.R")
devtools::test_active_file("tests/testthat/test-test.R")


library(testthat)
devtools::load_all()
devtools::test_active_file("tests/testthat/test-yml.R")
Sys.getenv("GITHUB_PAT")

library(testthat)
devtools::load_all()
.projr_test_coverage()
