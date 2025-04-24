library(testthat)
devtools::load_all()

testthat::test_file(
  "tests/testthat/test-build.R",
)
testthat::test_file(
  "tests/testthat/test-auth.R",
)

# working test files
# auth
# build