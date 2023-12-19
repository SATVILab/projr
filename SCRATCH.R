library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()

.test_set()
.test_set_fast()
.test_unset_fast()

# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-remote.R"
)
devtools::test_active_file(
  "tests/testthat/test-script.R"
)
