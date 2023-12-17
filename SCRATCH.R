library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()


# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-dir.R"
)

.projr_test_debug_copy_file("_bookdown.yml")
.projr_test_debug_copy_file("_output.yml")
