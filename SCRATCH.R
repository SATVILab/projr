library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file("tests/testthat/test-osf-config-dir.R")

Sys.setenv("PROJR_TEST" = "TRUE")
withr::defer(Sys.unsetenv("PROJR_TEST"))
withr::defer(unlink(dir_test, recursive = TRUE))
