library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()


# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-manifest.R"
)
devtools::test_active_file(
  "tests/testthat/test-remote.R"
)

test_fn <- function(...) {
  dots_list <- as.list(...)
  x <- 1
  y <- "abc"
}

debugonce(test_fn)
test_fn("a")
test_fn()
