library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()

y <- "a"
debugonce(.assert_opt)
debugonce(.assert_nm_get)
.assert_opt(y, "b")
deparse(substitute(x, env = parent.frame(1)))


# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-check.R"
)

a <- NULL
.assert_string_min(a, FALSE)
debugonce(.assert_check)
debugonce(.assert_given)
debugonce(.assert_nm_get)
debugonce(.assert_string)
debugonce(.assert_check)
a <- "abc"
.assert_string(a, FALSE)
.assert_string(a, TRUE)
a <- NULL
.assert_string(a, FALSE)
.assert_string(a, TRUE)

.is_abs <- function(x) {
  fs::is_absolute_path(x)
}

.is_null <- function(x) {
  is.null(x)
}




.is_rel <- function(x) {
  !.is_abs(x)
}

.projr_description_state_exists <- function() {
  file.exists(.projr_dir_proj_get("DESCRIPTION"))
}
