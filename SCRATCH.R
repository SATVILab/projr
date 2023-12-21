library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-build.R")

library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_unset_select()
.test_set_fast()
devtools::test()

# seems to be the third test stanza in test-git.R
x <- 1
