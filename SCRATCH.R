library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-git.R")

library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_unset_select()
.test_set_fast()
devtools::test()

gert::git_config_global_set(
  "user.name", "Miguel Rodo"
)
gert::git_config_global_set(
  "user.email", "miguel.rodo@uct.ac.za"
)
