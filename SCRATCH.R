library(testthat)
devtools::load_all()
.test_set_select()
# debugonce(.projr_remote_create_osf_project)
devtools::test_active_file(
  "tests/testthat/test-misc.R"
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


 mapply(
  save,
  list = objs,
  file = vapply(paths, .dir_proj_get, character(1)),
  MoreArgs = list(
    envir = envir, 
    compress = compress,
    version = version,
    ascii = ascii)
    )