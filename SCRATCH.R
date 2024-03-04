library(testthat)
devtools::load_all()
.test_set_select()
# debugonce(.projr_remote_create_osf_project)
devtools::test_active_file(
  "tests/testthat/test-remote.R"
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


debug(.projr_remote_file_ls_osf)
# debugonce(.projr_remote_file_ls_osf_dir)
# debugonce(.projr_remote_file_ls_osf_dir_non_null_ind)
.projr_remote_file_ls(
  "osf",
  remote = osf_tbl
)
