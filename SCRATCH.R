library(testthat)
devtools::load_all()
.test_set_select()
# devtools::test_active_file("tests/testthat/test-remote.R")
devtools::test_active_file("tests/testthat/test-plan.R")

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


debugonce(.projr_change_get)

change_list <- .projr_change_get(
  label = label, path_dir_local = path_dir_local,
  version_source = version_source, type = type, remote = remote
)

debugonce(.projr_change_get_manifest)
.projr_change_get_manifest(label = label)
manifest[version_vec_manifest < version_post, , drop = FALSE]$version |> unique()
