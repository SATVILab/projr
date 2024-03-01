library(testthat)
devtools::load_all()
.test_set_select()
# debugonce(.projr_yml_dest_add_get_list_add_extra_osf)
# debugonce(.projr_yml_dest_add_get_list_add_extra_osf_id_null)
# debugonce(.projr_remote_create_osf)
devtools::test_active_file(
  "tests/testthat/test-build-send-osf.R"
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



debugonce(.projr_remote_file_ls_github)
debugonce(.projr_remote_file_get_all_github)
.projr_remote_get_path_rel(
  path = path, path_append_label = path_append_label,
  label = label, structure = structure, type = "local"
)

.projr_yml_dest_get_title("Raw data", "local", NULL)
