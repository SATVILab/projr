library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-manual-osf-download.R"
)
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-manual-osf-download.R"
)
osfr::osf_download(
  x = osf_tbl, path = yml_projr_dir_lbl[["path"]],
  recurse = TRUE, conflicts = conflict
)
osfr::osf_download(
  x = osf_tbl, path = "_data_raw",
  recurse = TRUE, conflicts = "overwrite"
)

osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
