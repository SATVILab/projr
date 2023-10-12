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


# manifest download stuff
.projr_osf_download_node_manifest <- function(osf_tbl) {
  osf_tbl_files <- osf_tbl |> osfr::osf_ls_files()
  osf_tbl_manifest <- osf_tbl_files[
    osf_tbl_files[["name"]] == "manifest.csv",
  ]
  if (nrow(osf_tbl_manifest) == 0L) {
    return(data.frame(
      label = character(0),
      fn = character(0),
      version = character(0),
      hash = character(0)
    ))
  }
  path_save <- file.path(tempdir(), "manifest.csv")
  osfr::osf_download(
    osf_tbl_manifest,
    path = tempdir(), conflicts = "overwrite"
  )
  utils::read.csv(path_save)
}
