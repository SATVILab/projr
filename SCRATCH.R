library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-manifest.R"
)

list.dirs(dir_test, recursive = FALSE)
# tests passing:
# - test-osf-download.R
# - test-osf-yml.R
# - test-manual-osf-upload.R

# scratch code
yml_projr_dir <- projr_yml_get_unchecked()[["directories"]][["data-raw"]]

# ==========================================

# ==========================================
# debugging projr_osf_dest_add
# ==========================================

browser()

# debugonce(.projr_osf_yml_get_parent_vec)
# debugonce(.projr_osf_yml_find_parent)
# debugonce(.projr_osf_yml_find_parent_rec)
# debugonce(projr_osf_dest_add)
# debugonce(.projr_osf_yml_dest_add_comp_parent)
browser()

# debugonce(.projr_osf_yml_get_parent_vec)

# =========================================
# osf_download_to_dir_all
# =========================================

if (sub_dir) {
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
      x = osf_tbl_file[i, ],
      path = path_save,
      recurse = TRUE,
      conflicts = conflict
    )
  }
} else {
  osfr::osf_download(
    x = osf_tbl_file,
    path = path_save,
    recurse = TRUE,
    conflicts = conflict
  )
}

# =========================================
# manifest download stuff
# =========================================

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
>>>>>>> ccb904e (Allow version-based restoration)
