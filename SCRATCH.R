library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file("tests/testthat/test-osf-config-dir.R")

for (i in seq_len(nrow(osf_tbl_parent))) {
  osfr::osf_rm(osf_tbl_parent[i, ], check = FALSE, recurse = TRUE)
}

debugonce(.projr_osf_create_node)
osf_tbl <- .projr_osf_get_node("Test", yml_param = list(), parent_id = "q26c9")


# what are the upload methods again?
# well, we always want the `<node>/<label>` directory
# to mirror the local `<label>` directory.
# Now, we can go about ensuring that in various ways:
# - compare manifests, and make relevant changes
# - download the folder,
#   compare what is different, and make relevant changes
# - delete (will consider moving in future) the folder,
#   and  make relevant changes
# then we can also handle what has been changed/deleted in various
# ways:
# - move only what changed to new folder (`<node>/<key>/<archive>`) and
#   record what was moved where
# - simply delete it
# - move entire folder to archive folder, with versioning
# - Question: what if nothing changed?
#   - If only doing partial moves, then nothing.
#   - If simply deleting, then nothing
#   - If moving entire folder,
#     then I'm not sure whether to move it or "point".
#     - It would be nice to just point, e.g. for raw data.
# okay, and how do we go about restoring to a previous version?
# - If we simply delete, then we can't.
# - If we copy entire folders,
#   then we need to just know where that's kept.
# mirror
# okay, so what do we do?
# So, I think we create a node, upload some files
# Then we need to allow setting
# we haven't really thought about what to do with the manifest, though?
# so, we know that the manifest basically always needs to be uploaded
# to whatever not it's supposed to be uploaded to.
# and it's always a node, right? Good.

download = list(
  cue = NULL,
  sync_approach = NULL,
  version_source = NULL,
  conflict = NULL
),
upload = list(
  cue = NULL,
  sync_approach = NULL,
  version_source = NULL,
  conflict = NULL,
  archive = NULL
)


