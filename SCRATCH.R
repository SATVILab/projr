library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-remote.R"
)

try(suppressWarnings(suppressMessages(piggyback::pb_releases(
  repo = "MiguelRodo/ProjrGitHubTesttest_projr-0.7355"
)))) -> out

id <- try(osfr::osf_create_component(
  x = .projr_remote_get_osf(id = parent_id),
  title = title, description = body, public = public, category = category
)[["id"]])

list.dirs(dir_test, recursive = FALSE)

# scratch code
yml_projr_dir <- projr_yml_get_unchecked()[["directories"]][["data-raw"]]

# ==========================================

# ==========================================
# GitHub setup for testing
# ==========================================

# Define the URL of the remote repository
user <- "MiguelRodo"
repo <- basename(dir_test)
url <- paste0("https://github.com/", user, "/", repo, ".git")

# Call the 'git' command to add the remote repository
system2("git", args = c("remote", "add", "origin", url))
if (inherits(create_repo, "try-error")) {
  gert::git_remote_add(
    repo = paste0(gh::gh_whoami()$user, "/", basename(dir_test))
  )
}

# ==========================================
# old manifest_compare
# ==========================================

.projr_manifest_compare <- function(manifest_pre, manifest_post) {
  # get last version uploaded
  # get version that is being uploaded to
  # get list of whatever is in the OSF registry
  # get all files that have been added
  version_pre <- .projr_manifest_version_get_latest(manifest_pre)
  manifest_pre <- manifest_pre[
    manifest_pre[["version"]] == version_pre,
  ]
  manifest_post <- manifest_post[
    manifest_post[["version"]] == paste0("v", projr_version_get()),
  ]
  .projr_change_get_hash(
    hash_pre = manifest_pre,
    hash_post = manifest_post
  )
}

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
