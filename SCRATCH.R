library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-remote.R"
)

.projr_test_debug_read_rds(pb_tbl)

# ==========================================

path_zip <- .projr_zip_file(
  fn_rel = .projr_remote_file_ls("local", path_dir_source),
  path_dir_fn_rel = path_dir_source,
  fn_rel_zip = remote[["fn"]]
)
piggyback:::.pb_cache_clear()
piggyback::pb_upload(file = path_zip, tag = id)
fn_vec_source <- .projr_remote_file_ls("local", path_dir_source)
path_dir_dest <- .projr_dir_tmp_random_get()
.projr_remote_file_add(
  "github",
  fn = fn_vec_source,
  path_dir_local = path_dir_source,
  remote = remote
)
expect_identical(
  .projr_remote_file_ls("osf", osf_tbl),
  fn_vec_source
)

# remove some content
fn_vec_orig_osf <- .projr_remote_file_ls("osf", osf_tbl)
fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
expect_true(
  .projr_remote_file_rm("osf", fn = fn_vec_rm, remote = osf_tbl)
)
expect_identical(
  .projr_remote_file_ls(
    "osf",
    remote = osf_tbl
  ),
  fn_vec_dest_orig |> setdiff(fn_vec_rm)
)
unlink(path_dir_source, recursive = TRUE)

# ==========================================
# GitHub release debug
# ==========================================

# stop here
.projr_test_debug_save_rds(content_tbl_pre_delete)
Sys.sleep(2)
expect_true(.projr_remote_file_rm_all("github", remote = id))
piggyback:::.pb_cache_clear()
content_tbl <- piggyback::pb_list(tag = id)
wd <- getwd()
repo <- piggyback:::guess_repo()
is_memoised <- memoise::is.memoised(piggyback::pb_list)
.projr_test_debug_save_rds(is_memoised)
.projr_test_debug_save_rds(repo)
.projr_test_debug_save_rds(id)
.projr_test_debug_save_rds(wd)
.projr_test_debug_save_rds(content_tbl)
pb_info_sv <- piggyback:::pb_info
.projr_test_debug_save_rds(pb_info_sv)
expect_true(nrow(content_tbl) == 0L)

.projr_test_debug_read_rds(repo)
.projr_test_debug_read_rds(wd)
.projr_test_debug_read_rds(content_tbl_pre_delete)
.projr_test_debug_read_rds(content_tbl)
.projr_test_debug_read_rds(id)
.projr_test_debug_read_rds(opt)
.projr_test_debug_read_rds(is_memoised)

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
