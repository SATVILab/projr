library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-dest-send.R"
)

# =======================
# Excessive README code
# =======================

  readme <- readme_list[["readme"]]
  path_readme <- readme_list[["path_readme"]]
  readme_ind_install <- which(grepl("^You can install", readme))
  if (!is.null(nm_list[["gh"]])) {
    readme_install_devtools <-
      'if (!requireNamespace("devtools")) install.packages("devtools")'
    readme_install_pkg <-
      paste0(
        'devtools::install_github("',
        nm_list[["gh"]], "/", nm_list[["pkg"]], '")'
      )
    readme[readme_ind_install + 3] <- readme_install_devtools
    readme[readme_ind_install + 5] <- readme[readme_ind_install + 4]
    readme[readme_ind_install + 4] <- readme_install_pkg
  }

  if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
  if (file.exists(path_readme)) unlink(path_readme)
  writeLines(text = readme, con = path_readme)

# =====================
# Git improvements
# =====================

git_tbl_status <- gert::git_status()

# exit now if nothing to commit
if (nrow(git_tbl_status) == 0) {
  return(invisible(FALSE))
}

if (yml_projr[["build"]][["git"]][["add-untracked"]]) {
  git_tbl_status <- gert::git_status()
  fn_vec <- git_tbl_status[["file"]][!git_tbl_status[["staged"]]]
  if (length(fn_vec) > 0) {
    gert::git_add(fn_vec, repo = rprojroot::is_r_package$find_file())
  }
  gert::git_commit(message = msg_commit)
} else {
  gert::git_commit_all(
    message = msg_commit,
    repo = rprojroot::is_r_package$find_file()
  )
}

# =====================
# GitHub authentication 
# =====================

  if (nzchar(Sys.getenv("GH_TOKEN"))) {
    Sys.setenv("GITHUB_PAT" = Sys.getenv("GH_TOKEN"))
    eval(
      on.exit(Sys.unsetenv("GITHUB_PAT"), after = TRUE, add = TRUE),
      envir = env
    )
    return(invisible(TRUE))
  } else if (nzchar(Sys.getenv("GITHUB_TOKEN"))) {
    Sys.setenv("GITHUB_PAT" = Sys.getenv("GITHUB_TOKEN"))
    eval(
      on.exit(Sys.unsetenv("GITHUB_PAT"), after = TRUE, add = TRUE),
      envir = env
    )
    return(invisible(TRUE))

# ====================
# osf-specific stuff
# =====================

# from inside .projr_dest_add_osf
title <- .projr_remote_title_get(title = title, content = content)
yml_projr_orig_root <- .projr_yml_get_root_default()
yml_projr <- projr_yml_get_unchecked()

get_list <- ..projr_dest_add_list_get_osf_add_load_get_list(
  sync_approach = get_sync_approach,
  conflict = get_conflict
)

send_list <- ..projr_dest_add_list_get_osf_add_load_get_list(
  cue = send_cue,
  sync_approach = send_sync_approach,
  version_source = send_version_source,
  conflict = send_conflict
)

.projr_dest_add_osf_check( # nolint
  id = id,
  title = title,
  id_parent = id_parent,
  title_parent = title_parent,
  content = content,
  body = body,
  public = public,
  category = category,
  path = path,
  path_append_label = path_append_label,
  structure = structure,
  download = get_list,
  upload = send_list,
  overwrite = overwrite
)

# create list to add
# ------------------
list_add <- .projr_osf_dest_get_list_add(
  title = title,
  id = id,
  structure = structure,
  path = path,
  path_append_label = path_append_label,
  get_list = get_list,
  send_list = send_list
)

# find where to add list, and add it
# ----------------------------------

.projr_dest_add_osf_check <- function(id,
                                      title,
                                      id_parent,
                                      parent_title,
                                      content,
                                      body,
                                      public,
                                      category,
                                      path = NULL,
                                      path_append_label = NULL,
                                      structure = NULL,
                                      download,
                                      upload,
                                      overwrite) {
  .projr_remote_check_osf_base(
    title = title,
    id = id,
    id_parent = id_parent,
    category = category,
    body = body,
    public = public,
    parent_title = parent_title,
    path = path,
    path_append_label = path_append_label
  )
  .projr_remote_check_osf_overwrite(
    overwrite = overwrite
  )

  # structure
  # --------------------
  .projr_remote_check_osf_structure(
    structure = structure,
    nm_opt = c("latest", "version", "content")
  )
  # no requirement that there is content
  # if it's a project
  if (category != "project") {
    .projr_remote_check_osf_label(
      label = content,
      type_opt = c("data-raw", "cache", "output", "archive", "docs")
    )
  } else if (!is.null(content)) {
    .projr_remote_check_osf_label(
      label = content,
      type_opt = c("data-raw", "cache", "output", "archive", "docs")
    )
  }

  # download
  # ---------------------
  .projr_remote_check_osf_trans_list(trans_list = download)
  .projr_remote_check_osf_trans_names(
    trans_list = download,
    nm_opt = c("sync_approach", "conflict")
  )
  .projr_remote_check_osf_cue(
    trans_list = download,
    nm_opt = c("none", "build", "major", "minor", "patch")
  )
  .projr_remote_check_osf_sync_approach(
    trans_list = download,
    nm_opt = c(
      "download-all",
      "delete-then-download-all",
      "download-missing" # haven't implemented this one yet
    )
  )

  # upload
  # -----------------------
  .projr_remote_check_osf_trans_list(
    trans_list = upload
  )
  .projr_remote_check_osf_trans_names(
    trans_list = upload,
    nm_opt = c("cue", "sync-approach", "version-source", "conflict")
  )
  .projr_remote_check_osf_cue(
    trans_list = upload,
    nm_opt = c("none", "build", "major", "minor", "patch", "change")
  )
  .projr_remote_check_osf_sync_approach(
    trans_list = upload,
    nm_opt = c(
      "upload-missing",
      "upload-all",
      "sync-using-deletion",
      "sync-using-version" # haven't implemented this one yet
    )
  )
  .projr_remote_check_osf_conflict(trans_list = upload)
}


.projr_osf_dest_get_list_add <- function(title,
                                         id,
                                         path,
                                         path_append_label,
                                         structure,
                                         get_list,
                                         send_list) {
  list_add <- list()
  if (!is.null(id)) {
    list_add[["id"]] <- id
  }
  if (!is.null(path)) {
    list_add[["path"]] <- path
  }
  if (!is.null(path_append_label)) {
    list_add[["path_append_label"]] <- path_append_label
  }
  if (!is.null(structure)) {
    list_add[["structure"]] <- structure
  }
  if (!length(get_list) == 0L) {
    list_add[["download"]] <- get_list
  }
  if (!length(send_list) == 0L) {
    list_add[["upload"]] <- send_list
  }

  list(list_add) |> stats::setNames(title)
}

# =================
# base checks for ymls
# =================

.projr_yml_remote_check_base <- function(type, ...) {
  switch(type,
    local = .projr_yml_remote_check_base_local(...),
    osf = .projr_yml_remote_check_base_osf(...)
  )
  invisible(TRUE)
}

# local
.projr_yml_remote_check_base_local <- function(path) {
  if (missing(path)) {
    stop("path must be supplied")
  }
  if (!is.character(path)) {
    stop("path must be a character vector")
  }
  invisible(TRUE)
}
# osf
.projr_yml_remote_check_base_osf <- function(title,
                                             id,
                                             id_parent,
                                             category,
                                             body,
                                             public,
                                             title_parent,
                                             path,
                                             path_append_label) {
  .projr_yml_remote_check_osf_title(title = title)
  .projr_yml_remote_check_osf_public(public = public)
  .projr_yml_remote_check_osf_body(body = body)
  .projr_yml_remote_check_osf_category(category = category)
  .projr_yml_remote_check_osf_id_parent(id_parent = id_parent)
  .projr_yml_remote_check_osf_title_parent(title_parent = title_parent)
  .projr_yml_remote_check_osf_proj_with_parent(
    id_parent = id_parent, title_parent = title_parent, category = category
  )
  .projr_yml_remote_check_osf_path(path = path)
  .projr_yml_remote_check_osf_path_append_label(
    path_append_label = path_append_label
  )
}

# ==============================
# further remote check stuff
# ==============================

.projr_yml_remote_add_check(
  type = type,
  id = id,
  title = title,
  id_parent = id_parent,
  parent_title = parent_title,
  content = content,
  body = body,
  public = public,
  category = category,
  path = path,
  path_append_label = path_append_label,
  structure = structure,
  download = get_list,
  upload = send_list,
  overwrite = overwrite
)

.projr_dest_add_osf_check( # nolint
  id = id,
  title = title,
  id_parent = id_parent,
  parent_title = parent_title,
  content = content,
  body = body,
  public = public,
  category = category,
  path = path,
  path_append_label = path_append_label,
  structure = structure,
  download = get_list,
  upload = send_list,
  overwrite = overwrite
)


# ==============================
# remote get stuff
# ==============================

role <- "dest"

title <- .projr_remote_check_title(title = title, content = content)

content <- .projr_yml_remote_content_get(
  role = role, type = type, content = content
)

# ==============================
# remote check stuff
# ==============================

.projr_remote_check_osf_label <- function(label, type_opt) {
  label_nm <- deparse(substitute(label))
  # check supplied
  if (missing(label)) {
    stop(paste0(
      label_nn, " must be supplied"
    ))
  }
  if (is.null(label)) {
    stop(paste0(
      label_nn, " must be a character vector"
    ))
  }
  # check it only matches restricted types
  short_to_match_type <- c(
    "cache" = "^cache",
    "data-raw" = "^dataraw",
    "output" = "^output",
    "archive" = "^archive",
    "docs" = "^docs"
  )
  match_str <- paste0(
    short_to_match_type[type_opt],
    collapse = "|"
  )
  if (!grepl(match_str, .projr_dir_label_strip(label))) {
    stop(paste0(
      label_nm, " entries must match the following: ",
      match_str
    ))
  }

  # check that it's actually found
  label_vec <- unique(c(
    names(projr_yml_get_unchecked()[["directories"]]),
    "docs"
  ))
  if (!all(label %in% label_vec)) {
    diff_vec <- setdiff(content, label_vec)
    stop(paste0(
      "content must match a directory key in _projr.yml. ",
      "The following do not: ",
      paste0('"', diff_vec, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}


.projr_remote_check_osf_overwrite <- function(overwrite) {
  if (!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }
  invisible(TRUE)
}
.projr_remote_check_osf_path <- function(path) {
  if (is.null(path)) {
    return(invisible(FALSE))
  }
  if (!length(path) == 1L) {
    stop("path must be a character vector of length 1")
  }
  if (!is.character(path)) {
    stop("path must be a character vector")
  }
  if (!nchar(path) > 0L) {
    stop("path must have at least one character") # nolint
  }
  invisible(TRUE)
}

.projr_remote_check_osf_path_append_label <- function(path_append_label) {
  if (is.null(path_append_label)) {
    return(invisible(FALSE))
  }
  if (!length(path_append_label) == 1L) {
    stop("path_append_label must be a logical vector of length 1")
  }
  if (!is.logical(path_append_label)) {
    stop("path_append_label must be a logical vector")
  }
  invisible(TRUE)
}


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
