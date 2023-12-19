# ======================
# old local remote stuff
# ======================

.projr_local_send_dir <- function(path_dir_local,
                                  path_dir_local_dest,
                                  conflict) {
  # remove pre-existing files and error on overwrite
  fn_vec_rel <- list.files(path_dir_local, recursive = TRUE, full.names = TRUE)
  if (length(fn_vec_rel) == 0L) {
    return(invisible(FALSE))
  }
  .projr_local_send_file(
    fn_rel = fn_vec_rel,
    path_dir_local = path_dir_local,
    path_dir_local_dest = path_dir_local_dest,
    conflict = conflict
  )
}

.projr_local_send_file <- function(fn_rel,
                                   path_dir_local,
                                   path_dir_local_dest,
                                   conflict) {
  # remove pre-existing files and error on overwrite
  if (length(fn_rel) == 0L) {
    return(invisible(FALSE))
  }
  attach(
    .projr_local_conflict_manage(
      path_file_source = file.path(path_dir_local, fn_rel),
      path_file_dest = file.path(path_dir_local_dest, fn_rel),
      conflict = conflict
    ),
    warn.conflicts = FALSE
  )
  if (length(path_file_dest) == 0L) { # nolint
    return(invisible(FALSE))
  }
  .projr_local_dir_create(path_file_dest) # nolint

  # copy the files
  all(file.copy(path_file_source, path_file_dest, overwrite = TRUE)) # nolint
}

.projr_local_conflict_manage <- function(path_file_source,
                                         path_file_dest,
                                         conflict) {
  if (conflict == "overwrite") {
    return(list(
      "path_file_source" = path_file_source, "path_file_dest" = path_file_dest
    ))
  }
  skip_vec_ind <- file.exists(path_file_dest)
  if (sum(skip_vec_ind) > 0 && conflict == "error") {
    stop(paste0(
      "File already exists at destination: ",
      path_file_dest[skip_vec_ind], " and conflict = 'error'"
    ))
  }
  path_file_dest <- path_file_dest[!skip_vec_ind]
  path_file_source <- path_file_source[!skip_vec_ind]
  list(
    "path_file_source" = path_file_source, "path_file_dest" = path_file_dest
  )
}

.projr_local_dir_create <- function(fn) {
  dir_vec <- unique(dirname(fn))
  for (i in seq_along(dir_vec)) {
    .projr_dir_create(dir_vec[i])
  }
  invisible(TRUE)
}

.projr_local_rm_file <- function(fn_rel,
                                 path_dir) {
  all(file.remove(file.path(path_dir, fn_rel)))
}

.projr_get_dir_local <- function(label, bump_component) {
  projr_path_get_dir(
    label = label,
    safe = !.projr_run_output_check(bump_component)
  )
}


# ===============================
# build-manifest testing
# ===============================

test_that("projr_manifest_hash_dir works", {
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      yml_projr_init <- .projr_yml_get_root_full()
      # test getting hashes
      dir.create("_data_raw/sub", recursive = TRUE)
      invisible(file.create("_data_raw/abc.csv"))
      invisible(file.create("_data_raw/sub/def.csv"))
      dir.create("_tmp")
      expect_identical(
        nrow(.projr_manifest_hash_label("cache", output_run = TRUE)), 0L
      )
      expect_identical(
        ncol(.projr_manifest_hash_label("cache", output_run = TRUE)), 4L
      )
      invisible(file.create("_data_raw/sub/def.csv"))

      # test getting manifest
      # ----------------------------

      # pre:
      dir.create("_tmp/abc", recursive = TRUE)
      file.create("_tmp/test.txt")
      file.create("_tmp/abc/test.txt")

      manifest <- .projr_build_manifest_pre(TRUE) |> .projr_manifest_read()
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_pre(TRUE)
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- TRUE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_pre(TRUE)
      expect_identical(nrow(manifest), 2L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))

      # post:
      if (dir.exists("_data_raw")) {
        unlink("_data_raw", recursive = TRUE)
      }
      if (dir.exists("_tmp")) {
        unlink("_tmp", recursive = TRUE)
      }
      dir.create("_tmp/projr/v0.0.0-1/output/abc", recursive = TRUE)
      invisible(file.create("_tmp/projr/v0.0.0-1/output/test.txt"))
      invisible(file.create("_tmp/projr/v0.0.0-1/output/abc/test.txt"))
      dir.create("_data_raw/abc", recursive = TRUE)
      invisible(file.create("_data_raw/test.txt"))
      invisible(file.create("_data_raw/abc/test.txt"))
      # to fix:
      # - why did data-raw sub-directory not appear?
      # - what is going on with the output directory? It seems
      #   like the test is assuming the files will be in the cache directory,
      #   but they should actually be in the unsafe directory.
      #   well, I guess it doesn't matter.
      # seems like both are now fixed

      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 4L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["data-raw"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 2L)
      yml_projr[["directories"]][["output"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 0L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))

      # test saving manifest
      # -------------------------
      manifest_tbl <- data.frame(x = 1)
      .projr_manifest_write(manifest_tbl, output_run = TRUE)
      expect_true(file.exists("manifest.csv"))
      .projr_manifest_write(manifest_tbl, output_run = FALSE)
      expect_true(
        file.exists(projr_dir_get("output", "manifest.csv", output_run = FALSE))
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_manifest_compare works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .projr_dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  gert::git_init(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      # clean up
      if (dir.exists(projr_dir_get("data-raw"))) {
        unlink(projr_dir_get("data-raw"), recursive = TRUE)
      }
      # create files
      path_output_kept_unchanged <- projr_path_get(
        "output", "kept_unchanged.txt",
        safe = FALSE
      )
      path_output_kept_changed <- projr_path_get(
        "output", "kept_changed.txt",
        safe = FALSE
      )
      path_output_removed <- projr_path_get(
        "output", "removed.txt",
        safe = FALSE
      )
      invisible(file.create(path_output_kept_unchanged))
      invisible(file.create(path_output_kept_changed))
      invisible(file.create(path_output_removed))
      # get manifest beforehand
      manifest_tbl_pre <- .projr_build_manifest_hash_post(output_run = TRUE)
      # add a file, and change a file
      path_output_add <- projr_path_get(
        "output", "added.txt",
        safe = FALSE
      )
      invisible(file.create(path_output_add))
      cat("add", file = path_output_kept_changed, append = TRUE)
      unlink(path_output_removed)
      # get manifest afterwards
      manifest_tbl_post <- .projr_build_manifest_hash_post(output_run = TRUE)
      .projr_manifest_compare(manifest_tbl_pre, manifest_tbl_post)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre, manifest_tbl_post
      )
      expect_identical(
        "kept_unchanged.txt", manifest_compare_list$kept_unchanged$fn
      )
      expect_identical(
        "kept_changed.txt", manifest_compare_list$kept_changed$fn
      )
      expect_identical("removed.txt", manifest_compare_list$removed$fn)
      expect_identical("added.txt", manifest_compare_list$added$fn)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre[rep(FALSE, nrow(manifest_tbl_pre))], manifest_tbl_post
      )

      zero_row_tbl <- .projr_zero_tbl_get_manifest()
      expect_identical(manifest_compare_list$kept_unchanged, zero_row_tbl)
      expect_identical(manifest_compare_list$added, manifest_tbl_post)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre,
        manifest_tbl_post[rep(FALSE, nrow(manifest_tbl_pre)), ]
      )
      expect_identical(manifest_compare_list$kept_unchanged, zero_row_tbl)
      expect_identical(manifest_compare_list$removed, manifest_tbl_pre)
    },
    force = TRUE,
    quiet = TRUE
  )
})


# ===============================
# yml-dest testing
# ===============================

test_that(".projr_remote_create works", {
  # POSSIBLY VERY OUT OF DATE
  skip()
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # adding
      # -----------------------------------------------

      # check add where type did not exist before
      yml_root_orig <- list("build" = list("package" = FALSE))
      yml_merge_final <- list("build" = list(
        "local" = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
      ))
      expect_identical(
        .projr_dest_add_get_final(
          yml_root_orig = yml_root_orig,
          yml_merge_final = yml_merge_final,
          type = "local",
          title = "Zlatan"
        ),
        list(
          build = list(
            package = FALSE,
            local = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
          )
        )
      )

      # check add where type did exist before
      yml_root_orig <- list("build" = list(
        "package" = FALSE,
        "local" = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
      ))
      yml_merge_final <- yml_root_orig
      yml_merge_final[["build"]][["local"]][["ZlatanIsBack"]] <- list(
        "ID" = "KaPow", "Move" = "ChopChop"
      )
      expect_identical(
        .projr_dest_add_get_final(
          yml_root_orig = yml_root_orig,
          yml_merge_final = yml_merge_final,
          type = "local",
          title = "ZlatanIsBack"
        ),
        list(
          build = list(
            package = FALSE,
            "local" = list(
              Zlatan = list(ID = "MrMr", Move = "Karate"),
              ZlatanIsBack = list(ID = "KaPow", Move = "ChopChop")
            )
          )
        )
      )
    }
  )
})

test_that(".projr_remote_create works", {
  # POSSIBLY (LIKELY) OUT OF DATE
  skip()
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # adding
      # -----------------------------------------------

      # remote any pre-existing remotes
      .projr_test_yml_dest_remote_rm()

      # add it
      browser()
      debugonce(.projr_yml_remote_check_content)
      projr_dest_add_local(
        title = "Archive",
        content = "data-raw",
        path = "_archive"
      )
      yml_projr_build <- projr_yml_get()[["build"]]
      expect_true(
        identical(
          yml_projr_build[["local"]][["Archive"]],
          list(
            content = "data-raw",
            path = "_archive"
          )
        )
      )
    }
  )
})
  
# =======================
# Citation files
# =======================

# okay, so what do I need to run?

- create citation.cff file:
  - Add it to project directory?
  - Add it to `inst/`?
    - cffr::write_citation()
- Add codemeta.json file to projet directory?
- Add plain-text citation to README:
  - Markdown README:
    - Initially?
      - Write it to an R object, or temp file
      - Then add result to end of README
    - Update it upon builds?
      - Cannot do this
  - Rmd README:
    - Initially?
      - Add the function call
    - Update it upon builds?
- Add BibTeX citation to README?
  - Initially?
  - Update it upon builds?

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

  
  # may have to consider what to delete before uploading
  # all at once
  # okay, so, I'm stuck because I don't actually have an idea yet.
  # all right, let's start talking.
  # okay, so what's the story.
  # so, so, what do we know now:
  # - what files have been removed, added or changed since last upload
  # - remote type (local, osf, github)
  # - remote name (e.g. "My OSF project" or "Project inputs")
  # - remote structure (e.g. "latest" or "version")
  #   - Question: does this matter for GitHub?
  # - sync approach (upload-all, upload-missing, sync-using-deletion or
  #   sync-using version)
  # - conflict
  #   - what to do about file conflicts
  # - version source does not matter now, as we already have decided
  #   - well, wait - what if we just want to send everything?
  #   - in that case, presumably the change_list should be empty - don't
  #     bother getting it
  # - OSF-specific:
  #  - {remote_base (id), path and path_append_label} OR path_final
  #    (probably the former only)
  # - GitHub-specific:
  #   - body
  # - local-specific:
  #   - nothing
  # okay, so it seems to depend a lot on the structure,
  # the sync approach and the version_source
  # ya, when doing this for OSF the first thing I did
  # was to get the remote structure
  # then I deleted things if we used sync-using-deletion
  # so, we actually only get the change list if the
  # version-source is sync-using-version. Otherwise,
  # we just either upload everything or delete everything.
  # Okay, so now we just focus on the process for adding
  # decide what to do
  
# ==================================
# actually send
# ==================================

# send to a remote

.projr_dest_send_label_send <- function(path_dir_local,
                                        remote_final,
                                        change_list,
                                        structure) {
  # okay, so what variants do I have here?
  # okay, so the main thing is just figuring out
  # based on sync-structure what to upload
  # it would be nice if change list had everything.
  # I suppose, though, that that involves hashing.
  # maybe we should make it more general, and make
  # "action_list", as we don't actually need to
  # use hashes if we're using upload-all,
  # upload-missing or upload-using-deletion.
  # what do we need for upload-all?
  # well, we need to know what's in the local directory
  # - what do we need for upload-missing?
  # we need to know what's not on the remote
  # - what do we need for sync-using-deletion?
  # we need to know what's in the local directory
  # - what do we need for sync-using-version?
  # we need to know what's changed since the last version
  # - okay, so the change_list can be made more general.
  # no problem with it, really, and it kind of simplifies my thinking here.
  # doesn't this also depend on the remote structure?
  # well, let's see.
  # latest:
  # - upload-all: list of files locally
  # - upload-missing: list of files locally not on remote
  # - sync-using-deletion: list of files locally
  # - sync-using-version: list of files locally that have changed
  # (added, removed or changed)
  # version:
  # - upload-all: list of files locally
  # - upload-missing: list of files locally not on remote
  # - sync-using-deletion: list of files locally
  # - sync-using-version: list of files locally that have changed
  # how does version source affect the above?
  # if none:
  # - latest:
  #   - sync-using-deletion and upload-all need all files to upload
  #   - upload-missing will simply upload all files (so, it'll
  #     be different)
  #   - sync-using-version will upload all files (so, it'll be different)
  #   - In all cases, we basically upload everything.
  #   - The only difference is that for sync-using-deletiong
  #     we actually delete everything.
  #   - I suppose we also say that sync-using-version becomes
  #     sync-using-deletion
  # - version:
  #  - you know, it's the same, except that we know automatically
  #    that upload-missing is upload-all and sync-using-version
  #    is sync-using-deletion
  # if manifest/file:
  # - latest:
  #   - upload-all and sync-using-deletion need
  #     all files that are in local directory (same)
  #   - upload-missing just needs files
  #     that are not on remote (same)
  #   - sync-using-version will need files to add and files to remove,
  #     and will determine this by comparing hashes
  #     (different way of getting changes)
  # - version:
  #   - upload-all and sync-using-deletion upload everything
  #   - upload-missing uploads everything
  #   - sync-using-version uploads everything if anything has changed,
  #     so we need to check if anything has changed. If it has,
  #     we upload everything.
  # - how does vary for local vs osf vs github:
  # - osf & local:
  #   - as above
  # - github:
  #   -
  .projr_dest_send_label_
}


# ================================
# change functions
# ================================

# for multiple labels
# ------------------------
.projr_dest_change_get_content <- function(content,
                                           output_run,
                                           remote_type,
                                           remote_name,
                                           yml_remote) {
  args_list <- list(
    output_run = output_run, remote_type = remote_type,
    remote_name = remote_name, yml_remote = yml_remote
  )
  lapply(content, function(x) {
    do.call(.projr_dest_change_get_label, list(label = x) |> append(args_list))
  }) |>
    stats::setNames(content)
}

# for a single label
# ------------------------

.projr_dest_change_get_label <- function(label,
                                         output_run,
                                         remote_type,
                                         remote_base,
                                         remote_final,
                                         path_remote_rel,
                                         version_source) {
  # this will assume that the manifest knows
  # about what's online
  # TODO: make this function differently dependening
  # on what sync-approach and remote-structure are
  # (in addition to version source)
  switch(version_source,
    "manifest" = .projr_change_get_manifest(label = label),
    "file" = .projr_change_get_file(
      output_run = output_run,
      remote_type_pre = remote_type,
      remote_base_pre = remote_base,
      remote_final_pre = remote_final,
      path_remote_rel_pre = path_remote_rel,
      label_post = label
    ),
    stop(paste0("version_source '", version_source, "' not recognized"))
  )
}

# old test-dir.R code (seems to be a repeat of earlier, but with bugs not fixed)

test_that("projr_dir_ignore works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  gert::git_init(path = dir_test)
  usethis::with_project(
    path = dir_test,
    code = {
      # test adding to gitignore and buildignore
      gitignore_orig <- .projr_ignore_git_read()
      buildignore_orig <- .projr_ignore_rbuild_read()
      .projr_ignore_label_set("docs")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(
        gitignore == "docs/reportV0.0.0-1/**"
      )), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^docs/reportV0\\.0\\.0-1"
      )), 1L)
      .projr_ignore_label_set("data-raw")
      # test that nothing is done when directory is equal to working directory
      .projr_gitignore_set(gitignore_orig, append = FALSE)
      yml_bd_init <- .projr_yml_bd_get()
      yml_bd <- yml_bd_init
      yml_bd[["output_dir"]] <- "."
      .projr_yml_bd_set(yml_bd)
      .projr_ignore_label_set("docs")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(
        gitignore == "."
      )), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^\\."
      )), 0L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_data_raw/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_data_raw/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_ignore_label_set("output")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_output/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_output/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_ignore_label_set("archive")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_archive/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_archive/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_ignore_label_set("cache")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_tmp/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_tmp/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)

      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore_git"]] <- FALSE
        yml_projr[["directories"]][[i]][["ignore_rbuild"]] <- FALSE
      }
      .projr_yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_data_raw/**")), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_data_raw")), 0L)
      buildignore <- .projr_ignore_rbuild_read()

      # test not adding when the directory is not in wd
      yml_projr <- .projr_yml_get_root_full()
      dir_out <- file.path(
        dirname(rprojroot::is_r_package$find_file()), "test_2"
      )
      if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["path"]] <- dir_out
      }
      .projr_yml_set(yml_projr)
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(
        which(gitignore == "/tmp/RtmpkdBxQ9/test_2/**")
      ), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(
        which(buildignore == "^/tmp/RtmpkdBxQ9/test_2")
      ), 0L)
      buildignore <- .projr_ignore_rbuild_read()

      # test errors
      expect_error(.projr_ignore_label_set(c("abc", "def")))
      expect_error(.projr_ignore_label_set(1))
      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore_rbuild"]] <- 1
        yml_projr[["directories"]][[i]][["ignore_git"]] <- 1
      }
      names(yml_projr[["directories"]]) <- rep("data-raw", 4)
      .projr_yml_set(yml_projr)
      expect_error(.projr_ignore_label_set("data-raw"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})



# path_dir
"C:\Users\RUNNER~1\AppData\Local\Temp\RtmpuUR6PU/working_dir\RtmpgBf9hB/report"
# path_dir 
"C:/Users/RUNNER~1/AppData/Local/Temp/RtmpuUR6PU/working_dir/RtmpgBf9hB/report"
# dir_proj
"C:/Users/runneradmin/AppData/Local/Temp/RtmpuUR6PU/working_dir/RtmpgBf9hB/report"

# old ignore stuff
# =======================================
if (is.logical(ignore)) {
  if (ignore) {
    if (!txt_gitignore %in% gitignore) {
      .projr_gitignore_set(txt_gitignore, append = TRUE)
      .projr_gitignore_set("\n", append = TRUE)
    }
    if (!txt_rbuildignore %in% rbuildignore) {
      .projr_buildignore_set(txt_rbuildignore, append = TRUE)
      .projr_buildignore_set("\n", append = TRUE)
    }
    return(invisible(TRUE))
  } else {
    if (txt_gitignore %in% gitignore) {
      gitignore <- gitignore[!(gitignore == txt_gitignore)]
      .projr_gitignore_set(gitignore, append = FALSE)
    }
    if (txt_rbuildignore %in% rbuildignore) {
      rbuildignore <- rbuildignore[!(rbuildignore == txt_rbuildignore)]
      .projr_buildignore_set(rbuildignore, append = FALSE)
    }
  }
} else if (is.character(ignore)) {
  if ("git" %in% ignore) {
    if (!txt_gitignore %in% gitignore) {
      .projr_gitignore_set(txt_gitignore, append = TRUE)
      .projr_gitignore_set("\n", append = TRUE)
    }
  } else {
    if (txt_gitignore %in% gitignore) {
      gitignore <- gitignore[!(gitignore == txt_gitignore)]
      .projr_gitignore_set(gitignore, append = FALSE)
    }
  }
  if (!txt_rbuildignore %in% rbuildignore) {
    .projr_buildignore_set(txt_rbuildignore, append = TRUE)
    .projr_buildignore_set("\n", append = TRUE)
  } else {
    if (txt_rbuildignore %in% rbuildignore) {
      rbuildignore <- rbuildignore[!(rbuildignore == txt_rbuildignore)]
      .projr_buildignore_set(rbuildignore, append = FALSE)
    }
  }
}


gitignore <- .projr_gitignore_get()
rbuildignore <- .projr_buildignore_get()

dir_path <- fs::path_rel(dir_path, dir_proj)

txt_gitignore <- paste0(gsub("/*$", "", dir_path), "/**/*")
txt_rbuildignore <- paste0("^", gsub("\\.", "\\\\.", dir_path))

ignore <- yml_active_dir[["ignore"]]
if (is.null(ignore)) {
  ignore <- TRUE
}

invisible(TRUE)

# ==================================

fs::path_abs(dir_proj)


.test_set()
library(testthat)
projr::projr_build_output()

x <- 1
while (x < 10) {
  print(x)
  x <- x + 1
  break
}

# old clear dev version
# ==================================

.projr_build_clear_dev <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  proj_nm <- projr_name_get()
  version_format_list <- .projr_version_format_list_get()

  # clear old docs
  # -----------------------
  match_regex <- paste0(
    "^",
    proj_nm,
    "V\\d+",
    paste0("\\", version_format_list[["sep"]], "\\d+", collapse = ""),
    "$"
  )
  dir_report <- basename(
    list.dirs(dirname(
      projr_path_get_dir("docs", create = FALSE)
    ), recursive = FALSE)
  )
  dir_report_rm <- dir_report[grepl(match_regex, dir_report)]
  for (i in seq_along(dir_report_rm)) {
    unlink(file.path(
      dirname(projr_dir_get("docs", create = FALSE)), dir_report_rm[[i]]
    ), recursive = TRUE)
  }

  # clear old output
  # --------------------

  dir_version <- projr_path_get_dir("output", output_safe = FALSE)
  version_fn <- paste0("VERSION - ", projr_version_get())
  file.create(file.path(dir_version, version_fn))

  dir_tmp_vec_output <- list.dirs(
    projr_path_get_dir("cache", "projr_output"),
    recursive = FALSE
  )

  for (x in dir_tmp_vec_output) {
    unlink(x, recursive = TRUE)
  }

  invisible(TRUE)
}

# old initial question prompt stuff
# ================================

# initial question
answer_list <- .projr_init_prompt_ind_question(
  default_given = default_given,
  .var = .var,
  nm_item_long = nm_item_long,
  option_default = option_default,
  option_other = option_other
)

# gave no answer, want to complete later
if (!answer_list[["completed"]]) {
  return(.var)
}

check_correct <- .projr_init_prompt_ind_check(
  nm_long = nm_item_long,
  nm_item = answer_list[["nm"]],
  answer_translate = answer_translate,
  option_check = option_check
)

# old prompt stuff
# =======================


  while (ask_item) {
    if (!is.null(option_default)) {

    }
    cat(paste0(request_default_n, "\n"))
    nm_item <- readline(prompt = ">> ")
    if (!is.null(answer_translate)) {
      nm_item_check <- answer_translate[[nm_item]]
    } else {
      nm_item_check <- nm_item
    }
    check_init <- paste0("Is the ", nm_item_long, " `", nm_item_check, "` correct?") # nolint
    answer_item <- utils::menu(option_check, title = check_init)
    cat("\n")
    ask_item <- answer_item == 2
    completed_item <- answer_item != 3
  }
  if (!completed_item) {
    nm_item_check <- .var
  }
  nm_item_check
}


# old version getting stuff
# =========================

version_bd <- substr(
  yml_bd$book_filename,
  start = nchar(proj_nm) + 2,
  stop = nchar(yml_bd$book_filename)
)
version_bd_vec <- strsplit(
  version_bd,
  split = "\\-|\\."
)[[1]]
version_bd_vec <- as.numeric(version_bd_vec)
version_desc_vec <- strsplit(
  desc[1, "Version"][[1]],
  split = "\\-|\\."
)[[1]]

version_desc_vec <- as.numeric(version_desc_vec)
version_orig_vec <- rep("", length(version_bd))
diff_ind_vec <- which(
  version_bd_vec[seq_len(length(version_desc_vec))] != version_desc_vec
)
if (length(diff_ind_vec) == 0) {
  use_bd_vec <- TRUE
} else {
  diff_ind_min <- min(diff_ind_vec)
  use_bd_vec <- version_bd_vec[diff_ind_min] > version_desc_vec[diff_ind_min]
}
if (use_bd_vec) {
  version_orig_vec <- version_bd_vec
} else {
  if (length(version_desc_vec) < length(version_bd_vec)) {
    if (version_bd_vec[length(version_bd_vec)] >= 9000) {
      version_desc_vec <- c(version_desc_vec, 9000)
    } else {
      version_desc_vec <- c(version_desc_vec, 1)
    }
  }
  version_orig_vec <- version_desc_vec
}

# old piggyback stuff
# =======================

.projr_pb_upload_doc <- function(bump_component) {
  # get settings
  yml_projr_gh <- projr_yml_get()[["build"]][["github-release"]]
  yml_projr_gh_doc <- yml_projr_gh[["docs"]]

  # consider not uploading
  # ----------------------------
  if (!yml_projr_gh_doc[["add"]]) {
    return(invisible(FALSE))
  }
  if (!.projr_version_comp_min_check(
    bump_component = bump_component,
    version_min = yml_projr_gh_doc[["version-component-min"]]
  )
  ) {
    return(invisible(FALSE))
  }

  # zip
  # --------------------------
  dir_doc <- projr_path_get_dir("docs")
  path_zip <- file.path(dirname(dir_doc), "doc.zip")
  if (file.exists(path_zip)) {
    file.remove(path_zip)
  }
  if (!dir.exists(dirname(path_zip))) {
    dir.create(dirname(path_zip))
  }
  setwd(dir_doc)
  path_zip <- paste0(basename(dir_doc), ".zip")
  utils::zip(
    path_zip,
    files = list.files(
      getwd(),
      recursive = TRUE, full.names = FALSE, all.files = TRUE
    ),
    flags = "-r9Xq"
  )

  # upload
  # --------------------------
  setwd(dir_proj)
  gh_tbl_release <- suppressWarnings(suppressMessages(
    piggyback::pb_releases()
  ))
  tag <- switch(item_list[["name"]],
    "@version" = paste0("v", version_current),
    item_list[["name"]]
  )
  body <- switch(item_list[["name"]],
    "@version" = paste0("Version-linked source code, project inputs and/or outputs"),
    "Project source code, inputs and/or outputs"
  )
  if (!tag %in% gh_tbl_releases[["release_name"]]) {
    piggyback::pb_release_create(tag = tag, body = body)
    piggyback::pb_upload(
      file = file.path(dir_doc, path_zip), overwrite = TRUE, tag = tag
    )
  } else {
    piggyback::pb_upload(
      file = file.path(dir_doc, path_zip), overwrite = TRUE, tag = tag
    )
  }
  invisible(TRUE)
}

# add things up
# ----------------------------


.projr_pb_upload_code <- function(bump_component) {
  yml_projr_gh <- projr_yml_get()[["github-release"]]
  if (!"source-code" %in% names(yml_projr_gh)) {
    return(invisible(FALSE))
  }

  gh_tbl_release <- suppressWarnings(suppressMessages(
    piggyback::pb_releases()
  ))

  # consider exiting
  # ------------------

  upload <- .projr_pb_upload_consider(
    yml_projr_gh_item = yml_projr_gh[[
      which(names(yml_projr_gh) == "source-code")
    ]],
    gh_tbl_release = gh_tbl_release,
    bump_component = bump_component
  )
  if (!upload) {
    return(invisible(FALSE))
  }

  # upload
  # ------------------

  tag <- paste0("v", projr_version_get())
  body <- "Project source code, inputs and/or outputs"


  # remove if already uploaded, as source is always latest
  if (tag %in% gh_tbl_release[["release_name"]]) {
    piggyback::pb_release_delete(tag = tag)
  }

  # always create new source code update
  # (and source code always first to be updated)
  piggyback::pb_release_create(tag = tag, body = body)
  invisible(TRUE)
}


# new new old archive
# ====================


            # so now, this is the confusing one as archive is character.
            # we specifically want to archive to particular directories, I guess
            # (over and above any archiving done via output to other directories).
            # so we do this only if we do not archive to *these* directories.

            # clearly this does not happen if output is not specified
            if (!output_present) {
              continue_archive <- TRUE
              # output is now logical.
              # so, we will not archive to these directories
              # indirectly if output is FALSE.
              # We will archive to these directories if
              # output is TRUE and archive is TRUE for any (though in this case
              # we will not archive to all; if you want to archive to all, you
              # need to specify specifically where you want to archive).
              # actually, no, we need to make `archive: TRUE` behave 
              # like `output: TRUE` and archive to every output directory.
              # we just don't if all the outputs also do.
            } else if (output_logical) {
              if (!all(output_val)) {
                continue_archive <- TRUE
              } else {
                # saved to all outputs, so check if any are
                # archived
                output_key_ind <- grepl(
                  "^output",
                  .projr_dir_label_strip(names(yml_projr_dir))
                )
                output_key_vec <- names(yml_projr_dir)[output_key_ind]
                archived_via_output_vec <- vapply(
                  output_key_vec, function(x) {
                  yml_projr_dir_output <- yml_projr_dir[[x]]
                  if (all(is.logical(yml_projr_dir_output[["archive"]]))) {
                    return(all(yml_projr_dir_output[["archive"]]))
                  }
                  TRUE
                }, logical(1))
                continue_archive <- !any(archived_via_output_vec)
              }
          }
        output_false <- "archive" %in% names(yml_projr_dir[[label]])
        if (!output_specified_ind || ()) {
          # archive is specified and is logical, continue only
          # if archive is TRUE
          if (all(is.logical(yml_projr_dir[[label]][["archive"]]))) {
            continue_archive <- all(yml_projr_dir[[label]][["archive"]])
            # archive character
          } else if (all(is.character(yml_projr_dir[[label]][["archive"]]))) {
            continue_archive <- TRUE
            # archive not specified, don't archive
          } else {
            continue_archive <- FALSE
          }
          # output is specified
        } else {
          # - output is FALSE and archive is specified
          if (all(is.logical(yml_projr_dir[[label]][["output"]]))) {
            if (all(!yml_projr_dir[[label]][["output"]])) {
              continue_archive_output_false <- TRUE
            } else {
              continue_ <- FALSE
            }
            # - output is character, and none of those outputs
            # correspond to this archive
          } else if (all(is.character(yml_projr_dir[[label]][["output"]]))) {
            continue_archive_output_archive_else <- !(
              label %in% yml_projr_dir[[label]][["output"]]
            )
          }
          continue_archive <- continue_archive_output_missing ||
            continue_archive_output_false ||
            continue_archive_output_archive_else
        }
      }
      if (!continue_archive) {
        next
      }
  
# new old archive
# =====================

    yml_projr_dir_output <- yml_projr_dir[[x]]
    # archive not specified, it will be
    # archived there
    if (!"archive" %in% names(yml_projr_dir_output)) {
      return(TRUE)
    }
    # archive is logical, so will be archived
    # there
    if (all(is.logical(yml_projr_dir_output[["archive"]]))) {
      return(all(yml_projr_dir_output[["archive"]]))
    }
    # archive is character, so will be archived somewhere
    if (all(is.character(yml_projr_dir_output[["archive"]]))) {
      return(TRUE)
    }

# old archive
# =====================

test_that("projr_build_copy_dir works when archiving", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  .test_set()

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr_init <- .projr_yml_get_root_full()
      # do nothing when not output
      expect_false(.projr_build_archive(output_run = FALSE))
      version_run_on_list <- .projr_version_run_onwards_get(
        bump_component = "patch"
      )
      # do nothing when nothing saved
      expect_false(.projr_build_archive(
        output_run = FALSE,
        version_run_on_list = version_run_on_list
      ))

      # save files to output
      invisible({
        file.create(
          projr_path_get("output", "a.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "b.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_c", "c.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_d", "d.txt", output_safe = FALSE)
        )
      })

      .projr_build_archive(
        output_run = TRUE,
        version_run_on_list = version_run_on_list
      )

      dir_archive <- projr_path_get_dir(
        label = "archive",
        paste0("v", version_run_on_list$desc[["success"]])
      )
      expect_true(file.exists(file.path(dir_archive, "a.txt")))
      expect_true(file.exists(file.path(dir_archive, "b.txt")))
      expect_true(file.exists(file.path(dir_archive, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_archive, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_archive, "dir_c")))
      expect_false(dir.exists(file.path(dir_archive, "dir_d")))
    },
    quiet = TRUE,
    force = TRUE
  )
  .test_unset()
  unlink(dir_test, recursive = TRUE)
})


.projr_build_archive <- function(output_run, version_run_on_list) {
  # How is this supposed to work?

  # consider not archiving
  if (!output_run) {
    return(invisible(FALSE))
  }

  # what do we archive, and to where?
  # well, if we consider data-raw and cache and output,
  # then:

  # set up paths
  dir_output <- projr_path_get_dir(label = "output", output_safe = FALSE)
  dir_archive <- projr_path_get_dir(
    label = "archive",
    paste0("v", version_run_on_list$desc[["success"]])
  )
  if (!fs::is_absolute_path(dir_output)) {
    dir_output <- file.path(dir_proj, dir_output)
  }
  if (!fs::is_absolute_path(dir_archive)) {
    dir_archive <- file.path(dir_proj, dir_archive)
  }

  # check if there is anything to copy
  fn_vec <- list.files(
    dir_output,
    recursive = FALSE, all.files = TRUE, full.names = TRUE
  )
  if (length(fn_vec) == 0) {
    return(invisible(FALSE))
  }

  # copy individual files across
  fn_vec_fn <- fn_vec[fs::is_file(fn_vec)]
  if (length(fn_vec_fn) > 0) {
    file.copy(
      from = fn_vec_fn,
      to = file.path(dir_archive, basename(fn_vec_fn))
    )
  }

  # zip and copy directories across
  dir_vec <- list.dirs(dir_output, recursive = FALSE, full.names = TRUE)
  for (i in seq_along(dir_vec)) {
    path_dir <- dir_vec[i]
    path_zip <- file.path(dir_archive, paste0(basename(path_dir), ".zip"))
    .projr_zip_dir(
      path_dir = path_dir,
      path_zip = path_zip
    )
  }

  invisible(TRUE)
}

# old docs projr_dir_get settings
# =====================
  if (label == "docs") {

    if (file.exists(file.path(dir_proj, "_bookdown.yml"))) {
      yml_bd <- .projr_yml_bd_get()
      dir_base <- dir_active[["docs"]][["path"]]
      fn <- basename(yml_bd[["output_dir"]])
      path_bd <- file.path(dir_base, fn)
      yml_bd[["output_dir"]] <- path_bd
      .projr_yml_bd_set(yml_bd)
      path_final <- file.path(dir_base, fn, ...)
    } else {
      # quarto stuff
    }
    if (!fs::is_absolute_path(path_final)) {
      path_final <- fs::path_rel(
        file.path(rprojroot::is_r_package$find_file(), path_final),
        start = getwd()
      ) |>
        as.character()
    }
    if (!dir.exists(path_final)) {
      dir.create(path_final, recursive = TRUE)
    }
    return(as.character(path_final))
  }

# Archiving stuf
# =================

    # archive package (to all archives)
    dir_vec_match <- tolower(
      gsub("_", "", gsub("-", "", names(yml_projr[["directories"]])))
    )
    archive_vec_ind <- which(grepl("^archive", dir_vec_match))
    archive_vec <- names(yml_projr[["directories"]])[archive_vec_ind]
    for (x in archive_vec) {
      file.copy(
        from = path_pkg,
        to = projr_path_get(x, fn_pkg, output_safe = !output_run)
      )
    }


# QUITE DEVELOPED PIGGYBACK STUFF
# ====================================

version_format_list <- .projr_version_format_list_get()
bump_component <- "minor"
version_current <- "0.1.0"

yml_projr_dir <- projr_yml_get()[["directories"]]
  if (output_run && "github-release" %in% names(yml_projr[["build-output"]])) {
    # GitHub releases settings
    gh_list_settings <- yml_projr[["build-output"]][["github-release"]]
    # version information
    version_comp_vec <- version_format_list[["component"]] |>
        setdiff("dev")
    version_comp_vec <- rev(version_comp_vec)[
        seq_len(which(rev(version_comp_vec == item_version)))
    ]
    version_current <- projr_version_get()
    # ensure `piggyback` is available
    if (!requireNamespace("piggyback", quietly = TRUE)) {
        renv::install("piggyback")
    }
    # current releases
    gh_tbl_releases <- suppressWarnings(suppressMessages(
      piggyback::pb_releases()
      ))
    .projr_pb_upload_code(
      gh_tbl_release = gh_tbl_release,
      bump_component = bump_component
    )
    gh_list <- gh_list[-which(names(gh_list) == "source-code")]
    for (i in seq_along(gh_list)) {
        gh_tbl_releases <- piggyback::pb_releases()

        # =============================
        # sorting out tag and body
        # =============================

        label <- names(gh_list)[i]
        if (!item_list[["add"]]) {
            next
        }
        tag <- switch(item_list[["name"]],
            "@version" = paste0("v", version_current),
            item_list[["name"]]
        )
        body <- switch(item_list[["name"]],
            "@version" = paste0(
              "Version-linked source code, project inputs and/or outputs"
              ),
            "Project source code, inputs and/or outputs"
        )

        # ============================
        # uploading report
        # ============================

        if (label == "bookdown") {
            dir_bookdown <- projr_path_get_dir("bookdown")
            path_zip <- file.path(dirname(dir_bookdown), "doc.zip")
            if (file.exists(path_zip)) {
              file.remove(path_zip)
            }
            if (!dir.exists(dirname(path_zip))) {
              dir.create(dirname(path_zip))
            }
            setwd(dir_bookdown)
            path_zip <- paste0(basename(dir_bookdown), ".zip")
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip),
                    overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip),
                    overwrite = TRUE, tag = tag
                )
            }
            next
        }

        # ======================================
        # uploading non-report and non-source-code items
        # ======================================

        dir_input <- projr_path_get_dir("label", output_safe = FALSE)
        if (!fs::is_absolute_path(dir_input)) {
            dir_input <- file.path(dir_proj, dir_input)
        }
        fn_vec <- list.files(
            dir_input,
            recursive = TRUE, all.files = TRUE,
            full.names = TRUE
        )

        # removing other items from output directory if present
        # -----------------------------------------------

        if (label == "output") {
            nm_rem_vec <- names(yml_projr_dir)
            nm_rem_vec <- nm_rem_vec[!grepl("^archive|^output$|^bookdown$", nm_rem_vec)]
            for (i in seq_along(nm_rem_vec)) {
                fn_vec <- fn_vec[
                    !basename(fn_vec) %in% paste0(nm_rem_vec, ".zip")
                ]
            }
            dir_bookdown <- projr_path_get_dir("bookdown")
            fn_vec <- fn_vec[
                !basename(fn_vec) == paste0(basename(dir_bookdown), ".zip")
            ]
        }

        # =======================================
        # BEGIN HERE
        # =======================================

        if (length(fn_vec) == 0) next

        if (is.null(zip_val)) {
            zip_ind <- TRUE
        } else {
            zip_ind <- zip_val
        }

        if (zip_ind) {
            setwd(dir_input)
            path_zip <- paste0(label, ".zip")
            if (file.exists(path_zip)) {
                file.remove(path_zip)
            }
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            }
            unlink(file.path(dir_input, path_zip))
        } else {
            setwd(dir_input)
            fn_vec <- list.files(
                getwd(),
                all.files = TRUE, recursive = TRUE, full.names = FALSE,
            )
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            }
        }
    }
}


# UNDEVELOPED PIGGYBACK STUFF
# ===================================

  if (output_run && "github-release" %in% names(yml_projr[["build-output"]])) {
    version_comp_vec <- version_format_list[["component"]] |>
      setdiff("dev")
    version_comp_vec <- version_comp_vec[
      seq_len(which(version_comp_vec == bump_component))
    ]
    gh_list <- yml_projr[["build-output"]][["github-release"]]
    for (i in seq_along(gh_list)) {
      nm <- names(gh_list)[i]
      item_list <- gh_list[[i]]
      item_version <- item_list[["version-component-min"]]
      if (item_version != "any") {
        if (!item_version %in% version_comp_vec) next
      }
      if (!exists("gh_releases_list")) {
        renv_dep_file <- readLines(
          file.path(dir_proj, "_dependencies.R")
        )
        if (!"library(piggyback)" %in% renv_dep_file) {
          renv_dep_file <- c(renv_dep_file, "library(piggyback)", "")
          writeLines(renv_dep_file, file.path(dir_proj, "_dependencies.R"))
        }
        if (!requireNamespace("piggyback", quietly = TRUE)) {
          renv::install("piggyback")
        }
        gh_releases_list <- piggyback::pb_releases()
      }
      if (nm == "source-code") {
        #  piggyback::pb_
      }
    }
  }




############
## projr_path_get
############

    version_comp_vec <- version_format_list[["component"]] |>
      setdiff("dev")
    version_comp_vec <- version_comp_vec[
      seq_len(which(version_comp_vec == bump_component))
    ]
    gh_list <- yml_projr[["build-output"]][["github-release"]]
    for (i in seq_along(gh_list)) {
      nm <- names(gh_list)[i]
      item_list <- gh_list[[i]]
      item_version <- item_list[["version-component-min"]]
      if (item_version != "any") {
        if (!item_version %in% version_comp_vec) next
      }
      if (!exists("gh_releases_list")) {
        renv_dep_file <- readLines(
          file.path(dir_proj, "_dependencies.R")
        )
        if (!"library(piggyback)" %in% renv_dep_file) {
          renv_dep_file <- c(renv_dep_file, "library(piggyback)", "")
          writeLines(renv_dep_file, file.path(dir_proj, "_dependencies.R"))
        }
        if (!requireNamespace("piggyback", quietly = TRUE)) {
          renv::install("piggyback")
        }
        gh_releases_list <- piggyback::pb_releases()
      }
      if (nm == "source-code") {
        #  piggyback::pb_
      }
    }


#############
# HASH STUF
#############

library(testthat)
devtools::load_all()
# options(error = recover, warn = 2)
# dir_temp <- file.path(tempdir(), "test")
# debugonce(".projr_init_prompt")
.test_set()
projr_init()

cd /tmp; rm -rf testProjr; mkdir testProjr; cd testProjr; radian

testthat::test_file("tests/testthat/test-dir_create.R")
testthat::test_file("tests/testthat/test-build.R")

# code coverage
covr::report(file = "report.html", browse = FALSE)
cp$projr / report.html$w_dnld /

  path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2022_02", "sun", "version_1"
)


fn_vec <- list.files(path_dir, full.names = TRUE)
x <- readChar(fn_vec[1], file.info(fn_vec[1])$size)
digest::digest(x)

path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2021_01", "sun", "version_3", "WBA-ILC_20210319-ReAnalysis"
)
# standard digest
fn_vec <- list.files(path_dir, full.names = TRUE)
time_start <- proc.time()[3]
vapply(fn_vec[2:3], function(fn) {
  digest::digest(fn, serialize = FALSE, file = TRUE)
}, character(1))
time_end <- proc.time()[3]
(time_end - time_start) / 60

g
# vdigest and with a different algorithm
path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2021_01", "sun", "version_3", "WBA-ILC_20210319-ReAnalysis"
)
vdigest_file <- digest::getVDigest(algo = "xxhash64", errormode = "warn")
fn_vec <- list.files(path_dir, full.names = TRUE)
time_start_v <- proc.time()[3]
vapply(fn_vec[2:3], vdigest_file, character(1), serialize = FALSE, file = TRUE)
time_end_v <- proc.time()[3]
(time_end_v - time_start_v) / 60

x <- readBin(fn_vec[1], "raw")
digest::digest(x)
fn_vec <- "/mnt/h/Shared drives/ProjectILC/DataRawILC/2021_01/sun/version_3/WBA-ILC_20210319-ReAnalysis/HD10005_SATVI_20210308_MTBLysate_028.zip"
x <- readChar(fn_vec[1], file.info(fn_vec[1])$size)
"H:/Shared drives/ProjectILC/DataRawILC/2021_01/sun/version_3/HD10005_SATVI_20210308_MTBLysate_028.zip"


# =======================
# OLD TEST
# =======================

      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- yml_projr[["build-dev"]][
        -which(names(yml_projr[["build-dev"]]) == "copy-to-output")
      ]
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- list(a = FALSE, b = FALSE)
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)

      yml_projr <- projr_yml_get()
      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      dir.create("docs/reportV0.0.0-1", recursive = TRUE)
      dir.create("docs/reportV0.0.0-9000", recursive = TRUE)
      yml_projr <- .projr_yml_get()
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      projr_build_output(quiet = TRUE)
      expect_true(!dir.exists("docs/reportV0.0.0-1"))
      expect_true(!dir.exists("docs/reportV0.0.0-9000"))
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)),
        c("VERSION - 0.0.1", "reportV0.0.1.zip")
      )

      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      yml_projr <- .projr_yml_get()
      # check that copying non-default directories works as well
      copy_list <- list(
        `data-raw` = TRUE, cache = TRUE, bookdown = FALSE, package = TRUE
      )
      yml_projr[["build-output"]][["copy-to-output"]] <- copy_list
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      if (!dir.exists("_data_raw")) {
        dir.create("_data_raw")
      }
      invisible(file.create("_data_raw/test.txt"))
      if (!dir.exists("_tmp")) {
        dir.create("_tmp")
      }
      invisible(file.create("_tmp/test.txt"))
      dir_output_safe <- "_tmp/projr_output/0.0.2"
      if (!dir.exists(dir_output_safe)) {
        dir.create(dir_output_safe)
      }
      file.create(file.path(dir_output_safe, "abc.txt"))
      dir.create(file.path(dir_output_safe, "test_dir"))
      file.create(file.path(dir_output_safe, "test_dir", "def.txt"))

      invisible(file.create("_tmp/test.txt"))

      projr_build_output(quiet = TRUE)
      expect_identical(
        list.files("_tmp/projr_output/0.0.2"),
        character(0)
      )
      expect_identical(
        list.files("_output", recursive = TRUE),
        c(
          "VERSION - 0.0.2", "abc.txt", "cache.zip",
          "data-raw.zip", "report_0.0.2.tar.gz",
          "test_dir/def.txt"
        )
      )
      # test that it runs correctly when there is an error
      yml_bd <- .projr_yml_bd_get()
      writeLines(
        c("# Error", "\n", "```{r }", "\n", "stop()", "```"),
        con = "error.Rmd"
      )
      yml_bd[["rmd_files"]] <- c(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      # expect_error(projr_build_output(quiet = TRUE))
      # reset after error
      yml_bd <- .projr_yml_bd_get()
      file.remove("error.Rmd")
      yml_bd[["rmd_files"]] <- c("index.Rmd", "appendix.Rmd")
      .projr_yml_bd_set(yml_bd)


# =============================
# OLD BUILD TESTING
# =============================

test_that("projr_build_output works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  .test_set()

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr_init <- .projr_yml_get()
      yml_bd_init <- .projr_yml_bd_get()
      expect_false(.projr_build_output(output_run = FALSE))
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- yml_projr[["build-dev"]][
        -which(names(yml_projr[["build-dev"]]) == "copy-to-output")
      ]
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- list(a = FALSE, b = FALSE)
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)

      yml_projr <- projr_yml_get()
      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      dir.create("docs/reportV0.0.0-1", recursive = TRUE)
      dir.create("docs/reportV0.0.0-9000", recursive = TRUE)
      yml_projr <- .projr_yml_get()
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      projr_build_output(quiet = TRUE)
      expect_true(!dir.exists("docs/reportV0.0.0-1"))
      expect_true(!dir.exists("docs/reportV0.0.0-9000"))
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)),
        c("VERSION - 0.0.1", "reportV0.0.1.zip")
      )

      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      yml_projr <- .projr_yml_get()
      # check that copying non-default directories works as well
      copy_list <- list(
        `data-raw` = TRUE, cache = TRUE, bookdown = FALSE, package = TRUE
      )
      yml_projr[["build-output"]][["copy-to-output"]] <- copy_list
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      if (!dir.exists("_data_raw")) {
        dir.create("_data_raw")
      }
      invisible(file.create("_data_raw/test.txt"))
      if (!dir.exists("_tmp")) {
        dir.create("_tmp")
      }
      invisible(file.create("_tmp/test.txt"))
      dir_output_safe <- "_tmp/projr_output/0.0.2"
      if (!dir.exists(dir_output_safe)) {
        dir.create(dir_output_safe)
      }
      file.create(file.path(dir_output_safe, "abc.txt"))
      dir.create(file.path(dir_output_safe, "test_dir"))
      file.create(file.path(dir_output_safe, "test_dir", "def.txt"))

      invisible(file.create("_tmp/test.txt"))

      projr_build_output(quiet = TRUE)
      expect_identical(
        list.files("_tmp/projr_output/0.0.2"),
        character(0)
      )
      expect_identical(
        list.files("_output", recursive = TRUE),
        c(
          "VERSION - 0.0.2", "abc.txt", "cache.zip",
          "data-raw.zip", "report_0.0.2.tar.gz",
          "test_dir/def.txt"
        )
      )
      # test that it runs correctly when there is an error
      yml_bd <- .projr_yml_bd_get()
      writeLines(
        c("# Error", "\n", "```{r }", "\n", "stop()", "```"),
        con = "error.Rmd"
      )
      yml_bd[["rmd_files"]] <- c(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      # expect_error(projr_build_output(quiet = TRUE))
      # reset after error
      yml_bd <- .projr_yml_bd_get()
      file.remove("error.Rmd")
      yml_bd[["rmd_files"]] <- c("index.Rmd", "appendix.Rmd")
      .projr_yml_bd_set(yml_bd)
    },
    quiet = TRUE,
    force = TRUE
  )
  .test_unset()
  unlink(dir_test, recursive = TRUE)
})
