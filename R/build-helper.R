# ============================
# Pre/post
# ============================

# renv
.projr_build_renv_snapshot <- function(output_run) {
  if ((!output_run) || (Sys.getenv("PROJR_TEST") == "TRUE")) {
    return(invisible(FALSE))
  }
  yml_projr <- .projr_yml_get()
  if ("renv" %in% names(yml_projr[["build-output"]])) {
    if (!yml_projr[["build-output"]][["renv"]]) {
      return(invisible(FALSE))
    }
  }
  renv::snapshot(prompt = FALSE)
  invisible(TRUE)
}

# commit
.projr_build_git_commit <- function(output_run,
                                    bump_component,
                                    version_run_on_list,
                                    stage,
                                    msg) {
  yml_projr <- .projr_yml_get()

  # exit early if required
  # ------------------------
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (!"git" %in% names(yml_projr[["build-output"]])) {
    return(invisible(FALSE))
  }
  if (!yml_projr[["build-output"]][["git"]][["commit"]]) {
    return(invisible(FALSE))
  }
  if (!dir.exists(".git")) {
    stop("Git commits requested but no Git directory found")
  }

  # get commit message
  # ------------------
  msg_commit <- .projr_build_git_msg_get(
    stage = stage,
    version_run_on_list = version_run_on_list,
    bump_component = bump_component,
    msg = msg
  )

  # commit
  # ------------------
  if (!requireNamespace("gert", quietly = TRUE)) {
    renv::install("gert")
  }
  git_tbl_status <- gert::git_status()

  # exit now if nothing to commit
  if (nrow(git_tbl_status) == 0) {
    return(invisible(FALSE))
  }

  if (yml_projr[["build-output"]][["git"]][["add-untracked"]]) {
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
  invisible(TRUE)
}

# commit messages
.projr_build_git_msg_get <- function(stage,
                                     version_run_on_list,
                                     bump_component,
                                     msg) {
  msg_init <- switch(stage,
    "pre" = paste0(
      "State before ",
      bump_component,
      " bump to ",
      version_run_on_list[["desc"]][["success"]]
    ),
    "post" = paste0(
      paste0(
        toupper(substr(bump_component, 1, 1)),
        substr(bump_component, 2, nchar(bump_component))
      ),
      " bump to ",
      version_run_on_list[["desc"]][["success"]]
    ),
    "dev" = paste0(
      "Begin dev version ", version_run_on_list$bd[["success"]]
    )
  )
  if (nzchar(msg)) {
    msg_final <- paste0(msg_init, ": ", msg)
  } else {
    msg_final <- msg_init
  }
  msg_final
}

# ==========================
# Pre-build
# ==========================

# ignore
.projr_build_ignore <- function() {
  yml_projr <- projr_yml_get()
  for (x in names(yml_projr[["directories"]])) {
    .projr_dir_ignore(label = x)
  }
  .projr_dir_ignore("bookdown")
  invisible(TRUE)
}

.projr_build_version_set_pre <- function(version_run_on_list) {
  projr_version_set(version_run_on_list$desc[["run"]], "DESCRIPTION")
  projr_version_set(version_run_on_list$bd[["run"]], "bookdown")
  invisible(TRUE)
}

.projr_build_doc_output_dir_update <- function() {
  yml_bd <- .projr_yml_bd_get()
  yml_bd[["output_dir"]] <- projr_dir_get("bookdown")
  .projr_yml_bd_set(yml_bd)
  invisible(projr_dir_get("bookdown"))
}

.projr_build_output_clear <- function(output_run) {
  # consider not clearing if not dev
  # --------------------------
  if (!output_run) {
    yml_projr <- .projr_yml_get()
    # don't clear if nothing specific said
    if (!"clear_output" %in% names(yml_projr[["build-dev"]])) {
      return(invisible(FALSE))
    }
    # clear if specifically told said to clear
    if (!yml_projr[["build-dev"]][["copy-to-output"]]) {
      return(invisible(FALSE))
    }
  }

  # clear
  # -----------------

  # clear output directory
  dir_data_output <- projr_dir_get("output", output_safe = !output_run)
  if (dir.exists(dir_data_output)) {
    fn_vec <- list.files(dir_data_output, recursive = TRUE, full.names = TRUE)
    for (i in seq_along(fn_vec)) {
      unlink(fn_vec[i])
    }
  }
  # clear docs folder
  dir_data_output <- projr_dir_get("bookdown")
  if (dir.exists(dir_data_output)) {
    fn_vec <- list.files(dir_data_output, recursive = TRUE, full.names = TRUE)
    for (i in seq_along(fn_vec)) {
      unlink(fn_vec[i])
    }
  }
  # clear data folder
  if (output_run) {
    dir_data_r <- file.path(rprojroot::is_r_package$find_file(), "data")
    if (dir.exists(dir_data_r)) {
      fn_vec <- list.files(dir_data_r, recursive = TRUE, full.names = TRUE)
      for (i in seq_along(fn_vec)) {
        unlink(fn_vec[i])
      }
    }
  }
  invisible(TRUE)
}

# ==========================
# Post-build
# ==========================

.projr_build_roxygenise <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  dir_proj <- rprojroot::is_r_package$find_file()
  suppressMessages(suppressWarnings(invisible(
    roxygen2::roxygenise(package.dir = dir_proj)
  )))
  invisible(TRUE)
}

.projr_build_readme_rmd_render <- function(output_run) {
  dir_proj <- rprojroot::is_r_package$find_file()
  if ((!file.exists(file.path(dir_proj, "README.Rmd"))) || (!output_run)) {
    return(invisible(FALSE))
  }
  readme_rmd <- readLines(file.path(dir_proj, "README.Rmd"))
  pkg_use_detected_lib <- grepl(
    paste0(
      "library\\(", projr_name_get(), "\\)|",
      'library\\("', projr_name_get(), '"\\)|',
      "library\\('", projr_name_get(), "'\\)"
    ),
    readme_rmd
  ) |>
    any()
  pkg_use_detected_dots <- grepl(
    paste0(projr_name_get(), "::"),
    readme_rmd
  ) |>
    any()
  pkg_use_detected <- pkg_use_detected_lib | pkg_use_detected_dots
  if (pkg_use_detected) {
    renv_dep_file <- readLines(
      file.path(dir_proj, "_dependencies.R")
    )
    if (!"library(devtools)" %in% renv_dep_file) {
      renv_dep_file <- c(renv_dep_file, "library(devtools)", "")
      writeLines(renv_dep_file, file.path(dir_proj, "_dependencies.R"))
    }
    if (!requireNamespace("devtools", quietly = TRUE)) {
      renv::install("devtools")
    }
    devtools::install()
  }
  rmarkdown::render(
    file.path(dir_proj, "README.Rmd"),
    output_format = "md_document",
    quiet = TRUE
  )
  invisible(TRUE)
}

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
      projr_dir_get("bookdown", create = FALSE)
    ), recursive = FALSE)
  )
  dir_report_rm <- dir_report[grepl(match_regex, dir_report)]
  for (i in seq_along(dir_report_rm)) {
    unlink(file.path(
      dirname(projr_dir_get("bookdown", create = FALSE)), dir_report_rm[[i]]
    ), recursive = TRUE)
  }

  # clear old output
  # --------------------

  dir_version <- projr_dir_get("output", output_safe = FALSE)
  version_fn <- paste0("VERSION - ", projr_version_get())
  file.create(file.path(dir_version, version_fn))

  dir_tmp_vec_output <- list.dirs(
    projr_dir_get("cache", "projr_output"),
    recursive = FALSE
  )

  for (x in dir_tmp_vec_output) {
    unlink(x, recursive = TRUE)
  }

  invisible(TRUE)
}

.projr_build_output <- function(output_run,
                                bump_component,
                                version_run_on_list) {
  yml_projr <- .projr_yml_get()

  # consider not copying
  # -------------------

  # considerations for dev runs
  if (!output_run) {
    if (!"copy-to-output" %in% names(yml_projr[["build-dev"]])) {
      return(invisible(FALSE))
    }
    if (!yml_projr[["build-dev"]][["copy-to-output"]]) {
      return(invisible(FALSE))
    }
  }

  # exit if nothing is to be copied
  if (all(!unlist(yml_projr[["build-output"]][["copy-to-output"]]))) {
    return(invisible(FALSE))
  }

  # copy
  # -------------------

  # items saved directly to output
  .projr_build_copy_output_direct(output_run = output_run)

  # package
  .projr_build_copy_pkg(output_run = output_run)

  # items in projr directories
  .projr_build_copy_dir(bump_component, output_run)

  invisible(TRUE)
}

.projr_build_copy_output_direct <- function(output_run) {
  dir_proj <- rprojroot::is_r_package$find_file()

  # zip and then unlink any directories
  # -------------------------
  dir_data_output_safe <- projr_dir_get("output", output_safe = TRUE)

  dir_vec <- list.dirs(
    dir_data_output_safe,
    recursive = FALSE,
    full.names = TRUE
  )
  for (i in seq_along(dir_vec)) {
    path_dir <- dir_vec[i]
    if (!fs::is_absolute_path(path_dir)) {
      path_dir <- file.path(dir_proj, path_dir)
    }
    path_zip <- file.path(
      dir_data_output_safe,
      paste0(basename(dir_vec[i]), ".zip")
    )
    if (!fs::is_absolute_path(path_zip)) {
      path_zip <- file.path(dir_proj, path_zip)
    }
    .projr_zip_dir(
      path_dir = path_dir,
      path_zip = path_zip
    )
    unlink(dir_vec[i], recursive = TRUE)
  }

  # copy items from safe directory across to final directory
  # ----------------------
  if (!output_run) {
    return(invisible(TRUE))
  }
  dir_data_output_final <- projr_dir_get("output", output_safe = FALSE)

  fn_vec <- list.files(
    dir_data_output_safe,
    recursive = TRUE, full.names = TRUE
  )
  if (length(fn_vec) == 0) {
    return(invisible(TRUE))
  }
  fn_vec_rel <- fs::path_rel(fn_vec, start = dir_data_output_safe)
  file.rename(
    from = fn_vec,
    to = file.path(dir_data_output_final, fn_vec_rel)
  )
  invisible(TRUE)
}

# attempt package build first if required
# at this point, we don't need to check for
# whether it's a dev run or not
# as that's already been done.
# we copy if it says we do in build-output$copy-to-output$package
.projr_build_copy_pkg <- function(output_run) {
  yml_projr <- .projr_yml_get()
  # exit early if need be
  # ------------------------
  yml_projr_output <- yml_projr$`build-output`$`copy-to-output`
  if (!"package" %in% names(yml_projr_output)) {
    return(invisible(FALSE))
  }
  if (!yml_projr_output[["package"]]) {
    return(invisible(FALSE))
  }

  # build package
  # ------------------------
  dir_pkg <- projr_dir_get("output", output_safe = !output_run)
  version_pkg <- .projr_desc_get()[, "Version"][[1]]
  fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
  path_pkg <- file.path(dir_pkg, fn_pkg)
  pkgbuild::build(
    path = rprojroot::is_r_package$find_file(),
    dest_path = path_pkg,
    binary = FALSE,
    quiet = TRUE
  )
  invisible(TRUE)
}

.projr_build_copy_dir <- function(bump_component,
                                  output_run) {
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_projr <- projr_yml_get()
  yml_projr_dir <- yml_projr[["directories"]]
  yml_projr_copy <- yml_projr[["build-output"]][["copy-to-output"]]
  # copy data_raw and cache across, if desired
  data_raw_or_cache_ind <- grepl(
    "^data\\-raw|^cache|^data_raw|^dataraw|^bookdown|^quarto|^docs", # nolint
    tolower(names(yml_projr_copy))
  )
  label_vec <- names(yml_projr_copy)[data_raw_or_cache_ind]

  for (i in seq_along(label_vec)) {
    label <- label_vec[i]
    copy_item <- yml_projr_copy[[label]]

    # consider not copying
    # ---------------------

    # if copying is always done or not done
    if (is.logical(copy_item)) {
      if (!copy_item) {
        next
      }
    } else {
      # if copying is dependent on version component bumped
      version_min_check <- .projr_version_comp_min_check(
        bump_component = bump_component,
        version_min = as.character(copy_item)
      )
      if (!version_min_check) {
        next
      }
    }

    # zip
    # ------------------------

    # paths
    path_dir <- yml_projr_dir[[label]][["path"]]
    if (!fs::is_absolute_path(path_dir)) {
      path_dir <- file.path(dir_proj, path_dir)
    }
    path_zip <- projr_path_get(
      "output", paste0(label, ".zip"),
      output_safe = !output_run
    )
    if (!fs::is_absolute_path(path_zip)) {
      path_zip <- file.path(dir_proj, path_zip)
    }


    # exclude special folders
    # projr_output from cache folder(s)
    if (grepl("^cache", label)) {
      dir_exc <- "projr_output"
    } else {
      dir_exc <- NULL
    }
    if (grepl("^bookdown|^quarto|^docs", tolower(label))) {
      dir_inc <- paste0(
        projr_name_get(),
        "V",
        projr_version_get()
      )
    } else {
      dir_inc <- NULL
    }

    # zip
    .projr_zip_dir(
      path_dir = path_dir,
      path_zip = path_zip,
      dir_exc = dir_exc,
      dir_inc = dir_inc
    )
  }
  invisible(TRUE)
}


.projr_build_archive <- function(output_run, version_run_on_list) {
  # exit early if a dev run always
  if (!output_run) {
    return(invisible(FALSE))
  }
  dir_proj <- rprojroot::is_r_package$find_file()

  # set up paths
  dir_output <- projr_dir_get(label = "output", output_safe = FALSE)
  dir_archive <- projr_dir_get(
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

.projr_zip_dir <- function(path_dir,
                           path_zip,
                           dir_exc = NULL,
                           dir_inc = NULL,
                           fn_exc = NULL) {
  if (file.exists(path_zip)) {
    file.remove(path_zip)
  }
  if (!dir.exists(dirname(path_zip))) {
    dir.create(dirname(path_zip), recursive = TRUE)
  }
  wd_orig <- getwd()
  setwd(path_dir)
  sink(file.path(tempdir(), "zip123"))
  fn_vec <- list.files(
    getwd(),
    recursive = TRUE, full.names = FALSE, all.files = TRUE
  )
  if (!is.null(dir_exc)) {
    for (x in dir_exc) {
      fn_vec <- fn_vec[!grepl(paste0("^", x, "/"), fn_vec)]
    }
  }
  if (!is.null(dir_inc)) {
    for (x in dir_inc) {
      fn_vec <- fn_vec[grepl(paste0("^", x, "/"), fn_vec)]
    }
  }
  if (!is.null(fn_exc)) {
    fn_vec <- fn_vec[!fn_vec %in% fn_exc]
  }
  path_zip_temp <- basename(path_zip)
  utils::zip(
    path_zip_temp,
    files = fn_vec,
    flags = "-r9Xq"
  )
  sink(NULL)
  if (!identical(path_zip_temp, path_zip)) {
    file.rename(
      from = path_zip_temp,
      to = path_zip
    )
  }
  setwd(wd_orig)
  invisible(TRUE)
}

.projr_build_version_set_post <- function(version_run_on_list,
                                          success) {
  if (success) {
    projr_version_set(version_run_on_list$bd[["success"]], "bookdown")
  } else {
    projr_version_set(version_run_on_list$desc[["failure"]], "DESCRIPTION")
    projr_version_set(version_run_on_list$bd[["failure"]], "bookdown")
  }
  invisible(TRUE)
}

# commit
.projr_build_git_push <- function() {
  yml_projr <- .projr_yml_get()
  if (!"git" %in% names(yml_projr[["build-output"]])) {
    return(invisible(FALSE))
  }
  if (!"push" %in% names(yml_projr[["build-output"]][["git"]])) {
    return(invisible(FALSE))
  }
  if (!yml_projr[["build-output"]][["git"]][["push"]]) {
    return(invisible(FALSE))
  }
  gert::git_push()
  invisible(TRUE)
}
