# ============================
# Pre/post
# ============================

# renv
.projr_build_renv_snapshot <- function(output_run) {
  if ((!output_run) || (Sys.getenv("PROJR_TEST") == "TRUE")) {
    return(invisible(FALSE))
  }
  yml_projr <- projr_yml_get()
  if ("renv" %in% names(yml_projr[["build"]])) {
    if (!yml_projr[["build"]][["renv"]]) {
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
  yml_projr <- projr_yml_get()

  # exit early if required
  # ------------------------
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (!"git" %in% names(yml_projr[["build"]])) {
    return(invisible(FALSE))
  }
  if (!yml_projr[["build"]][["git"]][["commit"]]) {
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
  .projr_dir_ignore("docs")
  invisible(TRUE)
}

.projr_build_version_set_pre <- function(version_run_on_list) {
  projr_version_set(version_run_on_list$desc[["run"]])
  invisible(TRUE)
}

.projr_build_doc_output_dir_update <- function() {
  yml_bd <- .projr_yml_bd_get()
  yml_bd[["output_dir"]] <- projr_dir_get("docs")
  .projr_yml_bd_set(yml_bd)
  invisible(projr_dir_get("docs"))
}

.projr_build_output_clear <- function(output_run) {
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
  dir_data_output <- projr_dir_get("docs")
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
      projr_dir_get("docs", create = FALSE)
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


.projr_build_copy <- function(output_run,
                              bump_component,
                              version_run_on_list) {
  yml_projr <- projr_yml_get()

  # copy document across to correct directories
  # -------------------
  .projr_build_copy_docs()

  # consider not copying
  # -------------------

  # considerations for dev runs
  if (!output_run) {
    if (!"dev-output" %in% names(yml_projr[["build"]])) {
      return(invisible(FALSE))
    }
    if (yml_projr[["build"]][["dev-output"]]) {
      return(invisible(FALSE))
    }
  }

  # exit if nothing is to be copied
  if (all(!unlist(yml_projr[["build"]][["copy-to-output"]]))) {
    return(invisible(FALSE))
  }

  # copy
  # -------------------

  # items saved directly to output
  .projr_build_copy_output_direct(output_run = output_run)

  # package
  .projr_build_copy_pkg(output_run = output_run)

  # save to outpu
  .projr_build_copy_dir(output_run, dest_type = "output")

  # archive
  .projr_build_copy_dir(output_run, dest_type = "archive")

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
# we copy if it says we do in build$copy-to-output$package
.projr_build_copy_pkg <- function(output_run) {
  yml_projr <- projr_yml_get()
  # exit early if need be
  # ------------------------
  yml_projr_output <- yml_projr[["build"]]
  if (!"package" %in% names(yml_projr_output)) {
    return(invisible(FALSE))
  }
  pkg <- yml_projr_output[["package"]]
  # pkg is logical
  if (all(is.logical(pkg)) && length(pkg) == 1) {
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
    # pkg is character
  } else if (all(is.character(pkg))) {
    # check that it corresponds to a directory
    if (!all(pkg %in% names(yml_projr[["directories"]]))) {
      stop("Invalid value (non-existent projr director(ies))
      for build$package key in projr settings")
    }
    # check for cache, dataraw or archive
    for (x in pkg) {
      x_match <- .projr_dir_label_strip(x)
      if (grepl("^cache|^dataraw|^archive", x_match)) {
        stop("Invalid value (cache, dataraw or archive directory)
    for package in projr build settings")
      }
    }
    # build package, then copy afterwards
    dir_pkg <- projr_dir_get("cache")
    version_pkg <- .projr_desc_get()[, "Version"][[1]]
    fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
    if (file.exists(fn_pkg)) {
      invisible(file.remove(fn_pkg))
    }
    path_pkg <- file.path(dir_pkg, fn_pkg)
    pkgbuild::build(
      path = rprojroot::is_r_package$find_file(),
      dest_path = path_pkg,
      binary = FALSE,
      quiet = TRUE
    )
    # copy package to outputs
    for (x in pkg) {
      file.copy(
        from = path_pkg,
        to = projr_path_get(x, fn_pkg, output_safe = !output_run)
      )
    }
    # archiving to be handled by individual outputs' settings
  } else {
    stop("Invalid value (incorrect type)
    for build$package key in projr settings")
  }

  invisible(TRUE)
}

.projr_build_copy_docs <- function() {
  switch(.projr_engine_get(),
    "bookdown" = NULL,
    "quarto_project" = NULL,
    "quarto_document" = .projr_build_copy_docs_quarto(),
    "rmd" = .projr_build_copy_docs_rmd()
  )
}

# copy docs - rmd
# ------------------

.projr_build_copy_docs_rmd <- function() {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  for (fn in fn_vec_qmd) {
    .projr_build_copy_docs_rmd_ind(fn)
  }
  invisible(TRUE)
}

.projr_build_copy_docs_rmd_ind <- function(fn) {
  frontmatter_vec <- .projr_build_frontmatter_get(fn)
  format <- .projr_build_copy_docs_rmd_format_get(frontmatter_vec)
  fn_output_prefix <- .projr_build_copy_docs_rmd_fn_prefix_get(fn)
  path_vec <- .projr_build_copy_docs_rmd_path_get(format, fn_output_prefix)
  .projr_build_copy_docs_paths(path_vec)
  invisible(TRUE)
}

.projr_build_copy_docs_rmd_path_get <- function(format, fn_prefix) {
  fn_suffix <- .projr_build_copy_docs_rmd_fn_suffix_get(format)
  paste0(fn_prefix, ".", fn_suffix)
}

.projr_build_copy_docs_rmd_format_get <- function(frontmatter) {
  if (length(frontmatter) == 0) {
    return("html_document")
  }
  if (!"output" %in% names(frontmatter)) {
    return("html_document")
  }
  format <- frontmatter[["output"]]
  if (is.character(format)) {
    return(format)
  }
  names(format)[1]
}

.projr_build_copy_docs_rmd_fn_prefix_get <- function(fn) {
  gsub("\\.Rmd$|\\.rmd$", "", fn)
}

.projr_build_copy_docs_rmd_fn_suffix_get <- function(format) {
  switch(format,
    "html_notebook" = "nb.html",
    "word_document" = "docx",
    "tufte::tufte_handout" = ,
    "tufte_handout" = ,
    "tufte::tufte_book" = ,
    "tufte_book" = ,
    "context_document" = ,
    "beamer_presentation" = "pdf",
    "powerpoint_presentation" = "pptx",
    "revealjs::revealjs_presentation" = ,
    "revealjs_presentation" = ,
    "slidy_presentation" = ,
    "flexdashboard::flex_dashboard" = ,
    "flex_dashboard" = ,
    "tufte::tufte_html" = ,
    "tufte_html" = ,
    "html_vignette" = ,
    "ioslides_presentation" = "html",
    "github_document" = "md",
    gsub("_document", "", format)
  )
}

# copy docs - quarto
# ------------------

.projr_build_copy_docs_quarto <- function() {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  for (fn in fn_vec_qmd) {
    .projr_build_copy_docs_quarto_ind(fn)
  }
  invisible(TRUE)
}

.projr_build_copy_docs_quarto_ind <- function(fn) {
  frontmatter_vec <- .projr_build_frontmatter_get(fn)
  format <- .projr_build_copy_docs_quarto_format_get(frontmatter_vec)
  fn_output_prefix <- .projr_build_copy_docs_quarto_fn_prefix_get(
    frontmatter_vec, fn
  )
  path_vec <- .projr_build_copy_docs_quarto_path_get(format, fn_output_prefix)
  .projr_build_copy_docs_paths(path_vec)
  invisible(TRUE)
}


.projr_build_copy_docs_quarto_format_get <- function(frontmatter) {
  if (length(frontmatter) == 0) {
    return("html")
  }
  if (!"format" %in% names(frontmatter)) {
    return("html")
  }
  format <- frontmatter[["format"]]
  if (is.character(format)) {
    return(format)
  }
  names(format)[1]
}

.projr_build_copy_docs_quarto_path_get <- function(format, fn_prefix) {
  fn_suffix <- .projr_build_copy_docs_quarto_fn_suffix_get(format)
  fn <- paste0(fn_prefix, ".", fn_suffix)
  switch(format,
    "html" = ,
    "revealjs" = c(paste0(fn_prefix, "_files"), fn),
    fn
  )
}

.projr_build_copy_docs_quarto_fn_prefix_get <- function(frontmatter, fn) {
  if (!"output-file" %in% names(frontmatter)) {
    return(gsub("\\.qmd$", "", fn))
  }
  gsub("\\.qmd$", "", frontmatter[["output-file"]])
}


.projr_build_copy_docs_quarto_fn_suffix_get <- function(format) {
  switch(format,
    "revealjs" = "html",
    "beamer" = "pdf",
    format
  )
}

# copy docs - either
# ------------------

.projr_build_frontmatter_get <- function(path) {
  txt_vec <- readLines(path)
  txt_vec <- gsub("\\s+$", "", txt_vec)
  ind_vec_frontmatter <- which(txt_vec == "---")
  # no frontmatter detected
  if (length(ind_vec_frontmatter) < 2) {
    return(list())
  }
  # frontmatter detected
  txt_vec_frontmatter <- txt_vec[
    seq(ind_vec_frontmatter[1] + 1, ind_vec_frontmatter[2] - 1)
  ]
  path_yml <- file.path(tempdir(), "frontmatter.yml")
  writeLines(
    txt_vec_frontmatter,
    con = path_yml
  )
  yml_frontmatter <- yaml::read_yaml(path_yml)
  unlink(projr_dir_get("cache", "projr_cache"), recursive = TRUE)
  yml_frontmatter
}

.projr_build_copy_docs_paths <- function(path) {
  dir_proj <- rprojroot::is_r_package$find_file()
  for (x in path) {
    if (fs::is_file(x)) {
      file_to <- projr_path_get("docs", basename(x))
      if (file.exists(file_to)) {
        invisible(file.remove(file_to))
      }
      file.rename(from = x, to = file_to)
    } else if (fs::is_dir(x)) {
      fn_vec <- list.files(
        file.path(dir_proj, x),
        recursive = TRUE,
        all.files = TRUE
      )
      fn_vec_from <- file.path(dir_proj, x, fn_vec)
      fn_vec_to <- file.path(projr_dir_get("docs"), x, fn_vec)
      dir_vec_to <- dirname(fn_vec_to) |> unique()
      for (i in seq_along(dir_vec_to)) {
        if (!dir.exists(dir_vec_to[i])) {
          dir.create(dir_vec_to[i], recursive = TRUE)
        }
      }
      invisible(file.rename(from = fn_vec_from, to = fn_vec_to))
    }
  }
}

# copy all requested items to out and archive
# ===========================================

.projr_build_copy_dir <- function(output_run,
                                  dest_type = "output") {
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_projr <- projr_yml_get()
  yml_projr_dir <- yml_projr[["directories"]]
  # copy data_raw and cache across, if desired
  match_str <- switch(dest_type,
    "output" = "^cache|^dataraw|^docs",
    "archive" = "^cache|^dataraw|^docs|^output"
  )
  dir_copy_ind <- grepl(
    match_str, # nolint
    .projr_dir_label_strip(names(yml_projr_dir))
  )
  label_vec <- names(yml_projr_dir)[dir_copy_ind]
  label_vec_output_n <- label_vec[
    !grepl("^output", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  label_vec_output_n <- label_vec_output_n[!is.na(label_vec_output_n)]
  label_vec_output <- label_vec[
    grepl("^output", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  label_vec_output <- label_vec_output[!is.na(label_vec_output)]
  label_vec <- c(label_vec_output_n, label_vec_output)

  for (i in seq_along(label_vec)) {
    label <- label_vec[i]

    # get keys to copy to (skip early if none)
    # ----------------------------------------

    if (dest_type == "output") {
      key_copy_vec <- .projr_dir_copy_dir_check_output(label, yml_projr_dir)
    } else if (dest_type == "archive") {
      key_copy_vec <- .projr_dir_copy_dir_check_archive(label, yml_projr_dir)
    }
    if (!all(nzchar(key_copy_vec))) {
      next
    }

    # zip
    # ------------------------

    # paths
    # get path to copy from
    path_dir <- projr_dir_get(label, output_safe = !output_run)
    if (!fs::is_absolute_path(path_dir)) {
      path_dir <- file.path(dir_proj, path_dir)
    }

    dir_output_init <- key_copy_vec[1]
    if (dest_type == "output") {
      path_zip <- projr_path_get(
        dir_output_init, paste0(label, ".zip"),
        output_safe = !output_run
      )
    } else {
      path_zip <- projr_path_get(
        dir_output_init,
        paste0("v", projr_version_get()),
        paste0(label, ".zip"),
        output_safe = !output_run
      )
    }
    if (!fs::is_absolute_path(path_zip)) {
      path_zip <- file.path(dir_proj, path_zip)
    }

    # exclude special folders
    # projr_output from cache folder(s)
    if (grepl("^cache", .projr_dir_label_strip(label))) {
      key_vec_match_cache <- .projr_dir_label_strip(names(yml_projr_dir))
      key_copy_vec_cache_ind <- which(grepl("^output", key_vec_match_cache))
      key_copy_vec_cache <- names(yml_projr_dir)[key_copy_vec_cache_ind]
      dir_exc <- paste0("projr-", key_copy_vec_cache)
    } else {
      dir_exc <- NULL
    }
    dir_exc <- c(dir_exc, "projr_gh_release")

    dir_inc <- NULL

    # zip
    .projr_zip_dir(
      path_dir = path_dir,
      path_zip = path_zip,
      dir_exc = dir_exc,
      dir_inc = dir_inc
    )
    # copy zip to any extra directories
    key_output_extra <- key_copy_vec[-1]
    for (j in seq_along(key_output_extra)) {
      file.copy(
        from = path_zip,
        to = projr_path_get(
          key_output_extra[j], paste0(label, ".zip"),
          output_safe = !output_run
        )
      )
    }
  }
  invisible(TRUE)
}

.projr_dir_copy_dir_check_output <- function(label, yml_projr_dir) {
  if (!"output" %in% names(yml_projr_dir[[label]])) {
    return(character(1))
  }
  if (is.logical(yml_projr_dir[[label]][["output"]])) {
    if (!yml_projr_dir[[label]][["output"]]) {
      return(character(1))
    }
  }
  if (all(is.logical(yml_projr_dir[[label]][["output"]]))) {
    key_vec_match <- .projr_dir_label_strip(names(yml_projr_dir))
    key_copy_vec_ind <- which(grepl(
      paste0("^", "output"), key_vec_match
    ))
    key_copy_vec <- names(yml_projr_dir)[key_copy_vec_ind]
  } else {
    # save to those specified
    key_copy_vec <- yml_projr_dir[[label]][["output"]]
  }
  key_copy_vec
}

.projr_dir_copy_dir_check_archive <- function(label, yml_projr_dir) {
  # useful calculations
  # ===================

  # all possible archive directories
  # --------------------------------
  archive_key_ind <- grepl(
    "^archive",
    .projr_dir_label_strip(names(yml_projr_dir))
  )
  archive_key_vec <- names(yml_projr_dir)[archive_key_ind]

  # all possible output directories
  # --------------------------------
  output_key_ind <- grepl(
    "^output",
    .projr_dir_label_strip(names(yml_projr_dir))
  )
  output_key_vec <- names(yml_projr_dir)[output_key_ind]

  # which archives are saved to via which outputs
  # ---------------------------------------------
  output_to_archive_list <- lapply(output_key_vec, function(output_key) {
    # if archive is not specified, then it's saved to all archives
    if (!"archive" %in% names(yml_projr_dir[[output_key]])) {
      return(archive_key_vec)
    }
    # if archive is specified:
    # if it's logical:
    if (is.logical(yml_projr_dir[[output_key]][["archive"]])) {
      # if it's true, then return all archives
      if (all(yml_projr_dir[[output_key]][["archive"]])) {
        return(all(yml_projr_dir[[output_key]][["archive"]]))
      }
      # if it's FALSE, then return no archives
      return(NULL)
    }
    # if it's character, then just return the archives specified
    yml_projr_dir[[output_key]][["archive"]]
  }) |>
    stats::setNames(output_key_vec)

  # which archives can only be saved to directly
  # --------------------------------------------
  archive_vec_direct_only <- setdiff(
    archive_key_vec,
    unlist(output_to_archive_list)
  )

  # directory considered for copy is output
  # =====================================

  # easy case: output is the label type
  # -------------------------

  # just return whatever the output is
  if (grepl("^output", .projr_dir_label_strip(label))) {
    key_copy_vec <- output_to_archive_list[[label]]
    if (is.null(key_copy_vec)) {
      return(character(1))
    } else {
      return(key_copy_vec)
    }
  }

  # directory considered for copy is not an output directory
  # ========================================================

  # - output is not specified
  output_present <- "output" %in% names(yml_projr_dir[[label]])
  output_val <- yml_projr_dir[[label]][["output"]]
  output_logical <- all(is.logical(output_val))

  archive_present <- "archive" %in% names(yml_projr_dir[[label]])
  archive_val <- yml_projr_dir[[label]][["archive"]]
  archive_logical <- all(is.logical(archive_val))

  # easy case: archive not specified, so do nothing
  # --------------------------------------------------
  if (!archive_present) {
    return(character(1))
  }

  # useful calculations
  # -----------------


  # hard case: archive logical
  # ---------------------------
  if (all(archive_logical)) {
    # easy sub-case: skip if archive is just false
    if (!all(archive_val)) {
      return(character(1))
    } else {
      # hard sub-case: only save
      # to those that are not archived via output.
      # easy sub-sub-case 1: never saved via output
      if (!output_present) {
        # archive directly to all
        archive_vec_copy <- archive_key_vec
        # we will archive everywhere directly
        # as this is not archived via output at all
      } else if (output_logical) {
        # easy sub-sub-case 2: never saved via output
        if (!all(output_val)) {
          # archive directly to all
          archive_vec_copy <- archive_key_vec
        } else {
          # easy sub-sub-case 3:
          # outputted everywhere and no direct-only archives
          if (length(archive_vec_direct_only) == 0) {
            return(character(1))
          } else {
            # easy sub-sub-case 4: outputted everwhere, so
            # archive directly only to direct-only archives
            archive_vec_copy <- archive_vec_direct_only
          }
        }
        # output is now character.
        # so not every output directory counts.
      } else {
        # hard sub-sub-case 1: only certain outputs are saved to,
        # but we want all.

        # so check which are saved to
        archive_vec_via_output <- output_to_archive_list[
          output_val
        ] |>
          unlist() |>
          unique()

        # archive to the rest
        archive_vec_copy <- setdiff(
          archive_key_vec, archive_vec_via_output
        )
        # nothing that is needed to be archived to is missed,
        # so return early
        if (length(archive_vec_copy) == 0) {
          return(character(1))
        }
      }
    }
  } else {
    # now we know that we want it to be archived only in some places,
    # so let's just check if it's already saved there by output
    # so we need to check if it's archived via output.

    # easy sub-case 1: never saved via output
    if (!output_present) {
      archive_vec_copy <- yml_projr_dir[[label]][["archive"]]
      # output is now logical
    } else if (output_logical) {
      # easy sub-case 1: never saved via output
      if (!all(output_val)) {
        # archive directly to all
        archive_vec_copy <- yml_projr_dir[[label]][["archive"]]
      } else {
        # easy sub-case 2: outputted everwhere,
        # so save only to direct-only places
        if (length(archive_vec_direct_only) == 0) {
          return(character(1))
        } else {
          # skip those that are archived via output
          archive_vec_copy <- intersect(
            archive_vec_direct_only,
            yml_projr_dir[[label]][["archive"]]
          )
        }
      }
      # output is now character.
      # so not every output directory counts.
    } else {
      # now we find out which archives are saved to via output,
      # given that we don't do every output
      archive_vec_via_output <- output_to_archive_list[
        output_val
      ] |>
        unlist() |>
        unique()
      # archive to those projr directories
      # that aren't actually copied to via these outputs
      archive_vec_copy <- setdiff(
        yml_projr_dir[[label]][["archive"]], archive_vec_via_output
      )
      # if all is handled via actually-used outputs, then skip
      if (length(archive_vec_copy) == 0) {
        return(character(1))
      }
    }
  }
  archive_vec_copy
}

.projr_zip_dir <- function(path_dir,
                           path_zip,
                           dir_exc = NULL,
                           dir_inc = NULL,
                           fn_exc = NULL) {
  if (file.exists(path_zip)) {
    invisible(file.remove(path_zip))
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
    return(invisible(FALSE))
  }

  projr_version_set(version_run_on_list$desc[["failure"]])

  invisible(TRUE)
}

# commit
.projr_build_git_push <- function() {
  yml_projr <- projr_yml_get()
  if (!"git" %in% names(yml_projr[["build"]])) {
    return(invisible(FALSE))
  }
  if (!"push" %in% names(yml_projr[["build"]][["git"]])) {
    return(invisible(FALSE))
  }
  if (!yml_projr[["build"]][["git"]][["push"]]) {
    return(invisible(FALSE))
  }
  gert::git_push()
  invisible(TRUE)
}
