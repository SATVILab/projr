projr_dir_create <- function(type) {
  if (missing(type)) stop("type must be specified")
  if (!is.character(type)) stop("type must be of type character")
  yml_active_dir <- projr_yml_get()[["directories"]]
  yml_active_dir <- yml_active_dir[
    vapply(names(yml_active_dir), function(x) any(type %in% x), logical(1))
  ]
  if (length(yml_active_dir) == 0) {
    stop("type does not match any directory type")
  }
  # create
  for (i in seq_along(yml_active_dir)) {
    path <- yml_active_dir[[i]]$path
    if (names(yml_active_dir)[i] == "archive") {
      path <- file.path(
        path, .projr_version_current_get()
      )
    }
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
  # create one where the output and archive
  # directories are versioned.
  # separate, original one kept for
  # git versioning
  for (x in type) .projr_dir_ignore(x)

  invisible(TRUE)
}

.projr_dir_ignore <- function(type) {
  if (length(type) > 1) stop("type must be length 1")
  if (!is.character(type)) stop("type must be o type character")
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_active_dir <- projr_yml_get()[["directories"]]
  match_ind <-
    which(vapply(names(yml_active_dir), function(x) type == x, logical(1)))
  if (length(match_ind) > 1) {
    stop(paste0("More than one element has label ", type))
  }
  dir_label <- names(yml_active_dir)[[match_ind]]
  yml_active_dir <- yml_active_dir[[match_ind]]
  dir_path <- yml_active_dir[["path"]]
  within_wd <- fs::path_has_parent(dir_path, dir_proj)
  if (!within_wd) {
    return(invisible(TRUE))
  }

  # ignore
  if (!is.logical(yml_active_dir[["ignore"]])) {
    stop(paste0("ignore not of typical logical for directory ", dir_label))
  }

  gitignore <- .projr_gitignore_get()
  rbuildignore <- .projr_buildignore_get()

  dir_path <- fs::path_rel(dir_path, dir_proj)

  txt_gitignore <- paste0(gsub("/*$", "", dir_path), "/**/*")
  txt_rbuildignore <- paste0("^", Hmisc::escapeRegex(dir_path))

  if (yml_active_dir[["ignore"]]) {
    if (!txt_gitignore %in% gitignore) {
      .projr_gitignore_set(txt_gitignore, append = TRUE)
      .projr_gitignore_set("\n", append = TRUE)
    }
    if (!txt_rbuildignore %in% rbuildignore) {
      .projr_buildignore_set(txt_rbuildignore, append = TRUE)
      .projr_buildignore_set("\n", append = TRUE)
    }
    return(invisible(TRUE))
  }

  if (txt_gitignore %in% gitignore) {
    gitignore <- gitignore[!(gitignore == txt_gitignore)]
    .projr_gitignore_set(gitignore, append = FALSE)
  }
  if (txt_rbuildignore %in% rbuildignore) {
    rbuildignore <- rbuildignore[!(rbuildignore == txt_rbuildignore)]
    .projr_buildignore_set(rbuildignore, append = FALSE)
  }
  invisible(TRUE)
}

.projr_gitignore_get <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  suppressWarnings(readLines(
    file.path(dir_proj, ".gitignore")
  ))
}

.projr_buildignore_get <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  suppressWarnings(readLines(
    file.path(dir_proj, ".Rbuildignore")
  ))
}

.projr_gitignore_set <- function(gitignore, append) {
  dir_proj <- rprojroot::is_r_package$find_file()
  cat(
    gitignore,
    file = file.path(dir_proj, ".gitignore"),
    sep = "\n",
    append = append
  )
  invisible(file.path(dir_proj, ".gitignore"))
}

.projr_buildignore_set <- function(buildignore, append) {
  dir_proj <- rprojroot::is_r_package$find_file()
  cat(
    buildignore,
    file = file.path(dir_proj, ".Rbuildignore"),
    sep = "\n",
    append = append
  )
  invisible(file.path(dir_proj, ".Rbuildignore"))
}
