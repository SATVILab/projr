projr_dir_create <- function(type) {
  if (missing(type)) stop("type must be specified")
  yml_active_dir <- projr_yml_get()[["directories"]]
  yml_active_dir <- yml_active_dir[
    vapply(names(yml_active_dir), function(x) x %in% type, logical(1))
  ]
  if (length(yml_active_dir) == 0) {
    stop("type does not match any directory type")
  }
  # create
  for (i in seq_along(yml_active_dir)) {
    yml_curr_orig <- yml_active_dir[i]
    # create one where the output and archive
    # directories are versioned.
    # separate, original one kept for
    # git versioning
    yml_curr_versioned <- yml_curr_orig
    if (names(yml_curr_orig) %in% "archive") {
      yml_curr_versioned[[1]][["path"]] <- file.path(
        yml_curr_versioned[[1]][["path"]], .projr_version_current_get()
      )
    }
    if (!dir.exists(yml_curr_versioned[[1]][["path"]])) {
      dir.create(yml_curr_versioned[[1]][["path"]], recursive = TRUE)
    }
  }

  for (x in type) .projr_dir_ignore(type)

  invisible(TRUE)
}

.projr_dir_ignore <- function(type) {
  if (length(type) > 1) stop("type must be length 1")
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_active_dir <- projr_yml_get()[["directories"]]
  yml_active_dir <- yml_active_dir[[
    vapply(names(yml_active_dir), function(x) x == type, logical(1))
  ]]
  dir_path <- yml_active_dir[["path"]]
  within_wd <- fs::path_has_parent(dir_path, dir_proj)
  if (!within_wd) next

  # ignore
  gitignore <- .projr_gitignore_get()
  rbuildignore <- .projr_buildignore_get()

  dir_path <- fs::path_rel(dir_path, dir_proj)

  txt_gitignore <- paste0(gsub("/*$", "", dir_path), "/**/*")
  txt_rbuildignore <- paste0("^", Hmisc::escapeRegex(dir_path))

  if (!is.logical(yml_curr_orig[[1]][["ignore"]])) next
  if (yml_curr_orig[[1]][["ignore"]]) {
    if (!txt_gitignore %in% gitignore) {
      .projr_buildignore_set(txt_gitignore, append = TRUE)
    }
    if (!txt_rbuildignore %in% rbuildignore) {
      .projr_buildignore_set(txt_rbuildignore, append = TRUE)
    }
  } else {
    if (txt_gitignore %in% gitignore) {
      gitignore <- gitignore[!(gitignore == txt_gitignore), ]
      .projr_buildignore_set(gitignore, append = FALSE)
    }
    if (txt_rbuildignore %in% rbuildignore) {
      rbuildignore <- rbuildignore[!(rbuildignore == txt_rbuildignore), ]
      .projr_buildignore_set(rbuildignore, append = FALSE)
    }
  }
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
    sep = "",
    append = append
  )
  invisible(file.path(dir_proj, ".gitignore"))
}

.projr_buildignore_set <- function(buildignore, append) {
  dir_proj <- rprojroot::is_r_package$find_file()
  cat(
    buildignore,
    file = file.path(dir_proj, ".Rbuildignore"),
    sep = "",
    append = append
  )
  invisible(file.path(dir_proj, ".Rbuildignore"))
}
