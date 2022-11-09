#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' @param label character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"bookdown"}.
#' Class of directory to return.
#' The \code{"bookdown"} option returns the path to
#' the output directory from \code{bookdown::render_book}
#' (as specified in \code{"_bookdown.yml"}),
#' whereas the others returns paths as specified in \code{"_projr.yml"}.
#' @param ... Specifies sub-directory of directory returned.
#' Passed to `file.path`.
#' @param create logical.
#' If \code{TRUE}, then the directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @param path_relative_force logical.
#' If \code{TRUE}, then forces that the returned
#' path is relative to the project root.
#' Default is \code{FALSE}.
#' @param output_safe logical.
#' If \code{TRUE}, then the output directory
#' is set to be \code{"<path_to_cache>/projr_output"}
#' instead of \code{<path_to_output>} (as specified in \code{_projr.yml}).
#' The only time that this should be set to \code{TRUE}
#' should be when `projr_build_output` is being run, as otherwise
#' "development" or test runs will add to, delete or overwrite files
#' from the previous run of `projr_build_output`.
#' Default is \code{TRUE}.
#' @return Character.
#' Path to directory requested.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname projr_dir_get
#' @export
projr_dir_get <- function(label, ...,
                          create = TRUE,
                          path_relative_force = FALSE,
                          output_safe = TRUE) {
  # get active directories
  yml_active <- projr_yml_get()

  dir_active <- yml_active[["directories"]]

  if (!label %in% c(names(dir_active), "bookdown")) {
    stop(paste0("label `", label, "` not recognised."))
  }

  if (label == "bookdown") {
    yml_bd <- .projr_yml_bd_get()
    path_final <- file.path(yml_bd[["output_dir"]], ...)
    if (!dir.exists(path_final)) {
      dir.create(path_final, recursive = TRUE)
    }
    return(as.character(path_final))
  }

  # get current version
  if (label == "output" && output_safe) {
    label <- "cache"
    yml_active_dir_curr <- dir_active[label]
    path_final_root <- file.path(
      dir_active[[label]]$path, "projr_output",
      .projr_version_current_vec_get() |> .projr_version_chr_get()
    )
    yml_active_dir_curr[["output"]][["path"]] <- path_final_root
  } else {
    yml_active_dir_curr <- dir_active[label]
    path_final_root <- dir_active[[label]]$path
  }
  path_append <- list(...)
  dir_active <- yml_active_dir_curr
  path_final <- do.call(
    "file.path",
    args = list(path_final_root) |> append(path_append)
  )

  if (create) {
    projr_dir_create(label = label)
    if (!file.exists(path_final)) {
      if (fs::is_file(path_final)) {
        if (!dir.exists(dirname(path_final))) {
          dir.create(dirname(path_final), recursive = TRUE)
        }
      } else {
        dir.create(path_final, recursive = TRUE)
      }
    }
  }

  if (path_relative_force) {
    path_final <- fs::path_rel(
      path_final,
      start = rprojroot::is_r_package$find_file()
    )
  }
  as.character(path_final)
}


projr_dir_create <- function(label) {
  if (missing(label)) stop("label must be specified")
  if (!is.character(label)) stop("label must be of label character")
  yml_active_dir <- projr_yml_get()[["directories"]]
  yml_active_dir <- yml_active_dir[
    vapply(names(yml_active_dir), function(x) any(label %in% x), logical(1))
  ]
  if (length(yml_active_dir) == 0) {
    stop("label does not match any directory label")
  }
  # create
  for (i in seq_along(yml_active_dir)) {
    path <- yml_active_dir[[i]]$path
    if (names(yml_active_dir)[i] == "archive") {
      path <- file.path(
        path, paste0("v", projr_version_get())
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
  for (x in label) .projr_dir_ignore(x)

  invisible(TRUE)
}

.projr_dir_ignore <- function(label) {
  if (length(label) > 1) stop("label must be length 1")
  if (!is.character(label)) stop("label must be o label character")
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_active_dir <- try(projr_yml_get()[["directories"]])
  if (identical(class(yml_active_dir), "try-error")) {
    stop("_projr.yml not valid YAML")
  }
  match_ind <-
    which(vapply(names(yml_active_dir), function(x) label == x, logical(1)))
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
