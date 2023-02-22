#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' @param label character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"docs"}.
#' Class of directory to return.
#' The \code{"docs"} option returns the path to
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

  if (!label %in% c(names(dir_active), "docs")) {
    stop(paste0("label `", label, "` not recognised."))
  }
  # use the appropriate specification doc
  # if "docs" is the label
  if (label == "docs") {
    dir_proj <- rprojroot::is_r_package$find_file()

    if (file.exists(file.path(dir_proj, "_bookdown.yml"))) {
      path_final_root <- .projr_yml_bd_get()[["output_dir"]]
    } else {
      # quarto stuff
    }
    # exception for output when it's safe
  } else if (grepl("^output", .projr_dir_label_strip(label)) && output_safe) {
    cache_ind <- which(
      .projr_dir_label_strip(names(dir_active)) == "cache"
    )[1]
    path_final_root <- file.path(
      dir_active[[cache_ind]]$path, paste0("projr-", label),
      .projr_version_current_vec_get() |> .projr_version_chr_get()
    )
  } else {
    path_final_root <- dir_active[[label]][["path"]]
  }
  path_append <- list(...) |> unlist()
  path_final <- do.call(
    "file.path",
    args = list(path_final_root) |> append(path_append)
  )
  path_final <- as.character(path_final)

  if (create) {
    projr_dir_create(label = label)
    if (!dir.exists(path_final)) {
      dir.create(path_final, recursive = TRUE)
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

#' @title Return path
#'
#' @description Returns path to \code{projr} profile-specific directory.
#' Differs from \code{projr_dir_get} in that it does not assume
#' that the path is to a directory.
#'
#' Will create the parent directory of the specified
#' path if it does not exist, and ignore it
#' if requested by `_projr.yml`.
#'
#' @param label character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"docs"}.
#' Class of directory to return.
#' The \code{"docs"} option returns the path to
#' the output directory from \code{bookdown::render_book}
#' (as specified in \code{"_bookdown.yml"}),
#' whereas the others returns paths as specified in \code{"_projr.yml"}.
#' @param ... Specifies sub-path of directory returned.
#' @param create logical.
#' If \code{TRUE}, then the parent directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @inheritParams projr_dir_get
#'
#' @return Character.
#' Path to directory requested.
#'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname projr_path_get
#' @export
projr_path_get <- function(label, ...,
                           create = TRUE,
                           path_relative_force = FALSE,
                           output_safe = TRUE) {
  args_dotted <- list(...)
  if (length(args_dotted) == 0) {
    path_dir <- projr_dir_get(
      label = label,
      create = create,
      path_relative_force = path_relative_force,
      output_safe = output_safe
    )
    return(path_dir)
  }
  if (length(args_dotted) > 1) {
    path_dir <- do.call(
      what = "projr_dir_get",
      args = list(
        label = label,
        args_dotted[-length(args_dotted)],
        create = create,
        path_relative_force = path_relative_force,
        output_safe = output_safe
      )
    )
  } else {
    path_dir <- projr_dir_get(
      label = label,
      create = create,
      path_relative_force = path_relative_force,
      output_safe = output_safe
    )
  }
  file.path(path_dir, args_dotted[length(args_dotted)])
}


#' @title Create a directory in _projr.yml
#'
#' @description Creates a directory that is
#' listed in _projr.yml for the current projr profile.
#' Will add to \code{.gitignore} and \code{.Rbuildignore}
#' as well if required.
#' @inheritParams projr_dir_get
#'
#' @export
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
    if (!fs::is_absolute_path(path)) {
      path <- fs::path_rel(
        file.path(rprojroot::is_r_package$find_file(), path),
        start = getwd()
      ) |>
        as.character()
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
  if (label == "docs") {
    yml_bd <- try(.projr_yml_bd_get())
    if (identical(class(yml_bd), "try-error")) {
      stop("_bookdown.yml not valid YAML")
    }
    dir_path <- yml_bd[["output_dir"]]
  } else {
    if (identical(class(yml_active_dir), "try-error")) {
      stop("_projr.yml not valid YAML")
    }
    match_ind <-
      which(vapply(names(yml_active_dir), function(x) label == x, logical(1)))
    dir_label <- names(yml_active_dir)[[match_ind]]
    yml_active_dir <- yml_active_dir[[match_ind]]
    # ignore
    if (!is.logical(yml_active_dir[["ignore"]])) {
      if (!is.null(yml_active_dir[["ignore"]])) {
        stop(paste0("ignore not of typical logical for directory ", dir_label))
      }
    }
    dir_path <- yml_active_dir[["path"]]
  }
  within_wd <- fs::path_has_parent(dir_path, dir_proj)
  if (!within_wd) {
    return(invisible(TRUE))
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

.projr_dir_label_strip <- function(x) {
  gsub("_", "", gsub("-", "", x)) |>
    tolower()
}
