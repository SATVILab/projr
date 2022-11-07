
#' @title Set project version
#'
#' @description Set project version manually.
#' Sometimes this may be necessary if, for example, a collaborator has
#' built and pushed the project whilst you've made updates, and so you want
#' to manually increase your version
#' (rather than merging their changes in first).
#'
#' @param where "bookdown" and/or "DESCRIPTION"/
#' Where to set the version.
#' If it includes `"bookdown"`, then the version is updated
#' in the \code{book_filename} and \code{output_dir} fields
#' of \code{_bookdown.yml}.
#' If it includes \code{"DESCRIPTION"}
#' Default is \code{c("bookdown")}.
#' @export
projr_version_set <- function(version,
                              where = "bookdown") {
  if (missing(version)) stop("version must be supplied")
  .projr_version_format_check(version)
  stopifnot(is.character(version))
  # check that version is in correct format
  stopifnot(length(where) %in% 1:2)
  stopifnot(length(setdiff(where, c("bookdown", "DESCRIPTION"))) == 0)
  stopifnot(all(where %in% c("bookdown", "DESCRIPTION")))
  if ("DESCRIPTION" %in% where) {
    desc_file <- read.dcf("DESCRIPTION")
    desc_file[, "Version"] <- version
    write.dcf(desc_file, file = "DESCRIPTION")
  }
  if ("bookdown" %in% where) {
    yml_bd <- .projr_yml_bd_get()
    yml_projr <- projr_yml_get()
    proj_nm <- projr_name_get()
    fn_new <- paste0(proj_nm, "V", version)
    yml_bd$book_filename <- fn_new
    yml_bd$output_dir <- file.path(dirname(yml_bd$output_dir), fn_new) |>
      normalizePath(winslash = "/", mustWork = FALSE)
    .projr_yml_bd_set(yml_bd)
  }

  invisible(TRUE)
}

projr_version_format_set <- function(version_format) {
  if (missing(version_format)) {
    stop("version_format must be supplied")
  }
  if (!length(version_format) == 1) {
    stop("version format must be of length 1")
  }
  if (!is.character(version_format)) {
    stop("version format must be of type character")
  }
  version_valid_vec <- c(
    "major.minor.patch-dev",
    "major.minor.patch.dev",
    "major.minor-dev",
    "major.minor.dev",
    "major-dev",
    "major.dev"
  )
  if (!version_format %in% version_valid_vec) {
    stop("version format not valid")
  }
  yml_projr <- .projr_yml_get()
  yml_projr[["version_format"]] <- version_format
  .projr_yml_set(yml_projr)
  invisible(TRUE)
}

projr_version_format_get <- function() {
  version_format <- .projr_yml_get()[["version_format"]]
  version_valid_vec <- c(
    "major.minor.patch-dev",
    "major.minor.patch.dev",
    "major.minor-dev",
    "major.minor.dev",
    "major-dev",
    "major.dev"
  )
  if (!version_format %in% version_valid_vec) {
    stop(paste0("Current version format not valid: ", version_format))
  }
  version_format
}

.projr_version_format_list_get <- function() {
  version_format <- projr_yml_get()[["version_format"]]
  version_format_vec_sep <- strsplit(
    version_format, "major|minor|patch|dev"
  )[[1]][-1]
  if (any(!grepl("^[[:punct:]]$", version_format_vec_sep))) {
    stop(paste(
      "version_format of ", version_format,
      " in _projr.yml is not valid"
    ))
  }
  version_format_vec_comp <- strsplit(
    version_format, "\\-|\\."
  )[[1]]
  list(
    "components" = version_format_vec_comp,
    "sep" = version_format_vec_sep
  )
}

projr_name_get <- function() {
  basename(rprojroot::is_r_package$find_file())
}

.projr_version_format_check <- function(version) {
  version_format_list <- .projr_version_format_list_get()
  version_format <- projr_version_format_get()
  version_format_regex <- gsub("major", "\\\\d\\+", version_format)
  version_format_regex <- gsub("minor", "\\\\d\\+", version_format_regex)
  version_format_regex <- gsub("patch", "\\\\d\\+", version_format_regex)
  version_format_regex_dev_n <- gsub("\\.dev$|\\-dev", "", version_format_regex)
  version_format_regex_dev <- gsub("dev", "\\\\d\\+", version_format_regex)
  version_format_regex_dev <- gsub("\\.", "\\\\.", version_format_regex_dev)
  version_format_regex_dev <- gsub("\\-", "\\\\-", version_format_regex_dev)
  version_format_regex_dev <- paste0("^", version_format_regex_dev, "$")
  version_format_regex_dev_n <- gsub("\\.", "\\\\.", version_format_regex_dev_n)
  version_format_regex_dev_n <- gsub("\\-", "\\\\-", version_format_regex_dev_n)
  version_format_regex_dev_n <- paste0("^", version_format_regex_dev_n, "$")
  version_format_regex <- paste0(
    version_format_regex_dev_n, "|", version_format_regex_dev
  )
  if (!grepl(version_format_regex, version)) {
    stop("version does not match version format in _projr.yml")
  }
  invisible(TRUE)
}

.projr_version_current_vec_get <- function() {
  # basically, the idea
  # is to choose whichever is greater of
  # the _bookdown.yml and the
  # DESCRIPTION versions
  yml_bd <- .projr_yml_bd_get()
  yml_projr <- projr_yml_get()
  desc <- .projr_desc_get()
  proj_nm <- projr_name_get()
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

  version_orig_vec |> as.integer()
}

.projr_version_run_onwards_get <- function(bump_component) {
  version_orig_vec <- .projr_version_current_vec_get()
  version_format_list <- .projr_version_format_list_get()
  if (!is.null(bump_component)) {
    comp_to_update_ind <- which(
      version_format_list$components == bump_component
    )
    version_bd_update_vec <- version_orig_vec
    version_bd_update_vec[comp_to_update_ind] <- version_bd_update_vec[
      comp_to_update_ind
    ] + 1
    if (comp_to_update_ind < length(version_bd_update_vec)) {
      version_bd_update_vec[
        seq(comp_to_update_ind + 1, length(version_orig_vec))
      ] <- 0
    }
    if (bump_component %in% c("major", "minor", "patch")) {
      version_desc_failure <- version_bd_failure <- paste0(
        paste0(
          version_orig_vec[-length(version_orig_vec)],
          version_format_list$sep,
          collapse = ""
        ),
        version_orig_vec[length(version_orig_vec)]
      )
      version_bd_run <- version_desc_run <- version_desc_success <- paste0(
        paste0(
          version_bd_update_vec[-length(version_bd_update_vec)],
          collapse = version_format_list$sep[1]
        )
      )

      if (version_orig_vec[length(version_orig_vec)] >= 9000) {
        version_bd_dev_success <- 9000
      } else {
        version_bd_dev_success <- 1
      }
      version_bd_success <- paste0(
        version_bd_run,
        version_format_list$sep[length(version_format_list$sep)],
        version_bd_dev_success
      )
    } else {
      version_desc_run <- version_desc_failure <- version_desc_success <-
        version_bd_run <- version_bd_failure <- version_bd_success <- paste0(
          paste0(
            version_bd_update_vec[-length(version_bd_update_vec)],
            version_format_list$sep,
            collapse = ""
          ),
          version_bd_update_vec[length(version_bd_update_vec)]
        )
    }
  } else {
    version_desc_run <- version_desc_failure <- version_desc_success <-
      version_bd_run <- version_bd_failure <- version_bd_success <- paste0(
        paste0(
          version_orig_vec[-length(version_orig_vec)],
          version_format_list$sep,
          collapse = ""
        ),
        version_orig_vec[length(version_orig_vec)]
      )
  }
  list(
    "desc" = c(
      "run" = version_desc_run,
      "failure" = version_desc_failure,
      "success" = version_desc_success
    ),
    "bd" = c(
      "run" = version_bd_run,
      "failure" = version_bd_failure,
      "success" = version_bd_success
    )
  )
}

#' @title Return the next version to be built
#'
#' @description
#' Returns version of project currently being worked on,
#' which is defined as whichever version is later
#' between DESCRIPTION and _bookdown.yml.
#' Typically this will simply be the version in
#' \code{_bookdown.yml}.
#'
#' @return Character.
#'
#' @export
projr_version_get <- function() {
  .projr_version_current_vec_get() |>
    .projr_version_chr_get()
}

.projr_version_chr_get <- function(version) {
  version_str <- version[[1]]
  version_format_sep_vec <- .projr_version_format_list_get()[["sep"]]
  if (length(version) == length(version_format_sep_vec)) {
    version_format_sep_vec <- version_format_sep_vec[
      -length(version_format_sep_vec)
    ]
  }
  for (i in seq_along(version_format_sep_vec)) {
    version_str <- paste0(
      version_str, version_format_sep_vec[[i]], version[[i + 1]]
    )
  }
  version_str
}


#' @title Bump version
#'
#' @description Increase a component of the version.
#' By default, only increments the dev version
#' in _bookdown.yml (and not in DESCRIPTION as well).
#' This is because this function is intended primarily
#' to increase only the dev version in anticipation of the next build.
#' Non-dev versions should be bumped implicitly during runs of
#' \code{projr_build_output}.
#'
#' @param component character.
#' Component in version to bump.
#' Default is \code{"dev"}.
#' @inheritParams projr_version_set
#' @param onto_dev logical.
#' If \code{TRUE}, then the bumped version
#' will always have be a development version, i.e.
#' will end with the dev component specified.
#' Default is \code{TRUE}.
#'
#' @export
#'
#' @return Invisibly returns the new version.
projr_version_bump <- function(component = "dev",
                               where = "bookdown",
                               onto_dev = TRUE) {
  version_current_vec <- .projr_version_current_vec_get()
  version_format_list <- .projr_version_format_list_get()
  if (!component %in% version_format_list$components) {
    stop(paste0(
      "Component ", component, "
      not a part of version format in _projr.yml"
    ))
  }
  ind_to_bump <- which(version_format_list$components == component)
  version_current_vec[ind_to_bump] <- version_current_vec[ind_to_bump] + 1
  if (ind_to_bump < length(version_format_list$components)) {
    if (version_current_vec[length(version_current_vec)] >= 9000) {
      version_dev_base <- 9000
    } else {
      version_dev_base <- 1
    }
    version_current_vec[
      seq(ind_to_bump + 1, length(version_format_list$components))
    ] <- 0
    version_current_vec[length(version_current_vec)] <- version_dev_base
  }
  if (!onto_dev) {
    version_current_vec <- version_current_vec[-length(version_current_vec)]
  }
  version_new <- .projr_version_chr_get(version_current_vec)
  projr_version_set(version = version_new, where = where)
  version_new
}
