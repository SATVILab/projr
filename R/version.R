#' @title Set project version
#'
#' @description Set project version manually.
#' Sometimes this may be necessary if, for example, a collaborator has
#' built and pushed the project whilst you've made updates, and so you want
#' to manually increase your version
#' (rather than merging their changes in first).
#'
#' @param version character.
#' Version to set.
#' May be dev version (i.e. include the dev component) or not.
#'
#' @export
projr_version_set <- function(version) {
  if (missing(version)) stop("version must be supplied")
  if (!length(version) == 1L) {
    stop("version must be a character vector with one element")
  }
  if (!is.character(version)) {
    stop("version must be of type character")
  }
  if (grepl("^v", version)) {
    version <- gsub("^v", "", version)
  }
  .projr_version_format_check(version)
  # check that version is in correct format
  desc_file <- read.dcf("DESCRIPTION")
  desc_file[, "Version"] <- version
  write.dcf(desc_file, file = "DESCRIPTION")

  invisible(TRUE)
}

#' @title Get version format
#'
#' @description
#' Returns the version format,
#' which species the depth of semantic
#' versioning levels used (e.g. major or major, minor and patch)
#' as well as the separators between the versions (periods or dashes).
#'
#' @param version_format character.
#' The version format.
#' Specifies the version components ("major" and/or "minor" and/or "patch", and "dev"/"1"/"9000"),
#' as well as the separators between them.
#' If "1" is used instead of "dev", then "dev" component resets at "1".
#' If "9000" is used instead of "dev", then "dev" component resets at "9000".
#' If "dev" is used, then "dev" component resets at "9000".
#' Examples: "major.minor-1", "major-minor-patch-dev", "major.9000".
#' Default is "major.minor.patch.dev".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projr_version_format_set("major.minor-dev")
#' projr_version_format_set("major.minor.patch-1")
#' }
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
  yml_projr <- projr_yml_get()
  yml_projr[["version-format"]] <- version_format
  .projr_yml_set(yml_projr)
  invisible(TRUE)
}

#' @title Get version format
#'
#' @description
#' Returns the version format,
#' which species the depth of semantic
#' versioning levels used (e.g. major or major, minor and patch)
#' as well as the separators between the versions (periods or dashes).
#'
#' @export
projr_version_format_get <- function() {
  version_format <- projr_yml_get()[["version-format"]]
  if (length(version_format) == 0) {
    version_format <- "major.minor.patch-dev"
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
    stop(paste0("Current version format not valid: ", version_format))
  }
  version_format
}

.projr_version_format_list_get <- function() {
  version_format <- projr_yml_get()[["version-format"]]
  if (is.null(version_format)) {
    version_format <- "major.minor.patch-dev"
  }
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

.projr_version_current_vec_get <- function(dev_force = FALSE) {
  # basically, the idea
  # is to choose whichever is greater of
  # the _bookdown.yml and the
  # DESCRIPTION versions
  desc <- .projr_desc_get()
  version_desc_vec <- strsplit(
    desc[1, "Version"][[1]],
    split = "\\-|\\."
  )[[1]]
  version_format <- .projr_version_format_list_get()$components
  # if not forcing there to be a dev version
  if (!dev_force) {
    return(version_desc_vec |> as.integer())
  }
  if (length(version_format) == length(version_desc_vec)) {
    return(version_desc_vec |> as.integer())
  }
  if ("dev" %in% version_format) {
    version_desc_vec <- c(version_desc_vec, "1")
  } else {
    version_desc_vec <- c(
      version_desc_vec, version_format[length(version_format)]
    )
  }
  version_desc_vec |> as.integer()
}

.projr_version_run_onwards_get <- function(bump_component) {
  version_orig_vec <- .projr_version_current_vec_get(dev_force = TRUE)
  version_format_list <- .projr_version_format_list_get()

  if (!is.null(bump_component)) {
    comp_to_update_ind <- which(
      version_format_list$components == bump_component
    )
    version_update_vec <- version_orig_vec

    version_update_vec[comp_to_update_ind] <- version_update_vec[
      comp_to_update_ind
    ] + 1
    if (comp_to_update_ind < length(version_update_vec)) {
      version_update_vec[
        seq(comp_to_update_ind + 1, length(version_orig_vec))
      ] <- 0
    }
    if (bump_component %in% c("major", "minor", "patch")) {
      version_desc_failure <- paste0(
        paste0(
          version_orig_vec[-length(version_orig_vec)],
          version_format_list$sep,
          collapse = ""
        ),
        version_orig_vec[length(version_orig_vec)]
      )
      version_desc_run <- version_desc_success <- paste0(
        paste0(
          version_update_vec[-length(version_update_vec)],
          collapse = version_format_list$sep[1]
        )
      )
    } else {
      version_desc_run <- version_desc_failure <- version_desc_success <-
        paste0(
          paste0(
            version_update_vec[-length(version_update_vec)],
            version_format_list$sep,
            collapse = ""
          ),
          version_update_vec[length(version_update_vec)]
        )
    }
  } else {
    version_desc_run <- version_desc_failure <- version_desc_success <-
      paste0(
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
#' @param dev_force logical.
#' If `TRUE`, then the returned version
#' will necessarily have a `dev` component.
#' Default is `FALSE`.
#'
#' @return Character.
#'
#' @export
projr_version_get <- function(dev_force = FALSE) {
  .projr_version_current_vec_get(dev_force = dev_force) |>
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


#' @title Bump development version
#'
#' @description Increase development component of version.
#'
#' @export
#'
#' @return Invisibly returns the new version.
projr_version_dev_bump <- function() {
  version_current_vec <- .projr_version_current_vec_get()
  version_format_list <- .projr_version_format_list_get()
  ind_to_bump <- length(version_format_list$components)
  version_current_vec[ind_to_bump] <- version_current_vec[ind_to_bump] + 1
  version_new <- .projr_version_chr_get(version_current_vec)
  projr_version_set(version = version_new)
  version_new
}


.projr_version_comp_min_check <- function(bump_component,
                                          version_min) {
  version_comp_vec_min <- .projr_version_comp_vec_min_get(version_min)
  bump_component %in% version_comp_vec_min
}

.projr_version_comp_vec_min_get <- function(version_min) {
  version_vec_possible <- c("major", "minor", "patch")
  switch(version_min,
    "any" = version_vec_possible,
    version_vec_possible[seq_len(which(version_vec_possible == version_min))]
  )
}
