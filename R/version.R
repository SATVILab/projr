
#' @title Set project version
#'
#' @description Set project version manually.
#' Sometimes this may be necessary if, for example, a collaborator has
#' built and pushed the project whilst you've made updates, and so you want
#' to manually increase your version
#' (rather than merging their changes in first).
#'
#'
#'
#' @param where "bookdown" and/or "DESCRIPTION"/
#' Where to set the version.
#' If it includes `"bookdown"`, then the version is updated
#' in the \code{book_filename} and \code{output_dir} fields
#' of \code{_bookdown.yml}.
#' If it includes \code{"DESCRIPTION"}
#' Default is \code{c("bookdown", "DESCRIPTION")}.
projr_version_set <- function(version, where = c("bookdown", "DESCRIPTION")) {
  if (missing(version)) stop("version must be supplied")
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
    yml_bd <- yaml::read_yaml(
      rprojroot::is_r_package$find_file("_bookdown.yml")
    )
    yml_projr <- yaml::read_yaml(
      rprojroot::is_r_package$find_file("_projr.yml")
    )
    proj_nm <- .get_proj_nm(
      fn = yml_bd$book_filename,
      version_format = yml_projr$version
    )
    fn_new <- paste0(proj_nm, "V", version)
    yml_bd$book_filename <- fn_new
    yml_bd$output_dir <- file.path(dirname(yml_bd$output_dir), fn_new) |>
      normalizePath(winslash = "/", mustWork = FALSE)
    yaml::write_yaml(
      yml_bd,
      file = rprojroot::is_r_package$find_file("_bookdown.yml")
    )
  }

  invisible(TRUE)
}

.get_version_and_output_nm <- function() {
  yml_bd <- yaml::read_yaml(rprojroot::is_r_package$find_file("_bookdown.yml"))
  yml_projr <- yaml::read_yaml(rprojroot::is_r_package$find_file("_projr.yml"))
  version_format <- yml_projr$version
  version_sep <- strsplit(version_format, "major|minor|patch|dev")[[1]][-1]
  version_format_vec <- strsplit(version_format, "\\-|\\.")[[1]]
}

.get_version_format_list <- function(version_format) {
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

.get_proj_nm <- function(fn, version_format) {
  str_regex <- switch(version_format,
    "major.minor.patch-dev" =
      "V\\d+\\.\\d+\\.\\d+\\-\\d+$|V\\d+\\.\\d+\\.\\d+$",
    "major.minor.patch-dev" =
      "V\\d+\\.\\d+\\.\\d+\\.\\d+$|V\\d+\\.\\d+\\.\\d+$",
    "major.minor-dev" = "V\\d+\\.\\d+\\-\\d+$|V\\d+\\.\\d+$",
    "major.minor.dev" = "V\\d+\\.\\d+\\.\\d+$|V\\d+\\.\\d+$",
    "major-dev" = "V\\d+\\-\\d+$|V\\d+$",
    "major.dev" = "V\\d+\\.\\d+$|V\\d+$"
  )
  gsub(str_regex, "", fn)
}

.get_version_current <- function(fn, proj_nm) {
  version_list <- substr(
    fn,
    start = nchar(proj_nm) + 2,
    stop = nchar(fn)
  ) |>
    strsplit(
      split = "\\-|\\."
    )
  version_list[[1]] |>
    as.integer()
}

.get_version_final <- function(version_orig,
                               bump_component,
                               version_format_list) {
  if (!is.null(bump_component)) {
    comp_to_update_ind <- which(
      version_format_list$components == bump_component
    )
    version_update <- version_orig
    version_update[comp_to_update_ind] <- version_update[comp_to_update_ind] + 1
    if (comp_to_update_ind < length(version_update)) {
      version_update[seq(comp_to_update_ind + 1, length(version_orig))] <- 0
    }
  } else {
    version_update <- version_orig
  }

  version_final_dev <- paste0(
    paste0(
      version_update[-length(version_update)],
      version_format_list$sep,
      collapse = ""
    ),
    version_update[length(version_update)]
  )

  c(
    "dev" = version_final_dev,
    "no_dev" = gsub("\\-\\d+$|\\.\\d+$", "", version_final_dev)
  )
}
.get_version_and_fn_final <- function(version_format,
                                      fn_orig,
                                      bump_component) {
  proj_nm <- .get_proj_nm(
    fn = fn_orig, version_format = version_format
  )
  version_orig <- .get_version_current(
    fn = fn_orig, proj_nm = proj_nm
  )
  version_format_list <- .get_version_format_list(
    version_format
  )

  version_final_vec <- .get_version_final(
    version_orig = version_orig,
    bump_component = bump_component,
    version_format_list = version_format_list
  )
  fn_final_dev <- paste0(
    proj_nm, "V", version_final_vec["dev"]
  )
  c(
    "fn" = fn_final_dev,
    "version" = version_final_vec[["dev"]]
  )
}
