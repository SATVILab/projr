#' @title Set Project Version
#' @rdname projr_version_set
#'
#' @description
#' Sets the project version manually in the `DESCRIPTION` file and optionally in a `VERSION` file.
#' This is useful in cases where you need to increment the version manually, for example,
#' if a collaborator has pushed changes and you want to manually set your version before merging.
#'
#' @param version A character string specifying the version to set.
#'   It may include a development component (e.g., "1.2.3-dev") or just the stable version (e.g., "1.2.3").
#' @param only_if_exists A logical flag indicating whether to update the `VERSION` file
#'   only if it already exists (`TRUE`) or to create it if it doesn't exist (`FALSE`).
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns `TRUE` if successful.
#' @export
projr_version_set <- function(version, only_if_exists = TRUE) {
  if (file.exists(.path_get("DESCRIPTION"))) {
    .projr_version_set_desc(version)
    .projr_version_set_file(version, only_if_exists = TRUE)
  } else {
    .projr_version_set_file(version, only_if_exists = FALSE)
  }
  invisible(TRUE)
}

.projr_version_copy_dir <- function(path_dir) {
  # @title Copy Project Version to a Directory
  #
  # @description
  # Copies the current project version to a specified directory
  # by creating or updating
  # a `VERSION` file. This ensures that the specified directory
  # aligns with the project's version.
  #
  # @param path_dir. character.
  # Specifies the directory path to copy the version to.
  .assert_string(path_dir, TRUE)
  projr_version_get() |>
    .projr_version_set_file(path_dir = path_dir)
}


.projr_version_set_desc <- function(version) {
  .projr_version_check(version)
  desc_file <- read.dcf("DESCRIPTION")
  desc_file[, "Version"] <- .projr_version_v_rm(version)
  write.dcf(desc_file, file = "DESCRIPTION")
}

.projr_version_check <- function(version) {
  if (missing(version)) stop("version must be supplied")
  .assert_string(version, TRUE)
  if (grepl("^v", version)) {
    version <- gsub("^v", "", version)
  }
  .projr_version_format_check(version)
}

.projr_version_check_error_free <- function(version) {
  tryCatch(
    .projr_version_check(version),
    error = function(e) {
      FALSE
    }
  )
}




.projr_version_set_file <- function(version, path_dir = NULL, only_if_exists = FALSE) {
  .projr_version_check(version)
  .assert_string(path_dir)
  .assert_flag(only_if_exists)
  version <- version |> .projr_version_v_add()
  path_file <- if (is.null(path_dir)) {
    projr_path_get("project", "VERSION")
  } else {
    .dir_create(path_dir)
    file.path(path_dir, "VERSION")
  }
  if (!file.exists(path_file) && only_if_exists) {
    return(invisible(FALSE))
  }
  writeLines(version, con = path_file)
  invisible(TRUE)
}

projr_name_get <- function() basename(.path_get())

.projr_version_format_check <- function(version, profile = NULL) {
  version_format <- .projr_yml_metadata_get_version_format(profile)
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
  version_vec_init <- .projr_version_current_vec_get_init()

  version_format <- .projr_version_format_list_get(NULL)[["component"]]
  # if not forcing there to be a dev version
  if (!dev_force) {
    return(version_vec_init |> as.integer())
  }
  if (length(version_format) == length(version_vec_init)) {
    return(version_vec_init |> as.integer())
  }
  version_dev_append <- .projr_version_current_vec_get_dev(version_format)
  version_vec_init <- c(version_vec_init, version_dev_append)
  version_vec_init |> as.integer()
}

.projr_version_current_vec_get_dev <- function(version_format) {
  if ("dev" == version_format[length(version_format)]) {
    return("1")
  }
  version_format[length(version_format)]
}

.projr_version_current_vec_get_init <- function() {
  version_vec_init <- if (file.exists(.path_get("DESCRIPTION"))) {
    .projr_version_current_vec_get_init_desc()
  } else {
    .projr_version_current_vec_get_init_file()
  }
  .assert_chr_mid(version_vec_init)
  version_vec_init
}

.projr_version_current_vec_get_init_desc <- function() {
  desc <- .projr_desc_get()
  strsplit(
    desc[1, "Version"][[1]],
    split = "\\-|\\."
  )[[1]]
}

.projr_version_current_vec_get_init_file <- function() {
  if (!file.exists(.path_get("VERSION"))) {
    stop("VERSION file not found")
  }
  version_file <- readLines("VERSION")
  version_file <- version_file[[1]]
  version_file <- .projr_version_v_rm(version_file)
  strsplit(version_file, split = "\\-|\\.")[[1]]
}

.projr_version_run_onwards_get <- function(bump_component) {
  version_orig_vec <- .projr_version_current_vec_get(dev_force = TRUE)
  version_format_list <- .projr_version_format_list_get(NULL)

  if (is.null(bump_component)) {
    return(
      .projr_version_run_onwards_get_dev(
        version_orig_vec, version_format_list$sep
      )
    )
  }
  if (bump_component == "dev") {
    version_orig_vec[length(version_orig_vec)] <- version_orig_vec[
      length(version_orig_vec)
    ] + 1
    return(
      .projr_version_run_onwards_get_dev(
        version_orig_vec, version_format_list$sep
      )
    )
  }

  .projr_version_run_onwards_get_bump(
    version_orig_vec,
    version_format_list$component,
    version_format_list$sep,
    bump_component
  )
}

.projr_version_run_onwards_get_dev <- function(version_vec,
                                               version_format_sep) {
  version_vec <- .projr_version_run_onwards_get_dev_append_dev(
    version_vec, version_format_sep
  )
  version <- .projr_version_concat(version_vec, version_format_sep) 
  list(
    "desc" = c(
      "run" = version,
      "failure" = version,
      "success" = version
    )
  )
}

.projr_version_run_onwards_get_dev_append_dev <- function(version_vec,
                                                          version_format_sep) {
  if (length(version_vec) == length(version_format_sep) + 1) {
    return(version_vec)
  }
  version_format <- .projr_version_format_list_get(NULL)[["component"]]
  version_dev_append <- .projr_version_current_vec_get_dev(version_format)
  c(version_vec, version_dev_append)
}

.projr_version_concat <- function(version_vec, split_vec) {
  if (.is_len_1(version_vec)) {
    # major-version only format
    # output build
    return(version_vec)
  }
  version <- paste0(
    version_vec[seq_along(split_vec)],
    collapse = split_vec[-length(split_vec)]
  )
  if (length(version_vec) == length(split_vec)) {
    # major-version-<dev> format
    # output build
    return(version)
  }
  # dev build
  paste0(
    version,
    split_vec[length(split_vec)],
    version_vec[length(split_vec) + 1]
  )
}

.projr_version_run_onwards_get_bump <- function(version_vec,
                                                  version_format_comp,
                                                  version_format_sep,
                                                  bump_component) {
    
  version_update_vec <- .projr_version_run_onwards_get_bump_update_vec( 
    version_vec[-length(version_vec)], version_format_comp, bump_component
  )
  version_failure <- .projr_version_concat(version_vec, version_format_sep)
  version_run <- .projr_version_concat(version_update_vec, version_format_sep)
  version_success <- version_run
  list(
    "desc" = c(
      "run" = version_run,
      "failure" = version_failure,
      "success" = version_success
    )
  )
}

.projr_version_run_onwards_get_bump_update_vec <- function(version_vec,
                                                             version_format_comp,
                                                             bump_component) {
  comp_to_update_ind <- which(version_format_comp == bump_component)
  version_update_vec <- version_vec
  version_update_vec[comp_to_update_ind] <- version_update_vec[
    comp_to_update_ind
  ] + 1
  if (comp_to_update_ind < length(version_update_vec)) {
    version_update_vec[
      seq(comp_to_update_ind + 1, length(version_vec))
    ] <- 0
  }
  version_update_vec
}

#' @title Returns project version
#'
#' @description
#' Returns project version 
#'
#' @return Character.
#'
#' @export
projr_version_get <- function() {
  .projr_version_get(FALSE)
}

.projr_version_get <- function(dev_force = FALSE) {
  .projr_version_current_vec_get(dev_force = dev_force) |>
    .projr_version_chr_get()
}

.projr_version_chr_get <- function(version) {
  version_str <- version[[1]]
  version_format_sep_vec <- .projr_version_format_list_get(NULL)[["sep"]]
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



.projr_version_bump_dev <- function() {
  .projr_version_bump("dev")
}

.projr_version_bump_major <- function() {
  .projr_version_bump("major")
}
.projr_version_bump_minor <- function() {
  .projr_version_bump("minor")
}
.projr_version_bump_patch <- function() {
  .projr_version_bump("patch")
}

.projr_version_bump <- function(bump_component) {
  version_current_vec <- .projr_version_current_vec_get()
  version_format_list <- .projr_version_format_list_get(NULL)
  ind_to_bump <- which(version_format_list$component == bump_component)
  version_current_vec[ind_to_bump] <- version_current_vec[ind_to_bump] + 1
  for (i in seq_along(version_current_vec)) {
    if (i > ind_to_bump) {
      version_current_vec[i] <- 0
    }
  }
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

.projr_version_v_rm <- function(x) {
  gsub("^v+", "", tolower(x))
}

.projr_version_v_add <- function(x) {
  x_rm <- .projr_version_v_rm(x)
  paste0("v", x_rm)
}

.projr_version_get_earliest <- function(x) {
  x |>
    .projr_version_v_rm() |>
    unique() |>
    package_version() |>
    min() |>
    tail(1)
}

.projr_version_append <- function(path) {
  file.path(path, .projr_version_get_v())
}
.projr_version_get_v <- function() {
  paste0("v", projr_version_get())
}

.projr_version_get_latest <- function(x) {
  x |> 
    .projr_version_v_rm() |>
    unique() |>
    package_version() |>
    max() |>
    tail(1)
}
