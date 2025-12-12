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
    .version_set_desc(version)
    .version_set_file(version, only_if_exists = only_if_exists)
  } else {
    .version_set_file(version, only_if_exists = FALSE)
  }
  invisible(TRUE)
}

.version_copy_dir <- function(path_dir) {
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
  .version_get() |>
    .version_set_file(path_dir = path_dir)
}


.version_set_desc <- function(version) {
  .version_check(version)
  desc_file <- read.dcf("DESCRIPTION")
  desc_file[, "Version"] <- .version_v_rm(version)
  write.dcf(desc_file, file = "DESCRIPTION")
}

.version_check <- function(version) {
  if (missing(version)) stop("version must be supplied")
  .assert_string(version, TRUE)
  if (grepl("^v", version)) {
    version <- gsub("^v", "", version)
  }
  .version_format_check(version)
}

.version_check_error_free <- function(version) {
  tryCatch(
    {
      .version_check(version)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}


.version_set_file <- function(version, path_dir = NULL, only_if_exists = FALSE) {
  .version_check(version)
  .assert_string(path_dir)
  .assert_flag(only_if_exists)
  version <- version |> .version_v_add()
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

.version_format_check <- function(version, profile = NULL) {
  version_format <- .yml_metadata_get_version_format(profile)
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

.version_current_vec_get <- function(dev_force = FALSE) {
  version_vec_init <- .version_current_vec_get_init()

  version_format <- .version_format_list_get(NULL)[["component"]]
  # if not forcing there to be a dev version
  if (!dev_force) {
    return(version_vec_init |> as.integer())
  }
  if (length(version_format) == length(version_vec_init)) {
    return(version_vec_init |> as.integer())
  }
  version_dev_append <- .version_current_vec_get_dev(version_format)
  version_vec_init <- c(version_vec_init, version_dev_append)
  version_vec_init |> as.integer()
}

.version_current_vec_get_dev <- function(version_format) {
  if ("dev" == version_format[length(version_format)]) {
    return("1")
  }
  version_format[length(version_format)]
}

.version_current_vec_get_init <- function() {
  version_vec_init <- if (file.exists(.path_get("DESCRIPTION"))) {
    .version_current_vec_get_init_desc()
  } else {
    .version_current_vec_get_init_file()
  }
  .assert_chr_mid(version_vec_init)
  version_vec_init
}

.version_current_vec_get_init_desc <- function() {
  desc <- .desc_get()
  strsplit(
    desc[1, "Version"][[1]],
    split = "\\-|\\."
  )[[1]]
}

.version_current_vec_get_init_file <- function() {
  if (!file.exists(.path_get("VERSION"))) {
    stop("VERSION file not found")
  }
  version_file <- readLines("VERSION", warn = FALSE)
  if (length(version_file) == 0) {
    stop("VERSION file is empty")
  }
  version_file <- trimws(version_file[[1]])
  if (is.na(version_file) || version_file == "") {
    stop("VERSION file contains invalid content")
  }
  version_file <- .version_v_rm(version_file)
  strsplit(version_file, split = "\\-|\\.")[[1]]
}

.version_run_onwards_get <- function(bump_component) {
  version_orig_vec <- .version_current_vec_get(dev_force = TRUE)
  version_format_list <- .version_format_list_get(NULL)

  if (is.null(bump_component)) {
    return(
      .version_run_onwards_get_dev(
        version_orig_vec, version_format_list$sep
      )
    )
  }
  if (bump_component == "dev") {
    version_orig_vec[length(version_orig_vec)] <- version_orig_vec[
      length(version_orig_vec)
    ] + 1
    return(
      .version_run_onwards_get_dev(
        version_orig_vec, version_format_list$sep
      )
    )
  }

  .version_run_onwards_get_bump(
    version_orig_vec,
    version_format_list$component,
    version_format_list$sep,
    bump_component
  )
}

.version_run_onwards_get_dev <- function(version_vec,
                                         version_format_sep) {
  version_vec <- .version_run_onwards_get_dev_append_dev(
    version_vec, version_format_sep
  )
  version <- .version_concat(version_vec, version_format_sep)
  list(
    "desc" = c(
      "run" = version,
      "failure" = version,
      "success" = version
    )
  )
}

.version_run_onwards_get_dev_append_dev <- function(version_vec,
                                                    version_format_sep) {
  if (length(version_vec) == length(version_format_sep) + 1) {
    return(version_vec)
  }
  version_format <- .version_format_list_get(NULL)[["component"]]
  version_dev_append <- .version_current_vec_get_dev(version_format)
  c(version_vec, version_dev_append)
}

.version_concat <- function(version_vec, split_vec) {
  # First check if version_vec is empty before type validation
  if (length(version_vec) == 0) {
    stop("version_vec must have at least one element")
  }

  # Convert numeric to character if needed
  if (is.numeric(version_vec)) {
    version_vec <- as.character(version_vec)
  }

  # Validate input types
  .assert_chr(version_vec, required = TRUE)

  # Special case: single element version (no separators needed)
  if (.is_len_1(version_vec)) {
    return(version_vec)
  }

  # For multi-element versions, validate split_vec
  if (length(split_vec) == 0) {
    stop("split_vec must have at least one element when version_vec has more than one element")
  }
  .assert_chr(split_vec, required = TRUE)

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

.version_run_onwards_get_bump <- function(version_vec,
                                          version_format_comp,
                                          version_format_sep,
                                          bump_component) {
  version_update_vec <- .version_run_onwards_get_bump_update_vec(
    version_vec[-length(version_vec)], version_format_comp, bump_component
  )
  version_failure <- .version_concat(version_vec, version_format_sep)
  version_run <- .version_concat(version_update_vec, version_format_sep)
  version_success <- version_run
  list(
    "desc" = c(
      "run" = version_run,
      "failure" = version_failure,
      "success" = version_success
    )
  )
}

.version_run_onwards_get_bump_update_vec <- function(version_vec,
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
  .version_get(FALSE)
}

.version_get <- function(dev_force = FALSE) {
  .version_current_vec_get(dev_force = dev_force) |>
    .version_chr_get()
}

.version_chr_get <- function(version) {
  version_str <- version[[1]]
  version_format_sep_vec <- .version_format_list_get(NULL)[["sep"]]
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


.version_bump_dev <- function() {
  .version_bump("dev")
}

.version_bump_major <- function() {
  .version_bump("major")
}
.version_bump_minor <- function() {
  .version_bump("minor")
}
.version_bump_patch <- function() {
  .version_bump("patch")
}

.version_bump <- function(bump_component) {
  version_current_vec <- .version_current_vec_get()
  version_format_list <- .version_format_list_get(NULL)
  ind_to_bump <- which(version_format_list$component == bump_component)
  version_current_vec[ind_to_bump] <- version_current_vec[ind_to_bump] + 1
  for (i in seq_along(version_current_vec)) {
    if (i > ind_to_bump) {
      version_current_vec[i] <- 0
    }
  }
  version_new <- .version_chr_get(version_current_vec)
  projr_version_set(version = version_new)
  version_new
}

.version_comp_min_check <- function(bump_component,
                                    version_min) {
  version_comp_vec_min <- .version_comp_vec_min_get(version_min)
  bump_component %in% version_comp_vec_min
}

.version_comp_vec_min_get <- function(version_min) {
  version_vec_possible <- c("major", "minor", "patch")
  switch(version_min,
    "any" = version_vec_possible,
    version_vec_possible[seq_len(which(version_vec_possible == version_min))]
  )
}

.version_v_rm <- function(x) {
  # Convert package_version/numeric_version to character
  if (inherits(x, "package_version") || inherits(x, "numeric_version")) {
    x <- as.character(x)
  }
  .assert_string(x, required = TRUE)
  
  # Handle multi-version strings (semicolon-separated)
  if (grepl(";", x, fixed = TRUE)) {
    version_parts <- strsplit(x, ";", fixed = TRUE)[[1]]
    versions_clean <- vapply(version_parts, function(v) {
      gsub("^v+", "", tolower(v))
    }, character(1L), USE.NAMES = FALSE)
    return(paste(versions_clean, collapse = ";"))
  }
  
  gsub("^v+", "", tolower(x))
}

.version_v_add <- function(x) {
  # Convert package_version/numeric_version to character
  if (inherits(x, "package_version") || inherits(x, "numeric_version")) {
    x <- as.character(x)
  }
  .assert_string(x, required = TRUE)
  
  # Handle multi-version strings (semicolon-separated)
  if (grepl(";", x, fixed = TRUE)) {
    version_parts <- strsplit(x, ";", fixed = TRUE)[[1]]
    versions_with_v <- vapply(version_parts, function(v) {
      v_clean <- gsub("^v+", "", tolower(v))
      paste0("v", v_clean)
    }, character(1L), USE.NAMES = FALSE)
    return(paste(versions_with_v, collapse = ";"))
  }
  
  x_rm <- .version_v_rm(x)
  paste0("v", x_rm)
}

.version_get_earliest <- function(x) {
  .assert_chr(x, required = TRUE)
  if (length(x) == 0) {
    stop("x must have at least one element")
  }
  
  # Handle multi-version strings (semicolon-separated)
  all_versions <- character(0)
  for (ver_str in x) {
    if (!is.na(ver_str) && nzchar(ver_str)) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      all_versions <- c(all_versions, versions)
    }
  }
  
  if (length(all_versions) == 0) {
    stop("No valid versions found")
  }
  
  # Apply .version_to_package_version to each element
  vers_pkg <- vapply(all_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)
  vers_pkg |>
    unique() |>
    package_version() |>
    min() |>
    utils::tail(1)
}

.version_to_package_version <- function(version) {
  # Convert projr version (with dev suffix) to package_version compatible format
  # Handles versions like "v0.0.1-dev", "v0.0.1-1", "0.0.1-dev" etc.
  # Converts dev components to numeric for comparison
  # Examples: "0.0.1-dev" -> "0.0.1.9000", "0.0.1-1" -> "0.0.1.1"
  
  .assert_string(version, required = TRUE)
  
  # Remove leading "v" if present
  version_clean <- .version_v_rm(version)
  
  # Check if there's a dev component
  if (grepl("-", version_clean)) {
    parts <- strsplit(version_clean, "-", fixed = TRUE)[[1]]
    base_version <- parts[1]
    dev_part <- parts[2]
    
    # Convert dev suffix to numeric
    if (dev_part == "dev") {
      # Convert "-dev" to ".9000" (R convention for development versions)
      version_numeric <- paste0(base_version, ".9000")
    } else {
      # Convert "-N" to ".N"
      version_numeric <- paste0(base_version, ".", dev_part)
    }
  } else {
    version_numeric <- version_clean
  }
  
  package_version(version_numeric)
}

.version_append <- function(path) {
  .assert_string(path, required = TRUE)
  file.path(path, .version_get_v())
}
.version_get_v <- function() {
  paste0("v", projr_version_get())
}

.version_get_latest <- function(x) {
  .assert_chr(x, required = TRUE)
  if (length(x) == 0) {
    stop("x must have at least one element")
  }
  
  # Handle multi-version strings (semicolon-separated)
  all_versions <- character(0)
  for (ver_str in x) {
    if (!is.na(ver_str) && nzchar(ver_str)) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      all_versions <- c(all_versions, versions)
    }
  }
  
  if (length(all_versions) == 0) {
    stop("No valid versions found")
  }
  
  # Apply .version_v_rm to each element, convert to package_version and find max
  x_clean <- vapply(all_versions, .version_v_rm, character(1), USE.NAMES = FALSE)
  x_clean |>
    unique() |>
    package_version() |>
    max() |>
    utils::tail(1)
}
