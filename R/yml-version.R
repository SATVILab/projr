.version_format_list_get <- function(profile) {
  version_format <- .yml_metadata_get_version_format(profile)
  if (is.null(version_format) || length(version_format) == 0 || version_format == "") {
    stop("version_format cannot be empty or NULL")
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
    "component" = version_format_vec_comp,
    "sep" = version_format_vec_sep
  )
}

# version-format
# ---------------------------

.yml_metadata_get_version_format <- function(profile) {
  .yml_metadata_get_nm("version-format", profile) %||% "major.minor.patch-dev"
}

.yml_metadata_set_version_format <- function(version_format, profile) {
  .yml_version_format_set_check(version_format)
  .yml_metadata_set_nm(version_format, "version-format", profile)
}

.yml_version_format_set_check <- function(version_format) {
  if (is.null(version_format) || .is_len_0(version_format)) {
    return(invisible(TRUE))
  }
  version_valid_vec <- c(
    "major.minor.patch-dev",
    "major.minor.patch.dev",
    "major.minor-dev",
    "major.minor.dev",
    "major-dev",
    "major.dev"
  )
  version_valid_vec_9000 <- gsub(
    "dev$", "9000", version_valid_vec
  )
  version_valid_vec_1 <- gsub(
    "dev$", "1", version_valid_vec
  )
  version_valid_vec <- c(
    version_valid_vec,
    version_valid_vec_9000,
    version_valid_vec_1
  )
  .assert_given_mid(version_format)
  .assert_string(version_format, TRUE)
  .assert_in(version_format, version_valid_vec, TRUE)
}
