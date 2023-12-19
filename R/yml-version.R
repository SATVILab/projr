.projr_version_format_list_get <- function(profile) {
  version_format <- .projr_yml_version_get(profile)
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
    "component" = version_format_vec_comp,
    "sep" = version_format_vec_sep
  )
}

.projr_yml_version_get <- function(profile) {
  version_format <- projr_yml_get_unchecked(profile)[["version-format"]]
  if (.is_len_0(version_format)) {
    return(NULL)
  }
  version_format
}
