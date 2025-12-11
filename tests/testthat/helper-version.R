.test_version_v_get <- function(empty = FALSE,
                                version = NULL,
                                not_dev = TRUE) {
  version <- if (is.null(version)) projr_version_get() else version
  version <- gsub("^v+", "", version)
  version <- if (not_dev) gsub("-\\d+$", "", version) else version
  version_suffix <- if (empty) "-empty" else ""
  paste0("v", version, version_suffix)
}

.test_label_version_get <- function(label,
                                    empty = FALSE,
                                    version = NULL,
                                    not_dev = TRUE) {
  paste0(
    label, "-", .test_version_v_get(empty, version, not_dev), ".zip"
  )
}
