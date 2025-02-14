
# basic
# ---------------------------

.yml_metadata_get_nm <- function(nm, profile) {
  .yml_metadata_get(profile)[[nm]] %@@% NULL
}

.yml_metadata_set_nm <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .yml_metadata_set_nm_empty(nm, profile)
  } else {
    .yml_metadata_set_nm_non_empty(yml, nm, profile)
  }
}

.yml_metadata_set_nm_empty <- function(nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["metadata"]] <- yml_projr[["metadata"]][
    setdiff(names(yml_projr[["metadata"]]), nm)
  ]
  .yml_set(yml_projr, profile)
}

.yml_metadata_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["metadata"]][[nm]] <- yml
  .yml_set(yml_projr, profile)
}

# most basic
# ---------------------------

.yml_metadata_get <- function(profile) {
  .yml_get(profile)[["metadata"]]  %||% list()
}

.yml_metadata_set <- function(yml_metadata, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["metadata"]] <- yml_metadata
  .yml_set(yml_projr, profile)
}
