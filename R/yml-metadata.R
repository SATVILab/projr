
# basic
# ---------------------------

.projr_yml_metadata_get_nm <- function(nm, profile) {
  .projr_yml_metadata_get(profile)[[nm]] %@@% NULL
}

.projr_yml_metadata_set_nm <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .projr_yml_metadata_set_nm_empty(nm, profile)
  } else {
    .projr_yml_metadata_set_nm_non_empty(yml, nm, profile)
  }
}

.projr_yml_metadata_set_nm_empty <- function(nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["metadata"]] <- yml_projr[["metadata"]][
    setdiff(names(yml_projr[["metadata"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_metadata_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["metadata"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}

# most basic
# ---------------------------

.projr_yml_metadata_get <- function(profile) {
  .projr_yml_get(profile)[["metadata"]]  %||% list()
}

.projr_yml_metadata_set <- function(yml_metadata, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["metadata"]] <- yml_metadata
  .projr_yml_set(yml_projr, profile)
}
