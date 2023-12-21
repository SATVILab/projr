# basic
# ---------------------------

.projr_yml_build_get_dev_output_complete <- function(profile) {
  .projr_yml_build_get_dev_output(profile) %||% TRUE
}

.projr_yml_build_get_dev_output <- function(profile) {
  .projr_yml_build_get_nm("dev-output", profile)
}

.projr_yml_build_get_nm <- function(nm, profile) {
  .projr_yml_build_get(profile)[[nm]] %@@% NULL
}

.projr_yml_build_set_nm <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .projr_yml_build_set_nm_empty(nm, profile)
  } else {
    .projr_yml_build_set_nm_non_empty(yml, nm, profile)
  }
}

.projr_yml_build_set_nm_empty <- function(nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["build"]] <- yml_projr[["build"]][
    setdiff(names(yml_projr[["build"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_build_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["build"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}


# most basic
# ---------------------------
.projr_yml_build_get <- function(profile) {
  .projr_yml_get(profile)[["build"]]
}

.projr_yml_build_set <- function(yml_build, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["build"]] <- yml_build
  .projr_yml_set(yml_projr, profile)
}
