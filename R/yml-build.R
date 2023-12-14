# basic
# ---------------------------

.projr_yml_build_get_dev_output_complete <- function(profile) {
  .projr_yml_build_get_dev_output(profile) %||% TRUE
}

.projr_yml_build_get_dev_output <- function(profile) {
  .projr_yml_build_get_nm("dev-output", profile)
}

.projr_yml_build_get_renv <- function(profile) {
  .projr_yml_build_get_nm("renv", profile)
}

.projr_yml_build_get_nm <- function(nm, profile) {
  .projr_yml_build_get(profile)[[nm]] %@@% NULL
}

.projr_yml_build_set_nm <- function(yml, nm, profile) {
  if (.projr_state_null(yml) || .projr_state_len_z(yml)) {
    .projr_yml_build_set_nm_empty(nm, profile)
  } else {
    .projr_yml_build_set_nm_non_empty(yml, profile)
  }
}

.projr_yml_build_set_nm_empty <- function(nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]] <- yml_projr[["build"]][
    setdiff(names(yml_projr[["build"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_build_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}


# most basic
# ---------------------------
.projr_yml_build_get <- function(profile) {
  projr_yml_get_unchecked(profile)[["build"]]
}

.projr_yml_build_set <- function(yml_build, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]] <- yml_build
  .projr_yml_set(yml_projr, profile)
}
