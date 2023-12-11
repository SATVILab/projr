.projr_yml_build_get <- function(profile) {
  projr_yml_get_unchecked(profile)[["build"]]
}

.projr_yml_build_set <- function(yml_build, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]] <- yml_build
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_build_set_nm <- function(yml, nm, profile) {
  if (.projr_state_null(yml) || .projr_state_z(yml)) {
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
