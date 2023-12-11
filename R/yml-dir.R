# basic functions
# ---------------------------

.projr_yml_dir_get <- function(profile) {
  projr_yml_get_unchecked(profile)[["directories"]]
}

.projr_yml_dir_set <- function(yml_dir, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]] <- yml_dir
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_nm <- function(yml, nm, profile) {
  if (.projr_state_null(yml) || .projr_state_z(yml)) {
    .projr_yml_dir_set_nm_empty(nm, profile)
  } else {
    .projr_yml_set_nm_non_empty(yml, profile)
  }
}

.projr_yml_dir_set_nm_empty <- function(nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]] <- yml_projr[["directories"]][
    setdiff(names(yml_projr[["directories"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}
