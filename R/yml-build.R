# basic
# ---------------------------

.yml_build_get_dev_output_complete <- function(profile) {
  .yml_build_get_dev_output(profile) %||% TRUE
}

.yml_build_get_dev_output <- function(profile) {
  .yml_build_get_nm("dev-output", profile)
}

.yml_build_get_script <- function(profile) {
  .yml_build_get_nm("script", profile)
}

.yml_build_get_hooks <- function(profile) {
  .yml_build_get_nm("hooks", profile)
}

.yml_build_get_nm <- function(nm, profile) {
  .yml_build_get(profile)[[nm]] %@@% NULL
}

.yml_build_set_nm <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .yml_build_set_nm_empty(nm, profile)
  } else {
    .yml_build_set_nm_non_empty(yml, nm, profile)
  }
}

.yml_build_set_nm_empty <- function(nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["build"]] <- yml_projr[["build"]][
    setdiff(names(yml_projr[["build"]]), nm)
  ]
  .yml_set(yml_projr, profile)
}

.yml_build_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["build"]][[nm]] <- yml
  .yml_set(yml_projr, profile)
}


# most basic
# ---------------------------
.yml_build_get <- function(profile) {
  .yml_get(profile)[["build"]] %||% list()
}

.yml_build_set <- function(yml_build, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["build"]] <- yml_build
  .yml_set(yml_projr, profile)
}
