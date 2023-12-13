# detailed functions
# ---------------------------

# get package values for a particular label
.projr_yml_dir_get_pkg_nm_complete <- function(label, profile) {
  if (!label %in% .projr_yml_dir_get_label_output(profile)) {
    return(character())
  }
  .projr_yml_dir_get_pkg_nm(label, profile) |>
    .projr_yml_dir_complete_pkg(label)
}

.projr_yml_dir_get_pkg_nm <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["package"]]
}

.projr_yml_dir_complete_pkg <- function(pkg, label) {
  label[pkg]
}

# get output values for a particular label
.projr_yml_dir_get_output_nm_complete <- function(label, profile) {
  .projr_yml_dir_get_output_nm(label, profile) |>
    .projr_yml_dir_complete_output(label, profile)
}

.projr_yml_dir_get_output_nm <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["output"]]
}

.projr_yml_dir_complete_output <- function(output, label, profile) {
  switch(class(output),
    logical = .projr_yml_dir_complete_output_lgl(output, label),
    character = .projr_yml_dir_complete_output_chr(output, label, profile),
    character()
  )
}

.projr_yml_dir_complete_output_lgl <- function(output, label) {
  if (!output) {
    return(character())
  }
  .projr_yml_dir_complete_output_true(label)
}

.projr_yml_dir_complete_output_true <- function(label) {
  .projr_yml_dir_get_label_output() |> setdiff(label)
}

.projr_yml_dir_complete_output_chr <- function(output, label, profile) {
  output[output %in% .projr_yml_dir_get_label_output(profile)] |>
    setdiff(label)
}

# all labels matching certain types
.projr_yml_dir_get_label_output <- function(profile) {
  .projr_yml_dir_get_label_nm("output", profile)
}

.projr_yml_dir_get_label_data_raw <- function(profile) {
  .projr_yml_dir_get_label_nm("dataraw", profile)
}

.projr_yml_dir_get_label_cache <- function(profile) {
  .projr_yml_dir_get_label_nm("cache", profile)
}

.projr_yml_dir_get_label_nm <- function(nm, profile) {
  .projr_yml_dir_get_label_nm_raw(paste0("^", nm), profile)
}

.projr_yml_dir_get_label_nm_raw <- function(nm, profile) {
  yml_projr_dir <- .projr_yml_dir_get(profile)
  label_vec <- names(yml_projr_dir)
  label_vec[
    grepl(nm, .projr_dir_label_strip(label_vec))
  ]
}

.projr_yml_dir_get_nm <- function(nm, profile) {
  .projr_yml_dir_get(profile)[[nm]]
}
.projr_yml_dir_get_path <- function(label, profile) {
  .projr_yml_dir_get_nm(label, profile)[["path"]]
}

.projr_yml_dir_set_path <- function(path, label, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]][[label]][["path"]] <- path
  .projr_yml_set(yml_projr, profile)
}

# basic functions
# ---------------------------

.projr_yml_dir_get_label <- function(label, profile) {
  .projr_yml_dir_get(profile)[[label]]
}

.projr_yml_dir_get <- function(profile) {
  projr_yml_get_unchecked(profile)[["directories"]]
}

.projr_yml_dir_set <- function(yml_dir, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]] <- yml_dir
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_nm <- function(yml, nm, profile) {
  if (.projr_state_null(yml) || .projr_state_len_z(yml)) {
    .projr_yml_dir_set_nm_empty(nm, profile)
  } else {
    .projr_yml_dir_set_nm_non_empty(yml, nm, profile)
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
