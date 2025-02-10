# basic
# ---------------------------

.projr_yml_bd_set_output_dir <- function(path) {
  yml_bd <- .projr_yml_bd_get()
  yml_bd[["output_dir"]] <- path
  .projr_yml_bd_set(yml_bd)
}

.projr_yml_bd_get_output_dir <- function() {
  .projr_yml_bd_get_project()[["output_dir"]]
}

.projr_yml_bd_get_project <- function() {
  .projr_yml_bd_get()[["project"]]
}

.projr_yml_bd_get_nm <- function(nm) {
  .projr_yml_bd_get()[[nm]]
}

# most basic
# ---------------------------

.projr_yml_bd_get <- function() {
  path_yml <- .path_get("_bookdown.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_bd_set <- function(list_save) {
  path_yml <- .path_get("_bookdown.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}
