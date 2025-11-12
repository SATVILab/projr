# basic
# ---------------------------

.yml_bd_set_output_dir <- function(path) {
  yml_bd <- .yml_bd_get()
  yml_bd[["output_dir"]] <- path
  .yml_bd_set(yml_bd)
}

.yml_bd_get_output_dir <- function() {
  .yml_bd_get()[["output_dir"]]
}

.yml_bd_get_nm <- function(nm) {
  .yml_bd_get()[[nm]]
}

# most basic
# ---------------------------

.yml_bd_get <- function() {
  path_yml <- .path_get("_bookdown.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.yml_bd_set <- function(list_save) {
  path_yml <- .path_get("_bookdown.yml")
  yaml::write_yaml(list_save, path_yml)
  .newline_append(path_yml)
  invisible(TRUE)
}
