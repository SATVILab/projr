.projr_yml_quarto_get_output_dir <- function() {
  .projr_yml_quarto_get_project()[["output-dir"]]
}

.projr_yml_quarto_set_output_dir <- function(path) {
  yml_quarto <- .projr_yml_quarto_get()
  yml_quarto[["project"]][["output-dir"]] <- path
  .projr_yml_quarto_set(yml_quarto)
}

.projr_yml_quarto_get_project_type <- function() {
  .projr_yml_quarto_get_project()[["type"]]
}

.projr_yml_quarto_get_project <- function() {
  .projr_yml_quarto_get()[["project"]]
}
.projr_yml_quarto_get_nm <- function(nm) {
  .projr_yml_quarto_get()[[nm]]
}

# most basic
# ---------------------------

.projr_yml_quarto_get <- function() {
  path_yml <- .dir_proj_get("_quarto.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_quarto_set <- function(list_save) {
  path_yml <- .dir_proj_get("_quarto.yml")
  
  yaml::write_yaml(
    list_save, path_yml,
    handlers = list(logical = function(x) {
      value <- ifelse(x, "true", "false")
      structure(value, class = "verbatim")
    }
  )
  .projr_newline_append(path_yml)
  invisible(TRUE)
}
