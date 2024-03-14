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

  # the `render` key has to be a list (in YAML),
  # but is automatically converted to a string (in R)
  # so we need to convert it back to a list

  # project key
  if (.is_string(list_save[["project"]][["render"]])) {
    list_save[["project"]][["render"]] <- list(
      list_save[["project"]][["render"]]
    )
  }

  # website key
  # -------------

  # top level
  if (.is_string(list_save[["website"]][["other-links"]])) {
    list_save[["website"]][["other-links"]] <- list(
      list_save[["website"]][["other-links"]]
    )
  }
  if (.is_string(list_save[["website"]][["code-links"]])) {
    list_save[["website"]][["code-links"]] <- list(
      list_save[["website"]][["code-links"]]
    )
  }

  # navbar
  if (.is_string(list_save[["website"]][["navbar"]][["right"]])) {
    list_save[["website"]][["navbar"]][["right"]] <- list(
      list_save[["website"]][["navbar"]][["right"]]
    )
  }
  if (.is_string(list_save[["website"]][["navbar"]][["left"]])) {
    list_save[["website"]][["navbar"]][["left"]] <- list(
      list_save[["website"]][["navbar"]][["left"]]
    )
  }

  # START HERE: pick up at nav-items on this link: https://quarto.org/docs/reference/projects/websites.html


  # book key (https://quarto.org/docs/reference/projects/books.html)
  # ---------------
  if (.is_string(list_save[["book"]][["chapters"]])) {
    list_save[["book"]][["chapters"]] <- list(
      list_save[["book"]][["chapters"]]
    )
  }

  # manuscript key (https://quarto.org/docs/reference/projects/manuscripts.html)
  # ----------------

  yaml::write_yaml(
    list_save, path_yml,
    handlers = list(logical = function(x) {
      value <- ifelse(x, "true", "false")
      structure(value, class = "verbatim")
    })
  )
  .projr_newline_append(path_yml)
  invisible(TRUE)
}
