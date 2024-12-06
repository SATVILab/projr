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
  # -------------
  list_save <- wrap_in_list_if_string(list_save, c("project", "render"))

  # website key
  # -----------

  list_save <- wrap_in_list_if_string(list_save, c("website", "other-links"))
  list_save <- wrap_in_list_if_string(list_save, c("website", "code-links"))
  list_save <- wrap_in_list_if_string(list_save, c("website", "navbar", "right"))
  list_save <- wrap_in_list_if_string(list_save, c("website", "navbar", "left"))

  # START HERE: pick up at nav-items on this link: https://quarto.org/docs/reference/projects/websites.html


  # book key (https://quarto.org/docs/reference/projects/books.html)
  # ---------------
  list_save <- wrap_in_list_if_string(list_save, c("book", "chapters"))
  list_save <- wrap_in_list_if_string(list_save, c("book", "appendices"))

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

# Helper function to wrap a nested element in a list if it’s a string
wrap_in_list_if_string <- function(lst, path) {
  
  val <- get_nested_value(lst, path)
  if (.is_string(val)) {
    # Wrap this element in a list
    lst <- set_nested_value(lst, path, list(val))
  }
  
  lst
}

# Safely retrieve value at nested path
get_nested_value <- function(x, keys) {
  for (k in keys) {
    if (!is.list(x) || is.null(x[[k]])) return(NULL)
    x <- x[[k]]
  }
  x
}

# Assign a value at nested path
set_nested_value <- function(x, keys, value) {
  if (length(keys) == 1) {
    x[[keys]] <- value
  } else {
    x[[keys[1]]] <- set_nested_value(x[[keys[1]]], keys[-1], value)
  }
  x
}






