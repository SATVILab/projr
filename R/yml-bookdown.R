# basic
# ---------------------------

.yml_bd_set_output_dir <- function(path) {
  path <- .path_force_rel(path)
  if (all(fs::is_absolute_path(path))) {
    stop(
      "Bookdown requires a relative path for its output directory. ",
      "The path '", path, "' cannot be made relative to the project root ",
      "(e.g., it may be on a different drive).", 
      call. = FALSE
    )
  }
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

.yml_bd_get_book_filename <- function() {
  book_filename <- .yml_bd_get()[["book_filename"]]
  if (is.null(book_filename)) {
    return("_main") # Default bookdown book filename
  }
  book_filename
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
