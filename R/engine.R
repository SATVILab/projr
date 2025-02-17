.engine_get <- function() {
  if (file.exists(.path_get("_quarto.yml"))) {
    return("quarto_project")
  } else if (file.exists(.path_get("_bookdown.yml"))) {
    return("bookdown")
  } else if (
    length(list.files(.path_get(), pattern = "\\.qmd$")) > 0
  ) {
    return("quarto_document")
  } else if (
    length(list.files(.path_get(), pattern = "\\.Rmd$|\\.rmd$")) > 0
  ) {
    return("rmd")
  } else {
    character(1L)
  }
}
