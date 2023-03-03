.projr_engine_get <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (file.exists(file.path(dir_proj, "_quarto.yml"))) {
    return("quarto_project")
  } else if (file.exists(file.path(dir_proj, "_bookdown.yml"))) {
    return("bookdown")
  } else if (length(list.files(pattern = "\\.qmd$")) > 0) {
    return("quarto_document")
  } else if (length(list.files(pattern = "\\.Rmd$|\\.rmd$")) > 0) {
    return("rmd")
  } else {
    stop(
      "No project engine found
      (bookdown, quarto project, quarto document or rmd)"
    )
  }
}
