.projr_engine_get <- function() {
  if (file.exists(.projr_dir_proj_get("_quarto.yml"))) {
    return("quarto_project")
  } else if (file.exists(.projr_dir_proj_get("_bookdown.yml"))) {
    return("bookdown")
  } else if (
    length(list.files(.projr_dir_proj_get(), pattern = "\\.qmd$")) > 0
    ) {
    return("quarto_document")
  } else if (
    length(list.files(.projr_dir_proj_get(), pattern = "\\.Rmd$|\\.rmd$")) > 0
  ) {
    return("rmd")
  } else {
    stop(
      "No project engine found
      (bookdown, quarto project, quarto document or rmd)"
    )
  }
}
