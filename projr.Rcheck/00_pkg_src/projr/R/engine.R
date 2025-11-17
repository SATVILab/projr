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

# Determine engine from explicitly specified files
# This ensures build.scripts overrides _quarto.yml or _bookdown.yml
.engine_get_from_files <- function(files) {
  if (is.null(files) || length(files) == 0) {
    return(.engine_get())
  }

  # Check file extensions to determine engine
  has_qmd <- any(grepl("\\.qmd$", files, ignore.case = TRUE))
  has_rmd <- any(grepl("\\.Rmd$|\\.rmd$", files, ignore.case = TRUE))
  has_r <- any(grepl("\\.R$", files, ignore.case = TRUE))

  # Prioritize Quarto if any .qmd files
  if (has_qmd) {
    return("quarto_document")
  } else if (has_rmd) {
    return("rmd")
  } else if (has_r) {
    # R scripts can be run as regular documents
    return("rmd")
  } else {
    character(1L)
  }
}
