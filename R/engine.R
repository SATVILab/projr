.engine_get <- function(scripts) {
  if (length(scripts) > 0L) {
    .cli_debug("Detecting engine from specified scripts")
    .engine_get_from_scripts(scripts)
  } else {
    .cli_debug("Detecting engine from project configuration")
    .engine_get_auto()
  }
}

.engine_get_auto <- function() {
  if (file.exists(.path_get("_quarto.yml"))) {
    return("quarto_project")
  } else if (file.exists(.path_get("_bookdown.yml"))) {
    return("bookdown")
  } else if (length(list.files(.path_get(), pattern = "\\.qmd$")) > 0) {
    return("quarto_document")
  } else if (length(list.files(.path_get(), pattern = "\\.Rmd$|\\.rmd$")) > 0) {
    return("rmd")
  } else {
    character(1L)
  }
}

# Determine engine from explicitly specified files
# This ensures build.scripts overrides _quarto.yml or _bookdown.yml
.engine_get_from_scripts <- function(scripts) {
  # Check file extensions to determine engine
  has_qmd <- any(grepl("\\.qmd$", scripts, ignore.case = TRUE))
  has_rmd <- any(grepl("\\.Rmd$|\\.rmd$", scripts, ignore.case = TRUE))
  has_r <- any(grepl("\\.R$", scripts, ignore.case = TRUE))

  # Prioritize Quarto if any .qmd files
  if (has_qmd) {
    return("quarto_document")
  } else if (has_rmd) {
    return("rmd")
  } else if (has_r) {
    return("r")
  } else {
    character(1L)
  }
}
