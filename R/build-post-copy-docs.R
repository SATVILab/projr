.projr_build_copy_docs <- function(output_run) {
  switch(.projr_engine_get(),
    "bookdown" = NULL,
    "quarto_project" = NULL,
    "quarto_document" = .projr_build_copy_docs_quarto(output_run),
    "rmd" = .projr_build_copy_docs_rmd(output_run)
  )
}

# copy docs - rmd
# ------------------

.projr_build_copy_docs_rmd <- function(output_run) {
  fn_vec_qmd <- list.files(pattern = "\\.Rmd$|\\.rmd$")
  for (fn in fn_vec_qmd) {
    .projr_build_copy_docs_rmd_ind(fn, output_run)
  }
  invisible(TRUE)
}

.projr_build_copy_docs_rmd_ind <- function(fn, output_run) {
  frontmatter_vec <- .projr_build_frontmatter_get(fn)
  format <- .projr_build_copy_docs_rmd_format_get(frontmatter_vec)
  fn_output_prefix <- .projr_build_copy_docs_rmd_fn_prefix_get(fn)
  path_vec <- .projr_build_copy_docs_rmd_path_get(format, fn_output_prefix)
  .projr_build_copy_docs_paths(path_vec, output_run)
  invisible(TRUE)
}

.projr_build_copy_docs_rmd_path_get <- function(format, fn_prefix) {
  fn_suffix <- .projr_build_copy_docs_rmd_fn_suffix_get(format)
  paste0(fn_prefix, ".", fn_suffix)
}

.projr_build_copy_docs_rmd_format_get <- function(frontmatter) {
  if (length(frontmatter) == 0) {
    return("html_document")
  }
  if (!"output" %in% names(frontmatter)) {
    return("html_document")
  }
  format <- frontmatter[["output"]]
  if (is.character(format)) {
    return(format)
  }
  names(format)[1]
}

.projr_build_copy_docs_rmd_fn_prefix_get <- function(fn) {
  gsub("\\.Rmd$|\\.rmd$", "", fn)
}

.projr_build_copy_docs_rmd_fn_suffix_get <- function(format) {
  switch(format,
    "html_notebook" = "nb.html",
    "word_document" = "docx",
    "tufte::tufte_handout" = ,
    "tufte_handout" = ,
    "tufte::tufte_book" = ,
    "tufte_book" = ,
    "context_document" = ,
    "beamer_presentation" = "pdf",
    "powerpoint_presentation" = "pptx",
    "revealjs::revealjs_presentation" = ,
    "revealjs_presentation" = ,
    "slidy_presentation" = ,
    "flexdashboard::flex_dashboard" = ,
    "flex_dashboard" = ,
    "tufte::tufte_html" = ,
    "tufte_html" = ,
    "html_vignette" = ,
    "ioslides_presentation" = "html",
    "github_document" = "md",
    gsub("_document", "", format)
  )
}

# copy docs - quarto
# ------------------

.projr_build_copy_docs_quarto <- function(output_run) {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  for (fn in fn_vec_qmd) {
    .projr_build_copy_docs_quarto_ind(fn, output_run)
  }
  invisible(TRUE)
}

.projr_build_copy_docs_quarto_ind <- function(fn, output_run) {
  frontmatter_vec <- .projr_build_frontmatter_get(fn)
  format <- .projr_build_copy_docs_quarto_format_get(frontmatter_vec)
  fn_output_prefix <- .projr_build_copy_docs_quarto_fn_prefix_get(
    frontmatter_vec, fn
  )
  path_vec <- .projr_build_copy_docs_quarto_path_get(format, fn_output_prefix)
  .projr_build_copy_docs_paths(path_vec, output_run)
  invisible(TRUE)
}

.projr_build_copy_docs_quarto_format_get <- function(frontmatter) {
  if (length(frontmatter) == 0) {
    return("html")
  }
  if (!"format" %in% names(frontmatter)) {
    return("html")
  }
  format <- frontmatter[["format"]]
  if (is.character(format)) {
    return(format)
  }
  names(format)[1]
}

.projr_build_copy_docs_quarto_path_get <- function(format, fn_prefix) {
  fn_suffix <- .projr_build_copy_docs_quarto_fn_suffix_get(format)
  fn <- paste0(fn_prefix, ".", fn_suffix)
  switch(format,
    "html" = ,
    "revealjs" = c(paste0(fn_prefix, "_files"), fn),
    fn
  )
}

.projr_build_copy_docs_quarto_fn_prefix_get <- function(frontmatter, fn) {
  if (!"output-file" %in% names(frontmatter)) {
    return(gsub("\\.qmd$", "", fn))
  }
  gsub("\\.qmd$", "", frontmatter[["output-file"]])
}


.projr_build_copy_docs_quarto_fn_suffix_get <- function(format) {
  switch(format,
    "revealjs" = "html",
    "beamer" = "pdf",
    format
  )
}

# copy docs - either
# ------------------

.projr_build_frontmatter_get <- function(path) {
  txt_vec <- readLines(path)
  txt_vec <- gsub("\\s+$", "", txt_vec)
  ind_vec_frontmatter <- which(txt_vec == "---")
  # no frontmatter detected
  if (length(ind_vec_frontmatter) < 2) {
    return(list())
  }
  # frontmatter detected
  txt_vec_frontmatter <- txt_vec[
    seq(ind_vec_frontmatter[1] + 1, ind_vec_frontmatter[2] - 1)
  ]
  path_yml <- file.path(tempdir(), "frontmatter.yml")
  writeLines(
    txt_vec_frontmatter,
    con = path_yml
  )
  yml_frontmatter <- yaml::read_yaml(path_yml)
  yml_frontmatter
}

.projr_build_copy_docs_paths <- function(path, output_run) {
  for (x in path) {
    .projr_build_copy_docs_paths_file(x, output_run)
    .projr_build_copy_docs_paths_dir(x, output_run)
  }
  .projr_build_copy_docs_paths_rm_dir(path)
  invisible(TRUE)
}

.projr_build_copy_docs_paths_file <- function(path, output_run) {
  if (!file.exists(path) || !fs::is_file(path)) {
    return(invisible(FALSE))
  }
  file_to <- projr_path_get("docs", basename(path), safe = !output_run)
  if (file.exists(file_to)) {
    invisible(file.remove(file_to))
  }
  file.rename(from = path, to = file_to)
  invisible(TRUE)
}

.projr_build_copy_docs_paths_dir <- function(path, output_run) {
  if (!file.exists(path) || !fs::is_dir(path)) {
    return(invisible(FALSE))
  }
  path_dir_to <- file.path(
    projr_path_get_dir("docs", safe = !output_run), path
  )
  .dir_move_exact(
    path_dir_from = path,
    path_dir_to = path_dir_to
  )
  invisible(TRUE)
}

.projr_build_copy_docs_paths_rm_dir <- function(path) {
  for (x in path) {
    if (file.exists(x) && fs::is_dir(x)) {
      invisible(unlink(x, recursive = TRUE))
    }
  }
  invisible(TRUE)
}
