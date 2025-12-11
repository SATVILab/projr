.build_copy_docs <- function(output_run) {
  switch(.engine_get(),
    "bookdown" = .build_copy_docs_bookdown(output_run),
    "quarto_project" = .build_copy_docs_quarto_project(output_run),
    "quarto_document" = .build_copy_docs_quarto(output_run),
    "rmd" = .build_copy_docs_rmd(output_run)
  )
}

# copy docs - rmd
# ------------------

.build_copy_docs_rmd <- function(output_run) {
  fn_vec_rmd <- list.files(pattern = "\\.Rmd$|\\.rmd$")
  for (fn in fn_vec_rmd) {
    .build_copy_docs_rmd_ind(fn, output_run)
  }
  invisible(TRUE)
}

.build_copy_docs_rmd_ind <- function(fn, output_run) {
  frontmatter_vec <- .build_frontmatter_get(fn)
  format <- .build_copy_docs_rmd_format_get(frontmatter_vec)
  fn_output_prefix <- .build_copy_docs_rmd_fn_prefix_get(fn)
  path_vec <- .build_copy_docs_rmd_path_get(format, fn_output_prefix)
  .build_copy_docs_paths(path_vec, output_run)
  invisible(TRUE)
}

.build_copy_docs_rmd_path_get <- function(format, fn_prefix) {
  fn_suffix <- .build_copy_docs_rmd_fn_suffix_get(format)
  fn <- paste0(fn_prefix, ".", fn_suffix)
  # Check if this format produces HTML output that may have a _files folder
  if (.build_copy_docs_rmd_is_html_format(format)) {
    return(c(paste0(fn_prefix, "_files"), fn))
  }
  fn
}

.build_copy_docs_rmd_format_get <- function(frontmatter) {
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

.build_copy_docs_rmd_fn_prefix_get <- function(fn) {
  gsub("\\.Rmd$|\\.rmd$", "", fn)
}

.build_copy_docs_rmd_is_html_format <- function(format) {
  # Check if the format produces HTML output that may have a _files folder
  html_formats <- c(
    "html_notebook",
    "revealjs::revealjs_presentation",
    "revealjs_presentation",
    "slidy_presentation",
    "flexdashboard::flex_dashboard",
    "flex_dashboard",
    "tufte::tufte_html",
    "tufte_html",
    "html_vignette",
    "ioslides_presentation",
    "html_document"
  )

  # Check exact match with HTML formats
  if (format %in% html_formats) {
    return(TRUE)
  }

  # Check if it's an html_document variant (e.g., prettydoc::html_pretty)
  if (grepl("html", format, ignore.case = TRUE)) {
    return(TRUE)
  }

  FALSE
}

.build_copy_docs_rmd_fn_suffix_get <- function(format) {
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

.build_copy_docs_quarto <- function(output_run) {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  for (fn in fn_vec_qmd) {
    .build_copy_docs_quarto_ind(fn, output_run)
  }
  invisible(TRUE)
}

.build_copy_docs_quarto_ind <- function(fn, output_run) {
  frontmatter_vec <- .build_frontmatter_get(fn)
  format <- .build_copy_docs_quarto_format_get(frontmatter_vec)
  fn_output_prefix <- .build_copy_docs_quarto_fn_prefix_get(
    frontmatter_vec, fn
  )
  path_vec <- .build_copy_docs_quarto_path_get(format, fn_output_prefix)
  .build_copy_docs_paths(path_vec, output_run)
  invisible(TRUE)
}

.build_copy_docs_quarto_format_get <- function(frontmatter) {
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

.build_copy_docs_quarto_path_get <- function(format, fn_prefix) {
  fn_suffix <- .build_copy_docs_quarto_fn_suffix_get(format)
  fn <- paste0(fn_prefix, ".", fn_suffix)
  switch(format,
    "html" = ,
    "revealjs" = c(paste0(fn_prefix, "_files"), fn),
    fn
  )
}

.build_copy_docs_quarto_fn_prefix_get <- function(frontmatter, fn) {
  if (!"output-file" %in% names(frontmatter)) {
    return(gsub("\\.qmd$", "", fn))
  }
  gsub("\\.qmd$", "", frontmatter[["output-file"]])
}


.build_copy_docs_quarto_fn_suffix_get <- function(format) {
  switch(format,
    "revealjs" = "html",
    "beamer" = "pdf",
    format
  )
}

# copy docs - bookdown
# ------------------

.build_copy_docs_bookdown <- function(output_run) {
  if (!output_run) {
    # no copying required as we build directly to the
    # temporary location, which is final if
    # not in an output run
    return(invisible(FALSE))
  }

  # Get source directory (temporary build location in cache)
  source_dir <- file.path(
    .dir_get_cache_auto_version(profile = NULL),
    .dir_get_docs_bookdown() # Gets the bookdown output directory name
  )

  if (!dir.exists(source_dir)) {
    .cli_info("Bookdown output directory not found: {source_dir}")
    return(invisible(FALSE))
  }

  # Get destination directory (final docs location)
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)

  # Copy all contents from source to destination, excluding CHANGELOG.md
  .dir_move_exact(source_dir, dest_dir, fn_exc = "CHANGELOG.md")

  # Copy the <book_filename>_files directory if it exists
  # This contains knitr cache files (figures, etc.) from the working directory
  .build_copy_docs_bookdown_files(output_run)

  .cli_info("Copied bookdown output from {source_dir} to {dest_dir}")
  invisible(TRUE)
}

.build_copy_docs_bookdown_files <- function(output_run) {
  # Get the book filename from _bookdown.yml
  book_filename <- .yml_bd_get_book_filename()
  files_dir_name <- paste0(book_filename, "_files")

  # Source is in the cache build directory (working directory during build)
  cache_dir <- .dir_get_cache_auto_version(profile = NULL)
  source_files_dir <- file.path(cache_dir, files_dir_name)

  # Skip if the _files directory doesn't exist
  if (!dir.exists(source_files_dir)) {
    return(invisible(FALSE))
  }

  # Destination is in the final docs directory
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)
  dest_files_dir <- file.path(dest_dir, files_dir_name)

  # Copy the _files directory
  .dir_move_exact(source_files_dir, dest_files_dir)

  invisible(TRUE)
}

# copy docs - quarto project
# ------------------

.build_copy_docs_quarto_project <- function(output_run) {
  if (!output_run) {
    # no copying required as we build directly to the
    # temporary location, which is final if
    # not in an output run
    return(invisible(FALSE))
  }

  # Get source directory (temporary build location)
  source_dir <- file.path(
    .dir_get_cache_auto_version(profile = NULL), "docs"
  )

  if (!dir.exists(source_dir)) {
    .cli_info("Quarto output directory not found: {source_dir}")
    return(invisible(FALSE))
  }

  # Get destination directory (final docs location)
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)

  # Copy all contents from source to destination, excluding CHANGELOG.md
  .dir_move_exact(source_dir, dest_dir, fn_exc = "CHANGELOG.md")

  .cli_info("Copied quarto project output from {source_dir} to {dest_dir}")
  invisible(TRUE)
}

# copy docs - either
# ------------------

.build_frontmatter_get <- function(path) {
  txt_vec <- readLines(path, warn = FALSE)
  txt_vec <- gsub("\\s+$", "", txt_vec)
  ind_vec_frontmatter <- which(txt_vec == "---")
  # no frontmatter detected
  if (!1 %in% ind_vec_frontmatter || length(ind_vec_frontmatter) < 2) {
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

.build_copy_docs_paths <- function(path, output_run) {
  for (x in path) {
    .build_copy_docs_paths_file(x, output_run)
    .build_copy_docs_paths_dir(x, output_run)
  }
  .build_copy_docs_paths_rm_dir(path)
  invisible(TRUE)
}

.build_copy_docs_paths_file <- function(path, output_run) {
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

.build_copy_docs_paths_dir <- function(path, output_run) {
  if (!file.exists(path) || !fs::is_dir(path)) {
    return(invisible(FALSE))
  }
  path_dir_to <- file.path(
    projr_path_get_dir("docs", safe = !output_run), path
  )
  .dir_move_exact(
    path_dir_from = path,
    path_dir_to = path_dir_to,
    fn_exc = "CHANGELOG.md"
  )
  invisible(TRUE)
}

.build_copy_docs_paths_rm_dir <- function(path) {
  for (x in path) {
    if (file.exists(x) && fs::is_dir(x)) {
      invisible(unlink(x, recursive = TRUE))
    }
  }
  invisible(TRUE)
}
