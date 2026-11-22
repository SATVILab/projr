.build_copy_docs <- function(output_run, engines = .engines_get()) {
  .cli_debug("Starting .build_copy_docs()")
  .cli_debug("  output_run: {output_run}")

  # Move project output before the standalone documents sharing its destination.
  if ("bookdown" %in% names(engines)) {
    .build_copy_docs_bookdown(output_run)
  }
  if ("quarto_project" %in% names(engines)) {
    .build_copy_docs_quarto_project(output_run)
  }

  for (index in seq_along(engines)) {
    switch(names(engines)[[index]],
      "document_output" = .build_copy_docs_paths(engines[[index]], output_run),
      "quarto_document" = .build_copy_docs_quarto(output_run, engines[[index]]),
      "rmd" = .build_copy_docs_rmd(output_run, engines[[index]])
    )
  }
  .cli_debug("Finished .build_copy_docs()")
}

# copy docs - rmd
# ------------------

.build_copy_docs_rmd <- function(output_run, scripts = NULL) {
  .cli_debug("Starting .build_copy_docs_rmd()")
  .cli_debug("  output_run: {output_run}")

  fn_vec_rmd <- if (is.null(scripts)) {
    list.files(pattern = "\\.rmd$", ignore.case = TRUE)
  } else {
    grep("\\.rmd$", scripts, ignore.case = TRUE, value = TRUE)
  }
  .cli_debug("  Found {length(fn_vec_rmd)} Rmd files: {paste(fn_vec_rmd, collapse = ', ')}")

  for (fn in fn_vec_rmd) {
    .cli_debug("  Processing Rmd file: {fn}")
    .build_copy_docs_rmd_ind(fn, output_run)
    .cli_debug("  Completed processing: {fn}")
  }

  .cli_debug("Finished .build_copy_docs_rmd()")
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
  sub("\\.rmd$", "", fn, ignore.case = TRUE)
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

.build_copy_docs_quarto <- function(output_run, scripts = NULL) {
  .cli_debug("Starting .build_copy_docs_quarto()")
  .cli_debug("  output_run: {output_run}")

  fn_vec_qmd <- if (is.null(scripts)) {
    list.files(pattern = "\\.qmd$", ignore.case = TRUE)
  } else {
    grep("\\.qmd$", scripts, ignore.case = TRUE, value = TRUE)
  }
  .cli_debug("  Found {length(fn_vec_qmd)} qmd files: {paste(fn_vec_qmd, collapse = ', ')}")

  for (fn in fn_vec_qmd) {
    .cli_debug("  Processing qmd file: {fn}")
    .build_copy_docs_quarto_ind(fn, output_run)
    .cli_debug("  Completed processing: {fn}")
  }

  .cli_debug("Finished .build_copy_docs_quarto()")
  invisible(TRUE)
}

.build_copy_docs_quarto_ind <- function(fn, output_run) {
  frontmatter_vec <- .build_frontmatter_get(fn)
  .build_copy_docs_paths(.build_copy_docs_quarto_paths_get(frontmatter_vec, fn), output_run)
  invisible(TRUE)
}

.build_copy_docs_quarto_paths_get <- function(frontmatter, script, args_engine = list()) {
  formats <- names(frontmatter[["format"]]) %||% .build_copy_docs_quarto_format_get(frontmatter)
  requested_format <- args_engine[["output_format"]]
  if (!is.null(requested_format) && !identical(requested_format, "all")) {
    formats <- requested_format
  }
  paths <- lapply(formats, function(format) {
    options <- frontmatter[["format"]]
    if (is.list(options) && is.list(options[[format]])) {
      frontmatter[["output-file"]] <- options[[format]][["output-file"]] %||%
        frontmatter[["output-file"]]
    }
    frontmatter[["output-file"]] <- args_engine[["output_file"]] %||% frontmatter[["output-file"]]
    prefix <- .build_copy_docs_quarto_fn_prefix_get(frontmatter, script)
    .build_copy_docs_quarto_path_get(format, prefix)
  })
  files <- unlist(paths, use.names = FALSE)
  files <- files[!endsWith(files, "_files")]
  unique(c(paste0(tools::file_path_sans_ext(script), "_files"), files))
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
  extension <- paste0(".", fn_suffix)
  has_extension <- endsWith(fn_prefix, extension)
  fn <- if (has_extension) fn_prefix else paste0(fn_prefix, extension)
  stem <- if (has_extension) tools::file_path_sans_ext(fn_prefix) else fn_prefix
  c(paste0(stem, "_files"), fn)
}

.build_copy_docs_quarto_is_html_format <- function(format) {
  html_formats <- c(
    "html",
    "revealjs",
    "dashboard",
    "s5",
    "slidy",
    "dzslides"
  )
  format %in% html_formats
}

.build_copy_docs_quarto_fn_prefix_get <- function(frontmatter, fn) {
  if (!"output-file" %in% names(frontmatter)) {
    return(sub("\\.qmd$", "", fn, ignore.case = TRUE))
  }
  output_file <- sub("\\.qmd$", "", frontmatter[["output-file"]], ignore.case = TRUE)
  if (dirname(fn) == ".") output_file else file.path(dirname(fn), output_file)
}


.build_copy_docs_quarto_fn_suffix_get <- function(format) {
  switch(format,
    "revealjs" = "html",
    "beamer" = "pdf",
    "typst" = "pdf",
    "dashboard" = "html",
    "s5" = "html",
    "slidy" = "html",
    "dzslides" = "html",
    format
  )
}

# copy docs - bookdown
# ------------------

.build_copy_docs_bookdown <- function(output_run) {
  .cli_debug("Starting .build_copy_docs_bookdown()")
  .cli_debug("  output_run: {output_run}")

  if (!output_run) {
    # no copying required as we build directly to the
    # temporary location, which is final if
    # not in an output run
    .cli_debug("  Skipping copy: not an output run (built directly to temp location)")
    .cli_debug("Finished .build_copy_docs_bookdown() - early return")
    return(invisible(FALSE))
  }

  # Get source directory (temporary build location in cache)
  source_dir <- file.path(
    .dir_get_cache_auto_version(profile = NULL),
    .dir_get_docs_bookdown() # Gets the bookdown output directory name
  )
  .cli_debug("  Source dir: {source_dir}")

  if (!dir.exists(source_dir)) {
    .cli_info("Bookdown output directory not found: {source_dir}")
    .cli_debug("Finished .build_copy_docs_bookdown() - source not found")
    return(invisible(FALSE))
  }

  # Get destination directory (final docs location)
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)
  .cli_debug("  Dest dir: {dest_dir}")

  # Copy all contents from source to destination
  .cli_debug("  Calling .dir_move_exact() from {source_dir} to {dest_dir}")
  .dir_move_exact(source_dir, dest_dir)
  .cli_debug("  Completed .dir_move_exact()")

  # Copy the <book_filename>_files directory if it exists
  # This contains knitr cache files (figures, etc.) from the working directory
  .cli_debug("  Calling .build_copy_docs_bookdown_files()")
  .build_copy_docs_bookdown_files(output_run)
  .cli_debug("  Completed .build_copy_docs_bookdown_files()")

  .cli_info("Copied bookdown output from {source_dir} to {dest_dir}")
  .cli_debug("Finished .build_copy_docs_bookdown()")
  invisible(TRUE)
}

.build_copy_docs_bookdown_files <- function(output_run) {
  .cli_debug("Starting .build_copy_docs_bookdown_files()")
  .cli_debug("  output_run: {output_run}")

  # Get the book filename from _bookdown.yml
  book_filename <- .yml_bd_get_book_filename()
  files_dir_name <- paste0(book_filename, "_files")
  .cli_debug("  Book filename: {book_filename}")
  .cli_debug("  Files dir name: {files_dir_name}")

  # Source is in the cache build directory (working directory during build)
  cache_dir <- .dir_get_cache_auto_version(profile = NULL)
  source_files_dir <- file.path(cache_dir, files_dir_name)
  .cli_debug("  Source files dir: {source_files_dir}")

  # Skip if the _files directory doesn't exist
  if (!dir.exists(source_files_dir)) {
    .cli_debug("  Files directory does not exist, skipping")
    .cli_debug("Finished .build_copy_docs_bookdown_files() - no files dir")
    return(invisible(FALSE))
  }

  # Destination is in the final docs directory
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)
  dest_files_dir <- file.path(dest_dir, files_dir_name)
  .cli_debug("  Dest files dir: {dest_files_dir}")

  # Copy the _files directory
  .cli_debug("  Calling .dir_move_exact() from {source_files_dir} to {dest_files_dir}")
  .dir_move_exact(source_files_dir, dest_files_dir)
  .cli_debug("  Completed .dir_move_exact()")

  .cli_debug("Finished .build_copy_docs_bookdown_files()")
  invisible(TRUE)
}

# copy docs - quarto project
# ------------------

.build_copy_docs_quarto_project <- function(output_run) {
  .cli_debug("Starting .build_copy_docs_quarto_project()")
  .cli_debug("  output_run: {output_run}")

  if (!output_run) {
    # no copying required as we build directly to the
    # temporary location, which is final if
    # not in an output run
    .cli_debug("  Skipping copy: not an output run (built directly to temp location)")
    .cli_debug("Finished .build_copy_docs_quarto_project() - early return")
    return(invisible(FALSE))
  }

  # Get source directory (temporary build location)
  source_dir <- .dir_get_label_safe_path("docs")
  .cli_debug("  Source dir: {source_dir}")

  if (!dir.exists(source_dir)) {
    .cli_info("Quarto output directory not found: {source_dir}")
    .cli_debug("Finished .build_copy_docs_quarto_project() - source not found")
    return(invisible(FALSE))
  }

  # Get destination directory (final docs location)
  dest_dir <- projr_path_get_dir("docs", safe = !output_run)
  .cli_debug("  Dest dir: {dest_dir}")

  # Copy all contents from source to destination
  .cli_debug("  Calling .dir_move_exact() from {source_dir} to {dest_dir}")
  .dir_move_exact(source_dir, dest_dir)
  .cli_debug("  Completed .dir_move_exact()")

  .cli_info("Copied quarto project output from {source_dir} to {dest_dir}")
  .cli_debug("Finished .build_copy_docs_quarto_project()")
  invisible(TRUE)
}

# copy docs - either
# ------------------

.build_frontmatter_get <- function(path) {
  txt_vec <- readLines(path, warn = FALSE)
  txt_vec <- gsub("\\s+$", "", txt_vec)
  ind_vec_frontmatter <- which(txt_vec == "---")
  # No frontmatter, or an empty frontmatter block.
  if (!1 %in% ind_vec_frontmatter || length(ind_vec_frontmatter) < 2 ||
    ind_vec_frontmatter[2L] == 2L) {
    return(list())
  }
  # frontmatter detected
  txt_vec_frontmatter <- txt_vec[
    seq(ind_vec_frontmatter[1] + 1, ind_vec_frontmatter[2] - 1)
  ]
  yaml::yaml.load(paste(txt_vec_frontmatter, collapse = "\n")) %||% list()
}

.build_copy_docs_paths <- function(path, output_run) {
  .cli_debug("  Starting .build_copy_docs_paths()")
  .cli_debug("    Paths to copy: {paste(path, collapse = ', ')}")
  .cli_debug("    output_run: {output_run}")

  for (x in path) {
    .cli_debug("    Processing path: {x}")
    .build_copy_docs_paths_file(x, output_run)
    .build_copy_docs_paths_dir(x, output_run)
    .cli_debug("    Completed processing: {x}")
  }

  .cli_debug("  Finished .build_copy_docs_paths()")
  invisible(TRUE)
}

.build_copy_docs_paths_file <- function(path, output_run) {
  if (!file.exists(path) || !fs::is_file(path)) {
    .cli_debug("      Skipping {path}: not a file or doesn't exist")
    return(invisible(FALSE))
  }

  file_to <- projr_path_get("docs", basename(path), safe = !output_run)
  if (fs::path_abs(path) == fs::path_abs(file_to)) {
    return(invisible(TRUE))
  }
  .cli_debug("      Copying file {path} to {file_to}")

  if (file.exists(file_to)) {
    .cli_debug("        Removing existing file: {file_to}")
    invisible(file.remove(file_to))
  }

  fs::file_move(path, file_to)
  .cli_debug("      Completed copying file")
  invisible(TRUE)
}

.build_copy_docs_paths_dir <- function(path, output_run) {
  if (!file.exists(path) || !fs::is_dir(path)) {
    .cli_debug("      Skipping {path}: not a directory or doesn't exist")
    return(invisible(FALSE))
  }

  path_dir_to <- file.path(
    projr_path_get_dir("docs", safe = !output_run), basename(path)
  )
  if (fs::path_abs(path) == fs::path_abs(path_dir_to)) {
    return(invisible(TRUE))
  }
  .cli_debug("      Copying directory {path} to {path_dir_to}")

  .dir_move_exact(
    path_dir_from = path,
    path_dir_to = path_dir_to,
    fn_exc = "CHANGELOG.md"
  )
  .build_copy_docs_paths_rm_dir(path)
  .cli_debug("      Completed copying directory")
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
