#' Update `.gitignore` and `.Rbuildignore` with projr-managed ignores
#'
#' The `projr_ignore_auto()` function updates the project’s `.gitignore` and
#' `.Rbuildignore` files to reflect directories and files managed by
#' projr, as well as
#' other directories and files that should
#' clearly be ignored. 
#'
#' @return Called primarily for its side effects (modifying
#'   `.gitignore` and/or `.Rbuildignore`). Returns `TRUE` invisibly.
#'
#' @examples
#' \dontrun{
#'   projr_ignore_auto()
#' }
#'
#' @export
#' @seealso projr_ignore_add,projr_ignore_add_git,projr_ignore_add_rbuild
projr_ignore_auto <- function() {
  # ignore directories specified in
  # `_projr.yml`
  .projr_ignore_auto_diryml()
  # root level files
  .projr_ignore_auto_yml()
  .projr_ignore_auto_build_source()
  .projr_ignore_auto_ext()
  # directories at root level
  # unspecified by `directories` key
  # in `_projr.yml`
  .projr_ignore_auto_quarto()
  .projr_ignore_auto_devcontainer()
  .projr_ignore_auto_github()
  .projr_ignore_auto_vscode() # also code-workspace files
  .projr_ignore_auto_build_content_dir()
  invisible(TRUE)
}

# ===========================================================================
# Ignore specified directories
# ===========================================================================

# ignore _extensions directory if 
# projr engine is quarto and _extensions exists
.projr_ignore_auto_quarto <- function() {
  .projr_ignore_auto_quarto_rbuild()
  .projr_ignore_auto_quarto_git()
}

.projr_ignore_auto_quarto_rbuild <- function() {
  dir_vec <- c("_extensions", "index_files", ".quarto")
  dir_vec <- dir_vec[dir.exists(dir_vec)]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .projr_ignore_auto_dir_rbuild(dir_vec)
}

.projr_ignore_auto_quarto_git <- function() {
  dir_vec <- c("index_files", ".quarto")
  dir_vec <- dir_vec[dir.exists(dir_vec)]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .projr_ignore_auto_dir_git(dir_vec)
}

.projr_ignore_auto_devcontainer <- function() {
  if (dir.exists(".devcontainer")) {
    .projr_ignore_auto_dir_rbuild(".devcontainer")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.projr_ignore_auto_github <- function() {
  if (dir.exists(".github")) {
    .projr_ignore_auto_dir_rbuild(".github")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.projr_ignore_auto_vscode <- function() {
  if (dir.exists(".vscode")) {
    .projr_ignore_auto_dir_rbuild(".vscode")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = "\\.code\\-workspace$"
  )
  .projr_ignore_auto_file_rbuild(path_vec)
}

# ===========================================================================
# Ignore specified classes at root level
# ===========================================================================


# projr config files
# -------------------------------------------------------------------------
.projr_ignore_auto_yml <- function() {
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = "^_projr\\.yml$|^_projr-.+\\.yml|^_quarto\\.yml$|^_bookdown\\.yml$"
  )
  .projr_ignore_auto_file_rbuild(path_vec)
}

# projr build files
# -------------------------------------------------------------------------
.projr_ignore_auto_build_source <- function() {
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = "\\.qmd$|\\.rmd$|\\.Rmd$"
  )
  .projr_ignore_auto_file_rbuild(path_vec)
}

# misc files
# -------------------------------------------------------------------------

.projr_ignore_auto_misc <- function() {
  pattern <- c(
    "\\.Rproj$",
    "\\.Rproj\\.user$",
    "CHANGELOG\\.md$",
    "CONTRIBUTING\\.md$",
    "LICENSE$",
    "LICENSE\\.md$",
    "README\\.Rmd$"
  ) |>
    paste0(collapse = "|")
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = pattern
  )
  .projr_ignore_auto_file_rbuild(path_vec)
}

# content files
# -------------------------------------------------------------------------

.projr_ignore_auto_ext <- function() {
  .projr_ignore_auto_ext_rbuild()
  .projr_ignore_auto_ext_git()
}
.projr_ignore_auto_ext_rbuild <- function() {
  pattern <- c(
    "\\.log",
    "\\.aux",
    "\\.out",
    "\\.toc",
    "\\.fls",
    "\\.fdb_latexmk",
    "\\.synctex\\.gz",
    "\\.bbl",
    "\\.blg",
    "\\.rds",
    "\\.RData",
    "\\.pdf",
    "\\.html",
    "\\.docx",
    "\\.csv",
    "\\.tsv",
    "\\.json",
    "\\.md",          # Include .md files here to ignore them in Rbuildignore
    "\\.knit\\.md",
    "\\.utf8\\.md",
    "\\.tmp",
    "\\.swp",
    "\\.bak",
    "\\.~",
    "\\.Rproj\\.user/",
    "\\.Rhistory",
    "\\.DS_Store",
    "__pycache__/",
    "\\.pyc",
    "\\.code-workspace$",
    "\\.R",
    "\\.py",
    "\\.sh",
    "\\.yaml",
    "\\.yml",
    "\\.toml",
    "\\.json",
    "\\.txt"
  ) |>
    paste0(collapse = "|")
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = pattern
  ) |>
    setdiff("README.md")
  .projr_ignore_auto_file_rbuild(path_vec)
}

.projr_ignore_auto_ext_git <- function() {
  pattern <- c(
    "\\.log",
    "\\.aux",
    "\\.out",
    "\\.toc",
    "\\.fls",
    "\\.fdb_latexmk",
    "\\.synctex\\.gz",
    "\\.bbl",
    "\\.blg",
    "\\.rds",
    "\\.RData",
    "\\.pdf",
    "\\.html",
    "\\.docx",
    "\\.csv",
    "\\.tsv",
    "\\.json",
    "\\.md",
    "\\.knit\\.md",
    "\\.utf8\\.md",
    "\\.tmp",
    "\\.swp",
    "\\.bak",
    "\\.~",
    "\\.Rproj\\.user/",
    "\\.Rhistory",
    "\\.DS_Store",
    "__pycache__/",
    "\\.pyc"
  ) |>
    paste0(collapse = "|")

  # Example usage:
  path_vec <- list.files(
    path = .dir_proj_get(),
    pattern = pattern
  ) |>
    setdiff(
      c("manifest.csv", "CHANGELOG.md", "NEWS.md", "CONTRIBUTING.md",
        "LICENSE", "LICENSE.md", "README.md")
    )
  .projr_ignore_auto_path_add(path_vec, .dir_proj_get(".gitignore"))
}

# ===========================================================================
# Ignore specified files/directories from specified paths
# ===========================================================================

.projr_ignore_auto_file_git <- function(ignore) {
  .projr_ignore_auto_path_add(ignore, .dir_proj_get(".gitignore"))
}

.projr_ignore_auto_dir_git <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore <- paste0(ignore, "/**")
  .projr_ignore_auto_path_add(ignore, .dir_proj_get(".gitignore"))
}

.projr_ignore_auto_file_rbuild <- function(ignore) {
  ignore <- gsub("/+$", "", ignore) |>
    trimws() |>
    utils::glob2rx()
  .projr_ignore_auto_path_add(ignore, .dir_proj_get(".Rbuildignore"))
}

.projr_ignore_auto_dir_rbuild <- function(ignore) {
  # Remove trailing slashes and trim whitespace
  ignore <- gsub("/+$", "", ignore)
  ignore <- trimws(ignore)

  # Convert the glob pattern to a regular expression pattern
  patterns <- utils::glob2rx(ignore)

  # Handle directory-specific patterns
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- c(patterns, utils::glob2rx(ignore))

  # Add the patterns to the .Rbuildignore file
  .projr_ignore_auto_path_add(patterns, .dir_proj_get(".Rbuildignore"))
}

# ===========================================================================
# Ignore specified lines from .gitignore/.Rbuildignore
# ===========================================================================

.projr_ignore_auto_path_add <- function(ignore, path) {
  # ensure that certain lines are present 
  # in the text file at the specified path
  # (e.g., .gitignore, .Rbuildignore)

  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }

  file_vec <- .projr_ignore_auto_path_add_get_updated(path, ignore, FALSE)
  .projr_ignore_auto_path_write(file_vec, path)
  invisible(TRUE)
}

.projr_ignore_auto_path_add_get_updated <- function(path, ignore, override) {
  ignore_list <- .projr_ignore_auto_path_get_list(path, ignore, override)
  updated_content <- .projr_ignore_auto_path_get_updated_content(
    override, ignore, ignore_list[["content"]]
  )

  c(
    ignore_list[["start"]],
    updated_content,
    ignore_list[["end"]]
  )
}

# break the specified file into three parts: the top, the projr-managed
# section, and the bottom. The projr-managed section is the content between
# the "Start of projr section" and "End of projr section" comments.
.projr_ignore_auto_path_get_list <- function(path, ignore, override) {
  file_vec <- .projr_ignore_auto_path_read(path)
  if (length(file_vec) == 0) {
    return(.projr_ignore_auto_get_path_list_empty())
  }

  ind_vec <- .projr_ignore_auto_path_get_ind(file_vec, path)

  start_end_list <- .projr_ignore_auto_path_get_startend(
    ind_vec[["top"]], ind_vec[["bot"]], file_vec
  )

  content <- .projr_ignore_auto_path_get_content(
    ind_vec[["top"]], ind_vec[["bot"]], file_vec
  )

  list(
    start = start_end_list[["start"]],
    content = content,
    end = start_end_list[["end"]]
  )
}

# read the content of the specified file
.projr_ignore_auto_path_read <- function(path) {
  if (!file.exists(path)) {
    return(character(0))
  }
  suppressWarnings(readLines(path))
}

# return an empty list
.projr_ignore_auto_get_path_list_empty <- function() {
  list(start = character(0), content = character(0), end = character(0))
}

# get the indices of the projr-managed section from the specified file
.projr_ignore_auto_path_get_ind <- function(file_vec, path) {

  
  .projr_ignore_auto_path_get_check(
    match_str_top, match_str_bottom, file_vec, path
  )

  ind_top <- which(grepl(match_str_top, file_vec))
  ind_bot <- which(grepl(match_str_bottom, file_vec))
  c("top" = ind_top, "bot" = ind_bot)
}

# check that the projr-managed section in the specified file is well-formed
.projr_ignore_auto_path_get_check <- function(match_str_top,
                                         match_str_bottom,
                                         file_vec,
                                         path) {
  # Validate that the projr-managed section in .gitignore is well-formed                                                 
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, file_vec))
  projr_ignore_ind_top <- which(grepl(match_str_top, file_vec))
  
  if (length(projr_ignore_ind_top) > 1 ||
        length(projr_ignore_ind_bot) > 1) {
    stop(paste0(
      "Multiple projr sections found in ", basename(path)
    ))
  }
  
  found_top <- length(projr_ignore_ind_top) == 1
  found_bottom <- length(projr_ignore_ind_bot) == 1
  
  if (found_top && !found_bottom) {
    stop(paste0(
      "Found start of projr section but not end in ", basename(path)
    ))
  }
  
  if (!found_top && found_bottom) {
    stop(paste0(
      "Found end of projr section but not start in ", basename(path)
    ))
  }
  
  if (found_top && found_bottom) {
    if (projr_ignore_ind_top > projr_ignore_ind_bot) {
      stop(paste0(
        "Start of projr section found after end in ", basename(path)
      ))
    }
  }
  
  invisible(TRUE)
}

# get lines before and after the projr-managed section
.projr_ignore_auto_path_get_startend <- function(ind_top, ind_bot, file_vec) {
  if (length(ind_top) == 0L) {
    start <- c(
      file_vec,
      "# Start of projr section: do not edit by hand (update with projr_ignore_auto())"
    )
    end <- "# End of projr section"
  } else {
    start <- file_vec[seq_len(ind_top)]
    end <- file_vec[seq(ind_bot, length(file_vec))]
  }
  list(start = start, end = end)
}

# get lines in the projr-managed section
.projr_ignore_auto_path_get_content <- function(ind_top, ind_bot, file_vec) {
  if (length(ind_top) == 0L) {
    character(0)
  } else if (ind_top == ind_bot) {
    character(0)
  } else {
    file_vec[seq(ind_top + 1, ind_bot - 1)]
  }
}

# update the content of the specified file
.projr_ignore_auto_path_get_updated_content <- function(override,
                                                   ignore,
                                                   content) {
  if (override) {
    ignore |> unique()
  } else {
    c(content, ignore) |> unique()
  }
}

# write to the specified file
.projr_ignore_auto_path_write <- function(file_vec, path) {
  writeLines(file_vec, path)
  .projr_newline_append(path)
  invisible(path)
}


# ========================================================================
# Ignore temporary directories based on build files
# ========================================================================

.projr_ignore_auto_build_content_dir <- function() {
  switch(.projr_engine_get(),
    # bookdown and quarto_project handled by directories key
    "bookdown" = NULL,
    "quarto_project" = NULL,
    # only supporting quarto_document,
    # as rmd's by default are self contained
    "rmd" = NULL, 
    "quarto_document" = .projr_ignore_auto_build_quarto()
  )
}

# ignore - qmd output directories
# ------------------

.projr_ignore_auto_build_quarto <- function() {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  path_vec_dir <- NULL
  for (fn in fn_vec_qmd) {
    path_vec_dir <- c(
      path_vec_dir,
      .projr_ignore_auto_build_quarto_ind(fn)
    )
  }
  path_vec_dir <- unique(path_vec_dir)
  .projr_ignore_auto_dir_git(path_vec_dir)
  .projr_ignore_auto_dir_rbuild(path_vec_dir)
  invisible(TRUE)
}

.projr_ignore_auto_build_quarto_ind <- function(fn) {
  frontmatter_vec <- .projr_build_frontmatter_get(fn)
  format <- .projr_build_copy_docs_quarto_format_get(frontmatter_vec)
  fn_output_prefix <- .projr_build_copy_docs_quarto_fn_prefix_get(
    frontmatter_vec, fn
  )
  .projr_build_copy_docs_quarto_path_get(format, fn_output_prefix)
}

match_str_top <- "^# Start of projr section: do not edit by hand \\(update with projr_ignore_auto\\(\\))|^# Start of projr section: do not edit by hand \\(update with projr_ignore\\(\\))|^# Start of projr section: do not edit by hand \\(update with projr_dir_ignore\\(\\))"
match_str_bottom <- "^# End of projr section"