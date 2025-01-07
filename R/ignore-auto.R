#' Update `.gitignore` and `.Rbuildignore` with projr-managed ignores
#'
#' The `projr_ignore_auto()` function updates the project’s `.gitignore` and
#' `.Rbuildignore` files to reflect directories and files managed by
#' projr, as well as
#' other directories and files that should
#' clearly be ignored. 
#' They are kept up-to-date with the project's configuration,
#' and are written within a demarcated section of the file.
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
  .projr_ignore_diryml()
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
  ignore <- if (grepl("/\\*\\*$", ignore)) ignore else paste0(ignore, "/**")
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
  .projr_ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.projr_ignore_auto_path_add_get_updated <- function(path, ignore, override) {
  ignore_list <- .projr_ignore_path_get_list(path, ignore, override)
  updated_content <- .projr_ignore_auto_path_get_updated_content(
    override, ignore, ignore_list[["content"]]
  )

  c(
    ignore_list[["start"]],
    updated_content,
    ignore_list[["end"]]
  )
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

