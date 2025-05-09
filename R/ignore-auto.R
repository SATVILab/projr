#' Update `.gitignore` and `.Rbuildignore` with projr-managed ignores
#'
#' The projr_ignore_auto()` function updates the project’s `.gitignore` and
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
#' projr_ignore_auto()
#' }
#'
#' @export
#' @seealso.ignore_add.ignore_add_git.ignore_add_rbuild
projr_ignore_auto <- function() {
  .ignore_auto(FALSE)
}

.ignore_auto <- function(archive_local) {
  # ignore directories specified in
  # `_projr.yml`
  .ignore_diryml()
  # root level files
  .ignore_auto_yml()
  .ignore_auto_env()
  .ignore_auto_build_source()
  .ignore_auto_build_tex()
  .ignore_auto_ext()
  # directories at root level
  # unspecified by `directories` key
  # in `_projr.yml`
  .ignore_auto_quarto()
  .ignore_auto_devcontainer()
  .ignore_auto_github()
  .ignore_auto_vscode() # also code-workspace files
  .ignore_auto_build_content_dir()
  .ignore_auto_dest_local(archive_local)
  invisible(TRUE)
}

# ===========================================================================
# Ignore specified directories
# ===========================================================================

# ignore _extensions directory if
# projr engine is quarto and _extensions exists
.ignore_auto_quarto <- function() {
  .ignore_auto_quarto_rbuild()
  .ignore_auto_quarto_git()
}

.ignore_auto_quarto_rbuild <- function() {
  dir_vec <- c("_extensions", "index_files", ".quarto")
  dir_vec <- dir_vec[dir.exists(dir_vec)]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_dir_rbuild(dir_vec)
}

.ignore_auto_quarto_git <- function() {
  dir_vec <- c("index_files", ".quarto")
  dir_vec <- dir_vec[dir.exists(dir_vec)]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_dir_git(dir_vec)
}

.ignore_auto_devcontainer <- function() {
  if (dir.exists(".devcontainer")) {
    .ignore_auto_dir_rbuild(".devcontainer")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.ignore_auto_github <- function() {
  if (dir.exists(".github")) {
    .ignore_auto_dir_rbuild(".github")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.ignore_auto_vscode <- function() {
  if (dir.exists(".vscode")) {
    .ignore_auto_dir_rbuild(".vscode")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
  path_vec <- list.files(
    path = .path_get(),
    pattern = "\\.code\\-workspace$"
  )
  .ignore_auto_file_rbuild(path_vec)
}

# ===========================================================================
# Ignore specified classes at root level
# ===========================================================================


# projr config files
# -------------------------------------------------------------------------
.ignore_auto_yml <- function() {
  path_vec <- list.files(
    path = .path_get(),
    pattern = "^_projr\\.yml$|^_projr-.+\\.yml|^_quarto\\.yml$|^_bookdown\\.yml$" # nolint
  )
  .ignore_auto_file_rbuild(path_vec)
  if (file.exists(.path_get("_projr-local.yml"))) {
    .ignore_auto_file_git(.path_get("_projr-local.yml", relative = TRUE))
  }
}

.ignore_auto_env <- function() {
  path_vec <- list.files(
    path = .path_get(),
    pattern = "^_environment$|^_environment-[a-zA-Z0-9]+$|^_environment\\.required$|^_environment\\.local$" # nolint
  )
  .ignore_auto_dir_rbuild(path_vec)
  if (file.exists(.path_get("_environment.local"))) {
    .ignore_auto_dir_git(.path_get("_environment.local"))
  }
}

# projr build files
# -------------------------------------------------------------------------
.ignore_auto_build_source <- function() {
  path_vec <- list.files(
    path = .path_get(),
    pattern = "\\.qmd$|\\.rmd$|\\.Rmd$"
  )
  .ignore_auto_file_rbuild(path_vec)
}

# misc files
# -------------------------------------------------------------------------

.ignore_auto_misc <- function() {
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
    path = .path_get(),
    pattern = pattern
  )
  .ignore_auto_file_rbuild(path_vec)
}

# content files
# -------------------------------------------------------------------------

.ignore_auto_ext <- function() {
  .ignore_auto_ext_rbuild()
  .ignore_auto_ext_git()
}
.ignore_auto_ext_rbuild <- function() {
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
    "\\.nav",
    "\\.snm",
    "\\.vsb",
    "\\.html",
    "\\.docx",
    "\\.csv",
    "\\.tsv",
    "\\.json",
    "\\.md", # Include .md files here to ignore them in Rbuildignore
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
    "\\.txt",
    "\\.rmarkdown"
  ) |>
    paste0(collapse = "|")
  path_vec <- list.files(
    path = .path_get(),
    pattern = pattern
  ) |>
    setdiff("README.md")
  .ignore_auto_file_rbuild(path_vec)
}

.ignore_auto_ext_git <- function() {
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
    "\\.nav",
    "\\.snm",
    "\\.vsb",
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
    "\\.pyc",
    "\\.rmarkdown"
  ) |>
    paste0(collapse = "|")

  # Example usage:
  path_vec <- list.files(
    path = .path_get(),
    pattern = pattern
  ) |>
    setdiff(
      c(
        "manifest.csv", "CHANGELOG.md", "NEWS.md", "CONTRIBUTING.md",
        "LICENSE", "LICENSE.md", "README.md", "codemeta.json",
        "CITATION.cff"
      )
    )
  .ignore_auto_path_add(path_vec, .path_get(".gitignore"))
}

.ignore_auto_dest_local <- function(archive_local) {
  yml <- .yml_dest_get_type("local", NULL)
  if (archive_local && !"archive" %in% names(yml)) {
    .ignore_auto_dir("_archive")
  }
  if (is.null(yml)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(yml)) {
    .ignore_auto_dest_local_title(yml[[i]])
  }
  invisible(TRUE)
}

.ignore_auto_dest_local_title <- function(yml) {
  path <- yml[["path"]] %||% "_archive"
  ignore <- yml[["ignore"]]
  if (is.null(ignore)) {
    .ignore_auto_dir(path)
  } else if (ignore == "git") {
    .ignore_auto_dir_git(path)
  } else if (ignore == "rbuild") {
    .ignore_auto_dir_rbuild(path)
  }
}

# ===========================================================================
# Ignore tex files based on build files
# ===========================================================================

.ignore_auto_build_tex <- function() {
  .ignore_auto_build_tex_bookdown()
  .ignore_auto_build_tex_quarto()
  .ignore_auto_build_tex_rqmd()
}

.ignore_auto_build_tex_bookdown <- function() {
  if (!.engine_get() == "bookdown") {
    return(invisible(FALSE))
  }
  yml_bd <- .yml_bd_get()
  # Get the book_filename
  book_filename <- yml_bd$book_filename

  # Default to "_main" if book_filename is not specified
  if (is.null(book_filename)) {
    book_filename <- "_main"
  }
  path_tex <- paste0(book_filename, ".tex")
  if (!file.exists(path_tex)) {
    return(invisible(FALSE))
  }

  paste0(book_filename, ".tex") |> .ignore_auto_file_git()
  paste0(book_filename, ".tex") |> .ignore_auto_file_rbuild()
}

.ignore_auto_build_tex_quarto <- function() {
  if (!.engine_get() == "quarto_project") {
    return(invisible(FALSE))
  }
  if (!file.exists("index.tex")) {
    return(invisible(FALSE))
  }
  .ignore_auto_file_git("index.tex")
  .ignore_auto_file_rbuild("index.tex")
}

.ignore_auto_build_tex_rqmd <- function() {
  path_vec <- list.files(
    path = .path_get(), pattern = "\\.qmd$|\\.Rmd|\\.rmd"
  ) |>
    gsub("\\.qmd$|\\.Rmd$|\\.rmd$", ".tex", x = _)
  if (.is_len_0(path_vec)) {
    return(invisible(FALSE))
  }
  path_vec <- path_vec[file.exists(path_vec)]
  if (.is_len_0(path_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_file_git(path_vec)
  .ignore_auto_file_rbuild(path_vec)
}

# ========================================================================
# Ignore temporary directories based on build files
# ========================================================================

.ignore_auto_build_content_dir <- function() {
  switch(.engine_get(),
    # bookdown and quarto_project handled by directories key
    "bookdown" = NULL,
    "quarto_project" = NULL,
    # only supporting quarto_document,
    # as rmd's by default are self contained
    "rmd" = NULL,
    "quarto_document" = .ignore_auto_build_quarto()
  )
}

# ignore - qmd output directories
# ------------------

.ignore_auto_build_quarto <- function() {
  fn_vec_qmd <- list.files(pattern = "\\.qmd$")
  path_vec_dir <- NULL
  for (fn in fn_vec_qmd) {
    path_vec_dir <- c(
      path_vec_dir,
      .ignore_auto_build_quarto_ind(fn)
    )
  }
  path_vec_dir <- unique(path_vec_dir)
  .ignore_auto_dir_git(path_vec_dir)
  .ignore_auto_dir_rbuild(path_vec_dir)
  invisible(TRUE)
}

.ignore_auto_build_quarto_ind <- function(fn) {
  frontmatter_vec <- .build_frontmatter_get(fn)
  format <- .build_copy_docs_quarto_format_get(frontmatter_vec)
  fn_output_prefix <- .build_copy_docs_quarto_fn_prefix_get(
    frontmatter_vec, fn
  )
  .build_copy_docs_quarto_path_get(format, fn_output_prefix)
}


# ===========================================================================
# Ignore specified files/directories from specified paths
# ===========================================================================

.ignore_auto_file <- function(ignore) {
  .ignore_auto_file_git(ignore)
  .ignore_auto_file_rbuild(ignore)
  invisible(TRUE)
}

.ignore_auto_file_git <- function(ignore) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  .ignore_auto_path_add(ignore, .path_get(".gitignore"))
}

.ignore_auto_dir <- function(ignore) {
  .ignore_auto_dir_git(ignore)
  .ignore_auto_dir_rbuild(ignore)
  invisible(TRUE)
}

.ignore_auto_dir_git <- function(ignore) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore <- vapply(ignore, function(x) {
    if (grepl("/\\*\\*$", x)) {
      x
    } else {
      paste0(x, "/**")
    }
  }, character(1L))
  .ignore_auto_path_add(ignore, .path_get(".gitignore"))
}

.ignore_auto_file_rbuild <- function(ignore) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore <- gsub("/+$", "", ignore) |>
    trimws() |>
    utils::glob2rx()
  .ignore_auto_path_add(ignore, .path_get(".Rbuildignore"))
}

.ignore_auto_dir_rbuild <- function(ignore) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  # Remove trailing slashes and trim whitespace
  ignore <- gsub("/+$", "", ignore)
  ignore <- trimws(ignore)

  # Convert the glob pattern to a regular expression pattern
  patterns <- utils::glob2rx(ignore)

  # Handle directory-specific patterns
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- lapply(seq_along(patterns), function(i) {
    c(patterns[i], utils::glob2rx(ignore[i]))
  }) |>
    unlist()

  # Add the patterns to the .Rbuildignore file
  .ignore_auto_path_add(patterns, .path_get(".Rbuildignore"))
}

# ===========================================================================
# Ignore specified lines from .gitignore/.Rbuildignore
# ===========================================================================

.ignore_auto_path_add <- function(ignore, path) {
  # ensure that certain lines are present
  # in the text file at the specified path
  # (e.g., .gitignore, .Rbuildignore)

  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }

  file_vec <- .ignore_auto_path_add_get_updated(path, ignore, FALSE)
  .ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.ignore_auto_path_add_get_updated <- function(path, ignore, override) {
  ignore_list <- .ignore_path_get_list(path, ignore, override)
  updated_content <- .ignore_auto_path_get_updated_content(
    override, ignore, ignore_list[["content"]]
  )

  c(
    ignore_list[["start"]],
    updated_content,
    ignore_list[["end"]]
  )
}



# update the content of the specified file
.ignore_auto_path_get_updated_content <- function(override,
                                                  ignore,
                                                  content) {
  if (override) {
    ignore |> unique()
  } else {
    c(content, ignore) |> unique()
  }
}
