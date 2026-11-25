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
  previous <- setwd(.path_get())
  on.exit(setwd(previous))
  .cli_debug("Updating .gitignore and .Rbuildignore with projr-managed ignores")
  # ignore directories specified in
  # `_projr.yml`
  .ignore_diryml(create_git = TRUE)
  # root level files
  .ignore_auto_yml()
  .ignore_project_state()
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
  configured <- c(.yml_scripts_get(NULL), .yml_dev_get_scripts(NULL))
  configured <- trimws(configured)
  configured <- configured[nzchar(configured) & !startsWith(configured, "!")]
  .ignore_auto_document_outputs(c(
    .scripts_get_inc_all_qmd(),
    .scripts_get_inc_all_rmd(),
    .scripts_expand_patterns(configured)
  ))
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
  dir_vec <- dir_vec[dir.exists(.path_get(dir_vec))]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_dir_rbuild(dir_vec)
}

.ignore_auto_quarto_git <- function() {
  dir_vec <- c("index_files", ".quarto")
  dir_vec <- dir_vec[dir.exists(.path_get(dir_vec))]
  if (.is_len_0(dir_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_dir_git(dir_vec)
}

.ignore_auto_devcontainer <- function() {
  if (dir.exists(.path_get(".devcontainer"))) {
    .ignore_auto_dir_rbuild(".devcontainer")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.ignore_auto_github <- function() {
  if (dir.exists(.path_get(".github"))) {
    .ignore_auto_dir_rbuild(".github")
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

.ignore_auto_vscode <- function() {
  if (dir.exists(.path_get(".vscode"))) {
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
.ignore_project_state <- function() {
  .ignore_auto_file_git(c(
    "/.projr/manifests/.versions.lock",
    "/.projr/logs/**",
    "/.projr/source/**"
  ))
  if (file.exists(.path_get("DESCRIPTION"))) .ignore_auto_dir_rbuild(".projr")
}

.ignore_auto_yml <- function() {
  configs <- .yml_config_files()
  render_configs <- list.files(
    .path_get(),
    pattern = "^_(quarto|bookdown)\\.yml$"
  )
  .ignore_auto_file_rbuild(c(configs, render_configs))
  .ignore_auto_file_git(configs[basename(configs) == "_projr-local.yml"])
}

.ignore_auto_env <- function() {
  path_vec <- list.files(
    path = .path_get(),
    pattern = "^_environment$|^_environment-[a-zA-Z0-9]+$|^_environment\\.required$|^_environment\\.local$" # nolint
  )
  .ignore_auto_dir_rbuild(path_vec)
  if (file.exists(.path_get("_environment.local"))) {
    .ignore_auto_dir_git("_environment.local")
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

# content files
# -------------------------------------------------------------------------

.ignore_auto_ext <- function() {
  .ignore_auto_ext_rbuild()
  .ignore_auto_ext_git()
}
# The README renderer writes Markdown regardless of the format in its YAML.
.ignore_auto_readme_keep <- function() {
  c("README.md", if (file.exists(.path_get("README.Rmd"))) "README.html")
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
    setdiff(.ignore_auto_readme_keep())
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
        "manifest.csv",
        "CHANGELOG.md",
        "BUILDLOG.md",
        "NEWS.md",
        "CONTRIBUTING.md",
        "LICENSE",
        "LICENSE.md",
        .ignore_auto_readme_keep(),
        "codemeta.json",
        "CITATION.cff"
      )
    )
  .ignore_auto_path_add(path_vec, .path_get(".gitignore"))
}

.ignore_auto_dest_local <- function(archive_local) {
  yml <- .yml_get(NULL)
  titles <- .dest_send_type_get_title("local", archive_local)
  if (!length(titles)) {
    return(invisible(FALSE))
  }
  for (title in titles) {
    .ignore_auto_dest_local_title(.yml_remote_definition("local", title, yml))
  }
  invisible(TRUE)
}

.ignore_auto_dest_local_title <- function(yml) {
  path <- .ignore_project_relative(yml[["path"]] %||% "_archive", .path_get())
  if (!length(path)) {
    return(invisible(FALSE))
  }
  ignore <- yml[["ignore"]]
  if (is.null(ignore) || identical(ignore, "git")) {
    .ignore_auto_path_add(
      .ignore_directory_pattern(path, "git"),
      .path_get(".gitignore")
    )
  }
  if (is.null(ignore) || identical(ignore, "rbuild")) {
    .ignore_auto_path_add(
      .ignore_directory_pattern(path, "rbuild"),
      .path_get(".Rbuildignore")
    )
  }
  invisible(TRUE)
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
  if (!.engine_get_auto() == "bookdown") {
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
  if (!file.exists(.path_get(path_tex))) {
    return(invisible(FALSE))
  }

  paste0(book_filename, ".tex") |> .ignore_auto_file_git()
  paste0(book_filename, ".tex") |> .ignore_auto_file_rbuild()
}

.ignore_auto_build_tex_quarto <- function() {
  if (!.engine_get_auto() == "quarto_project") {
    return(invisible(FALSE))
  }
  if (!file.exists(.path_get("index.tex"))) {
    return(invisible(FALSE))
  }
  .ignore_auto_file_git("index.tex")
  .ignore_auto_file_rbuild("index.tex")
}

.ignore_auto_build_tex_rqmd <- function() {
  path_vec <- list.files(
    path = .path_get(),
    pattern = "\\.qmd$|\\.Rmd|\\.rmd"
  ) |>
    (\(x) gsub("\\.qmd$|\\.Rmd$|\\.rmd$", ".tex", x))()
  if (.is_len_0(path_vec)) {
    return(invisible(FALSE))
  }
  path_vec <- path_vec[file.exists(.path_get(path_vec))]
  if (.is_len_0(path_vec)) {
    return(invisible(FALSE))
  }
  .ignore_auto_file_git(path_vec)
  .ignore_auto_file_rbuild(path_vec)
}

# ========================================================================
# Ignore outputs beside document sources, including nested sources.
# ========================================================================

.ignore_auto_document_outputs <- function(scripts) {
  root <- .path_get()
  scripts <- unique(as.character(fs::path_abs(
    as.character(scripts),
    start = root
  )))
  scripts <- scripts[
    fs::path_has_parent(scripts, root) &
      file.exists(scripts) &
      tolower(tools::file_ext(scripts)) %in% c("qmd", "rmd")
  ]
  if (length(scripts) == 0L) {
    return(invisible(FALSE))
  }

  readme <- file.path(root, "README.Rmd")
  documents <- setdiff(scripts, readme)
  outputs <- as.character(unlist(
    lapply(documents, function(script) {
      frontmatter <- tryCatch(
        .build_frontmatter_get(script),
        error = function(error) list()
      )
      if (tolower(tools::file_ext(script)) == "qmd") {
        .build_copy_docs_quarto_paths_get(frontmatter, script)
      } else {
        format <- .build_copy_docs_rmd_format_get(frontmatter)
        prefix <- .build_copy_docs_rmd_fn_prefix_get(script)
        .build_copy_docs_rmd_path_get(format, prefix)
      }
    }),
    use.names = FALSE
  ))
  files <- outputs[!endsWith(outputs, "_files")]
  stems <- unique(tools::file_path_sans_ext(c(scripts, files)))
  intermediates <- as.vector(outer(
    stems,
    .ignore_auto_document_suffixes(),
    paste0
  ))
  source_stems <- tools::file_path_sans_ext(scripts)
  directories <- c(
    outputs[endsWith(outputs, "_files")],
    paste0(source_stems, "_files"),
    paste0(source_stems, "_cache")
  )
  if (file.exists(readme)) {
    # Keep the publication and linked figures even if another document shares its stem.
    files <- setdiff(files, file.path(root, "README.md"))
    directories <- setdiff(directories, file.path(root, "README_files"))
  }
  quarto <- scripts[tolower(tools::file_ext(scripts)) == "qmd"]
  if (length(quarto) > 0L) {
    directories <- c(
      directories,
      file.path(unique(c(root, dirname(quarto))), ".quarto")
    )
  }

  # Anchor each rule to the project and keep source and final docs paths available.
  files <- fs::path_rel(unique(c(files, intermediates)), start = root)
  directories <- fs::path_rel(unique(directories), start = root)
  .ignore_auto_file_git(c(paste0("/", files), paste0("/", directories, "/")))
  .ignore_auto_file_rbuild(files)
  .ignore_auto_dir_rbuild(directories)
  invisible(TRUE)
}

.ignore_auto_document_suffixes <- function() {
  c(
    ".tex",
    ".knit.md",
    ".utf8.md",
    ".rmarkdown",
    ".log",
    ".aux",
    ".out",
    ".toc",
    ".fls",
    ".fdb_latexmk",
    ".synctex.gz",
    ".bbl",
    ".blg",
    ".nav",
    ".snm",
    ".vrb"
  )
}

.ignore_auto_document_intermediates <- function(directories) {
  root <- .path_get()
  directories <- fs::path_abs(directories, start = root)
  directories <- directories[
    fs::path_has_parent(directories, root) | directories == root
  ]
  suffixes <- utils::glob2rx(paste0("*", .ignore_auto_document_suffixes()))
  files <- list.files(
    directories,
    paste(suffixes, collapse = "|"),
    full.names = TRUE
  )
  files[!dir.exists(files)]
}

.ignore_auto_failed_document_outputs <- function(directories, before) {
  tryCatch(
    {
      files <- setdiff(.ignore_auto_document_intermediates(directories), before)
      paths <- fs::path_rel(files, start = .path_get())
      if (length(paths) > 0L) {
        # Keep discovered filenames across future automatic ignore refreshes.
        projr_ignore_file_git(paste0("/", paths))
        projr_ignore_file_rbuild(paths)
      }
    },
    error = function(error) {
      .cli_debug(
        "Could not ignore failed render intermediates: {conditionMessage(error)}"
      )
    }
  )
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
  ignore <- vapply(
    ignore,
    function(x) {
      if (grepl("/\\*\\*$", x)) {
        x
      } else {
        paste0(x, "/**")
      }
    },
    character(1L)
  )
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
  .ignore_path_write(file_vec, path, refresh_directories = FALSE)
  invisible(TRUE)
}

.ignore_auto_path_add_get_updated <- function(path, ignore, override) {
  ignore_list <- .ignore_path_get_list(path, ignore, override)
  updated_content <- .ignore_auto_path_get_updated_content(
    override,
    ignore,
    ignore_list[["content"]]
  )

  c(
    ignore_list[["start"]],
    updated_content,
    ignore_list[["end"]]
  )
}


# update the content of the specified file
.ignore_auto_path_get_updated_content <- function(override, ignore, content) {
  if (override) {
    ignore |> unique()
  } else {
    # A path can be both a configured directory and a render or archive rule.
    # Keep each contribution when refreshing just the configured directories.
    split_at <- match("", content, nomatch = 0L)
    other <- utils::tail(content, length(content) - split_at)
    c(utils::head(content, split_at), unique(c(other, ignore)))
  }
}
