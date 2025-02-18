# taken from rstudio/renv (which
# presumably took it from elsewhere or
# Hadley Wickham's brain directly)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

`%@@%` <- function(x, y) {
  tryCatch(x, error = function(e) y)
}

.try_err_msg_get <- function(x, require_try_error = TRUE) {
  if (!inherits(x, "try-error")) {
    if (require_try_error) {
      stop("x must be a try-error object")
    } else {
      return(NULL)
    }
  }
  gsub("^Error in .* \\: \\n\\s*", "", x[1]) |>
    gsub(pattern = "\\n\\s*$", replacement = "")
}

if (!requireNamespace("piggyback", quietly = TRUE)) {
}

par_nm_vec <- c("parameters", "parameter", "param", "params", "par", "pars")

# ------------
# dependencies
# ------------

.dep_install <- function(dep) {
  for (x in dep) {
    .dep_add(x)
    if (!requireNamespace(x, quietly = TRUE)) {
      .dep_install_only(dep)
    }
  }
}

.dep_add <- function(dep) {
  # don't add to _dependencies
  # if renv already picks it up as a dependency
  if (!.renv_detect()) {
    return(invisible(FALSE))
  }
  if (.dep_in_renv(basename(dep))) {
    return(invisible(FALSE))
  }

  path_dep <- .path_get("_dependencies.R")
  dep_vec <- readLines(path_dep)
  for (i in seq_along(dep)) {
    dep_pattern <- paste0(
      "library\\(", basename(dep[[i]]), "\\)",
      collapse = ""
    )
    dep_txt <- paste0("library(", basename(dep[[i]]), ")", collapse = "")
    if (!any(grepl(dep_pattern, dep_vec))) {
      dep_vec <- c(dep_vec, dep_txt)
    }
  }
  writeLines(dep_vec, path_dep)
  .newline_append(path_dep)
  invisible(TRUE)
}

.dep_install_only <- function(dep) {
  # don't install any already available
  # (we're not trying to force the latest version)
  dep_required <- dep[
    vapply(dep, function(x) !requireNamespace(x, quietly = TRUE), logical(1))
  ]
  if (.is_len_0(dep_required)) {
    return(invisible(TRUE))
  }
  for (i in seq_along(dep_required)) {
    if (.renv_detect()) {
      .dep_install_only_rscript(dep_required[[i]])
    } else {
      if (grepl("^\\w+/\\w+", gsub("\\.", "", dep_required[[i]]))) {
        if (!requireNamespace("remotes", quietly = TRUE)) {
          utils::install.packages("remotes")
        }
        remotes::install_github(dep_required[[i]])
      } else {
        utils::install.packages(dep_required[[i]])
      }
    }
  }
  invisible(TRUE)
}

.dep_install_only_rscript <- function(dep) {
  do.call(
    renv::install,
    args = list(dep, prompt = FALSE)
  )
  # cmd_txt <- paste0(
  #   "-e '",
  #   "renv::install(",
  #   paste0('"', dep, '"'),
  #   ", prompt = FALSE)'"
  # )
  # system2(
  #   .path_rscript_get(),
  #   args = cmd_txt, stdout = FALSE
  # )
}

.dep_rm <- function(dep) {
  path_dep <- .path_get("_dependencies.R")
  dep_vec <- readLines(path_dep)
  for (i in seq_along(dep)) {
    dep_txt <- paste0("library(", basename(dep[[i]]), ")", collapse = "")
    dep_vec <- dep_vec[!grepl(dep_txt, dep_vec)]
  }
  writeLines(dep_vec, path_dep)
  .newline_append(path_dep)
  invisible(TRUE)
}

.dep_in_renv <- function(dep) {
  dep %in% names(.renv_lockfile_read()$Packages)
}

.renv_lockfile_read <- function() {
  path_lockfile <- .renv_lockfile_path_get()
  # renv:::renv_lockfile_read is
  # way too much to port across just quickly.
  # well, it's doable, but it's not worth it.
  # we'll just skip whatever is done there and
  # read in using the same json function as them:
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    utils::install.packages("jsonlite")
  }
  jsonlite::fromJSON(txt = path_lockfile)
}

.renv_lockfile_path_get <- function() {
  # taken from renv:::renv_paths_lockfile.
  # we're not finding the lockfile when testing on
  # GH to add dependencies, so we're gonna our
  # own .path_get to get the project root
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\")) {
      override <- paste0(override, "renv.lock")
    }
    return(override)
  }
  .path_get("renv.lock")
}

# taken from withr-with_dir
with_dir <- function(new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}

.run_output_check <- function(output_run = NULL, bump_component) {
  if (!is.null(output_run)) {
    return(output_run)
  }
  if (missing(bump_component)) {
    stop(paste0("bump_component must be specified if output_run is NULL"))
  }
  output_opt_vec <- c("patch", "minor", "major")
  .bump_component_get(bump_component) %in% output_opt_vec
}

.bump_component_get <- function(bump_component) {
  bump_component <- bump_component %||% "dev"
  if (is.logical(bump_component)) "dev" else bump_component
}

.zip_file <- function(fn_rel,
                            path_dir_fn_rel,
                            fn_rel_zip,
                            path_dir_fn_rel_zip = NULL) {
  if (length(fn_rel) == 0L) {
    return(character())
  }
  if (is.null(path_dir_fn_rel_zip)) {
    path_dir_fn_rel_zip <- file.path(tempdir(), "zip", signif(stats::rnorm(1)))
  }
  .dir_create(path_dir_fn_rel_zip)
  fn_rel_zip <- gsub("\\.zip$", "", fn_rel_zip)
  fn_rel_zip <- paste0(fn_rel_zip, ".zip")
  path_zip <- file.path(path_dir_fn_rel_zip, fn_rel_zip)
  with_dir(
    path_dir_fn_rel,
    {
      sink(file.path(tempdir(), "zip123"))
      fn_rel <- .file_filter_exists(fn_rel)
      if (length(fn_rel) == 0L) {
        return(character())
      }
      utils::zip(zipfile = path_zip, files = fn_rel, flags = "-r9Xq")
      sink(NULL)
    }
  )
  path_zip
}

.dir_count_lvl <- function(path_dir) {
  vapply(path_dir, function(x) {
    x <- gsub("^/|/$", "", x)
    if (x == ".") {
      return(0L)
    }
    strsplit(x, "/")[[1]] |> length()
  }, integer(1))
}

.newline_append <- function(path) {
  txt <- readLines(path)
  if (!identical(txt[[length(txt)]], "")) {
    txt <- c(txt, "")
    writeLines(txt, path)
  }
  invisible(TRUE)
}

.env_var_set <- function(nm, val) {
  do.call(
    Sys.setenv,
    args = list(val) |> stats::setNames(nm)
  )
}

.env_var_nm_get <- function(line) {
  sub("=.*", "", line) |> trimws()
}

.env_var_val_get <- function(line) {
  sub("^.*=", "", line) |> trimws()
}

.pkg_nm_get <- function() {
  if (file.exists(.path_get("DESCRIPTION"))) {
    fn_desc <- readLines(.path_get("DESCRIPTION"))
    line <- fn_desc[grepl("^Package:", fn_desc)][[1]]
    pkg_desc <- sub("^Package: ", "", line) |> trimws()
    if (pkg_desc == "{{ Package }}") {
      basename(.path_get())
    } else {
      pkg_desc
    }
 } else {
   basename(.path_get())
 }
}

.path_rscript_get <- function() {
  rscript <- Sys.which("Rscript")
  if (nzchar(rscript)) {
    return(rscript)
  }
  # Fallback to default R installation path
  file.path(R.home("bin"), "Rscript")
}

.dots_get <- function(...) {
  tryCatch(as.list(...),
    error = function(e) list()
  )
}

.dots_get_chr <- function(...) {
  dots_list <- .dots_get(...)
  if (.is_len_pos(dots_list)) {
    dots_list <- vapply(dots_list, as.character, character(1))
  }
  dots_list
}

.dots_get_chr_vec <- function(...) {
  dots_list_chr <- .dots_get_chr(...)
  if (.is_len_pos(dots_list_chr)) {
    dots_vec_chr <- unlist(dots_list_chr)
  } else {
    dots_vec_chr <- character()
  }
  dots_vec_chr
}

#' @title `projr` drop-in replacement for usethis::use_data
#'
#' @description
#' usethis::use_data always saves data to `data/`, which
#' conflicts with the temporary directories used by .build_dev`
#' and makes it difficult to restore after failed output builds.
#'
#' .use_data` is a drop-in replacement for `usethis::use_data`,
#' which saves data to the temporary directory when `safe = TRUE`.
#' This makes it easier to restore the project after a failed output build.
#'
#' The only other difference is that .use_data` invisibly
#' returns the path to the saved data file, whereas `usethis::use_data`
#' returns `TRUE`.
#'
#' @param ... Unquoted names of existing objects to save.
#' @param internal If `FALSE`, saves each object in its own `.rda`
#'   file in the `data/` directory. These data files bypass the usual
#'   export mechanism and are available whenever the package is loaded
#'   (or via [data()] if `LazyData` is not true).
#'
#'   If `TRUE`, stores all objects in a single `R/sysdata.rda` file.
#'   Objects in this file follow the usual export rules. Note that this means
#'   they will be exported if you are using the common `exportPattern()`
#'   rule which exports all objects except for those that start with `.`.
#' @param overwrite By default, .use_data()` will not overwrite existing
#'   files. If you really want to do so, set this to `TRUE`.
#' @param compress Choose the type of compression used by [save()].
#'   Should be one of "gzip", "bzip2", or "xz".
#' @param version The serialization format version to use. The default, 2, was
#'   the default format from R 1.4.0 to 3.5.3. Version 3 became the default from
#'   R 3.6.0 and can only be read by R versions 3.5.0 and higher.
#' @inheritParams base::save
#' @param safe logical.
#' Whether to save data to a temporary directory
#' (in `<cache>/"projr"/v<version>/data/`)
#' or "data/".
#' Default is the temporary directory (TRUE).
#'
#' @details
#' Taken directly from the documentation for the original
#' `usethis` function, and adjusted slightly.
#'
#' @seealso The [data chapter](https://r-pkgs.org/data.html) of [R
#'   Packages](https://r-pkgs.org).
#' @export
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- 1:100
#'
#' projr_use_data(x, y) # For external use
#' projr_use_data(x, y, internal = TRUE) # For internal use
#' }
projr_use_data <- function(...,
                           internal = FALSE,
                           overwrite = FALSE,
                           compress = "bzip2",
                           version = 2,
                           ascii = FALSE,
                           safe = TRUE) {
  # copied across from usethis::use_data,
  # except that we adjust output directory
  # based on whether it's an output run or not
  objs <- .usethis_get_objs_from_dots(.usethis_dots(...))
  # not going to add dependency as `projr` will only
  # work for R > 3.5.0
  if (!safe) {
    if (internal) {
      .dir_create("R")
      paths <- file.path("R", "sysdata.rda")
      objs <- list(objs)
    } else {
      .dir_create("data")
      paths <- fs::path("data", objs, ext = "rda")
      desc <- .usethis_proj_desc()
      if (!desc$has_fields("LazyData")) {
        desc$set(LazyData = "true")
        desc$write()
      }
    }
  } else {
    path_tmp_base <- .dir_get_cache_auto_version(profile = NULL)
    if (internal) {
      .dir_create(file.path(path_tmp_base, "R"))
      paths <- file.path(file.path(path_tmp_base, "R"), "sysdata.rda")
      objs <- list(objs)
    } else {
      .dir_create(file.path(path_tmp_base, "data"))
      paths <- fs::path(file.path(path_tmp_base, "data"), objs, ext = "rda")
      desc <- .usethis_proj_desc()
      # don't set the lazy data entry as this is a dev build
    }
  }
  if (!overwrite) {
    fn_vec_existing <- .file_filter_exists(.path_get(paths))
    if (length(fn_vec_existing) > 0L) {
      stop(
        "The following files already exist:\n",
        paste0("  ", fn_vec_existing, collapse = "\n"),
        call. = FALSE
      )
    }
  }
  envir <- parent.frame()
  paths_project <- vapply(paths, .path_get, character(1))
  mapply(save, list = objs, file = paths_project, MoreArgs = list(
    envir = envir,
    compress = compress, version = version, ascii = ascii
  ))
  invisible(paths_project)
}

.usethis_get_objs_from_dots <- function(.dots) {
  if (length(.dots) == 0L) {
    stop("Nothing to save", call. = FALSE)
  }
  is_name <- vapply(.dots, is.symbol, logical(1))
  if (any(!is_name)) {
    stop("Can only save existing named objects.", call. = FALSE)
  }
  objs <- vapply(.dots, as.character, character(1))
  duplicated_objs <- which(stats::setNames(
    duplicated(objs),
    objs
  ))
  if (length(duplicated_objs) > 0L) {
    objs <- unique(objs)
  }
  objs
}

.usethis_dots <- function(...) {
  eval(substitute(alist(...)))
}

.usethis_proj_desc <- function(path = .path_get()) {
  desc::desc(file = path)
}

.zip_dir <- function(path_dir,
                     path_zip,
                     dir_exc = NULL,
                     dir_inc = NULL,
                     fn_exc = NULL) {
  .file_rm(path_zip)
  .dir_create(dirname(path_zip))
  wd_orig <- getwd()
  setwd(path_dir)
  sink(file.path(tempdir(), "zip123"))
  fn_vec <- .file_ls(getwd())
  if (!is.null(dir_exc)) {
    for (x in dir_exc) {
      fn_vec <- fn_vec[!grepl(paste0("^", x, "/"), fn_vec)]
    }
  }
  if (!is.null(dir_inc)) {
    for (x in dir_inc) {
      fn_vec <- fn_vec[grepl(paste0("^", x, "/"), fn_vec)]
    }
  }
  if (!is.null(fn_exc)) {
    fn_vec <- fn_vec[!fn_vec %in% fn_exc]
  }
  if (length(fn_vec) == 0) {
    setwd(wd_orig)
    return(invisible(FALSE))
  }
  path_zip_temp <- basename(path_zip)
  utils::zip(
    path_zip_temp,
    files = fn_vec,
    flags = "-r9Xq"
  )
  sink(NULL)
  if (!identical(path_zip_temp, path_zip)) {
    file.rename(
      from = path_zip_temp,
      to = path_zip
    )
  }
  setwd(wd_orig)
  invisible(TRUE)
}

# options
# -------

.opt_remote_get_osf_cat <- function() {
  c(
    "analysis", "communication", "data", "hypothesis", "methods",
    "procedure", "project", "question", "other"
  )
}

.opt_remote_get_structure <- function() {
  c("archive", "latest")
}

.opt_remote_get_type <- function() {
  c("local", "osf", "github")
}

.opt_dir_get_label <- function(profile) {
  c(
    .yml_dir_get(profile) |> names(),
    "docs",
    "data",
    "code",
    "project"
  )
}

.opt_dir_get_label_send <- function(profile, type = NULL) {
  .opt_dir_get_label(profile) |>
    setdiff("project") |>
    c(.yml_dir_get_label_docs(profile)) |>
    c("package") |>
    unique()
}

.opt_dir_get_label_get <- function(profile) {
  .opt_dir_get_label(profile) |> setdiff(c("code", "project"))
}

.opt_cue_get <- function() {
  c("if-change", "always", "never")
}

.opt_remote_strategy_get <- function() {
  c("upload-missing", "upload-all", "sync-purge", "sync-diff")
}

.opt_remote_conflict_get <- function() {
  c("overwrite", "skip", "error")
}

.opt_remote_inspect_get <- function() {
  c("file", "manifest", "none")
}

.opt_remote_transfer_names_get <- function() {
  c("cue", "strategy", "conflict", "inspect")
}

.try_repeat <- function(fn, args, n_try = NULL, n_sleep = 3) {
  last_error <- NULL

  if (is.null(n_try)) {
    if (Sys.getenv("TESTTHAT") == "true") {
      n_try <- 10
    } else {
      n_try <- 3
    }
  }

  for (i in seq_len(n_try)) {
    result_obj <- tryCatch(
      {
        list(success = TRUE, result = do.call(fn, args))
      },
      error = function(err) {
        Sys.sleep(n_sleep)
        list(success = FALSE, result = err)
      }
    )

    if (result_obj$success) {
      return(result_obj$result)
    }

    last_error <- result_obj$result
  }

  stop("All attempts failed with the following error: ", last_error)
}

.init_readme_std_contents <- function() {
  c(
    "# README",
    "",
    "The purpose of this project is to",
    "`[briefly describe the project's goals and objectives]`",
    "",
    "## Reproducing the analysis",
    "",
    "Run the following code from the project working directory:",
    "",
    "```r",
    "if (!requireNamespace(\"projr\", quietly = TRUE)) {",
    "  if (!requireNamespace(\"remotes\", quietly = TRUE)) {",
    "    install.packages(\"remotes\")",
    "  }",
    "  remotes::install_github(\"SATVILab/projr\")",
    "}",
    "projr::projr_build_dev()",
    "```",
    "",
    "<!--",
    "Change the above instructions, e.g. specify non-default parameters or",
    "another approach entirely",
    "-->",
    "",
    "## Directory Structure",
    "",
    "This project follows a structured directory layout to organise input",
    "materials, generated outputs, and temporary files efficiently.",
    "",
    "- **`_reference/`**: Static documents that are not generated by analyses,",
    "  such as assignment questions and reference materials.",
    "- **`_raw_data/`**: Raw, unprocessed data files.",
    "- **`docs/`**: Rendered documents produced from R Markdown (`Rmd`) or",
    "  Quarto (`qmd`) scripts.",
    "- **`_output/`**: Final analysis results, processed data, and exported",
    "  reports.",
    "- **`_tmp/`**: Temporary storage for cached outputs, intermediate",
    "  results, and shared files.",
    "",
    "## Links",
    "",
    "- `[URLs to data sources (e.g. OneDrive), GitHub repos, etc.]`",
    "",
    "## Details",
    "",
    "`[Methods, timeline, team, data sources, software/tools, etc.]`"
  )
}

.init_desc_std_contents <- function() {
  c(
    "Package: {{ Package }}",
    "Title: {{ Title }}",
    "Version: 1.0.0",
    "Authors@R (parsed):",
    "    * Jo Doe <jodoe@dom.ain> [aut, cre]",
    "Maintainer: {{ Maintainer }}",
    "Description: {{ Description }}",
    "License: {{ License }}",
    "URL: {{ URL }}",
    "BugReports: {{ BugReports }}",
    "Encoding: UTF-8"
  )
}

.init_engine_bookdown_contents_bookdown <- function() {
  c(
    "bookdown::gitbook:",
    "  toc_depth: 6",
    "  css: style.css",
    "  config:",
    "    toc:",
    "      before: |",
    "        <li><a href=\"./\">TODO: ADD SHORT DESCRIPTION</a></li>",
    "      after: |",
    "        <li><a href=\"https://github.com/[GITHUB_USER]/[GITHUB_REPO]\" target=\"blank\">SATVILab/TODO:_ADD_REPO_NAME</a></li>", # nolint
    "    download: [\"pdf\", \"epub\"]",
    "bookdown::pdf_book:",
    "  latex_engine: xelatex",
    "  citation_package: natbib",
    "  keep_tex: yes",
    "bookdown::epub_book: default"
  )
}

init_engine_bookdown_contents_output <- function() {
  c(
    "book_filename: docs",
    "delete_merged_file: yes",
    "language:",
    "  ui:",
    "    chapter_name: 'Chapter '",
    "rmd_files:",
    "- index.Rmd",
    "output_dir: docs"
  )
}

init_engine_bookdown_contents_index <- function() {
  c(
    "---",
    "title: \"[Title]\"",
    "author: \"[Author]\"",
    "date: \"`r Sys.Date()`\"",
    "site: bookdown::bookdown_site",
    "documentclass: book",
    "---",
    "",
    "# Introduction",
    "",
    "```{r , include = FALSE}",
    "",
    "```"
  )
}

.init_engine_quarto_projects_content_yml <- function() {
  list(
    project = list(type = "website"),
    website = list(
      title = "[Title]",
      navbar = list(
        left = list(
          list(href = "index.qmd", text = "Home")
        )
      )
    ),
    format = list(
      html = list(
        theme = "cosmo",
        toc = TRUE
      )
    )
  )
}