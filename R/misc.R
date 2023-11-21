# taken from rstudio/renv (which
# presumably took it from elsewhere or
# Hadley Wickham's brain directly)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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

.projr_dep_add <- function(dep) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_dep <- file.path(dir_proj, "_dependencies.R")
  dep_vec <- readLines(path_dep)
  for (i in seq_along(dep)) {
    dep_txt <- paste0("library(", dep[[i]], ")", collapse = "")
    if (!any(grepl(dep_txt, dep_vec))) {
      dep_vec <- c(dep_vec, dep_txt)
    }
  }
  writeLines(dep_vec, path_dep)
  .projr_newline_append(path_dep)
  invisible(TRUE)
}

if (!requireNamespace("piggyback", quietly = TRUE)) {
}
.projr_dep_install <- function(dep) {
  for (x in dep) {
    if (requireNamespace(x, quietly = TRUE)) {
      next
    }
    renv::install(x, prompt = FALSE)
    .projr_dep_add(x)
  }
}

# taken from withr::with_dir
with_dir <- function(new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}

.projr_run_output_check <- function(output_run = NULL, bump_component) {
  if (!is.null(output_run)) {
    return(output_run)
  }
  if (missing(bump_component)) {
    stop(paste0("bump_component must be specified if output_run is NULL"))
  }
  output_opt_vec <- c("patch", "minor", "major")
  .projr_bump_component_get(bump_component) %in% output_opt_vec
}

.projr_bump_component_get <- function(bump_component) {
  bump_component <- bump_component %||% "dev"
  if (is.logical(bump_component)) "dev" else bump_component
}

.projr_zip_file <- function(fn_rel,
                            path_dir_fn_rel,
                            fn_rel_zip,
                            path_dir_fn_rel_zip = NULL) {
  if (length(fn_rel) == 0L) {
    return(character())
  }
  if (is.null(path_dir_fn_rel_zip)) {
    path_dir_fn_rel_zip <- file.path(tempdir(), "zip", signif(rnorm(1)))
  }
  if (!dir.exists(path_dir_fn_rel_zip)) {
    dir.create(path_dir_fn_rel_zip, recursive = TRUE)
  }
  path_zip <- file.path(path_dir_fn_rel_zip, fn_rel_zip)
  with_dir(
    path_dir_fn_rel,
    {
      sink(file.path(tempdir(), "zip123"))
      fn_rel <- fn_rel[file.exists(fn_rel)]
      if (length(fn_rel) == 0L) {
        return(character())
      }
      utils::zip(zipfile = path_zip, files = fn_rel, flags = "-r9Xq")
    }
  )
  path_zip
}
