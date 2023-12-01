# taken from rstudio/renv (which
# presumably took it from elsewhere or
# Hadley Wickham's brain directly)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.projr_dir_proj_get <- function(...) {
  if (is.null(dir_proj)) {
    dir_proj <- tryCatch(
      rprojroot::is_r_package$find_file(),
      error = function(e) getwd()
    )
  }
  list_sub <- list(...)
  if (length(list_sub) == 0L) {
    return(dir_proj)
  }
  do.call(
    file.path,
    args = list(dir_proj) |> append(list_sub)
  )
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



.projr_dep_install <- function(dep) {
  for (x in dep) {
    .projr_dep_add(x)
    if (!requireNamespace(x, quietly = TRUE)) {
      .projr_dep_install_only(dep)
    }
  }
}

.projr_dep_add <- function(dep) {
  # don't add to _dependencies
  # if renv already picks it up as a dependency
  if (.projr_dep_in_renv(dep)) {
    return(invisible(FALSE))
  }
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

.projr_dep_install_only <- function(dep) {
  for (x in dep) {
    if (requireNamespace(x, quietly = TRUE)) {
      next
    }
    renv::install(x, prompt = FALSE)
  }
}

.projr_dep_rm <- function(dep) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_dep <- file.path(dir_proj, "_dependencies.R")
  dep_vec <- readLines(path_dep)
  for (i in seq_along(dep)) {
    dep_txt <- paste0("library(", dep[[i]], ")", collapse = "")
    dep_vec <- dep_vec[!grepl(dep_txt, dep_vec)]
  }
  writeLines(dep_vec, path_dep)
  .projr_newline_append(path_dep)
  invisible(TRUE)
}

.projr_dep_in_renv <- function(dep) {
  dep %in% names(renv::lockfile_read()$Packages)
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
  fn_rel_zip <- gsub("\\.zip$", "", fn_rel_zip)
  fn_rel_zip <- paste0(fn_rel_zip, ".zip")
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
      sink(NULL)
    }
  )
  path_zip
}

.projr_dir_tmp_random_get <- function() {
  path_dir <- file.path(tempdir(), "projr", signif(rnorm(1), 6))
  while (dir.exists(path_dir)) {
    if (dir.exists(path_dir)) {
      path_dir <- file.path(
        tempdir(), "projr", signif(rnorm(1), 6)
      )
    }
  }
  if (!dir.exists(path_dir)) {
    dir.create(path_dir, recursive = TRUE)
  }
  path_dir
}

.projr_dir_tmp_random_get_if_needed <- function(path_dir) {
  if (missing(path_dir) || is.null(path_dir)) {
    path_dir <- .projr_dir_tmp_random_get()
  } else {
    if (!dir.exists(path_dir)) {
      dir.create(path_dir, recursive = TRUE)
    }
  }
  path_dir
}

.projr_dir_tree_copy <- function(path_dir_from, path_dir_to) {
  path_dir_from_vec_tree <- list.dirs(
    path_dir_from,
    recursive = TRUE, full.names = FALSE
  ) |>
    setdiff("")
  path_dir_to_vec_tree <- file.path(path_dir_to, path_dir_from_vec_tree)
  if (!dir.exists(path_dir_to)) {
    dir.create(path_dir_to, recursive = TRUE)
  }
  for (i in seq_along(path_dir_to_vec_tree)) {
    if (!dir.exists(path_dir_to_vec_tree[[i]])) {
      dir.create(path_dir_to_vec_tree[[i]], recursive = TRUE)
    }
  }
  invisible(TRUE)
}

.projr_dir_count_lvl <- function(path_dir) {
  vapply(path_dir, function(x) {
    x <- gsub("^/|/$", "", x)
    if (x == ".") {
      return(0L)
    }
    strsplit(x, "/")[[1]] |> length()
  }, integer(1))
}

.projr_newline_append <- function(path) {
  txt <- readLines(path)
  if (!identical(txt[[length(txt)]], "")) {
    txt <- c(txt, "")
    writeLines(txt, path)
  }
  invisible(TRUE)
}

.projr_env_var_unset_on_exit <- function(nm, env) {
  eval(
    parse(text = paste0(
      "on.exit(",
      "Sys.unsetenv('", nm, "'), ",
      "add = TRUE, after = TRUE)",
      ")"
    )),
    envir = env
  )
}

.projr_env_var_set <- function(nm, val) {
  do.call(
    Sys.setenv,
    args = list(val) |> stats::setNames(nm)
  )
}

.projr_env_var_nm_get <- function(line) {
  sub("=.*", "", line) |> trimws()
}

.projr_env_var_val_get <- function(line) {
  sub("^.*=", "", line) |> trimws()
}
