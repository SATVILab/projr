# taken from rstudio/renv (which
# presumably took it from elsewhere or
# Hadley Wickham's brain directly)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

`%@@%` <- function(x, y) {
  tryCatch(x, error = function(e) y)
}

.projr_dir_proj_get <- function(...) {
  dir_proj <- tryCatch(
    rprojroot::is_r_package$find_file(),
    error = function(e) getwd()
  )
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
  path_dep <- .projr_dir_proj_get("_dependencies.R")
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
    .projr_dep_install_only_rscript(x)
  }
}

.projr_dep_install_only_rscript <- function(dep) {
  cmd_txt <- paste0(
    "-e '",
    "renv::install(",
    paste0('"', dep, '"'),
    ", prompt = FALSE)'"
  )
  system2(
    .projr_path_rscript_get(),
    args = cmd_txt, stdout = FALSE
  )
}

.projr_dep_rm <- function(dep) {
  path_dep <- .projr_dir_proj_get("_dependencies.R")
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
  .projr_dir_create(path_dir_fn_rel_zip)
  fn_rel_zip <- gsub("\\.zip$", "", fn_rel_zip)
  fn_rel_zip <- paste0(fn_rel_zip, ".zip")
  path_zip <- file.path(path_dir_fn_rel_zip, fn_rel_zip)
  with_dir(
    path_dir_fn_rel,
    {
      sink(file.path(tempdir(), "zip123"))
      fn_rel <- .projr_file_filter_exists(fn_rel)
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
  .projr_dir_create(path_dir)
  path_dir
}

.projr_dir_tmp_random_get_if_needed <- function(path_dir) {
  if (missing(path_dir) || is.null(path_dir)) {
    path_dir <- .projr_dir_tmp_random_get()
  } else {
    .projr_dir_create(path_dir)
  }
  path_dir
}

.projr_dir_copy_tree <- function(path_dir_from, path_dir_to) {
  path_dir_from_vec_tree <- list.dirs(
    path_dir_from,
    recursive = TRUE, full.names = FALSE
  ) |>
    setdiff("")
  path_dir_to_vec_tree <- file.path(path_dir_to, path_dir_from_vec_tree)
  .projr_dir_create(path_dir_to_vec_tree)
  for (i in seq_along(path_dir_to_vec_tree)) {
    .projr_dir_create(path_dir_to_vec_tree[[i]])
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

.projr_pkg_nm_get <- function() {
  desc::desc_get_field(
    "Package",
    file = .projr_dir_proj_get("DESCRIPTION")
  )
}

.projr_path_rscript_get <- function() {
  file.path(R.home("bin"), "Rscript")
}

.projr_dots_get <- function(...) {
  tryCatch(as.list(...),
    error = function(e) list()
  )
}

.projr_dots_get_chr <- function(...) {
  dots_list <- .projr_dots_get(...)
  if (.is_len_pos(dots_list)) {
    dots_list <- vapply(dots_list, as.character, character(1))
  }
  dots_list
}

.projr_dots_get_chr_vec <- function(...) {
  dots_list_chr <- .projr_dots_get_chr(...)
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
#' conflicts with the temporary directories used by `projr_build_dev`
#' and makes it difficult to restore after failed output builds.
#'
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
  objs <- .projr_usethis_get_objs_from_dots(.projr_usethis_dots(...))
  # not going to add dependency as `projr` will only
  # work for R > 3.5.0
  if (!safe) {
    if (internal) {
      .projr_dir_create("R")
      paths <- fs:::path("R", "sysdata.rda")
      objs <- list(objs)
    } else {
      .projr_dir_create("data")
      paths <- fs::path("data", objs, ext = "rda")
      desc <- .projr_usethis_proj_desc()
      if (!desc$has_fields("LazyData")) {
        desc$set(LazyData = "true")
        desc$write()
      }
    }
  } else {
    path_tmp_base <- .projr_dir_get_cache_auto(profile = NULL)
    if (internal) {
      .projr_dir_create(file.path(path_tmp_base, "R"))
      paths <- fs:::path(file.path(path_tmp_base, "R"), "sysdata.rda")
      objs <- list(objs)
    } else {
      .projr_dir_create(file.path(path_tmp_base, "data"))
      paths <- fs::path(file.path(path_tmp_base, "data"), objs, ext = "rda")
      desc <- .projr_usethis_proj_desc()
      # don't set the lazy data entry as this is a dev build
    }
  }
  if (!overwrite) {
    fn_vec_existing <- .projr_file_filter_exists(.projr_dir_proj_get(paths))
    if (length(fn_vec_existing) > 0L) {
      stop(
        "The following files already exist:\n",
        paste0("  ", fn_vec_existing, collapse = "\n"),
        call. = FALSE
      )
    }
  }
  envir <- parent.frame()
  mapply(save, list = objs, file = .projr_dir_proj_get(paths), MoreArgs = list(
    envir = envir,
    compress = compress, version = version, ascii = ascii
  ))
  invisible()
}

.projr_usethis_get_objs_from_dots <- function(.dots) {
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

.projr_usethis_dots <- function(...) {
  eval(substitute(alist(...)))
}

.projr_usethis_proj_desc <- function(path = .projr_dir_proj_get()) {
  desc::desc(file = path)
}

.projr_zip_dir <- function(path_dir,
                           path_zip,
                           dir_exc = NULL,
                           dir_inc = NULL,
                           fn_exc = NULL) {
  .projr_file_rm(path_zip)
  .projr_dir_create(dirname(path_zip))
  wd_orig <- getwd()
  setwd(path_dir)
  sink(file.path(tempdir(), "zip123"))
  fn_vec <- .projr_dir_ls(getwd())
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

.assert_test <- function() {
  .is_testing()
}
