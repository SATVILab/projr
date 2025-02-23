.osf_is_dir <- function(x) {
  if (nrow(x) == 0L) {
    return(logical(0))
  }
  vapply(seq_len(nrow(x)), function(i) {
    x$meta[[i]][["attributes"]][["kind"]] == "folder"
  }, logical(1))
}

.osf_ls_files <- function(osf_tbl,
                          path_dir_parent = NULL) {
  osf_tbl_file <- osf_tbl |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  dir_vec_ind <- .osf_is_dir(osf_tbl_file)
  if (any(!dir_vec_ind)) {
    fn_vec_fn <- osf_tbl_file[["name"]][!dir_vec_ind]
    if (!is.null(path_dir_parent)) {
      fn_vec_fn <- file.path(path_dir_parent, fn_vec_fn)
    }
  } else {
    fn_vec_fn <- NULL
  }
  fn_vec_dir <- NULL
  if (any(dir_vec_ind)) {
    dir_vec_int <- which(dir_vec_ind)
    for (i in seq_along(dir_vec_int)) {
      path_dir_osf <- osf_tbl_file[["name"]][dir_vec_int[i]]
      if (!is.null(path_dir_parent)) {
        path_dir_parent_curr <- file.path(
          basename(path_dir_parent), path_dir_osf
        )
      } else {
        path_dir_parent_curr <- path_dir_osf
      }
      fn_vec_dir_ind <- .osf_ls_files(
        osf_tbl = .osf_mkdir(x = osf_tbl, path = path_dir_osf),
        path_dir_parent = path_dir_parent_curr
      )
      if (length(fn_vec_dir_ind > 0L)) {
        fn_vec_dir <- c(fn_vec_dir, fn_vec_dir_ind)
      }
    }
  }
  c(fn_vec_fn, fn_vec_dir) |> unique()
}

.remote_complete_osf_category <- function(category) {
  if (!is.null(category)) category else "project"
}
.remote_complete_osf_public <- function(public) {
  if (!is.null(public)) public else FALSE
}

.osf_upload <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_upload,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_create_project <- function(..., n_try = NULL, n_sleep = 3) {
  .dep_install("osfr")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_create_project,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_create_component <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_create_component,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_retrieve_node <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_retrieve_node,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_mkdir <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_mkdir,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_ls_files <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_ls_files,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_rm <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_rm,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_download <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_download,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_retrieve_user <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_retrieve_user,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_ls_nodes <- function(..., n_try = NULL, n_sleep = 3) {
  args_list <- list(...)
  .try_repeat(
    osfr::osf_ls_nodes,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}
