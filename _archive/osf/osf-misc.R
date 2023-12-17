.projr_osf_filter_dir <- function(x) {
  x[.projr_osf_is_dir(x), ]
}
.projr_osf_is_dir <- function(x) {
  vapply(seq_len(nrow(x)), function(i) {
    x$meta[[i]][["attributes"]][["kind"]] == "folder"
  }, logical(1))
}

.projr_osf_node_dir_get <- function(osf_tbl,
                                    label,
                                    path,
                                    path_append_label,
                                    structure,
                                    version = NULL) {
  path_osf <- .projr_osf_path_get(
    osf_tbl = osf_tbl,
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    version = version
  )
  if (path_osf == ".") {
    return(osf_tbl)
  }
  osfr::osf_mkdir(x = osf_tbl, path = path_osf)
}

.projr_osf_path_get <- function(osf_tbl,
                                path,
                                path_append_label,
                                label,
                                structure,
                                version = NULL) {
  path_base <- .projr_osf_path_get_base(
    path = path,
    path_append_label = path_append_label,
    label = label
  )
  if (structure == "latest") {
    return(path_base)
  }
  if (structure == "version") {
    if (!is.null(version) && version == "latest") {
      osf_tbl_file <- osfr::osf_mkdir(x = osf_tbl, path = path_base)
      osf_tbl_file <- osf_tbl_file |> osfr::osf_ls_files(n_max = Inf)
      version_vec <- osf_tbl_file[["name"]]
      if (length(version_vec) == 0L) {
        return(NULL)
      }
      max_version <- max(gsub("^v", "", version_vec))
      version <- version_vec[version_vec == paste0("v", max_version)]
      if (length(version) == 0L) {
        return(NULL)
      }
    }
    version <- version %||% projr_version_get()
    if (path_base == ".") {
      return(version)
    } else {
      return(file.path(path_base, paste0("v", version)))
    }
  }
}

.projr_osf_path_get_base <- function(path,
                                     path_append_label,
                                     label) {
  if (is.null(path) && !path_append_label) {
    return(".")
  }
  if (is.null(path) && path_append_label) {
    return(label)
  }
  if (!is.null(path) && !path_append_label) {
    return(path)
  }
  file.path(path, label)
}

.projr_osf_ls_files <- function(osf_tbl,
                                path_dir_parent = NULL) {
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  dir_vec_ind <- .projr_osf_is_dir(osf_tbl_file)
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
      fn_vec_dir_ind <- .projr_osf_ls_files(
        osf_tbl = osfr::osf_mkdir(x = osf_tbl, path = path_dir_osf),
        path_dir_parent = path_dir_parent_curr
      )
      if (length(fn_vec_dir_ind > 0L)) {
        fn_vec_dir <- c(fn_vec_dir, fn_vec_dir_ind)
      }
    }
  }
  c(fn_vec_fn, fn_vec_dir) |> unique()
}
