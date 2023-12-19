.projr_osf_filter_dir <- function(x) {
  x[.projr_osf_is_dir(x), ]
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
