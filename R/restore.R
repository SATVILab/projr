#' @title Restore project artefact directory
#'
#' @param label character vector.
#' Must all be labels found in `_projr.yml` (or
#' default labels).
#' If `NULL` (the default), then all `raw` labels
#' are restored (e.g. `raw-data`, `raw-docs`, `raw`, etc.).
#' @param type character.
#' Type of remote to restore from.
#' One of `local`, `osf` and `github`.
#' If `NULL`, then an appropriate remote is searched
#' for.
#' This is the first destination that has
#' this `label` in its `source` field (or
#' has its `source` field as `TRUE`).
#' If none found, then the first type
#' and title combination that has this
#' label in its contents is used.
#' @param title character.
#' Title of remote.
#' Ignored if `type` is not specified.
#'
#' @return
#' Invisible returns `TRUE` if successful.
#'
#' @examples
#' \dontrun{
#' projr_restore("raw-data")
#' }
#' @export
projr_restore <- function(label = NULL,
                          type = NULL,
                          title = NULL) {
  if (!file.exists(.path_get("manifest.csv"))) {
    stop(
      "No manifest.csv file found, so no builds have occurred, so nothing to restore." # nolint
    )
  }
  label <- .restore_get_label(label)
  for (i in seq_along(label)) {
    .restore_label(label[[i]], type, title)
  }
}

.restore_get_label <- function(label) {
  .restore_get_label_check(label)
  if (!is.null(label)) {
    return(label)
  }
  # default to raw labels, if none other requested
  nm_vec <- .yml_dir_get(NULL) |>
    names()
  nm_vec[grepl("^raw", .dir_label_strip(nm_vec))]
}

.restore_get_label_check <- function(label) {
  if (!is.null(label)) {
    opt_vec <- .yml_dir_get(NULL) |>
      names()
    .assert_len_pos(opt_vec)
    for (x in label) {
      .assert_in(x, opt_vec)
    }
  }
  invisible(TRUE)
}

.restore_label <- function(label, type, title) {
  if (!.restore_label_check_non_empty(label)) {
    return(invisible(FALSE))
  }
  # get source remote (type and title)
  source_vec <- .restore_label_get_source(label, type, title)
  yml_title <- .yml_dest_get_title_complete(
    source_vec[["title"]], source_vec[["type"]], NULL, FALSE, FALSE
  )
  remote_pre <- .remote_get_final(
    source_vec[["type"]], yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]], NULL, TRUE
  )
  version_remote <- if (yml_title[["structure"]] == "latest") {
    .remote_get_version_label_non_project_file(
      remote_pre, source_vec[["type"]], label
    )
  } else {
    .remote_get_version_label_non_project_archive(
      remote_pre, source_vec[["type"]], label, "archive"
    )
  }
  if (is.null(version_remote)) {
    message("No version found for ", label)
    message("Skipping restore for ", label)
    return(invisible(FALSE))
  }
  message(
    "Restoring ", label,
    " from ", source_vec[["type"]],
    " ", source_vec[["title"]]
  )
  message("Version: ", version_remote)
  remote_source <- .remote_get_final(
    source_vec[["type"]], yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]], version_remote
  )
  path_dir_local <- projr_path_get_dir(label, safe = TRUE)
  if (!dir.exists(path_dir_local)) {
    dir.create(path_dir_local, recursive = TRUE)
  }
  .remote_file_get_all(source_vec[["type"]], remote_source, path_dir_local)
  invisible(TRUE)
}

.restore_label_check_non_empty <- function(label) {
  manifest <- .manifest_read(.path_get("manifest.csv")) |>
    .manifest_filter_label(label)
  fn_vec <- unique(manifest[["fn"]])
  label_recorded_and_unused <- .is_len_1(fn_vec) && fn_vec == ""
  label_unrecorded <- .is_len_0(fn_vec)
  nothing_to_restore <- label_recorded_and_unused || label_unrecorded
  if (nothing_to_restore) {
    message("No files kept in ", label)
    message("Skipping restore for ", label)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


.restore_label_get_source <- function(label, type, title) {
  # get source remote (type and title)
  if (!is.null(type) && !is.null(title)) {
    return(c("type" = type, "title" = title))
  }
  if (is.null(type)) {
    nm_vec <- .yml_build_get(NULL) |>
      names()
    type <- nm_vec[grepl("^github$|^local$|^osf", nm_vec)]
    if (.is_len_0(type)) {
      if (.remote_check_exists("github", "archive")) {
        return(c("type" = "github", "title" = "archive"))
      } else {
        stop("No source found for ", label)
      }
      stop("No source found for ", label)
    }
  }
  tp_first <- NULL
  tt_first <- NULL

  for (tp in type) {
    yml_type <- .yml_dest_get_type(tp, NULL)
    title <- names(yml_type)
    for (i in seq_along(title)) {
      tt <- title[[i]]
      yml_title <- .yml_dest_get_title_complete(
        tt, tp, NULL, FALSE, FALSE
      )
      if (!label %in% yml_title[["content"]]) {
        next
      }
      if ("source" %in% names(yml_title)) {
        source_vec <- yml_title[["source"]]
        if (isTRUE(source_vec)) {
          return(c(type = tp, title = tt))
        } else if (isFALSE(source_vec)) {
          next
        } else if (label %in% source_vec) {
          return(c(type = tp, title = tt))
        }
      }
      if (is.null(tt_first)) {
        tp_first <- tp
        tt_first <- tt
      }
    }
  }
  if (is.null(tt_first)) {

  }
  c("type" = tp_first, "title" = tt_first)
}
