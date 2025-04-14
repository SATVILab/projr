#' Restore project artefact directories
#'
#' Use `projr_restore()` to restore all artefacts needed for the current project.
#' If the project isn't available locally yet, 
#' `projr_restore_repo()` will clone it and then restore its artefacts.
#'
#' @param label character vector. Specifies labels of artefacts to restore.
#'   Default is `NULL`, restoring all `raw` artefacts (e.g. `raw-data`).
#' @param pos character vector. Specifies preferred source: `"source"` (directories)
#'   or `"dest"` (build). Default is `NULL`, checking both.
#' @param type character. Remote type: `"local"`, `"osf"` or `"github"`.
#'   Default is `NULL`, automatically choosing the first available remote.
#' @param title character. Remote title as specified in `_projr.yml`. Default is `NULL`.
#' @param repo character. GitHub repository (`"owner/repo"` or `"repo"`).
#'   (Only for repository restoration functions.)
#' @param path character. Local path for cloning the repository. Default is `NULL`,
#'   creating a subdirectory named after the repo. `"."` restores directly into the current directory.
#'
#' @return Invisibly returns `TRUE` if restoration is successful.
#'
#' @details
#' - `projr_restore()` restores artefacts in an existing local project without any cloning required.
#' - `projr_restore_repo()` clones a GitHub repository into a subdirectory (or specified path), then restores artefacts.
#' - `projr_restore_repo_wd()` clones directly into the current working directory, then restores artefacts.
#' 
#' @examples
#' \dontrun{
#'   # Restore all artefacts in existing local project
#'   projr_restore()
#'
#'   # Clone repository into subdirectory and restore artefacts
#'   projr_restore_repo("owner/repo")
#'
#'   # Clone repository into current directory and restore artefacts
#'   projr_restore_repo_wd("owner/repo")
#' }
#'
#' @name projr_restore
#' @rdname projr_restore
#' @export
projr_restore <- function(label = NULL,
                          pos = NULL,
                          type = NULL,
                          title = NULL) {
  .title <- title
  if (!file.exists(.path_get("manifest.csv"))) {
    stop(
      "No manifest.csv file found, so no builds have occurred, so nothing to restore." # nolint
    )
  }
  label <- .restore_get_label(label)
  for (i in seq_along(label)) {
    tryCatch(
      .restore_label(label[[i]], pos, type, .title),
      error = function(e) {
        message("Error restoring label: ", label[[i]], " - ", e$message)
      }
    )
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

.restore_label <- function(label, pos, type, .title) {
  if (!.restore_label_check_non_empty(label)) {
    return(invisible(FALSE))
  }
  # get source remote (type and title)
  source_vec <- .restore_label_get_source(pos, label, type, .title)
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
  # Check if version is marked as untrusted in VERSION file
  untrusted <- .remote_check_version_untrusted(remote_pre, source_vec[["type"]], label)
  if (untrusted) {
    message("Note: This version is marked as untrusted")
  }
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

.restore_label_get_source <- function(pos, label, type, .title) {
  pos <- if (is.null(pos)) c("source", "dest") else pos
  .assert_in(pos, c("source", "dest"))
  if ("source" %in% pos) {
    source_vec <- .restore_label_get_source_source(label, type, .title)
    if (!is.null(source_vec)) {
      return(source_vec)
    }
  }
  if ("dest" %in% pos) {
    source_vec <- .restore_label_get_source_dest(label, type, .title)
    if (!is.null(source_vec)) {
      return(source_vec)
    }
  }
  # if we get here, then we have no source
  # to restore from
  # so we return NULL
  NULL
}

.restore_label_get_source_source <- function(label, type, .title) {
  yml_source <- .yml_dir_get_source(label, NULL)
  # nothing to look at here
  if (is.null(yml_source)) {
    return(NULL)
  }
  # look within the type
  .restore_label_get_source_source_type(
    yml_source, type, .title
  )
}

.restore_label_get_source_source_type <- function(yml_source, type, .title) {
  if (!is.null(type)) {
    .restore_label_get_source_source_type_spec(
      yml_source, type, .title
    )
  } else {
    .restore_label_get_source_source_type_first(yml_source, .title)
  }
}

.restore_label_get_source_source_type_spec <- function(yml_source,
                                                       type,
                                                       .title) {
  if (type %in% names(yml_source)) {
    yml_type <- yml_source[[type]]
    .restore_label_get_source_source_title_spec(
      yml_type, type, .title
    )
  } else {
    NULL
  }
}

.restore_label_get_source_source_title <- function(yml_type,
                                                   type,
                                                   .title) {
  if (!is.null(.title)) {
    .restore_label_get_source_source_title_spec(yml_type, type, .title)
  } else {
    .restore_label_get_source_source_title_first(yml_type, type)
  }
}

.restore_label_get_source_source_title_spec <- function(yml_type, type, .title) {
  if (is.list(yml_type)) {
    .restore_label_get_source_source_title_spec_list(
      yml_type, type, .title
    )
  } else if (all(is.character(yml_type))) {
    .restore_label_get_source_source_title_spec_chr(
      yml_type, type, .title
    )
  } else {
    NULL
  }
}

.restore_label_get_source_source_title_spec_list <- function(yml_type, type, .title) { # nolint
  if (.title %in% names(yml_type)) {
    c("pos" = "source", "type" = type, "title" = .title)
  } else {
    NULL
  }
}

.restore_label_get_source_source_title_spec_chr <- function(yml_type, type, .title) { # nolint
  if (.title %in% yml_type) {
    c("pos" = "source", "type" = type, "title" = .title)
  } else {
    NULL
  }
}

.restore_label_get_source_source_title_first <- function(yml_type, type) {
  if (all(is.character(yml_type))) {
    c("pos" = "source", "type" = type, "title" = yml_type[[1]])
  } else {
    c("pos" = "source", "type" = type, "title" = names(yml_type)[[1]])
  }
}

.restore_label_get_source_source_type_first <- function(yml_source, .title) {
  type <- names(yml_source)[[1]]
  .restore_label_get_source_source_type_spec(
    yml_source[[type]], type, .title
  )
}

# try find restore from destination

.restore_label_get_dest_no_type <- function(label) {
  if (.remote_check_exists("github", "archive")) {
    return(c("pos" = "dest", "type" = "github", "title" = "archive"))
  } else {
    if (.remote_check_exists("local", "_archive")) {
      return(c("pos" = "dest", "type" = "local", "title" = "_archive"))
    } else {
      stop("No source found for ", label)
    }
  }
}

.restore_label_get_source_dest <- function(label, type, .title) {
  type_vec <- .restore_label_get_source_dest_get_type(type, label)
  if (.is_len_0(type_vec)) {
    return(.restore_label_get_dest_no_type(label))
  }
  # choose the first type and title
  # provided as the restore destination
  # if none specifically
  tp_first <- NULL
  tt_first <- NULL

  for (tp in type_vec) {
    yml_type <- .yml_dest_get_type(tp, NULL)
    title_vec_type <- names(yml_type)
    # if no title is provided, then
    # we need to check all titles
    if (is.null(.title)) {
      title_considered <- title_vec_type
    } else {
      # if a title is provided, then
      # we need to check if it is in the
      # list of titles
      # if not, then we skip this type
      if (!.title %in% title_vec_type) {
        next
      }
      title_considered <- .title
    }
    for (i in seq_along(title_considered)) {
      tt <- title_considered[[i]]
      yml_title <- .yml_dest_get_title_complete(
        tt, tp, NULL, FALSE, FALSE
      )
      if (!label %in% yml_title[["content"]]) {
        next
      }
      if ("source" %in% names(yml_title)) {
        source_vec <- yml_title[["source"]]
        if (isTRUE(source_vec)) {
          return(c("pos" = "dest", "type" = tp, "title" = tt))
        } else if (isFALSE(source_vec)) {
          next
        } else if (label %in% source_vec) {
          return(c("pos" = "dest", "type" = tp, "title" = tt))
        }
      }
      if (is.null(tt_first)) {
        tp_first <- tp
        tt_first <- tt
      }
    }
  }

  # if nothing found, try parameter-specified removes
  if (is.null(tt_first)) {
    tryCatch(
      .restore_label_get_dest_no_type(label),
      error = function(e) {
        message("No source found for ", label)
        message("Skipping restore for ", label)
        return(invisible(FALSE))
      }
    )
  } else {
    c("pos" = "dest",  "type" = tp_first, "title" = tt_first)
  }

}

.restore_label_get_source_dest_get_type <- function(type, label) {
  if (!is.null(type)) {
    return(type)
  }
  nm_vec <- .yml_build_get(NULL) |> names()
  nm_vec[grepl("^github$|^local$|^osf", nm_vec)]
}

.restore_label_get_source_dest_get_title <- function(type, title) {
  if (!is.null(title)) {
    return(title)
  }
}

# Function to check if a version is marked as untrusted in VERSION file
.remote_check_version_untrusted <- function(remote_pre, type, label) {
  version_file <- .remote_get_version_file(type, remote_pre)
  match_str <- utils::glob2rx(label) |>
    gsub("\\$", "", x = _) |>
    paste0(": ")
  label_regex <- grep(match_str, version_file, value = TRUE)
  if (.is_len_0(label_regex)) {
    return(FALSE)
  }
  # Return TRUE if ends in an asterisk
  grepl("\\*$", label_regex)
}