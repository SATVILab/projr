#' Restore project artefact directories
#'
#' Use `projr_content_update()` to restore all artefacts needed for the current project.
#' Use `projr_checkout()` to restore artefacts from a specific project version.
#' If the project isn't available locally yet,
#' `projr_restore_repo()` will clone it and then restore its artefacts.
#'
#' @param label character vector or NULL. Specifies labels of artefacts to restore.
#'   Default is `NULL`, restoring all `raw` artefacts (e.g. `raw-data`).
#'   Must be NULL or a non-empty character vector with valid directory labels.
#' @param pos character vector or NULL. Specifies preferred source: `"source"` (directories)
#'   or `"dest"` (build destinations). Default is `NULL`, checking both in order.
#'   Must be NULL or one/both of `"source"` and `"dest"`.
#' @param type character or NULL. Remote type: `"local"`, `"osf"` or `"github"`.
#'   Default is `NULL`, automatically choosing the first available remote.
#'   Must be NULL or one of the valid remote types.
#' @param title character or NULL. Remote title as specified in `_projr.yml`.
#'   Default is `NULL`, using the first available title for the selected type.
#'   Must be NULL or a single non-empty character string.
#' @param version character or NULL. Version to restore (e.g., `"0.0.1"` or `"v0.0.1"`).
#'   Default is `NULL`, which restores the latest available version.
#'   (Only for `projr_checkout()`.)
#'   Must be NULL or a single non-empty character string with valid version format.
#' @param repo character. GitHub repository (`"owner/repo"` or `"repo"`).
#'   (Only for repository restoration functions.)
#'   Must be a single non-empty character string.
#' @param path character or NULL. Local path for cloning the repository. Default is `NULL`,
#'   creating a subdirectory named after the repo. `"."` restores directly into the
#'   current directory. Must be NULL or a single non-empty character string.
#' @param clear logical. If `TRUE`, clears existing local artefact directories
#'   before restoration. Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE` if all restorations are successful, `FALSE` otherwise.
#'   For `projr_content_update()` and `projr_checkout()`, returns `FALSE` if no labels are
#'   found to restore or if any restoration fails. For `projr_restore_repo()`, returns `FALSE`
#'   if cloning or restoration fails.
#'
#' @details
#' These functions restore artefact directories from remote sources:
#'
#' - `projr_content_update()` restores artefacts in an existing local project without any cloning required.
#'   Requires a `manifest.csv` file in the project root. Always restores the latest version.
#' - `projr_checkout()` restores artefacts from a specific project version. Similar to
#'   `projr_content_update()` but allows specifying which version to restore.
#' - `projr_restore_repo()` clones a GitHub repository into a subdirectory (or specified path),
#'   then restores artefacts from that repository's remote sources.
#' - `projr_restore_repo_wd()` clones directly into the current working directory, then restores artefacts.
#'
#' **Input Validation:**
#' All parameters are validated before execution:
#' - `label`: Must be NULL or a non-empty character vector of valid directory labels
#' - `pos`: Must be NULL or contain only "source" and/or "dest"
#' - `type`: Must be NULL or one of "local", "osf", or "github"
#' - `title`: Must be NULL or a single character string
#' - `version`: Must be NULL or a single character string with valid version format
#' - `repo`: Must be a single non-empty character string
#' - `path`: Must be NULL or a single non-empty character string
#'
#' **Error Handling:**
#' The functions handle errors gracefully:
#' - Missing `manifest.csv` triggers an informative error
#' - Invalid labels or missing remote sources result in warning messages and skipped restoration
#' - Git clone failures are caught and reported
#' - Errors during restoration are caught per label, allowing partial success
#' - Invalid or non-existent versions trigger informative errors
#'
#' @examples
#' \dontrun{
#' # Restore all raw artefacts in existing local project (latest version)
#' projr_content_update()
#'
#' # Restore specific labels
#' projr_content_update(label = c("raw-data", "cache"))
#'
#' # Restore from specific source type
#' projr_content_update(type = "local", title = "archive")
#'
#' # Restore specific version
#' projr_checkout(version = "0.0.1")
#'
#' # Restore specific labels from specific version
#' projr_checkout(label = c("raw-data", "output"), version = "v0.0.2")
#'
#' # Clone repository into subdirectory and restore artefacts
#' projr_restore_repo("owner/repo")
#'
#' # Clone to specific path
#' projr_restore_repo("owner/repo", path = "my-project")
#'
#' # Clone repository into current directory and restore artefacts
#' projr_restore_repo_wd("owner/repo")
#' }
#'
#' @name projr_restore
#' @rdname projr_restore
#' @export
projr_content_update <- function(label = NULL,
                                 pos = NULL,
                                 type = NULL,
                                 title = NULL,
                                 clear = FALSE) {
  # Input validation
  if (!is.null(label)) {
    if (!is.character(label)) {
      stop("'label' must be NULL or a character vector")
    }
    if (length(label) == 0) {
      stop("'label' must have at least one element if not NULL")
    }
  }
  if (!is.null(pos)) {
    if (!is.character(pos)) {
      stop("'pos' must be NULL or a character vector")
    }
    if (length(pos) == 0) {
      stop("'pos' must have at least one element if not NULL")
    }
    invalid_pos <- pos[!pos %in% c("source", "dest")]
    if (length(invalid_pos) > 0) {
      stop(
        "'pos' must be 'source' or 'dest'. Invalid values: ",
        paste(invalid_pos, collapse = ", ")
      )
    }
  }
  if (!is.null(type)) {
    if (!is.character(type)) {
      stop("'type' must be NULL or a character vector")
    }
    if (length(type) == 0) {
      stop("'type' must have at least one element if not NULL")
    }
    if (length(type) > 1) {
      stop("'type' must be a single character value")
    }
    valid_types <- c("local", "osf", "github")
    if (!type %in% valid_types) {
      stop(
        "'type' must be one of: ",
        paste(valid_types, collapse = ", ")
      )
    }
  }
  if (!is.null(title)) {
    if (!is.character(title)) {
      stop("'title' must be NULL or a character vector")
    }
    if (length(title) == 0) {
      stop("'title' must have at least one element if not NULL")
    }
    if (length(title) > 1) {
      stop("'title' must be a single character value")
    }
  }

  .title <- title
  if (!file.exists(.path_get("manifest.csv"))) {
    msg <- "No manifest.csv file found, so no builds have occurred, so nothing to restore." # nolint
    msg <- if (clear) paste0(msg, " No files cleared.") else msg
    .cli_debug(msg)
    stop("", .call = FALSE)
  }

  label <- .content_update_get_label(label)

  # Handle case where no labels to restore
  if (length(label) == 0) {
    msg <- "No labels found to restore."
    msg <- if (clear) paste0(msg, " No files cleared.") else msg
    .cli_debug(msg)
    stop("", .call = FALSE)
  }

  success <- TRUE
  for (i in seq_along(label)) {
    if (clear) {
      .cli_debug(
        "Clearing existing files for label: ", label[[i]]
      )
      path_dir_local <- projr_path_get_dir(
        label[[i]],
        safe = FALSE, create = FALSE
      )
      if (dir.exists(path_dir_local)) {
        unlink(path_dir_local, recursive = TRUE, force = TRUE)
      }
    }
    .cli_debug(
      "Restoring label: ", label[[i]]
    )
    result <- tryCatch(
      .content_update_label(label[[i]], pos, type, .title, NULL),
      error = function(e) {
        .cli_debug(
          "Error restoring label: ", label[[i]], " - ", e$message
        )
        FALSE
      }
    )
    if (isFALSE(result)) {
      success <- FALSE
    }
  }
  invisible(success)
}

#' @rdname projr_restore
#' @export
projr_checkout <- function(version,
                           label = NULL,
                           pos = NULL,
                           type = NULL,
                           title = NULL,
                           clear = FALSE) {
  # Input validation for version
  if (is.null(version)) {
    stop("'version' cannot be NULL")
  }
  if (!is.character(version)) {
    stop("'version' must be a character string")
  }
  if (length(version) == 0) {
    stop("'version' must have at least one element")
  }
  if (length(version) > 1) {
    stop("'version' must be a single character value")
  }
  if (nchar(version) == 0) {
    stop("'version' cannot be an empty string")
  }

  # Normalize version (ensure it has 'v' prefix for internal use)
  version_normalized <- .version_v_add(version)

  # Input validation for other parameters (same as projr_content_update)
  if (!is.null(label)) {
    if (!is.character(label)) {
      stop("'label' must be NULL or a character vector")
    }
    if (length(label) == 0) {
      stop("'label' must have at least one element if not NULL")
    }
  }
  if (!is.null(pos)) {
    if (!is.character(pos)) {
      stop("'pos' must be NULL or a character vector")
    }
    if (length(pos) == 0) {
      stop("'pos' must have at least one element if not NULL")
    }
    invalid_pos <- pos[!pos %in% c("source", "dest")]
    if (length(invalid_pos) > 0) {
      stop(
        "'pos' must be 'source' or 'dest'. Invalid values: ",
        paste(invalid_pos, collapse = ", ")
      )
    }
  }
  if (!is.null(type)) {
    if (!is.character(type)) {
      stop("'type' must be NULL or a character vector")
    }
    if (length(type) == 0) {
      stop("'type' must have at least one element if not NULL")
    }
    if (length(type) > 1) {
      stop("'type' must be a single character value")
    }
    valid_types <- c("local", "osf", "github")
    if (!type %in% valid_types) {
      stop(
        "'type' must be one of: ",
        paste(valid_types, collapse = ", ")
      )
    }
  }
  if (!is.null(title)) {
    if (!is.character(title)) {
      stop("'title' must be NULL or a character vector")
    }
    if (length(title) == 0) {
      stop("'title' must have at least one element if not NULL")
    }
    if (length(title) > 1) {
      stop("'title' must be a single character value")
    }
  }

  .title <- title
  if (!file.exists(.path_get("manifest.csv"))) {
    msg <- "No manifest.csv file found, so no builds have occurred, so nothing to restore."
    msg <- if (clear) paste0(msg, " No files cleared.") else msg
    .cli_debug(msg)
    stop("", .call = FALSE)
  }

  label <- .content_update_get_label(label)

  # Handle case where no labels to restore
  if (length(label) == 0) {
    msg <- "No labels found to restore."
    msg <- if (clear) paste0(msg, " No files cleared.") else msg
    .cli_debug(msg)
    stop("", .call = FALSE)
  }

  success <- TRUE
  for (i in seq_along(label)) {
    if (clear) {
      .cli_debug(
        "Clearing existing files for label: ", label[[i]]
      )
      path_dir_local <- projr_path_get_dir(
        label[[i]],
        safe = FALSE, create = FALSE
      )
      if (dir.exists(path_dir_local)) {
        unlink(path_dir_local, recursive = TRUE, force = TRUE)
      }
    }
    .cli_debug(
      "Restoring label: ", label[[i]], " at version: ", version_normalized
    )
    result <- tryCatch(
      .content_update_label(label[[i]], pos, type, .title, version_normalized),
      error = function(e) {
        .cli_debug(
          "Error restoring label: ", label[[i]], " - ", e$message
        )
        FALSE
      }
    )
    if (isFALSE(result)) {
      success <- FALSE
    }
  }
  invisible(success)
}

.content_update_get_label <- function(label) {
  .content_update_get_label_check(label)
  if (!is.null(label)) {
    return(label)
  }
  # default to raw labels, if none other requested
  nm_vec <- .yml_dir_get(NULL) |>
    names()
  nm_vec[grepl("^raw", .dir_label_strip(nm_vec))]
}

.content_update_get_label_check <- function(label) {
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

.content_update_label <- function(label, pos, type, .title, version = NULL) {
  if (!.content_update_label_check_non_empty(label)) {
    return(invisible(FALSE))
  }
  # get source remote (type and title)
  source_vec <- .content_update_label_get_source(pos, label, type, .title)

  # Check if source was found
  if (is.null(source_vec)) {
    .cli_debug(
      "No source found for label: ", label
    )
    .cli_debug(
      "Skipping restore for label: ", label
    )
    return(invisible(FALSE))
  }

  yml_title <- .yml_dest_get_title_complete(
    source_vec[["title"]], source_vec[["type"]], NULL, FALSE, FALSE
  )
  remote_pre <- .remote_final_get(
    source_vec[["type"]], yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]], NULL, TRUE
  )

  # If version is provided, use it; otherwise, detect the latest version
  if (!is.null(version)) {
    # Use the provided version (should already be normalized with 'v' prefix)
    version_remote <- .version_v_rm(version)
    .cli_debug(
      "Using specified version ", version_remote, " for ", label
    )
  } else {
    # Detect latest version automatically
    version_remote <- if (yml_title[["structure"]] == "latest") {
      .remote_get_version_latest_label_non_project_file(
        remote_pre, source_vec[["type"]], label
      )
    } else {
      .remote_get_version_latest_label_non_project_archive(
        remote_pre, source_vec[["type"]], label, "archive"
      )
    }
  }

  if (is.null(version_remote)) {
    .cli_debug("No version found for ", label)
    .cli_debug("Skipping restore for ", label)
    return(invisible(FALSE))
  }
  .cli_debug(
    "Restoring version ", version_remote, " of ", label,
    " from ", source_vec[["type"]],
    " ", source_vec[["title"]]
  )
  # Check if version is marked as untrusted in VERSION file
  untrusted <- .remote_check_version_untrusted(
    remote_pre, source_vec[["type"]], label
  )
  if (untrusted) {
    .cli_info("Note: This version is marked as untrusted")
  }

  remote_exists <- .remote_final_check_exists(
    source_vec[["type"]], yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]], version_remote,
    empty = FALSE
  )
  if (!remote_exists) {
    .cli_debug("Remote source does not exist for ", label)
    if (source_vec[["type"]] == "github") {
      .cli_debug("Checking GitHub direct for empty asset for ", label)
      remote_empty_exists <- .remote_final_check_exists(
        source_vec[["type"]], yml_title[["id"]], label,
        yml_title[["structure"]], yml_title[["path"]],
        yml_title[["path-append-label"]], version_remote,
        TRUE
      )
      if (remote_empty_exists) {
        .cli_debug("Remote source is empty for ", label)
        return(invisible(FALSE))
      } else {
        .cli_debug("Empty remote source does not exist for ", label)
        return(invisible(FALSE))
      }
    } else {
      .cli_debug("Remote source does not exist for ", label)
      return(invisible(FALSE))
    }
  }

  # Restore directly into the project directory rather than the cache build area
  remote_source <- .remote_final_get(
    source_vec[["type"]], yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]], version_remote,
    FALSE, FALSE
  )
  path_dir_local <- projr_path_get_dir(label, safe = FALSE)
  .remote_file_get_all(source_vec[["type"]], remote_source, path_dir_local)

  invisible(TRUE)
}

.content_update_label_check_non_empty <- function(label) {
  manifest <- .manifest_read(.path_get("manifest.csv")) |>
    .manifest_filter_label(label)
  fn_vec <- unique(manifest[["fn"]])
  label_recorded_and_unused <- .is_len_1(fn_vec) && fn_vec == ""
  label_unrecorded <- .is_len_0(fn_vec)
  nothing_to_restore <- label_recorded_and_unused || label_unrecorded
  if (nothing_to_restore) {
    .cli_info("No files kept in {label}")
    .cli_info("Skipping restore for {label}")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.content_update_label_get_source <- function(pos, label, type, .title) {
  pos <- if (is.null(pos)) c("source", "dest") else pos
  .assert_in(pos, c("source", "dest"))
  if ("source" %in% pos) {
    source_vec <- .content_update_label_get_source_source(label, type, .title)
    if (!is.null(source_vec)) {
      return(source_vec)
    }
  }
  if ("dest" %in% pos) {
    source_vec <- .content_update_label_get_source_dest(label, type, .title)
    if (!is.null(source_vec)) {
      return(source_vec)
    }
  }
  # if we get here, then we have no source
  # to restore from
  # so we return NULL
  NULL
}

.content_update_label_get_source_source <- function(label, type, .title) {
  yml_source <- .yml_dir_get_source(label, NULL)
  # nothing to look at here
  if (is.null(yml_source)) {
    return(NULL)
  }
  # look within the type
  .content_update_label_get_source_source_type(
    yml_source, type, .title
  )
}

.content_update_label_get_source_source_type <- function(yml_source, type, .title) {
  if (!is.null(type)) {
    .content_update_label_get_source_source_type_spec(
      yml_source, type, .title
    )
  } else {
    .content_update_label_get_source_source_type_first(yml_source, .title)
  }
}

.content_update_label_get_source_source_type_spec <- function(yml_source,
                                                              type,
                                                              .title) {
  if (type %in% names(yml_source)) {
    yml_type <- yml_source[[type]]
    .content_update_label_get_source_source_title_spec(
      yml_type, type, .title
    )
  } else {
    NULL
  }
}

.content_update_label_get_source_source_title <- function(yml_type,
                                                          type,
                                                          .title) {
  if (!is.null(.title)) {
    .content_update_label_get_source_source_title_spec(yml_type, type, .title)
  } else {
    .content_update_label_get_source_source_title_first(yml_type, type)
  }
}

.content_update_label_get_source_source_title_spec <- function(yml_type, type, .title) {
  if (is.list(yml_type)) {
    .content_update_label_get_source_source_title_spec_list(
      yml_type, type, .title
    )
  } else if (all(is.character(yml_type))) {
    .content_update_label_get_source_source_title_spec_chr(
      yml_type, type, .title
    )
  } else {
    NULL
  }
}

.content_update_label_get_source_source_title_spec_list <- function(yml_type, type, .title) { # nolint
  if (.title %in% names(yml_type)) {
    c("pos" = "source", "type" = type, "title" = .title)
  } else {
    NULL
  }
}

.content_update_label_get_source_source_title_spec_chr <- function(yml_type, type, .title) { # nolint
  if (.title %in% yml_type) {
    c("pos" = "source", "type" = type, "title" = .title)
  } else {
    NULL
  }
}

.content_update_label_get_source_source_title_first <- function(yml_type, type) {
  if (all(is.character(yml_type))) {
    c("pos" = "source", "type" = type, "title" = yml_type[[1]])
  } else {
    c("pos" = "source", "type" = type, "title" = names(yml_type)[[1]])
  }
}

.content_update_label_get_source_source_type_first <- function(yml_source, .title) {
  type <- names(yml_source)[[1]]
  .content_update_label_get_source_source_type_spec(
    yml_source[[type]], type, .title
  )
}

# try find restore from destination

.content_update_label_get_dest_no_type <- function(label) {
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

.content_update_label_get_source_dest <- function(label, type, .title) {
  type_vec <- .content_update_label_get_source_dest_get_type(type, label)
  if (.is_len_0(type_vec)) {
    return(.content_update_label_get_dest_no_type(label))
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
      .content_update_label_get_dest_no_type(label),
      error = function(e) {
        .cli_info("No source found for {label}")
        .cli_info("Skipping restore for {label}")
        return(invisible(FALSE))
      }
    )
  } else {
    c("pos" = "dest", "type" = tp_first, "title" = tt_first)
  }
}

.content_update_label_get_source_dest_get_type <- function(type, label) {
  if (!is.null(type)) {
    return(type)
  }
  nm_vec <- .yml_build_get(NULL) |> names()
  nm_vec[grepl("^github$|^local$|^osf", nm_vec)]
}

.content_update_label_get_source_dest_get_title <- function(type, title) {
  if (!is.null(title)) {
    return(title)
  }
}

# Function to check if a version is marked as untrusted in VERSION file
.remote_check_version_untrusted <- function(remote_pre, type, label) {
  version_file <- .remote_get_version_file(type, remote_pre)
  match_str <- utils::glob2rx(label) |>
    (\(x) gsub("\\$", "", x))() |>
    paste0(": ")
  label_regex <- grep(match_str, version_file, value = TRUE)
  if (.is_len_0(label_regex)) {
    return(FALSE)
  }
  # Return TRUE if ends in an asterisk
  grepl("\\*$", label_regex)
}
