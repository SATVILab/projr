.build_check_remote_versions <- function(
  proposed,
  bump_component,
  archive_github,
  archive_local,
  always_archive
) {
  selection <- .yml_restrictions_get_not_behind_remote(NULL)
  if (isFALSE(selection)) {
    return(invisible(FALSE))
  }
  types <- .dest_send_get_type_opt()
  archives <- stats::setNames(
    lapply(types, .dest_send_get_archive_type, archive_github, archive_local),
    types
  )
  if (isTRUE(selection)) {
    selected <- lapply(types, function(type) {
      .dest_send_publish_titles(
        type,
        bump_component,
        archives[[type]],
        .dest_send_get_always_archive(type, always_archive)
      )
    })
    names(selected) <- types
  } else {
    selected <- .build_restriction_titles(selection, archives)
  }
  if (!sum(lengths(selected))) {
    return(invisible(FALSE))
  }

  base <- .buildlog_last_version() %||% .version_completed_file()
  proposed <- .version_v_add(proposed)
  rule <- .yml_restrictions_get_rule(NULL)
  heading <- paste0(
    "Local base: ",
    base %||% "none",
    ", proposed: ",
    proposed,
    " (",
    rule,
    ")."
  )
  workspace <- .dir_create_tmp_random()
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  report <- character()
  failed <- FALSE
  for (type in names(selected)) {
    for (title in unique(selected[[type]])) {
      version <- tryCatch(
        {
          target <- .yml_dest_get_title_complete(
            title,
            type,
            NULL,
            archives[[type]],
            .dest_send_get_always_archive(type, always_archive)
          )
          .build_remote_version(type, target, workspace)
        },
        error = identity
      )
      if (inherits(version, "error")) {
        outcome <- paste("could not verify:", conditionMessage(version))
        failed <- TRUE
      } else if (is.null(version)) {
        outcome <- "empty, check passed"
      } else {
        remote_number <- numeric_version(.version_v_rm(version))
        reason <- if (
          numeric_version(.version_v_rm(proposed)) <= remote_number
        ) {
          "proposed version must be newer"
        } else if (
          rule == "base_version_not_behind" &&
            (is.null(base) ||
              numeric_version(.version_v_rm(base)) < remote_number)
        ) {
          "local base is behind"
        }
        failed <- failed || !is.null(reason)
        outcome <- paste0(version, ", ", reason %||% "check passed")
      }
      report <- c(report, paste0(type, ".", title, ": ", outcome))
    }
  }
  if (failed) {
    stop(paste(c(heading, report), collapse = "\n"), call. = FALSE)
  }
  .cli_info("{heading}")
  for (line in report) {
    .cli_info("{line}")
  }
  invisible(TRUE)
}

.build_restriction_titles <- function(selection, archives) {
  yml <- .yml_get(NULL)
  available <- lapply(names(archives), function(type) {
    unique(c(
      names(yml$remotes[[type]]),
      names(yml$build[[type]]),
      .dest_send_type_get_title_param(archives[[type]])
    ))
  })
  names(available) <- names(archives)
  if (is.character(selection)) {
    result <- list()
    for (title in unique(selection)) {
      types <- names(Filter(function(titles) title %in% titles, available))
      if (!length(types)) {
        stop("Unknown remote title: ", title, call. = FALSE)
      }
      if (length(types) > 1L) {
        stop(
          "Ambiguous remote title: ",
          title,
          ". Specify its type (",
          paste(types, collapse = " or "),
          ").",
          call. = FALSE
        )
      }
      result[[types]] <- c(result[[types]], title)
    }
    return(result)
  }
  for (type in names(selection)) {
    missing <- setdiff(selection[[type]], available[[type]])
    if (length(missing)) {
      stop(
        "Unknown ",
        type,
        " remote title: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  selection
}

.build_remote_version <- function(type, target, workspace) {
  source <- list(type = type)
  if (type == "github") {
    source$repo <- target$repo %||% .gh_repo_get()
    source$tag <- target$id
    assets <- .remote_get_info_github_httr(
      source$repo,
      source$tag,
      missing_ok = TRUE
    )
    if (!length(assets)) {
      return(NULL)
    }
    names(assets) <- vapply(assets, `[[`, character(1), "name")
    source$assets <- names(assets)
    source$version_asset <- assets[["VERSION.zip"]]
  } else {
    source$root <- target$id
    if (!file.exists(file.path(source$root, "VERSION"))) {
      entries <- tryCatch(
        fs::dir_ls(source$root, all = TRUE),
        ENOENT = function(e) {
          if (fs::link_exists(source$root)) {
            stop(e)
          }
          character()
        }
      )
      if (!length(entries)) return(NULL)
    }
  }
  version <- .get_archive_version(source, workspace)
  if (is.null(version)) {
    stop(
      "The destination contains files but has no valid project VERSION. Inspect its contents for an incomplete upload before retrying.",
      call. = FALSE
    )
  }
  version
}
