#' @rdname projr_restore
#' @export
projr_restore_repo <- function(repo,
                               path = NULL,
                               label = NULL,
                               pos = NULL,
                               type = NULL,
                               title = NULL) {
  # Input validation
  if (is.null(repo)) {
    stop("'repo' cannot be NULL")
  }
  if (!is.character(repo)) {
    stop("'repo' must be a character string")
  }
  if (length(repo) == 0) {
    stop("'repo' must have at least one element")
  }
  if (length(repo) > 1) {
    stop("'repo' must be a single character value")
  }
  if (nchar(repo) == 0) {
    stop("'repo' cannot be an empty string")
  }

  if (!is.null(path)) {
    if (!is.character(path)) {
      stop("'path' must be NULL or a character string")
    }
    if (length(path) > 1) {
      stop("'path' must be a single character value")
    }
    if (nchar(path) == 0) {
      stop("'path' cannot be an empty string")
    }
  }

  # Validate label, pos, type, title (same as projr_restore)
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
    valid_types <- c("local", "github")
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
  result <- tryCatch(
    {
      .git_clone(repo, path)
      .restore_repo_labels(path, label, pos, type, .title)
    },
    error = function(e) {
      .cli_info("Error in projr_restore_repo: {e$message}")
      return(FALSE)
    }
  )
  invisible(result)
}

#' @rdname projr_restore
#' @export
projr_restore_repo_wd <- function(repo,
                                  label = NULL,
                                  pos = NULL,
                                  type = NULL,
                                  title = NULL) {
  projr_restore_repo(repo, path = ".", label = label, pos = pos, type = type, title = title)
}

.restore_repo_labels <- function(path,
                                 label,
                                 pos,
                                 type,
                                 .title) {
  if (!is.null(path)) {
    orig_wd <- getwd()
    on.exit(setwd(orig_wd), add = TRUE)
    setwd(path)
  }
  projr_content_update(label, pos, type, .title)
  invisible(TRUE)
}
