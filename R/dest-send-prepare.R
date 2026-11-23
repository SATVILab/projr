# Prepare GitHub releases before sending to destinations
.dest_prepare_github_releases <- function(bump_component,
                                          archive_github,
                                          archive_local,
                                          always_archive,
                                          strict) {
  # Early exit if not an output build
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # Check if GitHub is one of the destination types
  type_vec <- .dest_send_get_type(archive_github, archive_local)

  if (!"github" %in% type_vec) {
    return(invisible(FALSE))
  }

  targets <- .dest_github_targets(archive_github, always_archive, NULL)
  if (.is_len_0(targets)) {
    return(invisible(FALSE))
  }

  if (strict && !nzchar(.auth_get_github_pat_find())) {
    stop("Publishing a GitHub release requires a token with write access to the destination repository.", call. = FALSE)
  }

  for (target in targets) {
    .with_github_repo(target[["repo"]], {
      tag <- target[["tag"]]
      if (!.remote_check_exists("github", id = tag)) {
        .cli_debug("GitHub release preparation: Creating missing tag '{tag}'")
        if (is.null(target[["branch"]])) {
          .remote_create("github", tag)
        } else {
          .remote_create("github", tag, branch = target[["branch"]])
        }
      } else {
        .cli_debug("GitHub release preparation: Tag '{tag}' already exists")
      }
    })
  }
}

# Derive needed GitHub tags from configuration
.dest_github_tags_needed <- function(archive_github,
                                     always_archive,
                                     profile = NULL) {
  targets <- .dest_github_targets(archive_github, always_archive, profile)
  unique(vapply(targets, `[[`, character(1), "tag", USE.NAMES = FALSE))
}

.dest_github_targets <- function(archive_github, always_archive, profile) {
  # 1. From YAML-configured build.github destinations
  yml_github <- .yml_dest_get_type("github", profile)
  titles <- names(yml_github)

  # 2. From archive_github parameter, if no "archive" title already
  if (!isFALSE(archive_github) &&
    !is.null(archive_github) &&
    !"archive" %in% titles) {
    titles <- c(titles, "archive")
  }

  targets <- list()
  for (title in titles) {
    yml_title <- .yml_dest_get_title_complete(
      title = title,
      type = "github",
      profile = profile,
      archive_type = if (title %in% names(yml_github)) FALSE else archive_github,
      always_archive = always_archive
    )

    tag <- yml_title[["id"]]
    repo <- yml_title[["repo"]]
    key <- paste(repo %||% "", tag, sep = ":")
    target <- list(tag = tag, repo = repo, branch = yml_title[["branch"]])
    if (key %in% names(targets) && !identical(targets[[key]], target)) {
      stop("Conflicting target branches for GitHub release '", tag, "'.", call. = FALSE)
    }
    targets[[key]] <- target
  }
  targets
}
