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
    .cli_debug(
      "GitHub release preparation: Skipped (no GitHub destinations)"
    )
    return(invisible(FALSE))
  }

  targets <- .dest_github_targets(archive_github, always_archive, NULL)
  tags <- names(targets)
  if (.is_len_0(tags)) {
    .cli_debug(
      "GitHub release preparation: No tags required (from config)"
    )
    return(invisible(FALSE))
  }

  .cli_debug(
    "GitHub release preparation: Starting for {length(tags)} tag(s)"
  )

  for (x in tags) {
    .cli_debug(
      "GitHub release preparation: Preparing tag '{x}'"
    )
    if (!.remote_check_exists("github", id = x)) {
      .cli_debug(
        "GitHub release preparation: Creating missing tag '{x}'"
      )
      branch <- targets[[x]]
      if (is.null(branch)) {
        .remote_create("github", x)
      } else {
        .remote_create("github", x, branch = branch)
      }
    } else {
      .cli_debug(
        "GitHub release preparation: Tag '{x}' already exists"
      )
    }
  }
}

# Derive needed GitHub tags from configuration
.dest_github_tags_needed <- function(archive_github,
                                     always_archive,
                                     profile = NULL) {
  as.character(names(.dest_github_targets(archive_github, always_archive, profile)))
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
    branch <- yml_title[["branch"]]
    if (tag %in% names(targets) && !identical(targets[[tag]], branch)) {
      stop("Conflicting target branches for GitHub release '", tag, "'.", call. = FALSE)
    }
    targets[tag] <- list(branch)
  }
  targets
}
