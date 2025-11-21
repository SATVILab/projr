# Prepare GitHub releases before sending to destinations
.dest_prepare_github_releases <- function(bump_component,
                                          archive_github,
                                          archive_local,
                                          strict,
                                          output_level = "std",
                                          log_file = NULL) {
  # Early exit if not an output build
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # Check if GitHub is one of the destination types
  type_vec <- .dest_send_get_type(archive_github, archive_local)

  if (!"github" %in% type_vec) {
    .cli_debug(
      "GitHub release preparation: Skipped (no GitHub destinations)",
      output_level = output_level,
      log_file = log_file
    )
    return(invisible(FALSE))
  }

  tags <- .dest_github_tags_needed(archive_github)
  if (.is_len_0(tags)) {
    .cli_debug(
      "GitHub release preparation: No tags required (from config)",
      output_level = output_level,
      log_file = log_file
    )
    return(invisible(FALSE))
  }

  .cli_debug(
    "GitHub release preparation: Starting for {length(tags)} tag(s)",
    output_level = output_level,
    log_file = log_file
  )

  for (x in tags) {
    .cli_debug(
      "GitHub release preparation: Preparing tag '{x}'",
      output_level = output_level,
      log_file = log_file
    )
    if (!.remote_check_exists("github", id = x)) {
      .cli_debug(
        "GitHub release preparation: Creating missing tag '{x}'",
        output_level = output_level,
        log_file = log_file
      )
      .remote_create("github", x)
    } else {
      .cli_debug(
        "GitHub release preparation: Tag '{x}' already exists",
        output_level = output_level,
        log_file = log_file
      )
    }
  }
}

# Derive needed GitHub tags from configuration
.dest_github_tags_needed <- function(archive_github, profile = NULL) {
  # 1. From YAML-configured build.github destinations
  yml_github <- .yml_dest_get_type("github", profile)
  titles <- names(yml_github)

  # 2. From archive_github parameter, if no "archive" title already
  if (!isFALSE(archive_github) &&
        !is.null(archive_github) &&
        !"archive" %in% titles) {
    titles <- c(titles, "archive")
  }

  if (.is_len_0(titles)) {
    return(character(0L))
  }

  tag_vec <- character(0L)
  for (title in titles) {
    yml_title <- .yml_dest_get_title_complete(
      title = title,
      type  = "github",
      profile = profile,
      archive_type = archive_github,
      always_archive = NULL
    )

    id <- yml_title[["id"]]  # configured id/title
    tag <- .remote_misc_github_tag_get(id)
    tag <- .remote_misc_github_tag_format(tag)
    tag_vec <- c(tag_vec, tag)
  }

  unique(tag_vec)
}


