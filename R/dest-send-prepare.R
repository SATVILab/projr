# Prepare GitHub releases before sending to destinations
.dest_prepare_github_releases <- function(
  bump_component,
  archive_github,
  archive_local,
  always_archive,
  strict
) {
  # Early exit if not an output build
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # Check if GitHub is one of the destination types
  type_vec <- .dest_send_get_type(archive_github, archive_local)

  if (!"github" %in% type_vec) {
    return(invisible(FALSE))
  }

  archive_type <- .dest_send_get_archive_type(
    "github",
    archive_github,
    archive_local
  )
  titles <- .dest_send_publish_titles(
    "github",
    bump_component,
    archive_type,
    always_archive
  )
  targets <- .dest_github_targets(archive_github, always_archive, NULL, titles)
  if (.is_len_0(targets)) {
    return(invisible(FALSE))
  }

  if (strict && !nzchar(.auth_get_github_pat_find())) {
    stop(
      "Publishing a GitHub release requires a token with write access to the destination repository.",
      call. = FALSE
    )
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
.dest_github_tags_needed <- function(
  archive_github,
  always_archive,
  profile = NULL
) {
  targets <- .dest_github_targets(archive_github, always_archive, profile)
  unique(vapply(targets, `[[`, character(1), "tag", USE.NAMES = FALSE))
}

.dest_github_targets <- function(
  archive_github,
  always_archive,
  profile,
  titles = .dest_send_type_get_title("github", archive_github, profile)
) {
  yml_github <- .yml_dest_get_type("github", profile)

  targets <- list()
  for (title in titles) {
    yml_title <- .yml_dest_get_title_complete(
      title = title,
      type = "github",
      profile = profile,
      archive_type = if (title %in% names(yml_github)) {
        FALSE
      } else {
        archive_github
      },
      always_archive = always_archive
    )

    tag <- yml_title[["id"]]
    repo <- yml_title[["repo"]]
    key <- .dest_github_target_key(repo, tag)
    target <- list(tag = tag, repo = repo, branch = yml_title[["branch"]])
    if (
      key %in%
        names(targets) &&
        !identical(targets[[key]][["branch"]], target[["branch"]])
    ) {
      stop(
        "Conflicting target branches for GitHub release '",
        tag,
        "'.",
        call. = FALSE
      )
    }
    targets[[key]] <- target
  }
  targets
}

.dest_github_target_key <- function(repo, tag) {
  paste(tolower(repo %||% ""), tag, sep = ":")
}
