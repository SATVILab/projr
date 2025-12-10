# ====================================================
# Send to all request remotes
# ====================================================

#' @title Send build outputs to configured destinations
#' @description Top-level dispatcher that evaluates `_projr.yml` destinations and
#'   optional function parameters to decide which remotes should receive updated
#'   content.
#' @param bump_component Character describing the version bump trigger (passed
#'   to `.build_get_output_run()`).
#' @param archive_github Logical or character. When `TRUE`, archive all GitHub
#'   content; when character vector, archive specific labels. `FALSE`/`NULL`
#'   leaves behavior to `_projr.yml`.
#' @param archive_local Logical/character equivalent for local destinations.
#' @param always_archive Logical flag forcing archive behavior even when YAML is
#'   silent; `NULL` defers to YAML definitions.
#' @param output_level CLI verbosity level (`"none"`, `"std"`, `"debug"`).
#' @return Invisibly returns `FALSE` when no destinations are processed or `TRUE`
#'   when at least one send action occurs.
#' @keywords internal
#' @noRd
.dest_send <- function(bump_component,
                       archive_github,
                       archive_local,
                       always_archive,
                       output_level = "std") {
  # consider early exit
  # ------------------
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # loop over types of remotes
  type_vec <- .dest_send_get_type(archive_github, archive_local)

  .cli_debug(
    "Starting destination send process for {length(type_vec)} remote type(s): {paste(type_vec, collapse = ', ')}",
    output_level = output_level
  )

  for (type in type_vec) {
    .cli_debug(
      "Processing remote type: {type}",
      output_level = output_level
    )

    # force archive_type to FALSE if it
    # there is a destination with title "archive"
    # of the same type as `type`.
    # archive_type essentially replaces
    # archive_github and archive_local,
    # and carries info as to which
    # content to archive (TRUE means all, otherwise
    # a character vector with the content names).
    archive_type <- .dest_send_get_archive_type(
      type, archive_github, archive_local
    )
    # check if we are always archiving based on the parameter.
    # this may not be that important...
    always_archive <- .dest_send_get_always_archive(type, always_archive)
    .dest_send_type(
      type, bump_component, archive_type, always_archive,
      output_level
    )
  }
}

#' @title Determine whether send operations should run
#' @description Wrapper around `.build_get_output_run()` to allow early exit when
#'   no build outputs are available for sending.
#' @inheritParams .dest_send
#' @return Logical flag from `.build_get_output_run()`.
#' @keywords internal
#' @noRd
.dest_send_check <- function(bump_component) {
  .build_get_output_run(bump_component) # nolint
}

#' @title Determine which remote types should be processed
#' @description Combines `_projr.yml` definitions with function parameters to
#'   produce the list of remote types that require sends.
#' @inheritParams .dest_send
#' @return Character vector of remote types.
#' @keywords internal
#' @noRd
.dest_send_get_type <- function(archive_github, archive_local) {
  type_vec_yml <- .dest_send_get_type_yml()
  type_vec_param <- .dest_send_get_type_param(archive_github, archive_local)
  unique(c(type_vec_yml, type_vec_param))
}

#' @title Extract remote types from YAML configuration
#' @description Reads `_projr.yml` build destinations and returns available
#'   remote type names.
#' @return Character vector of remote types configured in YAML.
#' @keywords internal
#' @noRd
.dest_send_get_type_yml <- function() {
  .dest_send_get_type_opt()[
    .dest_send_get_type_opt() %in% names(.yml_build_get(NULL))
  ]
}

#' @title Derive remote types from function parameters
#' @description Determines whether the `archive_github` or `archive_local`
#'   parameters explicitly request processing for their respective remote types.
#' @inheritParams .dest_send
#' @return Character vector containing "github" and/or "local" when requested.
#' @keywords internal
#' @noRd
.dest_send_get_type_param <- function(archive_github, archive_local) {
  out_vec <- character(0L)
  archive_github_lgl <- isTRUE(archive_github) ||
    (is.character(archive_github) && length(archive_github) > 0L)
  archive_local_lgl <- isTRUE(archive_local) ||
    (is.character(archive_local) && length(archive_local) > 0L)
  if (archive_github_lgl) {
    out_vec <- c(out_vec, "github")
  }
  if (archive_local_lgl) {
    out_vec <- c(out_vec, "local")
  }
  if (.is_len_0(out_vec)) {
    return(character(0L))
  }
  out_vec
}

#' @title List all supported destination remote types
#' @description Returns the canonical remote types understood by the destination
#'   system.
#' @return Character vector of supported remote types.
#' @keywords internal
#' @noRd
.dest_send_get_type_opt <- function() {
  c("local", "github", "osf")
}

#' @title Resolve archive behavior for a remote type
#' @description Determines whether archive uploads should occur for a specific
#'   remote type based on parameters and `_projr.yml` configuration.
#' @param type Remote type being processed (from `.dest_send_get_type()`).
#' @inheritParams .dest_send
#' @return `FALSE` when archives are controlled by YAML or not requested;
#'   otherwise returns `TRUE` or a character vector of labels specifying archive
#'   behavior.
#' @keywords internal
#' @noRd
.dest_send_get_archive_type <- function(type,
                                        archive_github,
                                        archive_local) {
  # Essentially, here we check whether we are
  # archiving based on a parameter call,
  # and if we are, we return either TRUE (meaning
  # archive all content) or a character (archive
  # only specified contents).
  # FALSE means we are not archiving based on a parameter call,
  # which means either the user did not ask for it
  # or the _projr.yml file already specifies the archive
  # with the title "archive" for the type.
  # Will activate for either only if the
  # current type corresponds (e.g. type is "github"
  # for archive_github) and
  # if the parameter (archive_github/archive_local) is:
  # - not FALSE (means we didn't ask for uploads based on the parameter)
  #   - it could be TRUE (all content) or character
  # - not NULL (same as above)
  # - and if the type is not already specified in the _projr.yml
  #  - _projr.yml takes precedence over the parameters.
  switch(type,
    "github" = .dest_send_check_is_archive_param_github(archive_github),
    "local"  = .dest_send_check_is_archive_param_local(archive_local),
    FALSE
  )
}

#' @title Check if GitHub archive behavior is parameter-driven
#' @description Returns the GitHub archive parameter when it should override
#'   YAML settings; otherwise returns `FALSE`.
#' @inheritParams .dest_send
#' @return `FALSE`, `TRUE`, or a character vector depending on the parameter.
#' @keywords internal
#' @noRd
.dest_send_check_is_archive_param_github <- function(archive_github) {
  is_param <- !isFALSE(archive_github) && !is.null(archive_github) &&
    !"archive" %in% names(.yml_dest_get_type("github", NULL))
  if (is_param) archive_github else FALSE
}

#' @title Check if local archive behavior is parameter-driven
#' @description Equivalent to `.dest_send_check_is_archive_param_github()` but
#'   for local destinations.
#' @inheritParams .dest_send
#' @return `FALSE`, `TRUE`, or a character vector depending on the parameter.
#' @keywords internal
#' @noRd
.dest_send_check_is_archive_param_local <- function(archive_local) {
  is_param <- !isFALSE(archive_local) && !is.null(archive_local) &&
    !"archive" %in% names(.yml_dest_get_type("local", NULL))
  if (is_param) archive_local else FALSE
}

#' @title Resolve always-archive override for a remote type
#' @description Prevents function parameters from overriding explicit YAML
#'   archive definitions while still allowing global "always archive" behavior
#'   when YAML is silent.
#' @param type Remote type identifier.
#' @param always_archive Logical flag passed to `.dest_send()`.
#' @return `NULL` when YAML controls behavior; otherwise the `always_archive`
#'   flag.
#' @keywords internal
#' @noRd
.dest_send_get_always_archive <- function(type, always_archive) {
  if ("archive" %in% names(.yml_dest_get_type(type, NULL))) {
    # if it was specified, then we don't want to
    # override it with the parameter
    NULL
  } else {
    # specified based on a parameter
    always_archive
  }
}

# ====================================================
# Send to a single request remotes
# ====================================================

#' @title Send all titles for a specific remote type
#' @description Retrieves the titles that require processing for the given
#'   remote type and forwards each one to `.dest_send_title()`.
#' @param type Remote type identifier (e.g., "github").
#' @param archive_type Logical flag or character vector describing the archive
#'   request for this remote type.
#' @param always_archive Logical override when YAML omits archive titles.
#' @inheritParams .dest_send
#' @return Invisibly returns `FALSE` when no titles are processed or `TRUE`
#'   otherwise.
#' @keywords internal
#' @noRd
.dest_send_type <- function(type,
                            bump_component,
                            archive_type,
                            always_archive,
                            output_level = "std") {
  # ensure that these are not NULL only if not
  # specified in _projr.yml. Reaason is that,
  # if they are specified in the `yml`, the settings
  # in the `yml` will be used, so we want to make
  # that consistent that the `content` setting is
  # also specified in the `yml`.
  # so, the parameters are not overrides.
  title_vec <- .dest_send_type_get_title(type, archive_type)

  .cli_debug(
    "Remote type '{type}': Processing {length(title_vec)} destination(s): {paste(title_vec, collapse = ', ')}",
    output_level = output_level
  )

  for (x in title_vec) {
    .dest_send_title(
      x, type, bump_component, archive_type, always_archive,
      output_level
    )
  }
}
#' @title Determine titles to process for a remote type
#' @description Merges YAML-defined titles with parameter-driven titles (such as
#'   archive requests) and returns the unique set.
#' @param type Remote type identifier.
#' @param archive_type Archive override information returned by
#'   `.dest_send_get_archive_type()`.
#' @return Character vector of titles to process.
#' @keywords internal
#' @noRd
.dest_send_type_get_title <- function(type,
                                      archive_type) {
  # get all titles to upload, for the type
  nm_vec_yml <- names(.yml_dest_get_type(type, NULL))
  nm_vec_param <- .dest_send_type_get_title_param(archive_type)
  unique(c(nm_vec_yml, nm_vec_param))
}
#' @title Titles from YAML configuration for a remote type
#' @description Helper that simply returns the names under the remote type entry
#'   in `_projr.yml`.
#' @param type Remote type identifier.
#' @return Character vector of titles from YAML.
#' @keywords internal
#' @noRd
.dest_send_type_get_title_yml <- function(type) {
  .yml_dest_get_type(type, NULL) |>
    names()
}
#' @title Titles implied by function parameters
#' @description When archive behavior is requested directly through function
#'   parameters, this helper signals that an "archive" title should be
#'   processed even if YAML does not list it.
#' @param archive_type Archive override information.
#' @return Character vector containing "archive" or empty vector.
#' @keywords internal
#' @noRd
.dest_send_type_get_title_param <- function(archive_type) {
  is_archive_lgl <- isTRUE(archive_type) ||
    (is.character(archive_type) && length(archive_type) > 0L)
  if (is_archive_lgl) "archive" else character(0L)
}

# ====================================================
# Send to a single final remote
# ====================================================

#' @title Send content for a single destination title
#' @description Applies cue logic to decide whether a destination title should
#'   run, determines which content labels to send, and iterates over
#'   `.dest_send_label()`.
#' @param title Destination title (i.e., label) under the remote type.
#' @inheritParams .dest_send_type
#' @inheritParams .dest_send
#' @return Invisibly returns `FALSE` when skipped or `TRUE` when processed.
#' @keywords internal
#' @noRd
.dest_send_title <- function(title,
                             type,
                             bump_component,
                             archive_type,
                             always_archive,
                             output_level = "std") {
  force(title)

  .cli_debug(
    "Destination '{title}' (type: {type}): Checking if send is needed",
    output_level = output_level
  )

  may_send <- .dest_send_title_check(
    title, type, bump_component, archive_type, always_archive
  )

  if (!may_send) {
    .cli_debug(
      "Destination '{title}': Send SKIPPED (cue condition not met)",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  content_vec <- .dest_send_title_get_content(
    title, type, archive_type
  )

  .cli_debug(
    "Destination '{title}': Send APPROVED - Processing {length(content_vec)} content label(s): {paste(content_vec, collapse = ', ')}",
    output_level = output_level
  )

  for (x in content_vec) {
    .dest_send_label(
      x, title, type, .build_get_output_run(bump_component),
      archive_type, always_archive, x == content_vec[length(content_vec)],
      output_level
    )
  }

  invisible(TRUE)
}
#' @title Evaluate cue logic for a destination title
#' @description Uses `_projr.yml` metadata to determine if the destination title
#'   should run for the current build.
#' @inheritParams .dest_send_title
#' @return Logical indicating whether content should be sent.
#' @keywords internal
#' @noRd
.dest_send_title_check <- function(title,
                                   type,
                                   bump_component,
                                   archive_type,
                                   always_archive) {
  force(title)
  .yml_dest_get_title_complete(
    title, type, NULL, archive_type, always_archive
  )[["cue"]] |>
    .is_cue(bump_component)
}
#' @title Determine content labels for a destination title
#' @description Chooses whether to rely on YAML content definitions or parameter
#'   overrides (e.g., archive requests) for the supplied title.
#' @inheritParams .dest_send_title
#' @return Character vector of content labels to send.
#' @keywords internal
#' @noRd
.dest_send_title_get_content <- function(title,
                                         type,
                                         archive_type) {
  force(title)
  is_param_content <- .dest_send_title_get_content_check_yml(
    title, archive_type
  )
  if (is_param_content) {
    .dest_send_title_get_content_param(archive_type)
  } else {
    .dest_send_title_get_content_yml(title, type)
  }
}
#' @title Check if content should come from parameters
#' @description Detects whether the `archive_type` parameter should define the
#'   content labels for the supplied title (only when the title is "archive").
#' @inheritParams .dest_send_title
#' @return Logical flag: `TRUE` means parameters drive content, `FALSE` means
#'   YAML definitions apply.
#' @keywords internal
#' @noRd
.dest_send_title_get_content_check_yml <- function(title,
                                                   archive_type) {
  # we are not using yml if the title is "archive"
  # and archive_type is not FALSE (i.e. it has been specified
  # to request something and we've already confirmed
  # before that there is not corresponding type with title "archive").
  # Returns `TRUE` if we are using the parameter
  # to get the content, and `FALSE` if we are using the
  # yml file.
  title == "archive" && !isFALSE(archive_type)
}
#' @title Content labels from YAML for a destination title
#' @description Reads the `content` node under the specified title and normalises
#'   it to a character vector.
#' @inheritParams .dest_send_title
#' @return Character vector of content labels.
#' @keywords internal
#' @noRd
.dest_send_title_get_content_yml <- function(title, type) {
  force(title)
  content <- .yml_dest_get_title(title, type, NULL)[["content"]]
  if (is.null(content) || isFALSE(content)) {
    return(character(0L))
  } else if (isTRUE(content)) {
    .dest_send_title_get_content_auto()
  } else {
    content
  }
}
#' @title Content labels derived from archive parameters
#' @description When archive behavior is driven by function parameters, this
#'   helper returns the requested content labels or resolves to automatic
#'   detection when `TRUE`.
#' @param archive_type Archive override information (logical or character).
#' @return Character vector of content labels.
#' @keywords internal
#' @noRd
.dest_send_title_get_content_param <- function(archive_type) {
  if (.is_chr(archive_type)) {
    archive_type
  } else {
    .dest_send_title_get_content_auto()
  }
}
#' @title Auto-detect content labels for archive operations
#' @description Builds the list of content labels that qualify for archive sends
#'   by inspecting directory classes.
#' @return Character vector of detected content labels.
#' @keywords internal
#' @noRd
.dest_send_title_get_content_auto <- function() {
  yml_dir <- .yml_dir_get(NULL)
  nm_vec <- names(yml_dir) |>
    c("docs") |>
    unique()
  nm_vec_output <- nm_vec[.yml_dir_label_class_detect_output(nm_vec)]
  nm_vec_raw <- nm_vec[.yml_dir_label_class_detect_raw(nm_vec)]
  nm_vec_docs <- nm_vec[.yml_dir_label_class_detect_docs(nm_vec)]
  c(nm_vec_output, nm_vec_raw, nm_vec_docs)
}
