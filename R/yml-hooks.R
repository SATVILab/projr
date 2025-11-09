#' @rdname yml-hooks
#' @title Build hook-related functions
#'
#' @description
#' Convenience functions to add or remove hooks
#' to run before or after the build.
#'
#' - .yml_hooks_add`: Add a script to run before or after the build.
#' - .yml_hooks_rm`: Remove scripts to run.
#'
#' .yml_hooks_add_pre` and .yml_hooks_add_post`
#' are wrappers around .yml_hooks_add` that set the `stage` argument
#' to `"pre"` or `"post"`, respectively.
#' .yml_hooks_rm_all` removes all scripts.
#'
#' @export
#' @param path character vector.
#' Path(s) to hooks, relative to project root (if not absolute).
#' @param title character.
#' Title for set of hooks.
#' Initial and trailing spaces are removed, and
#' the middle spaces are converted to dashes.
#' For example, `" a b "` is converted to
#' `"a-b"`. `
#' @param stage "pre" or "post".
#' Whether to run the hook before or after the build.
#' @param cue "build", "dev", "patch", "minor" or "major".
#' Which minimum build level triggers the hooks.
#' "build" and "dev" are equivalent, and
#' always trigger the scripts.
#' @param overwrite logical.
#' Whether to overwrite any hook settings
#' of the same title in the `projr`
#' configuration file.
#' If `FALSE` and there already exists
#' a key under `build/hooks` with the name
#' `title`, an error is thrown.
#' Default is `TRUE`.
#' @param profile character.
#' Profile to add the hook to.
#' If `"default"`` (the default),
#' the hook is added to the default profile,
#' which is `_projr.yml`.
#'
#'
#' @details
#' Within a stage (pre- or post-build), hooks
#' are run in the order set in `_projr.yml`.
#' They are not run in the same environment as the
#' build process.
#' The pre-build scripts are run immediately after
#' bumping the project version (if that is done) and immediately
#' before committing the present state of the code to Git.
#' The post-build scripts are run immediately after
#' committing the present state of the code to Git,
#' and before distributing project artefacts to the remotes.
projr_yml_hooks_add <- function(path,
                                 title,
                                 stage,
                                 cue = NULL,
                                 overwrite = TRUE,
                                 profile = "default") {
  .yml_hooks_check(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  )

  .yml_hooks_add(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  ) |>
    .yml_hooks_set(profile = profile)
}

.yml_hooks_check <- function(path,
                              title,
                              stage,
                              cue,
                              overwrite,
                              profile) {
  .assert_chr(path, TRUE)
  .assert_string(title, TRUE)
  .assert_in_single(stage, c("pre", "post"), TRUE)
  if (.is_given_mid(profile)) {
    .assert_string(profile)
  }
  .assert_in_single(cue, c("build", "dev", "patch", "minor", "major"))
  .assert_flag(overwrite, TRUE)
}

.yml_hooks_add <- function(path,
                            title,
                            stage,
                            cue = NULL,
                            profile = "default",
                            overwrite = TRUE) {
  title <- gsub("^\\s*|\\s*$", "", title) |>
    gsub("\\s+", "-", x = _)
  .yml_hooks_check_overwrite(title, overwrite, profile = profile)
  yml_hooks <- .yml_hooks_get(profile)
  yml_hooks[[title]] <- .yml_hooks_add_get(
    path = path, stage = stage, cue = cue
  )
  .yml_hooks_set(yml_hooks, profile)
}

.yml_hooks_add_get <- function(path, stage, cue = NULL) {
  add_list <- list(stage = stage, path = path)
  if (!is.null(cue)) {
    add_list[["cue"]] <- cue
  }
  add_list
}

#' @rdname yml-hooks
#' @export
projr_yml_hooks_rm <- function(title, path = NULL, profile = "default") {
  .assert_string(title, TRUE)
  .assert_string(path)
  if (.is_given_mid(profile)) {
    .assert_string(profile)
  }
  .yml_hooks_rm(title = title, path = path, profile = profile)
}

.yml_hooks_rm <- function(title, path = NULL, profile = "default") {
  yml_hooks <- .yml_hooks_get(profile)
  if (!is.null(path)) {
    .yml_hooks_rm_path(title, path, profile = profile)
  } else if (title %in% names(yml_hooks)) {
    .yml_hooks_rm_title(title, profile = profile)
  }
}

.yml_hooks_rm_path <- function(title, path, profile) {
  yml_hooks <- .yml_hooks_get(profile)
  if (!title %in% names(yml_hooks)) {
    return(invisible(FALSE))
  }
  path_vec <- yml_hooks[[title]][["path"]] |>
    setdiff(path)
  if (length(path_vec) == 0) {
    yml_hooks[[title]] <- NULL
  } else {
    yml_hooks[[title]][["path"]] <- path_vec
  }
  .yml_hooks_set(yml_hooks, profile)
}

.yml_hooks_rm_title <- function(title, profile) {
  yml_hooks <- .yml_hooks_get(profile)
  if (!title %in% names(yml_hooks)) {
    return(invisible(FALSE))
  }
  yml_hooks[[title]] <- NULL
  .yml_hooks_set(yml_hooks, profile)
}

#' @rdname yml-hooks
#' @export
projr_yml_hooks_rm_all <- function(profile = "default") {
  .yml_hooks_rm_all(profile = profile)
}

.yml_hooks_rm_all <- function(profile = "default") {
  yml_hooks <- .yml_hooks_get(profile)
  if (!length(yml_hooks) > 0) {
    return(invisible(FALSE))
  }
  .yml_hooks_set(NULL, profile)
}

.yml_hooks_check_overwrite <- function(title, overwrite, profile) {
  yml_hooks <- .yml_hooks_get(profile)
  if (!overwrite && title %in% names(yml_hooks)) {
    stop(paste0(
      "Hook with title '", title, "' already exists. ",
      "Set overwrite = TRUE to overwrite."
    ))
  }
  invisible(TRUE)
}

.yml_hooks_get <- function(profile) {
  .yml_build_get_hooks(profile)
}

.yml_hooks_set <- function(yml_hooks, profile = NULL) {
  .yml_build_set_nm(yml_hooks, "hooks", profile)
}

#' @rdname yml-hooks
#' @export
projr_yml_hooks_add_pre <- function(path,
                                     title,
                                     cue = NULL,
                                     overwrite = TRUE,
                                     profile = "default") {
  .yml_hooks_add(
    path = path, title = title,
    stage = "pre", cue = cue, profile = profile, overwrite = overwrite
  )
}

#' @rdname yml-hooks
#' @export
projr_yml_hooks_add_post <- function(path,
                                      title,
                                      cue = NULL,
                                      overwrite = TRUE,
                                      profile = "default") {
  .yml_hooks_add(
    path = path, title = title,
    stage = "post", cue = cue, profile = profile, overwrite = overwrite
  )
}
