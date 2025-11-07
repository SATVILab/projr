#' @rdname yml-script
#' @title Build script-related functions
#'
#' @description
#' Convenience functions to add or remove scripts
#' to run before or after the build.
#'
#' - .yml_script_add`: Add a script to run before or after the build.
#' - .yml_script_rm`: Remove scripts to run.
#'
#' .yml_script_add_pre` and .yml_script_add_post`
#' are wrappers around .yml_script_add` that set the `stage` argument
#' to `"pre"` or `"post"`, respectively.
#' .yml_script_rm_all` removes all scripts.
#'
#' @export
#' @param path character vector.
#' Path(s) to scripts, relative to project root (if not absolute).
#' @param title character.
#' Title for set of scripts.
#' Initial and trailing spaces are removed, and
#' the middle spaces are converted to dashes.
#' For example, `" a b "` is converted to
#' `"a-b"`. `
#' @param stage "pre" or "post".
#' Whether to run the script before or after the build.
#' @param cue "build", "dev", "patch", "minor" or "major".
#' Which minimum build level triggers the scripts.
#' "build" and "dev" are equivalent, and
#' always trigger the scripts.
#' @param overwrite logical.
#' Whether to overwrite any script settings
#' of the same title in the `projr`
#' configuration file.
#' If `FALSE` and there already exists
#' a key under `build/script` with the name
#' `title`, an error is thrown.
#' Default is `TRUE`.
#' @param profile character.
#' Profile to add the script to.
#' If `"default"`` (the default),
#' the script is added to the default profile,
#' which is `_projr.yml`.
#'
#'
#' @details
#' Within a stage (pre- or post-build), scripts
#' are run in the order set in `_projr.yml`.
#' They are not run in the same environment as the
#' build process.
#' The pre-build scripts are run immediately after
#' bumping the project version (if that is done) and immediately
#' before committing the present state of the code to Git.
#' The post-build scripts are run immediately after
#' committing the present state of the code to Git,
#' and before distributing project artefacts to the remotes.
projr_yml_script_add <- function(path,
                                 title,
                                 stage,
                                 cue = NULL,
                                 overwrite = TRUE,
                                 profile = "default") {
  .yml_script_check(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  )

  .yml_script_add(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  ) |>
    .yml_script_set(profile = profile)
}

.yml_script_check <- function(path,
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

.yml_script_add <- function(path,
                            title,
                            stage,
                            cue = NULL,
                            profile = "default",
                            overwrite = TRUE) {
  title <- gsub("^\\s*|\\s*$", "", title) |>
    gsub("\\s+", "-", x = _)
  .yml_script_check_overwrite(title, overwrite, profile = profile)
  yml_script <- .yml_script_get(profile)
  yml_script[[title]] <- .yml_script_add_get(
    path = path, title = title, stage = stage, cue = cue
  )[[title]]
  .yml_script_set(yml_script, profile)
}

.yml_script_add_get <- function(path, title, stage, cue = NULL) {
  add_list <- list(stage = stage, path = path)
  if (!is.null(cue)) {
    add_list[["cue"]] <- cue
  }
  list(add_list) |> stats::setNames(title)
}

#' @rdname yml-script
#' @export
projr_yml_script_rm <- function(title, path = NULL, profile = "default") {
  .assert_string(title, TRUE)
  .assert_string(path)
  if (.is_given_mid(profile)) {
    .assert_string(profile)
  }
  .yml_script_rm(title = title, path = path, profile = profile)
}

.yml_script_rm <- function(title, path = NULL, profile = "default") {
  yml_script <- .yml_script_get(profile)
  if (!is.null(path)) {
    .yml_script_rm_path(title, path, profile = profile)
  } else if (title %in% names(yml_script)) {
    .yml_script_rm_title(title, profile = profile)
  }
}

.yml_script_rm_path <- function(title, path, profile) {
  yml_script <- .yml_script_get(profile)
  if (!title %in% names(yml_script)) {
    return(invisible(FALSE))
  }
  path_vec <- yml_script[[title]][["path"]] |>
    setdiff(path)
  if (length(path_vec) == 0) {
    yml_script[[title]] <- NULL
  } else {
    yml_script[[title]][["path"]] <- path_vec
  }
  .yml_script_set(yml_script, profile)
}

.yml_script_rm_title <- function(title, profile) {
  yml_script <- .yml_script_get(profile)
  if (!title %in% names(yml_script)) {
    return(invisible(FALSE))
  }
  yml_script[[title]] <- NULL
  .yml_script_set(yml_script, profile)
}

#' @rdname yml-script
#' @export
projr_yml_script_rm_all <- function(profile = "default") {
  .yml_script_rm_all(profile = profile)
}

.yml_script_rm_all <- function(profile = "default") {
  yml_script <- .yml_script_get(profile)
  if (!length(yml_script) > 0) {
    return(invisible(FALSE))
  }
  .yml_script_set(NULL, profile)
}

.yml_script_check_overwrite <- function(title, overwrite, profile) {
  yml_script <- .yml_script_get(profile)
  if (!overwrite && title %in% names(yml_script)) {
    stop(paste0(
      "Script with title '", title, "' already exists. ",
      "Set overwrite = TRUE to overwrite."
    ))
  }
  invisible(TRUE)
}

.yml_script_get <- function(profile) {
  .yml_build_get_script(profile)
}

.yml_script_set <- function(yml_script, profile = NULL) {
  .yml_build_set_nm(yml_script, "script", profile)
}

#' @rdname yml-script
#' @export
projr_yml_script_add_pre <- function(path,
                                     title,
                                     cue = NULL,
                                     overwrite = TRUE,
                                     profile = "default") {
  .yml_script_add(
    path = path, title = title,
    stage = "pre", cue = cue, profile = profile, overwrite = overwrite
  )
}

#' @rdname yml-script
#' @export
projr_yml_script_add_post <- function(path,
                                      title,
                                      cue = NULL,
                                      overwrite = TRUE,
                                      profile = "default") {
  .yml_script_add(
    path = path, title = title,
    stage = "post", cue = cue, profile = profile, overwrite = overwrite
  )
}
