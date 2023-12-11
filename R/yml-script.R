#' @rdname yml-script
#' @title Build script-related functions
#'
#' @description
#' Convenience functions to add or remove scripts
#' to run before or after the build.
#'
#' - `projr_yml_script_add`: Add a script to run before or after the build.
#' - `projr_yml_script_rm`: Remove scripts to run.
#'
#' `projr_yml_script_add_pre` and `projr_yml_script_add_post`
#' are wrappers around `projr_yml_script_add` that set the `stage` argument
#' to `"pre"` or `"post"`, respectively.
#' `projr_yml_script_rm_all` removes all scripts.
#'
#' @export
#' @param path character vector.
#' Path(s) to scripts, relative to project root (if not absolute).
#' @param title character.
#' Title for set of scripts.
#' @param stage "pre" or "post".
#' Whether to run the script before or after the build.
#' @param cue "build", "dev", "patch", "minor" or "major".
#' Which minimum build level triggers the scripts.
#' "build" and "dev" are equivalent, and
#' always trigger the scripts.
#' @param profile character.
#' Profile to add the script to.
#' If `"default"`` (the default),
#' the script is added to the default profile,
#' which is `_projr.yml`.
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
  .projr_yml_script_check(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  )

  .projr_yml_script_add(
    path = path, title = title, stage = stage, cue = cue,
    overwrite = overwrite, profile = profile
  ) |>
    .projr_yml_script_set(profile = profile)
}

.projr_yml_script_check <- function(path,
                                    title,
                                    stage,
                                    cue,
                                    overwrite,
                                    profile) {
  .projr_check_chr_nz(path, "path", required = TRUE)
  .projr_check_chr_single(title, "title", required = TRUE)
  .projr_check_chr_single_opt(
    stage, "stage",
    required = TRUE, opt = c("pre", "post")
  )
  if (.projr_state_given(profile)) {
    .projr_check_chr_single(profile, "profile")
  }
  .projr_check_chr_single_opt(
    cue, "cue",
    required = FALSE,
    opt = c("build", "dev", "patch", "minor", "major")
  )
  .projr_check_lgl_single(overwrite, "overwrite", required = TRUE)
}

.projr_yml_script_add <- function(path,
                                  title,
                                  stage,
                                  cue,
                                  profile,
                                  overwrite = TRUE) {
  .projr_yml_script_check_overwrite(title, overwrite, profile = profile)
  yml_script <- .projr_yml_script_get(profile)
  yml_script[[stage]][[title]] <- .projr_yml_script_add_get(
    path = path, title = title, stage = stage, cue = cue
  )
}

.projr_yml_script_add_get <- function(path, title, stage, cue = NULL) {
  add_list <- list(stage = stage, path = path)
  if (!is.null(cue)) {
    add_list[["cue"]] <- cue
  }
  list(add_list) |> stats::setNames(title)
}

#' @rdname yml-script
#' @export
projr_yml_script_rm <- function(title, path = NULL, profile = "default") {
  .projr_check_chr_single(title, "title", required = TRUE)
  .projr_check_chr(path, "path")
  if (.projr_state_given(profile)) {
    .projr_check_chr_nz(profile)
  }
  yml_script <- .projr_yml_script_get(profile)
  if (!is.null(path)) {
    .projr_yml_script_rm_path(title, path, profile = profile)
  } else if (title %in% names(yml_script)) {
    .projr_yml_script_rm_title(title, profile = profile)
  }
}

.projr_yml_script_rm_path <- function(title, path, profile) {
  yml_script <- .projr_yml_script_get(profile)
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
  .projr_yml_script_set(yml_script, profile)
}

.projr_yml_script_rm_title <- function(title, profile) {
  yml_script <- .projr_yml_script_get(profile)
  if (!title %in% names(yml_script)) {
    return(invisible(FALSE))
  }
  yml_script[[title]] <- NULL
  .projr_yml_script_set(yml_script, profile)
}

#' @rdname yml-script
#' @export
projr_yml_script_rm_all <- function(profile = "default") {
  yml_script <- .projr_yml_script_get(profile)
  if (!length(yml_script) > 0) {
    return(invisible(FALSE))
  }
  .projr_yml_script_set(NULL, profile)
}

.projr_yml_script_check_overwrite <- function(title, overwrite, profile) {
  yml_script <- .projr_yml_script_get(profile)
  if (!overwrite && title %in% names(yml_script)) {
    stop(paste0(
      "Script with title '", title, "' already exists. ",
      "Set overwrite = TRUE to overwrite."
    ))
  }
  invisible(TRUE)
}

.projr_yml_script_get <- function(profile) {
  yml_script <- projr_yml_get_unchecked(profile)[["build"]][["script"]]
  if (length(yml_script) == 0L) {
    return(list())
  }
  yml_script
}

.projr_yml_script_set <- function(yml_script, profile = NULL) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]][["script"]] <- yml_script
  .projr_yml_set(yml_projr, profile)
}

#' @rdname yml-script
#' @export
projr_yml_script_add_pre <- function(path, title, cue, profile) {
  projr_yml_script_add(path, title, stage = "pre", cue = cue, profile = profile)
}

#' @rdname yml-script
#' @export
projr_yml_script_add_post <- function(path, title, cue, profile) {
  projr_yml_script_add(path, title, stage = "post", cue = cue, profile = profile)
}

.projr_yml_script_complete_cue <- function(x) {
  if (.projr_state_null(x)) {
    return(invisible("dev"))
  }
  invisible(x)
}
