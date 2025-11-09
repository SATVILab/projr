#' @rdname yml-hooks
#' @title Build hook-related functions
#'
#' @description
#' Convenience functions to add or remove hooks
#' to run before or after the build.
#'
#' - \code{.yml_hooks_add}: Add a script to run before or after the build.
#' - \code{.yml_hooks_rm}: Remove scripts to run.
#'
#' \code{.yml_hooks_add_pre} and \code{.yml_hooks_add_post}
#' are wrappers around \code{.yml_hooks_add} that set the \code{stage} argument
#' to \code{"pre"} or \code{"post"}, respectively.
#' \code{.yml_hooks_rm_all} removes all scripts.
#'
#' @export
#' @param path character vector.
#' Path(s) to hooks, relative to project root (if not absolute).
#' @param title character.
#' Title for set of hooks.
#' Initial and trailing spaces are removed, and
#' the middle spaces are converted to dashes.
#' For example, \code{" a b "} is converted to
#' \code{"a-b"}.
#' @param stage "pre", "post", or "both".
#' Whether to run the hook before the build ("pre"), 
#' after the build ("post"), or in both stages ("both").
#' Hooks with stage "pre" are stored under `build.hooks.pre`,
#' hooks with stage "post" are stored under `build.hooks.post`,
#' and hooks with stage "both" are stored directly under `build.hooks`.
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
  .assert_in_single(stage, c("pre", "post", "both"), TRUE)
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
  .yml_hooks_check_overwrite(title, stage, overwrite, profile = profile)
  yml_hooks <- .yml_hooks_get(profile)
  
  # For "both" stage, add to root level (not under pre or post)
  # For "pre" or "post", add under that sub-key
  if (stage == "both") {
    yml_hooks[[title]] <- .yml_hooks_add_get(path = path, cue = cue)
  } else {
    # Add under build.hooks.pre or build.hooks.post
    if (is.null(yml_hooks[[stage]])) {
      yml_hooks[[stage]] <- list()
    }
    yml_hooks[[stage]][[title]] <- .yml_hooks_add_get(path = path, cue = cue)
  }
  
  yml_hooks
}

.yml_hooks_add_get <- function(path, cue = NULL) {
  add_list <- list(path = path)
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
  if (is.null(yml_hooks)) {
    return(invisible(FALSE))
  }
  
  # Check if title exists at root level (both pre and post)
  if (title %in% names(yml_hooks)) {
    if (!is.null(path)) {
      .yml_hooks_rm_path(title, path, profile = profile, stage = NULL)
    } else {
      .yml_hooks_rm_title(title, profile = profile, stage = NULL)
    }
    return(invisible(TRUE))
  }
  
  # Check in pre and post sub-keys
  for (stage in c("pre", "post")) {
    if (!is.null(yml_hooks[[stage]]) && title %in% names(yml_hooks[[stage]])) {
      if (!is.null(path)) {
        .yml_hooks_rm_path(title, path, profile = profile, stage = stage)
      } else {
        .yml_hooks_rm_title(title, profile = profile, stage = stage)
      }
      return(invisible(TRUE))
    }
  }
  
  invisible(FALSE)
}

.yml_hooks_rm_path <- function(title, path, profile, stage = NULL) {
  yml_hooks <- .yml_hooks_get(profile)
  
  if (is.null(stage)) {
    # Remove from root level
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
  } else {
    # Remove from pre or post sub-key
    if (is.null(yml_hooks[[stage]]) || !title %in% names(yml_hooks[[stage]])) {
      return(invisible(FALSE))
    }
    path_vec <- yml_hooks[[stage]][[title]][["path"]] |>
      setdiff(path)
    if (length(path_vec) == 0) {
      yml_hooks[[stage]][[title]] <- NULL
    } else {
      yml_hooks[[stage]][[title]][["path"]] <- path_vec
    }
  }
  
  .yml_hooks_set(yml_hooks, profile)
}

.yml_hooks_rm_title <- function(title, profile, stage = NULL) {
  yml_hooks <- .yml_hooks_get(profile)
  
  if (is.null(stage)) {
    # Remove from root level
    if (!title %in% names(yml_hooks)) {
      return(invisible(FALSE))
    }
    yml_hooks[[title]] <- NULL
  } else {
    # Remove from pre or post sub-key
    if (is.null(yml_hooks[[stage]]) || !title %in% names(yml_hooks[[stage]])) {
      return(invisible(FALSE))
    }
    yml_hooks[[stage]][[title]] <- NULL
  }
  
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

.yml_hooks_check_overwrite <- function(title, stage, overwrite, profile) {
  yml_hooks <- .yml_hooks_get(profile)
  
  # Check based on stage
  if (stage == "both") {
    # Check root level
    if (!overwrite && title %in% names(yml_hooks)) {
      stop(paste0(
        "Hook with title '", title, "' already exists at root level. ",
        "Set overwrite = TRUE to overwrite."
      ))
    }
  } else {
    # Check in pre or post sub-key
    if (!is.null(yml_hooks[[stage]]) && !overwrite && title %in% names(yml_hooks[[stage]])) {
      stop(paste0(
        "Hook with title '", title, "' already exists in ", stage, " hooks. ",
        "Set overwrite = TRUE to overwrite."
      ))
    }
  }
  
  invisible(TRUE)
}

.yml_hooks_get <- function(profile) {
  .yml_build_get_hooks(profile)
}

# Get hooks that run in a specific stage
# Structure: build.hooks.both, build.hooks.pre, build.hooks.post
# All are simple file paths (strings or arrays), no titles or "path" keys
.yml_hooks_get_stage <- function(stage, profile) {
  yml_hooks <- .yml_hooks_get(profile)
  if (is.null(yml_hooks)) {
    return(NULL)
  }
  
  hooks_list <- c()
  
  # Get stage-specific hooks from build.hooks.pre or build.hooks.post
  if (stage %in% names(yml_hooks)) {
    stage_hooks <- yml_hooks[[stage]]
    if (!is.null(stage_hooks)) {
      # Can be a single string or a vector of strings
      hooks_list <- c(hooks_list, as.character(stage_hooks))
    }
  }
  
  # Get hooks that run in both pre and post (from build.hooks.both)
  if ("both" %in% names(yml_hooks)) {
    both_hooks <- yml_hooks[["both"]]
    if (!is.null(both_hooks)) {
      hooks_list <- c(hooks_list, as.character(both_hooks))
    }
  }
  
  if (length(hooks_list) == 0) {
    return(NULL)
  }
  
  hooks_list
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
  ) |>
    .yml_hooks_set(profile = profile)
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
  ) |>
    .yml_hooks_set(profile = profile)
}
