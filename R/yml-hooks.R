#' @rdname yml-hooks
#' @title Build hook-related functions
#'
#' @description
#' Convenience functions to add or remove hooks
#' to run before or after the build.
#'
#' - \code{projr_yml_hooks_add}: Add hook script(s) to run before or after the build.
#' - \code{projr_yml_hooks_rm_all}: Remove all hooks.
#' - \code{projr_yml_hooks_add_pre}: Add hook(s) to run before the build.
#' - \code{projr_yml_hooks_add_post}: Add hook(s) to run after the build.
#'
#' @export
#' @param path character vector.
#' Path(s) to hook scripts, relative to project root (if not absolute).
#' @param stage "pre", "post", or "both".
#' Whether to run the hook before the build ("pre"), 
#' after the build ("post"), or in both stages ("both").
#' Hooks with stage "pre" are stored under \code{build.hooks.pre},
#' hooks with stage "post" are stored under \code{build.hooks.post},
#' and hooks with stage "both" are stored under \code{build.hooks.both}.
#' @param overwrite logical.
#' Whether to overwrite existing hooks or append to them.
#' Default is \code{TRUE}.
#' @param profile character.
#' Profile to add the hook to.
#' If \code{"default"} (the default),
#' the hook is added to the default profile,
#' which is \code{_projr.yml}.
#'
#' @details
#' Within a stage (pre- or post-build), hooks
#' are run in the order specified in \code{_projr.yml}.
#' They are not run in the same environment as the
#' build process.
#' The pre-build hooks are run immediately after
#' bumping the project version (if that is done) and immediately
#' before committing the present state of the code to Git.
#' The post-build hooks are run immediately after
#' committing the present state of the code to Git,
#' and before distributing project artifacts to the remotes.
#' 
#' Hooks are stored as simple character vectors in the YAML:
#' \preformatted{
#' build:
#'   hooks:
#'     pre: ["pre-hook.R"]
#'     post: ["post-hook.R"]
#'     both: ["both-hook.R"]
#' }
projr_yml_hooks_add <- function(path,
                                 stage,
                                 overwrite = TRUE,
                                 profile = "default") {
  .yml_hooks_check(
    path = path, stage = stage, overwrite = overwrite, profile = profile
  )

  .yml_hooks_add(
    path = path, stage = stage, overwrite = overwrite, profile = profile
  ) |>
    .yml_hooks_set(profile = profile)
}

.yml_hooks_check <- function(path,
                              stage,
                              overwrite,
                              profile) {
  .assert_chr(path, TRUE)
  .assert_in_single(stage, c("pre", "post", "both"), TRUE)
  .assert_flag(overwrite, TRUE)
  if (.is_given_mid(profile)) {
    .assert_string(profile)
  }
}

.yml_hooks_add <- function(path,
                            stage,
                            profile = "default",
                            overwrite = TRUE) {
  yml_hooks <- .yml_hooks_get(profile)
  
  # Add path to the appropriate stage vector
  # stage can be "both", "pre", or "post"
  if (is.null(yml_hooks[[stage]])) {
    yml_hooks[[stage]] <- character(0)
  }
  
  # Add paths to the vector
  if (overwrite) {
    yml_hooks[[stage]] <- path
  } else {
    # Append without duplicates
    existing_paths <- yml_hooks[[stage]]
    new_paths <- setdiff(path, existing_paths)
    yml_hooks[[stage]] <- c(existing_paths, new_paths)
  }
  
  yml_hooks
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
                                     overwrite = TRUE,
                                     profile = "default") {
  .yml_hooks_add(
    path = path,
    stage = "pre", profile = profile, overwrite = overwrite
  ) |>
    .yml_hooks_set(profile = profile)
}

#' @rdname yml-hooks
#' @export
projr_yml_hooks_add_post <- function(path,
                                      overwrite = TRUE,
                                      profile = "default") {
  .yml_hooks_add(
    path = path,
    stage = "post", profile = profile, overwrite = overwrite
  ) |>
    .yml_hooks_set(profile = profile)
}
