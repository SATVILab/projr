# git
# -----------------

#' @rdname yml-git
#' @title Set Git options
#'
#' @description
#'
#' .yml_git_set` sets Git options for the project.
#'
#' The options are:
#' \itemize{
#' \item commit: whether to commit changes automatically upon
#' project builds.
#' \item add_untracked: whether to add untracked files automatically upon
#' project builds.
#' \item push: whether to push changes automatically upon
#' project builds.
#' }
#' The default is to leave all the settings unchanged.
#'
#' If these settings are not setting in `_projr.yml`,
#' then the default is to commit, add untracked files and push.
#'
#' .yml_git_set_default` sets all Git options to default (`TRUE`).
#'
#' @param all logical.
#' Whether to set all the options
#' to `TRUE` or `FALSE`.
#' If `NULL`, then `commit`, `add_untracked` and `push` are used.
#' Default is `NULL`.
#' @param commit logical.
#' Whether to commit changes automatically upon
#' project builds.
#' If `NULL`, then setting is not changed.
#' Default is `NULL`.
#' @param add_untracked logical.
#' Whether to add untracked files automatically upon
#' project builds.
#' If `NULL`, then setting is not changed.
#' Default is `NULL`.
#' @param push logical.
#' Whether to push changes automatically upon
#' project builds.
#' If `NULL`, then setting is not changed.
#' Default is `NULL`.
#' @param simplify_identical logical.
#' If `TRUE`, then if all the settings are the same
#' (for `commit`, `push` and `add_untracked`),
#' then only `git: TRUE` or `git: FALSE` is written to `_projr.yml`.
#' Default is `TRUE`.
#' @param simplify_default logical.
#' If `TRUE`, then if the settings are the same as the default
#' (which is TRUE),
#' then the settings are removed from `_projr.yml`.
#' Default is `TRUE`.
#' @param profile character.
#' Profile to add the script to.
#' If `"default"`` (the default),
#' the script is added to the default profile,
#' which is `_projr.yml`.
#' If `NULL`, then the active profile is used
#' (i.e the merge of `_projr-local.yml`, `_projr-<profile>.yml`
#' and `_projr.yml`) and written to `_projr.yml`.
#' If another character vector, then the corresponding profile is used
#' and written to `_projr-<profile>.yml`.
#'
#' @examples
#' \dontrun{
#' # set all to TRUE
#' projr_yml_git_set(all = TRUE)
#'
#' # set all to FALSE
#' projr_yml_git_set(all = FALSE)
#'
#' # set only add_untracked to FALSE
#' projr_yml_git_set(add_untracked = FALSE)
#'
#' # revert to defaults
#' projr_yml_git_set_default()
#' }
#' @export
projr_yml_git_set <- function(all = NULL,
                              commit = NULL,
                              add_untracked = NULL,
                              push = NULL,
                              simplify_identical = TRUE,
                              simplify_default = TRUE,
                              profile = "default") {
  .yml_git_set_check(
    all = all, commit = commit, add_untracked = add_untracked,
    push = push, simplify_default = simplify_default,
    simplify_identical = simplify_identical, profile = profile
  )
  if (!is.null(all)) {
    commit <- all
    add_untracked <- all
    push <- all
  }
  .yml_git_set_ind(
    commit = commit, add_untracked = add_untracked,
    push = push, simplify_default = simplify_default,
    profile = profile
  )

  .yml_git_simplify(simplify_identical, simplify_default, profile)
}

.yml_git_set_check <- function(all,
                               commit,
                               add_untracked,
                               push,
                               simplify_identical,
                               simplify_default,
                               profile) {
  if (!is.null(all)) {
    .assert_flag(all)
  } else {
    .assert_flag(commit)
    .assert_flag(add_untracked)
    .assert_flag(push)
  }
  .assert_flag(simplify_identical, TRUE)
  .assert_flag(simplify_default, TRUE)
  .assert_string(profile)
}

.yml_git_set_ind <- function(commit,
                             add_untracked,
                             push,
                             simplify_default,
                             profile) {
  if (!is.null(commit)) {
    .yml_git_set_commit(commit, simplify_default, profile)
  }
  if (!is.null(add_untracked)) {
    .yml_git_set_add_untracked(add_untracked, simplify_default, profile)
  }
  if (!is.null(push)) {
    .yml_git_set_push(push, simplify_default, profile)
  }
}

#' @rdname yml-git
#' @export
projr_yml_git_set_default <- function(profile = "default",
                                      simplify_identical = TRUE,
                                      simplify_default = TRUE) {
  projr_yml_git_set(
    all = TRUE, profile = profile, simplify_identical = simplify_identical,
    simplify_default = simplify_default
  )
}

# Generic setter for git fields (commit, add-untracked, push)
.yml_git_set_field <- function(field_name,
                               field_value,
                               simplify_default,
                               profile) {
  field_value_pre <- .yml_git_get_field(field_name, profile)
  if (isTRUE(field_value_pre) && isTRUE(field_value) && simplify_default) {
    return(invisible(FALSE))
  }
  field_list <- list()
  field_list[[field_name]] <- field_value
  .yml_git_set_mix(field_list, profile)
  invisible(TRUE)
}

# Generic getter for git fields with optional override logic
.yml_git_get_field <- function(field_name,
                               profile,
                               override_if_commit_false = FALSE) {
  yml_git <- .yml_git_get(profile)
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  # Handle override logic for add-untracked and push
  if (override_if_commit_false &&
      !is.null(yml_git[["commit"]]) && isFALSE(yml_git[["commit"]])) {
    return(FALSE)
  }
  if (is.null(yml_git[[field_name]])) {
    return(TRUE)
  }
  yml_git[[field_name]]
}

.yml_git_set_commit <- function(commit,
                                simplify_default,
                                profile) {
  .yml_git_set_field("commit", commit, simplify_default, profile)
}

.yml_git_get_commit <- function(profile) {
  .yml_git_get_field("commit", profile)
}

.yml_git_set_mix <- function(git_list_single, profile) {
  .yml_git_get_combn_with_written(
    git_list_single, profile
  ) |>
    .yml_git_get_ordered() |>
    .yml_git_set(profile)
}

.yml_git_get_combn_with_written <- function(git_list_single, profile) {
  yml_git <- .yml_git_get(profile)
  git_list_single |> append(
    yml_git[setdiff(names(yml_git), names(git_list_single))]
  )
}

.yml_git_get_ordered <- function(yml_git) {
  nm_vec_actual <- names(yml_git)
  nm_vec_possible <- c("commit", "add-untracked", "push")
  nm_vec_ordered <- nm_vec_possible[nm_vec_possible %in% nm_vec_actual]
  yml_git[nm_vec_ordered]
}

.yml_git_set_add_untracked <- function(add_untracked,
                                       simplify_default,
                                       profile) {
  .yml_git_set_field("add-untracked", add_untracked, simplify_default, profile)
}

.yml_git_get_add_untracked <- function(profile) {
  # override add-untracked value if commit is FALSE
  .yml_git_get_field("add-untracked", profile, override_if_commit_false = TRUE)
}

.yml_git_set_push <- function(push,
                              simplify_default,
                              profile) {
  .yml_git_set_field("push", push, simplify_default, profile)
}

.yml_git_get_push <- function(profile) {
  # override push value if commit is FALSE
  # (i.e. if commit is FALSE, then push is also FALSE)
  # as there is no reason to push if not committing,
  # and this makes the upfront check for Git setup easier
  # (as we can now rule out the case where commit is FALSE)
  .yml_git_get_field("push", profile, override_if_commit_false = TRUE)
}

.yml_git_get <- function(profile) {
  .yml_get(profile)[["build"]][["git"]]
}

.yml_git_set <- function(yml_git, profile) {
  .yml_build_set_nm(yml_git, "git", profile)
}

.yml_git_simplify <- function(simplify_identical,
                              simplify_default,
                              profile) {
  .yml_git_simplify_identical(simplify_identical, profile)
  .yml_git_simplify_default(simplify_default, profile)
}

.yml_git_simplify_identical <- function(simplify_identical,
                                        profile) {
  if (!simplify_identical) {
    return(invisible(FALSE))
  }

  commit <- .yml_git_get_commit(profile)
  push <- .yml_git_get_push(profile)
  add_untracked <- .yml_git_get_add_untracked(profile)
  if (all(c(commit, push, add_untracked))) {
    .yml_git_set(TRUE, profile)
  } else if (!any(c(commit, push, add_untracked))) {
    .yml_git_set(FALSE, profile)
  }

  invisible(TRUE)
}

.yml_git_simplify_default <- function(simplify_default,
                                      profile) {
  if (!simplify_default) {
    return(invisible(FALSE))
  }

  commit <- .yml_git_get_commit(profile)
  push <- .yml_git_get_push(profile)
  add_untracked <- .yml_git_get_add_untracked(profile)
  if (all(c(commit, push, add_untracked))) {
    .yml_git_set(NULL, profile)
  }
  invisible(TRUE)
}

# github
# --------------------

.yml_unset_github_dest <- function() {
  yml_projr_root <- .yml_get_default()
  yml_projr_root[["build"]][["github"]] <- NULL
  .yml_set_root(yml_projr_root)
  invisible(TRUE)
}
