# git
# -----------------

#' @rdname yml-git
#' @title Set Git options
#'
#' @description
#'
#' `projr_yml_git_set` sets Git options for the project.
#'
#' The options are:
#' \itemize{
#' \item{commit: }{whether to commit changes automatically upon
#' project builds.}
#' \item{add_untracked: }{whether to add untracked files automatically upon
#' project builds.}
#' \item{push: }{whether to push changes automatically upon
#' project builds.}
#' }
#' The default is to leave all the settings unchanged.
#'
#' If these settings are not setting in `_projr.yml`,
#' then the default is to commit, add untracked files and push.
#'
#' `projr_yml_git_set_default` sets all Git options to default (`TRUE`).
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
  .projr_yml_git_set_check(
    all = all, commit = commit, add_untracked = add_untracked,
    push = push, simplify_default = simplify_default,
    simplify_identical = simplify_identical, profile = profile
  )
  if (!.projr_state_null(all)) {
    commit <- all
    add_untracked <- all
    push <- all
  }
  .projr_yml_git_set_ind(
    commit = commit, add_untracked = add_untracked,
    push = push, simplify_default = simplify_default,
    profile = profile
  )

  .projr_yml_git_simplify(simplify_identical, simplify_default, profile)
}

.projr_yml_git_set_check <- function(all,
                                     commit,
                                     add_untracked,
                                     push,
                                     simplify_identical,
                                     simplify_default,
                                     profile) {
  if (!.projr_state_null(all)) {
    .projr_check_lgl_single(all, "all")
  } else {
    .projr_check_lgl_single(commit, "commit")
    .projr_check_lgl_single(add_untracked, "add_untracked")
    .projr_check_lgl_single(push, "push")
  }
  .projr_check_lgl_single(simplify_identical, "simplify_identical", required = TRUE)
  .projr_check_lgl_single(simplify_default, "simplify_default", required = TRUE)
  .projr_check_chr_single(profile, "profile")
}

.projr_yml_git_set_ind <- function(commit,
                                   add_untracked,
                                   push,
                                   simplify_default,
                                   profile) {
  if (!.projr_state_null(commit)) {
    .projr_yml_git_set_commit(commit, simplify_default, profile)
  }
  if (!.projr_state_null(add_untracked)) {
    .projr_yml_git_set_add_untracked(add_untracked, simplify_default, profile)
  }
  if (!.projr_state_null(push)) {
    .projr_yml_git_set_push(push, simplify_default, profile)
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

.projr_yml_git_set_commit <- function(commit,
                                      simplify_default,
                                      profile) {
  commit_pre <- .projr_yml_git_get_commit(profile = profile)
  if (all(commit_pre, commit) && simplify_default) {
    return(invisible(FALSE))
  }
  .projr_yml_git_set_mix(list("commit" = commit), profile)
  invisible(TRUE)
}

.projr_yml_git_get_commit <- function(profile) {
  yml_git <- .projr_yml_git_get(profile)
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  if (.projr_state_null(yml_git[["commit"]])) {
    return(TRUE)
  }
  yml_git[["commit"]]
}

.projr_yml_git_set_mix <- function(git_list_single, profile) {
  .projr_yml_git_get_combn_with_written(
    git_list_single, profile
  ) |>
    .projr_yml_git_get_ordered() |>
    .projr_yml_git_set(profile)
}

.projr_yml_git_get_combn_with_written <- function(git_list_single, profile) {
  yml_git <- .projr_yml_git_get(profile)
  git_list_single |> append(
    yml_git[setdiff(names(yml_git), names(git_list_single))]
  )
}

.projr_yml_git_get_ordered <- function(yml_git) {
  nm_vec_actual <- names(yml_git)
  nm_vec_possible <- c("commit", "add-untracked", "push")
  nm_vec_ordered <- nm_vec_possible[nm_vec_possible %in% nm_vec_actual]
  yml_git[nm_vec_ordered]
}

.projr_yml_git_set_add_untracked <- function(add_untracked,
                                             simplify_default,
                                             profile) {
  add_untracked_pre <- .projr_yml_git_get_add_untracked(profile = profile)
  if (all(add_untracked_pre, add_untracked) && simplify_default) {
    return(invisible(FALSE))
  }
  .projr_yml_git_set_mix(list("add-untracked" = add_untracked), profile)
  invisible(TRUE)
}

.projr_yml_git_get_add_untracked <- function(profile) {
  yml_git <- .projr_yml_git_get(profile)
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  if (.projr_state_null(yml_git[["add-untracked"]])) {
    return(TRUE)
  }
  yml_git[["add-untracked"]]
}

.projr_yml_git_set_push <- function(push,
                                    simplify_default,
                                    profile) {
  push_pre <- .projr_yml_git_get_push(profile = profile)
  if (all(push_pre, push) && simplify_default) {
    return(invisible(FALSE))
  }
  .projr_yml_git_set_mix(list("push" = push), profile)
  invisible(TRUE)
}

.projr_yml_git_get_push <- function(profile) {
  yml_git <- .projr_yml_git_get(profile)
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  if (.projr_state_null(yml_git[["push"]])) {
    return(TRUE)
  }
  yml_git[["push"]]
}

.projr_yml_git_get <- function(profile) {
  init_list <- projr_yml_get_unchecked(profile)[["build"]][["git"]]
  if (length(init_list) == 0L) {
    return(NULL)
  }
  init_list
}

.projr_yml_git_set <- function(yml_git, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]][["git"]] <- yml_git
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_git_simplify <- function(simplify_identical,
                                    simplify_default,
                                    profile) {
  .projr_yml_git_simplify_identical(simplify_identical, profile)
  .projr_yml_git_simplify_default(simplify_default, profile)
}

.projr_yml_git_simplify_identical <- function(simplify_identical,
                                              profile) {
  if (!simplify_identical) {
    return(invisible(FALSE))
  }

  commit <- .projr_yml_git_get_commit(profile)
  push <- .projr_yml_git_get_push(profile)
  add_untracked <- .projr_yml_git_get_add_untracked(profile)
  if (all(c(commit, push, add_untracked))) {
    .projr_yml_git_set(TRUE, profile)
  } else if (!any(c(commit, push, add_untracked))) {
    .projr_yml_git_set(FALSE, profile)
  }

  invisible(TRUE)
}

.projr_yml_git_simplify_default <- function(simplify_default,
                                            profile) {
  if (!simplify_default) {
    return(invisible(FALSE))
  }

  commit <- .projr_yml_git_get_commit(profile)
  push <- .projr_yml_git_get_push(profile)
  add_untracked <- .projr_yml_git_get_add_untracked(profile)
  if (all(c(commit, push, add_untracked))) {
    .projr_yml_git_set(NULL, profile)
  }
  invisible(TRUE)
}

# github
# --------------------

.projr_yml_unset_github_dest <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["github"]] <- NULL
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}
