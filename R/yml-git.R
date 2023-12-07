# git
# -----------------

.projr_yml_set_git <- function(all = NULL,
                               commit = NULL,
                               add_untracked = NULL,
                               push = NULL) {
  if (!is.null(all)) {
    commit <- all
    add_untracked <- all
    push <- all
  }
  if (!is.null(commit)) {
    .projr_yml_set_git_commit(commit)
  }
  if (!is.null(add_untracked)) {
    .projr_yml_set_git_add_untracked(add_untracked)
  }
  if (!is.null(push)) {
    .projr_yml_set_git_push(push)
  }
}

.projr_yml_unset_git <- function() {
  .projr_yml_set_git(all = FALSE)
}

.projr_yml_set_git_commit <- function(commit = TRUE) {
  commit_pre <- .projr_yml_get_git_commit()
  if (identical(commit_pre, commit)) {
    return(invisible(FALSE))
  }
  push <- .projr_yml_get_git_push()
  add_untracked <- .projr_yml_get_git_add_untracked()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "commit")
  }
  invisible(TRUE)
}

.projr_yml_get_git_commit <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["commit"]]
}

.projr_yml_set_git_true <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_git_orig <- yml_projr_root[["build"]][["git"]]
  if (!is.null(yml_git_orig)) {
    yml_projr_root[["build"]][["git"]] <- TRUE
    .projr_yml_set_root(yml_projr_root)
  }
  return(invisible(TRUE))
}

.projr_yml_set_git_false <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["git"]] <- FALSE
  .projr_yml_set_root(yml_projr_root)
  return(invisible(TRUE))
}

.projr_yml_set_git_mix <- function(yml_git_full, nm) {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_git_orig <- yml_projr_root[["build"]][["git"]]
  nm_vec_sel <- factor(
    c(nm, names(yml_git_orig)) |> unique(),
    levels = c("commit", "add-untracked", "push")
  ) |>
    sort() |>
    as.character()
  yml_git <- yml_git_full[nm_vec_sel]
  yml_projr_root[["build"]][["git"]] <- yml_git
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}

.projr_yml_set_git_add_untracked <- function(add_untracked = TRUE) {
  add_untracked_pre <- .projr_yml_get_git_add_untracked()
  if (identical(add_untracked_pre, add_untracked)) {
    return(invisible(FALSE))
  }
  push <- .projr_yml_get_git_push()
  commit <- .projr_yml_get_git_commit()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "add-untracked")
  }
}

.projr_yml_get_git_add_untracked <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["add-untracked"]]
}

.projr_yml_set_git_push <- function(push = TRUE) {
  push_pre <- .projr_yml_get_git_push()
  if (identical(push_pre, push)) {
    return(invisible(FALSE))
  }
  commit <- .projr_yml_get_git_commit()
  add_untracked <- .projr_yml_get_git_add_untracked()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "push")
  }
  invisible(TRUE)
}

.projr_yml_get_git_push <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["push"]]
}

.projr_yml_get_git <- function(profile = NULL) {

}

# github
# --------------------

.projr_yml_unset_github_dest <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["github"]] <- NULL
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}
