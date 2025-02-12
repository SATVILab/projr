#' @title Check active `projr` settings.
#'
#' @description
#' Checks correctness of active `projr` settings.

#' @param profile character().
#' Profile whose file needs to be checked.
#' If not supplied, then the merged profile is used.
#' Default is NULL.
#'
#' @return
#' Returns `TRUE` if all checks pass.
#' Otherwise throws an error.
#'
#' @export
projr_yml_check <- function(profile = NULL) {
  .projr_yml_dir_check(profile)
  .projr_yml_build_check(profile)
  invisible(TRUE)
}

# directory
# ----------------------
.projr_yml_dir_check <- function(profile) {
  yml_dir <- .projr_yml_dir_get(profile)
  nm_vec <- names(yml_dir)
  nm_vec_strip <- .projr_dir_label_strip(nm_vec)
  .assert_len(nm_vec, unique(nm_vec) |> length())
  .assert_len(nm_vec, nm_vec[nzchar(nm_vec)] |> length())
  .assert_nz(nm_vec)
  .assert_detect_any(nm_vec_strip, "^cache")
  .assert_detect_any(nm_vec_strip, "^output")
  for (x in nm_vec) {
    .projr_yml_dir_check_label(x, profile)
  }
}

# directory labels
# ----------------------

.projr_yml_dir_check_label <- function(label, profile) {
  yml_label <- .projr_yml_dir_get_label(label, profile)
  .assert_in(
    names(yml_label),
    c("path", "ignore-git", "ignore-rbuild", "ignore", "output")
  )
  yml_label |>
    .projr_yml_dir_check_label_path(label, profile) |>
    .projr_yml_dir_check_label_ignore() |>
    .projr_yml_dir_check_label_git_track_adjust() |>
    .projr_yml_dir_check_label_output(label, profile)
}

.projr_yml_dir_check_label_path <- function(yml_label, label, profile) {
  is_docs <- .projr_yml_dir_label_class_detect_docs(label)
  if (is_docs && !"path" %in% names(yml_label)) {
    return(invisible(TRUE))
  }
  .assert_has(names(yml_label), "path")
  .assert_string(yml_label[["path"]], TRUE)
  .projr_yml_dir_check_label_path_restricted(yml_label, label, profile)


  invisible(TRUE)
}

.projr_yml_dir_check_label_path_restricted <- function(yml_label, label, profile) {
  label_vec_restricted <- .projr_yml_dir_get_label_in(profile) |>
    c(.projr_yml_dir_get_label_output(profile))
  if (label %in% label_vec_restricted) {
    .assert_path_not_sub(yml_label[["path"]], c("data", "man", "R", "tests"))
  }
  invisible(TRUE)
}

.projr_yml_dir_check_label_ignore <- function(yml_label) {
  for (x in c("ignore", "ignore-git", "ignore-rbuild")) {
    .projr_yml_dir_check_label_ignore_ind(yml_label, x)
  }
  invisible(TRUE)
}

.projr_yml_dir_check_label_ignore_ind <- function(yml_label, nm) {
  if (nm %in% names(yml_label)) {
    val <- yml_label[[nm]]
    .assert_class_any(val, c("logical", "character"), TRUE)
    .assert_len_1(val, TRUE)
    if (is.logical(val)) .assert_flag(val)
    if (is.character(val)) .assert_in(val, c("manual", "ignore", "no-ignore"))
  }
  invisible(TRUE)
}

.projr_yml_dir_check_label_git_track_adjust <- function(yml_label) {
  if (!"git-track-adjust" %in% names(yml_label)) {
    return(invisible(TRUE))
  }
  .assert_flag(yml_label[["git-track-adjust"]], TRUE)
}

.projr_yml_dir_check_label_output <- function(yml_label, label, profile) {
  label_vec_output_valid <- .projr_yml_dir_get_label_output(profile) |>
    c(.projr_yml_dir_get_label_in(profile)) |>
    c("data", "docs")
  if (!label %in% label_vec_output_valid) {
    .assert_has_not(names(yml_label), "output")
  }
  if (!"output" %in% names(yml_label)) {
    return(invisible(TRUE))
  }

  .assert_len_1(yml_label[["output"]], TRUE)
  .assert_class_any(yml_label[["output"]], c("logical", "character"), TRUE)
  if (is.logical(yml_label[["output"]])) {
    .assert_flag(yml_label[["output"]])
  } else if (is.character(yml_label[["output"]])) {
    .assert_in(
      yml_label[["output"]],
      .projr_yml_dir_get_label_output(profile) |> c("output")
    )
  }
}

# build
# ----------------------

.projr_yml_build_check <- function(profile) {
  .projr_yml_build_check_label(profile)
  .projr_yml_build_check_git(profile)
  .projr_yml_build_check_dest(profile)
}

# build: label
# ----------------------

.projr_yml_build_check_label <- function(profile) {
  yml_build <- .projr_yml_build_get(profile)
  nm_vec <- names(yml_build)
  if (is.null(nm_vec) || .is_len_0(nm_vec)) {
    return(invisible(FALSE))
  }
  .assert_in(
    nm_vec,
    c(
      "dev-output", "script", "git",
      "github", "package", "local", "osf"
    )
  )
  .assert_flag(.projr_yml_build_get_dev_output(profile))
}

# build: git
# ----------------------

.projr_yml_build_check_git <- function(profile) {
  yml_git <- .projr_yml_git_get(profile)
  if (is.null(yml_git)) {
    return(invisible(TRUE))
  }
  .assert_in(names(yml_git), c("commit", "add-untracked", "push"))
  if ("commit" %in% names(yml_git)) {
    .assert_flag(yml_git[["commit"]])
  }
  if ("add-untracked" %in% names(yml_git)) {
    .assert_flag(yml_git[["add-untracked"]])
  }
  if ("push" %in% names(yml_git)) {
    .assert_flag(yml_git[["push"]])
  }
  invisible(TRUE)
}

# build: local
# ----------------------

.projr_yml_build_check_dest <- function(profile) {
  for (x in .projr_opt_remote_get_type()) {
    if (is.null(.projr_yml_dest_get_type(x, profile))) {
      next
    }
    title_vec <- names(.projr_yml_dest_get_type(x, profile))
    for (i in seq_along(title_vec)) {
      .projr_yml_build_check_dest_title(
        .projr_yml_dest_get_title(title_vec[i], x, profile),
        title = title_vec[i],
        type = x
      )
    }
  }
}

.projr_yml_build_check_dest_title <- function(yml_title, title, type) {
  .assert_string(title, TRUE)
  .assert_in(yml_title[["content"]], .projr_opt_dir_get_label_send(NULL), TRUE)
  .assert_in(yml_title[["structure"]], .projr_opt_remote_get_structure())
  .assert_string(yml_title[["path"]], type == "local")
  .assert_flag(yml_title[["path-append-label"]])
  .assert_string(yml_title[["id"]], type == "osf")
  .assert_nchar(yml_title[["id"]], 5L)
  if ("get" %in% names(yml_title)) {
    get_list <- yml_title[["get"]]
    .assert_in(names(get_list), c("strategy", "conflict"))
    .assert_in(
      get_list[["strategy"]], .projr_opt_remote_strategy_get()
    )
    .assert_in(
      get_list[["conflict"]], .projr_opt_remote_conflict_get()
    )
  }
  if ("send" %in% names(yml_title)) {
    send_list <- yml_title[["send"]]
    .assert_in(names(send_list), .projr_opt_remote_transfer_names_get())
    .assert_in(send_list[["cue"]], .projr_opt_cue_get())
    .assert_in(
      send_list[["strategy"]], .projr_opt_remote_strategy_get()
    )
    .assert_in(send_list[["conflict"]], .projr_opt_remote_conflict_get())
    .assert_in(
      send_list[["inspect"]], .projr_opt_remote_inspect_get()
    )
  }
  if ("source" %in% names(yml_title)) {
    source_elem <- yml_title[["source"]]
    if (!isTRUE(source_elem) && !isFALSE(source_elem)) {
      .assert_chr(source_elem)
      for (i in seq_along(source_elem)) {
        .assert_in(
          source_elem[[i]],
          .projr_opt_dir_get_label_send(NULL)
        )
      }
    }
  }
  invisible(TRUE)
}
