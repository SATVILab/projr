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
  .yml_dir_check(profile)
  .yml_build_check(profile)
  .yml_dev_check(profile)
  .yml_metadata_check(profile)
  .yml_scripts_check(profile)
  .yml_hooks_check_config(profile)
  .yml_cite_check_config(profile)
  .yml_restrictions_check_config(profile)
  invisible(TRUE)
}

# directory
# ----------------------
.yml_dir_check <- function(profile) {
  yml_dir <- .yml_dir_get(profile)
  nm_vec <- names(yml_dir)
  nm_vec_strip <- .dir_label_strip(nm_vec)
  .assert_len(nm_vec, unique(nm_vec) |> length())
  .assert_len(nm_vec, nm_vec[nzchar(nm_vec)] |> length())

  # Check that no directory label ends in "-empty"
  labels_ending_empty <- nm_vec[grepl("-empty$", nm_vec)]
  if (length(labels_ending_empty) > 0) {
    stop(
      "Directory labels must not end in '-empty'. ",
      "Invalid label(s): ", paste0(labels_ending_empty, collapse = ", "),
      call. = FALSE
    )
  }

  .assert_nz(nm_vec)
  .assert_detect_any(nm_vec_strip, "^cache")
  .assert_detect_any(nm_vec_strip, "^output")
  for (x in nm_vec) {
    .yml_dir_check_label(x, profile)
  }
}

# directory labels
# ----------------------

.yml_dir_check_label <- function(label, profile) {
  yml_label <- .yml_dir_get_label(label, profile)
  .assert_in(
    names(yml_label),
    c("path", "ignore-git", "ignore-rbuild", "ignore", "output", "source", "license", "hash", "package")
  )
  yml_label |>
    .yml_dir_check_label_path(label, profile) |>
    .yml_dir_check_label_ignore() |>
    .yml_dir_check_label_git_track_adjust() |>
    .yml_dir_check_label_output(label, profile)
}

.yml_dir_check_label_path <- function(yml_label, label, profile) {
  is_docs <- .yml_dir_label_class_detect_docs(label)
  if (is_docs && !"path" %in% names(yml_label)) {
    return(yml_label)
  }
  .assert_has(names(yml_label), "path")
  .assert_string(yml_label[["path"]], TRUE)
  .yml_dir_check_label_path_restricted(yml_label, label, profile)


  yml_label
}

.yml_dir_check_label_path_restricted <- function(yml_label, label, profile) {
  label_vec_restricted <- .yml_dir_get_label_in(profile) |>
    c(.yml_dir_get_label_output(profile))
  if (label %in% label_vec_restricted) {
    .assert_path_not_sub(yml_label[["path"]], c("data", "man", "R", "tests"))
  }
  invisible(TRUE)
}

.yml_dir_check_label_ignore <- function(yml_label) {
  for (x in c("ignore", "ignore-git", "ignore-rbuild")) {
    .yml_dir_check_label_ignore_ind(yml_label, x)
  }
  yml_label
}

.yml_dir_check_label_ignore_ind <- function(yml_label, nm) {
  if (nm %in% names(yml_label)) {
    val <- yml_label[[nm]]
    .assert_class_any(val, c("logical", "character"), TRUE)
    .assert_len_1(val, TRUE)
    if (is.logical(val)) .assert_flag(val)
    if (is.character(val)) .assert_in(val, c("manual", "ignore", "no-ignore"))
  }
  invisible(TRUE)
}

.yml_dir_check_label_git_track_adjust <- function(yml_label) {
  if (!"git-track-adjust" %in% names(yml_label)) {
    return(yml_label)
  }
  .assert_flag(yml_label[["git-track-adjust"]], TRUE)
  yml_label
}

.yml_dir_check_label_output <- function(yml_label, label, profile) {
  # Allow output key on any directory label
  if (!"output" %in% names(yml_label)) {
    return(yml_label)
  }

  .assert_class_any(yml_label[["output"]], c("logical", "character"), TRUE)
  if (is.logical(yml_label[["output"]])) {
    .assert_len_1(yml_label[["output"]], TRUE)
    .assert_flag(yml_label[["output"]])
  } else if (is.character(yml_label[["output"]])) {
    # Character can be a vector of output labels
    .assert_chr(yml_label[["output"]], TRUE)
    
    # Get available output labels - these are the labels that start with "output"
    available_output_labels <- .yml_dir_get_label_output(profile)
    
    # Validate that specified output labels exist
    .assert_in(
      yml_label[["output"]],
      available_output_labels
    )
  }
  
  yml_label
}

# build
# ----------------------

.yml_build_check <- function(profile) {
  .yml_build_check_label(profile)
  .yml_build_check_git(profile)
  .yml_build_check_dest(profile)
}

# build: label
# ----------------------

.yml_build_check_label <- function(profile) {
  yml_build <- .yml_build_get(profile)
  nm_vec <- names(yml_build)
  if (is.null(nm_vec) || .is_len_0(nm_vec)) {
    return(invisible(FALSE))
  }
  .assert_in(
    nm_vec,
    c(
      "dev-output", "script", "hooks", "scripts", "git",
      "github", "package", "local", "osf", "cite", "restrictions"
    )
  )
  .assert_flag(.yml_build_get_dev_output(profile))
}

# build: git
# ----------------------

.yml_build_check_git <- function(profile) {
  yml_git <- .yml_git_get(profile)
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

.yml_build_check_dest <- function(profile) {
  for (x in .opt_remote_get_type()) {
    if (is.null(.yml_dest_get_type(x, profile))) {
      next
    }
    title_vec <- names(.yml_dest_get_type(x, profile))
    for (i in seq_along(title_vec)) {
      .yml_build_check_dest_title(
        .yml_dest_get_title(title_vec[i], x, profile),
        title = title_vec[i],
        type = x
      )
    }
  }
}

.yml_build_check_dest_title <- function(yml_title, title, type) {
  .assert_string(title, TRUE)
  .assert_in(yml_title[["content"]], .opt_dir_get_label_send(NULL), TRUE)
  .assert_in(yml_title[["structure"]], .opt_remote_get_structure())
  .assert_string(yml_title[["path"]], type == "local")
  .assert_flag(yml_title[["path-append-label"]])
  .assert_string(yml_title[["id"]], type == "osf")
  .assert_nchar(yml_title[["id"]], 5L)
  if ("get" %in% names(yml_title)) {
    get_list <- yml_title[["get"]]
    .assert_in(names(get_list), c("strategy", "conflict"))
    .assert_in(
      get_list[["strategy"]], .opt_remote_strategy_get()
    )
    .assert_in(
      get_list[["conflict"]], .opt_remote_conflict_get()
    )
  }
  if ("send" %in% names(yml_title)) {
    send_list <- yml_title[["send"]]
    .assert_in(names(send_list), .opt_remote_transfer_names_get())
    .assert_in(send_list[["cue"]], .opt_cue_get())
    .assert_in(
      send_list[["strategy"]], .opt_remote_strategy_get()
    )
    .assert_in(send_list[["conflict"]], .opt_remote_conflict_get())
    .assert_in(
      send_list[["inspect"]], .opt_remote_inspect_get()
    )
  }
  if ("source" %in% names(yml_title)) {
    source_elem <- yml_title[["source"]]
    if (!isTRUE(source_elem) && !isFALSE(source_elem)) {
      .assert_chr(source_elem)
      for (i in seq_along(source_elem)) {
        .assert_in(
          source_elem[[i]],
          .opt_dir_get_label_send(NULL)
        )
      }
    }
  }
  invisible(TRUE)
}

# dev
# ----------------------

.yml_dev_check <- function(profile) {
  yml_dev <- .yml_dev_get(profile)
  if (is.null(yml_dev)) {
    return(invisible(TRUE))
  }

  # Only "scripts" and "hooks" keys are allowed under dev
  nm_vec <- names(yml_dev)
  if (!is.null(nm_vec) && length(nm_vec) > 0) {
    .assert_in(nm_vec, c("scripts", "hooks"))
  }

  invisible(TRUE)
}

# metadata
# ----------------------

.yml_metadata_check <- function(profile) {
  yml_metadata <- .yml_metadata_get(profile)
  if (is.null(yml_metadata) || length(yml_metadata) == 0) {
    return(invisible(TRUE))
  }

  # Check version-format if present
  if ("version-format" %in% names(yml_metadata)) {
    .yml_version_format_set_check(yml_metadata[["version-format"]])
  }

  invisible(TRUE)
}

# scripts
# ----------------------

.yml_scripts_check <- function(profile) {
  # Check build.scripts structure
  yml_scripts <- .yml_scripts_get(profile)
  if (!is.null(yml_scripts)) {
    # build.scripts should be a plain character vector
    if (!is.character(yml_scripts)) {
      stop("build.scripts must be a character vector")
    }
  }

  # Check dev.scripts structure
  yml_dev_scripts <- .yml_dev_get_scripts(profile)
  if (!is.null(yml_dev_scripts)) {
    # dev.scripts should be a plain character vector
    if (!is.character(yml_dev_scripts)) {
      stop("dev.scripts must be a character vector")
    }
  }

  invisible(TRUE)
}

# hooks config
# ----------------------

.yml_hooks_check_config <- function(profile) {
  # Check build.hooks structure
  yml_hooks <- .yml_hooks_get(profile)
  if (!is.null(yml_hooks)) {
    # Validate that only valid stage keys exist
    valid_stages <- c("pre", "post", "both")
    .assert_in(names(yml_hooks), valid_stages)

    # Validate that each stage contains character vectors
    for (stage in names(yml_hooks)) {
      stage_hooks <- yml_hooks[[stage]]
      if (!is.null(stage_hooks)) {
        if (!is.character(stage_hooks)) {
          stop(paste0("build.hooks.", stage, " must be a character vector"))
        }
      }
    }
  }

  # Check dev.hooks structure
  yml_dev_hooks <- .yml_dev_get_hooks(profile)
  if (!is.null(yml_dev_hooks)) {
    # Validate that only valid stage keys exist
    valid_stages <- c("pre", "post", "both")
    .assert_in(names(yml_dev_hooks), valid_stages)

    # Validate that each stage contains character vectors
    for (stage in names(yml_dev_hooks)) {
      stage_hooks <- yml_dev_hooks[[stage]]
      if (!is.null(stage_hooks)) {
        if (!is.character(stage_hooks)) {
          stop(paste0("dev.hooks.", stage, " must be a character vector"))
        }
      }
    }
  }

  invisible(TRUE)
}

# cite config
# ----------------------

.yml_cite_check_config <- function(profile) {
  yml_cite <- .yml_cite_get(profile)
  if (is.null(yml_cite)) {
    return(invisible(TRUE))
  }

  # If it's a logical, that's valid (TRUE or FALSE)
  if (is.logical(yml_cite)) {
    .assert_flag(yml_cite)
    return(invisible(TRUE))
  }

  # If it's a list, check the structure
  if (is.list(yml_cite)) {
    valid_keys <- c("codemeta", "cff", "inst-citation")
    .assert_in(names(yml_cite), valid_keys)

    # Check each value is logical
    for (key in names(yml_cite)) {
      .assert_flag(yml_cite[[key]])
    }

    return(invisible(TRUE))
  }

  # If it's neither logical nor list, it's invalid
  stop("build.cite must be either a logical value or a list with keys: codemeta, cff, inst-citation")
}

# scripts and hooks existence check
# ----------------------

.yml_scripts_hooks_check_exist <- function(profile = NULL) {
  # Check scripts
  scripts_build <- .yml_scripts_get_build(profile)
  if (!is.null(scripts_build)) {
    for (script in scripts_build) {
      if (!file.exists(script)) {
        stop(paste0("Build script '", script, "' does not exist."))
      }
    }
  }

  # Check dev scripts (only from dev.scripts, no fallback)
  scripts_dev <- .yml_dev_get_scripts(profile)
  if (!is.null(scripts_dev)) {
    for (script in scripts_dev) {
      if (!file.exists(script)) {
        stop(paste0("Dev script '", script, "' does not exist."))
      }
    }
  }

  # Check hooks (from build.hooks)
  for (stage in c("pre", "post")) {
    hooks <- .yml_hooks_get_stage(stage, profile)
    if (!is.null(hooks)) {
      for (hook in hooks) {
        if (!file.exists(hook)) {
          stop(paste0("Hook '", hook, "' (stage: ", stage, ") does not exist."))
        }
      }
    }
  }

  # Check dev hooks (from dev.hooks)
  dev_hooks_yml <- .yml_dev_get_hooks(profile)
  if (!is.null(dev_hooks_yml)) {
    for (stage in c("both", "pre", "post")) {
      if (stage %in% names(dev_hooks_yml)) {
        dev_hooks <- dev_hooks_yml[[stage]]
        if (!is.null(dev_hooks)) {
          for (hook in dev_hooks) {
            if (!file.exists(hook)) {
              stop(paste0("Dev hook '", hook, "' (stage: ", stage, ") does not exist."))
            }
          }
        }
      }
    }
  }

  invisible(TRUE)
}

# restrictions config
# ----------------------

.yml_restrictions_check_config <- function(profile) {
  yml_restrictions <- .yml_restrictions_get(profile)
  if (is.null(yml_restrictions)) {
    return(invisible(TRUE))
  }

  # Check that only valid keys exist
  valid_keys <- c("branch", "not_behind")
  .assert_in(names(yml_restrictions), valid_keys)

  # Check branch restriction if present
  if ("branch" %in% names(yml_restrictions)) {
    branch <- yml_restrictions[["branch"]]
    if (!is.null(branch)) {
      # Must be logical, character, or empty list (from YAML serialization)
      is_empty_list <- is.list(branch) && length(branch) == 0
      if (!is.logical(branch) && !is.character(branch) && !is_empty_list) {
        stop("build.restrictions.branch must be logical or character")
      }
      if (is.logical(branch)) {
        .assert_flag(branch)
      }
      if (is.character(branch)) {
        .assert_chr(branch)
      }
    }
  }

  # Check not_behind restriction if present
  if ("not_behind" %in% names(yml_restrictions)) {
    not_behind <- yml_restrictions[["not_behind"]]
    if (!is.null(not_behind)) {
      .assert_flag(not_behind)
    }
  }

  invisible(TRUE)
}
