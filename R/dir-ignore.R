.projr_ignore_label_set <- function(label, git_skip_adjust = TRUE) {
  dir_path <- .projr_dir_get(label, safe = FALSE)
  if (grepl("^archive", .projr_dir_label_strip(label))) {
    dir_path <- dirname(dir_path)
  }
  if (identical(dir_path, ".")) {
    return(invisible(TRUE))
  }

  within_wd <- fs::path_has_parent(dir_path, .dir_proj_get())
  if (!within_wd) {
    return(invisible(TRUE))
  }

  dir_path_rel <- fs::path_rel(dir_path, .dir_proj_get())
  dir_path_rel <- gsub("\\s*/*\\s*$", "", dir_path_rel) # nolint

  # rbuildignore
  ignore_rbuild <- .projr_ignore_get_rbuild(label)
  .projr_ignore_rbuild_set(dir_path_rel, ignore_rbuild)

  # git
  ignore_git <- .projr_ignore_get_git(label)
  git_skip_adjust <- .projr_ignore_get_git_skip_adjust(
    label = label, git_skip_adjust = git_skip_adjust
  )

  .projr_ignore_git_set(
    path = dir_path_rel,
    ignore_git = ignore_git,
    git_skip_adjust = git_skip_adjust
  )

  .projr_ignore_rbuild_set(
    path = dir_path_rel,
    ignore_rbuild = ignore_rbuild
  )

  invisible(TRUE)
}

# Git
# ========================================

.projr_ignore_get_git <- function(label) {
  yml_projr <- .projr_yml_get(NULL)
  ignore_git <- yml_projr[["directories"]][[label]][["ignore-git"]]
  if (is.null(ignore_git)) {
    return("ignore")
  }
  if (is.logical(ignore_git)) {
    if (ignore_git) {
      return("ignore")
    } else {
      return("no-ignore")
    }
  }
  if (!is.character(ignore_git)) {
    stop("ignore_git must be logical or character")
  }
  if (!length(ignore_git) == 1L) {
    stop("ignore_git must be length 1")
  }
  ignore_git <- gsub("^//s", "", ignore_git) |>
    gsub("//s$", "", x = _)
  extra_vec <- setdiff(ignore_git, c("no-ignore", "ignore", "manual"))
  if (length(extra_vec) > 0) {
    stop(paste0(
      'if of type character, then ignore_git must be "ignore", "no-ignore" or "manual"', # nolint
      ": problem with label `", label, "` in _projr.yml",
      collapse = ""
    ))
  }
  ignore_git
}

# get whether to adjust skipping or not
.projr_ignore_get_git_skip_adjust <- function(label, git_skip_adjust) {
  if (!is.null(git_skip_adjust)) {
    if (!is.logical(git_skip_adjust)) {
      stop("git_skip_adjust must be logical")
    }
    return(git_skip_adjust)
  }
  yml_projr <- .projr_yml_get(NULL)
  yml_projr_skip <- yml_projr[["directories"]][["label"]][["git_skip_adjust"]]
  if (is.null(yml_projr_skip)) {
    return(TRUE)
  }
  if (!is.logical(yml_projr_skip)) {
    stop("git_skip_adjust must be logical")
  }
  yml_projr_skip
}

.projr_ignore_git_set <- function(path, ignore_git, git_skip_adjust) {
  gitignore_vec <- .projr_ignore_git_read()
  top_line <-
    "# Start of projr section: do not edit by hand (until `# End of projr section`)" # nolint
  bottom_line <- "# End of projr section"
  match_str <-
    "^# Start of projr section: do not edit by hand \\(until `# End of projr section`\\)" # nolint
  projr_ignore_present_top <- grepl(match_str, gitignore_vec) |> any()
  projr_ignore_present_bot <- grepl(paste0("^", bottom_line), gitignore_vec) |>
    any()
  projr_ignore_present <- projr_ignore_present_top && projr_ignore_present_bot
  if (!projr_ignore_present) {
    if (ignore_git == "manual") {
      return(invisible(TRUE))
    } else {
      gitignore_vec <- c(gitignore_vec, top_line, bottom_line)
    }
  }

  # get indices to save in between
  projr_ignore_ind_start <- which(gitignore_vec == top_line)
  projr_ignore_ind_start <- projr_ignore_ind_start[[
    length(projr_ignore_ind_start)
  ]]
  projr_ignore_ind_end <- which(gitignore_vec == bottom_line)
  projr_ignore_ind_end <- projr_ignore_ind_end[[
    length(projr_ignore_ind_end)
  ]]
  if (projr_ignore_ind_start > projr_ignore_ind_end) {
    stop("projr section in .gitignore is malformed")
  }

  # get before, actual and after sections
  gitignore_vec_before <- gitignore_vec[seq_len(projr_ignore_ind_start - 1)]
  gitignore_vec_projr <- gitignore_vec[
    seq(projr_ignore_ind_start, projr_ignore_ind_end - 1)
  ]
  gitignore_vec_after <- gitignore_vec[
    seq(projr_ignore_ind_end, length(gitignore_vec))
  ]

  if (ignore_git == "manual") {
    if (projr_ignore_ind_start == projr_ignore_ind_end - 1) {
      gitignore_vec <- c(
        gitignore_vec_before,
        gitignore_vec_after[-1]
      )
      .projr_ignore_git_write(gitignore_vec, FALSE)
      return(invisible(TRUE))
    }
  }

  # add to gitignore_vec_projr if needed
  txt_add <- paste0(path, "/**")

  if (ignore_git == "ignore") {
    if (!txt_add %in% gitignore_vec_projr) {
      gitignore_vec_projr <- c(gitignore_vec_projr, txt_add)
    }
    # skip everything in directory that's tracked
    if (git_skip_adjust) {
      .projr_git_skip(path)
    }
  } else {
    # ensure that we can track things in directory again
    if (git_skip_adjust) {
      .projr_git_unskip(path)
    }
    gitignore_vec_projr <- setdiff(gitignore_vec_projr, txt_add)
    if (length(gitignore_vec_projr) == 0L) {
      gitignore_vec_projr <- ""
      gitignore_vec_after <- setdiff(
        gitignore_vec_after, "# End of projr section"
      )
    }
  }

  # return
  gitignore_vec <- c(
    gitignore_vec_before,
    gitignore_vec_projr,
    gitignore_vec_after
  )
  .projr_ignore_git_write(gitignore_vec, FALSE)
}

# skipping

.projr_git_skip <- function(path) {
  fn_vec_tracked <- .projr_git_get_tracked(path)
  if (length(fn_vec_tracked) == 0L) {
    return(invisible(TRUE))
  }
  # the problem with `git rm -r --cached`` is that
  # then these files will be deleted
  # upon `git pull` (see comments to
  # https://stackoverflow.com/a/19095988/7936619
  # and next/previous answer).
  # so we use skip-worktree instead (
  # https://stackoverflow.com/a/20241145/7936619):
  # get path to bit bucket that depends on Windows/non-Windows
  if (Sys.info()["sysname"] == "Windows") {
    path_bit_bucket <- "NUL"
  } else {
    path_bit_bucket <- "/dev/null"
  }
  # now stop skipping them:
  fn_vec_tracked <- paste0("'", fn_vec_tracked, "'")
  # break it up into units of fifty files:
  while (length(fn_vec_tracked) > 0L) {
    ind_vec <- seq_len(min(50L, length(fn_vec_tracked)))
    fn_vec_curr <- fn_vec_tracked[ind_vec]
    system2(
      "git",
      paste0(
        "update-index ",
        "--skip-worktree ",
        ">", path_bit_bucket,
        " 2>&1 -- ",
        paste0(fn_vec_curr, collapse = " "),
        collapse = ""
      )
    )
    fn_vec_tracked <- fn_vec_tracked[-ind_vec]
  }
  invisible(TRUE)
}

.projr_git_get_tracked <- function(path) {
  fn_vec <- system2(
    "git",
    paste0(
      "ls-files -v ",
      path,
      collapse = ""
    ),
    wait = TRUE,
    stdout = TRUE
  )
  if (length(fn_vec) == 0L) {
    return(character(0L))
  }
  fn_vec[grepl("^H|^R|^C", fn_vec)] |>
    gsub("^H |^R |^C ", "", x = _)
}


.projr_git_unskip <- function(path) {
  fn_vec_skipped <- .projr_git_get_skipped(path)
  if (length(fn_vec_skipped) == 0L) {
    return(invisible(TRUE))
  }
  # the problem with `git rm -r --cached`` is that
  # then these files will be deleted
  # upon `git pull` (see comments to
  # https://stackoverflow.com/a/19095988/7936619
  # and next/previous answer).
  # so we use skip-worktree instead (
  # https://stackoverflow.com/a/20241145/7936619):
  # get path to bit bucket that depends on Windows/non-Windows
  if (Sys.info()["sysname"] == "Windows") {
    path_bit_bucket <- "NUL"
  } else {
    path_bit_bucket <- "/dev/null"
  }
  # now stop skipping them:
  fn_vec_skipped <- paste0("'", fn_vec_skipped, "'")
  # break it up into units of fifty files:
  while (length(fn_vec_skipped) > 0L) {
    ind_vec <- seq_len(min(50L, length(fn_vec_skipped)))
    fn_vec_curr <- fn_vec_skipped[ind_vec]
    system2(
      "git",
      paste0(
        "update-index ",
        "--no-skip-worktree ",
        ">", path_bit_bucket,
        " 2>&1 -- ",
        paste0(fn_vec_curr, collapse = " "),
        collapse = ""
      )
    )
    fn_vec_skipped <- fn_vec_skipped[-ind_vec]
  }
  invisible(TRUE)
}

.projr_git_get_skipped <- function(path) {
  fn_vec <- system2(
    "git",
    paste0(
      "ls-files -v ",
      path,
      collapse = ""
    ),
    wait = TRUE,
    stdout = TRUE
  )
  if (length(fn_vec) == 0L) {
    return(character(0L))
  }
  fn_vec[grepl("^S", fn_vec)] |>
    gsub("^S ", "", x = _)
}






# getting and adjusting .gitignore
# --------------------------------
.projr_ignore_git_read <- function() {
  suppressWarnings(readLines(
    .dir_proj_get(".gitignore")
  ))
}

.projr_ignore_git_write <- function(gitignore, append) {
  cat(
    gitignore,
    file = .dir_proj_get(".gitignore"),
    sep = "\n",
    append = append
  )
  .projr_newline_append(.dir_proj_get(".gitignore"))
  invisible(.dir_proj_get(".gitignore"))
}

# Rbuildignore
# ========================================

.projr_ignore_get_rbuild <- function(label) {
  yml_projr <- .projr_yml_get(NULL)
  ignore_rbuild <- yml_projr[["directories"]][[label]][["ignore-rbuild"]]
  if (is.null(ignore_rbuild)) {
    return("ignore")
  }
  if (is.logical(ignore_rbuild)) {
    if (ignore_rbuild) {
      return("ignore")
    } else {
      return("no-ignore")
    }
  }
  if (!is.character(ignore_rbuild)) {
    stop("ignore_rbuild must be logical or character")
  }
  if (!length(ignore_rbuild) == 1L) {
    stop("ignore_rbuild must be length 1")
  }
  ignore_rbuild <- gsub("^//s", "", ignore_rbuild) |>
    gsub("//s$", "", x = _)
  extra_vec <- setdiff(ignore_rbuild, c("no-ignore", "ignore", "manual"))
  if (length(extra_vec) > 0) {
    stop(paste0(
      'if of type character, then ignore_rbuild must be "ignore", "no-ignore" or "manual"', # nolint
      ": problem with label `", label, "` in _projr.yml",
      collapse = ""
    ))
  }
  ignore_rbuild
}

.projr_ignore_rbuild_set <- function(path, ignore_rbuild) {
  rbuild_vec <- .projr_ignore_rbuild_read()
  top_line <-
    "# Start of projr section: do not edit by hand (until `# End of projr section`)" # nolint
  bottom_line <- "# End of projr section"
  match_str <-
    "^# Start of projr section: do not edit by hand \\(until `# End of projr section`\\)" # nolint
  projr_ignore_present_top <- grepl(match_str, rbuild_vec) |> any()
  projr_ignore_present_bot <- grepl(paste0("^", bottom_line), rbuild_vec) |>
    any()
  projr_ignore_present <- projr_ignore_present_top && projr_ignore_present_bot
  if (!projr_ignore_present) {
    if (ignore_rbuild == "manual") {
      return(invisible(TRUE))
    } else {
      rbuild_vec <- c(rbuild_vec, top_line, bottom_line)
    }
  }

  # get indices to save in between
  projr_ignore_ind_start <- which(rbuild_vec == top_line)
  projr_ignore_ind_start <- projr_ignore_ind_start[[
    length(projr_ignore_ind_start)
  ]]
  projr_ignore_ind_end <- which(rbuild_vec == bottom_line)
  projr_ignore_ind_end <- projr_ignore_ind_end[[
    length(projr_ignore_ind_end)
  ]]
  if (projr_ignore_ind_start > projr_ignore_ind_end) {
    stop("projr section in .Rbuildignore is malformed")
  }

  # get before, actual and after sections
  rbuild_vec_before <- rbuild_vec[seq_len(projr_ignore_ind_start - 1)]
  rbuild_vec_projr <- rbuild_vec[
    seq(projr_ignore_ind_start, projr_ignore_ind_end - 1)
  ]
  rbuild_vec_after <- rbuild_vec[
    seq(projr_ignore_ind_end, length(rbuild_vec))
  ]

  if (ignore_rbuild == "manual") {
    if (projr_ignore_ind_start == projr_ignore_ind_end - 1) {
      rbuild_vec <- c(
        rbuild_vec_before,
        rbuild_vec_after[-1]
      )
      .projr_ignore_rbuild_write(rbuild_vec, FALSE)
      return(invisible(TRUE))
    }
  }

  # add to rbuild_vec_projr if needed
  txt_add <- utils::glob2rx(path) |>
    gsub("\\$$", "", x = _) |> # ignore anything matching in sub-directory
    paste0("/") |>
    c(utils::glob2rx(path)) # ignore directory itself
  if (ignore_rbuild == "ignore") {
    for (x in txt_add) {
      if (!x %in% rbuild_vec_projr) {
        rbuild_vec_projr <- c(rbuild_vec_projr, txt_add)
      }
    }
  } else {
    rbuild_vec_projr <- setdiff(rbuild_vec_projr, txt_add)
    if (length(rbuild_vec_projr) == 0L) {
      rbuild_vec_projr <- ""
      rbuild_vec_after <- setdiff(
        rbuild_vec_after, "# End of projr section"
      )
    }
  }

  # return
  rbuild_vec <- c(
    rbuild_vec_before,
    rbuild_vec_projr,
    rbuild_vec_after
  )
  .projr_ignore_rbuild_write(rbuild_vec, FALSE)
}

.projr_ignore_rbuild_read <- function() {
  suppressWarnings(readLines(
    .dir_proj_get(".Rbuildignore")
  ))
}

.projr_ignore_rbuild_write <- function(buildignore, append) {
  cat(
    buildignore,
    file = .dir_proj_get(".Rbuildignore"),
    sep = "\n",
    append = append
  )
  .projr_newline_append(.dir_proj_get(".Rbuildignore"))
  invisible(.dir_proj_get(".Rbuildignore"))
}
