.projr_ignore_diryml <- function(git_skip_adjust = NULL) {
  # Update `.gitignore` and `.Rbuildignore` Using Projr Configuration
  #
  # The `projr_dir_ignore()` function updates the project’s `.gitignore` and
  # `.Rbuildignore` files to reflect the directories managed by Projr. 
  #
  # @param git_skip_adjust A logical flag indicating whether to explicitly enable (`TRUE`)
  #   or disable (`FALSE`) Git skip-worktree adjustments for the managed directories, overriding
  #   any settings from the project configuration. Defaults to `NULL`, which respects
  #   the existing project configuration. If this is not set there either,
  #   the effective default is `TRUE`.
  # 
  # @details 
  # 
  # The `projr_dir_ignore()` function updates the project’s `.gitignore` and
  # `.Rbuildignore` files to reflect the directories managed by Projr. It does so by:
  #
  # 1. Removing any previously established Projr-managed ignore entries.
  # 2. Re-inserting the correct ignores based on the current project configuration.
  #
  # As a result, directories specified in the Projr YAML configuration are correctly
  # excluded from version control (Git) and R package building processes.
  #
  # Optionally, you can control whether files within these directories should have
  # their Git skip-worktree flags adjusted. By default, `projr_dir_ignore()` will
  # respect the settings derived from the project configuration. However, you may
  # override this behavior using the `git_skip_adjust` argument.
  #
  # **Key Effects:**
  #
  # - Ensures `.gitignore` includes Projr-managed directories, keeping them out of Git commits.
  # - Ensures `.Rbuildignore` excludes these directories from R build processes.
  # - Optionally adjusts Git skip-worktree flags for tracked files in these directories.
  .projr_ignore_diryml_git(git_skip_adjust)
  .projr_ignore_diryml_rbuild()
  invisible(TRUE)
}


.projr_ignore_diryml_git <- function(git_skip_adjust = NULL) {

  if (!.projr_git_repo_check_exists() &&
        !file.exists(.dir_proj_get(".gitignore"))) {
    return(invisible(FALSE))
  }

  # get paths to ignore, skip and/or unskip
  instr_list <- .projr_ignore_diryml_git_get_instructions(git_skip_adjust)

  # ignore, skip and unskip paths
  .projr_ignore_diryml_git_update_gitignore(instr_list$ignore)
  .projr_ignore_diryml_git_update_skip(instr_list$skip, instr_list$unskip)

  invisible(TRUE)
}

# .gitignore Management (As previously defined)
.projr_ignore_diryml_git_get_instructions <- function(git_skip_adjust = NULL) {
  # get which paths to ignore, skip, and/or unskip
  yml_dir <- .projr_yml_dir_get(NULL)
  path_ignore <- path_skip <- path_unskip <- character(0)
  
  for (i in seq_along(yml_dir)) {
    instr_list_label <- .projr_ignore_diryml_git_get_instructions_label(
      names(yml_dir)[[i]], git_skip_adjust
    )
    path_ignore <- c(path_ignore, instr_list_label$ignore)
    path_skip <- c(path_skip, instr_list_label$skip)
    path_unskip <- c(path_unskip, instr_list_label$unskip)
  }
  list(ignore = path_ignore, skip = path_skip, unskip = path_unskip)
}

.projr_ignore_diryml_git_get_instructions_label <- function(label,
                                                         git_skip_adjust) {
  path <- .projr_ignore_diryml_path_get(label)
  if (length(path) == 0) {
    c0 <- character(0L)
    return(list(ignore = c0, skip = c0, unskip = c0))
  }

  .projr_ignore_diryml_git_get_instructions_label_actual(
    path, label, git_skip_adjust
  )
}

.projr_ignore_diryml_git_get_instructions_label_actual <- function(path,
                                                                label,
                                                                git_skip_adjust) {
  # ignore
  ignore_git <- .projr_ignore_get_git(label)
  path_ignore <- if (ignore_git) paste0(path, "/**") else character(0L)

  # skip
  git_skip_adjust <- .projr_ignore_get_git_skip_adjust(
    label, git_skip_adjust
    )

  if (!git_skip_adjust) {
    path_skip <- character(0L)
    path_unskip <- character(0L)
  } else {
    path_skip <- if (ignore_git) path else character(0L)
    path_unskip <- if (!ignore_git) path else character(0L)
  }
  list(ignore = path_ignore, skip = path_skip, unskip = path_unskip)
}

.projr_ignore_diryml_git_update_gitignore <- function(ignore) {

  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }

  file_vec <- .projr_ignore_path_add_get_updated(path, ignore, TRUE)
  .projr_ignore_path_write(file_vec, path)
  invisible(TRUE)
}




.projr_ignore_get_git <- function(label) {
  ignore_git <- .projr_yml_dir_get_label(label, NULL)[["ignore-git"]]
  ignore <- .projr_yml_dir_get_label(label, NULL)[["ignore"]]
  if (is.null(ignore_git) && is.null(ignore)) {
    return(TRUE)
  }
  if (.is_flag(ignore_git) && .is_flag(ignore)) {
    no_match <- ignore_git != ignore
    if (no_match) {
      stop(paste0(
        "Inconsistent ignore settings for label '", label, "': ",
        "ignore-git = ", ignore_git, ", ignore = ", ignore
      ))
    }
  }
  if (is.null(ignore_git)) {
    .assert_flag(ignore)
    return(ignore)
  }
  .assert_flag(ignore_git)
  ignore_git
}

.projr_ignore_get_git_skip_adjust <- function(label, git_skip_adjust) {
  if (!.projr_ignore_check_git_skip_adjust_possible()) {
    return(invisible(FALSE))
  }
  if (!is.null(git_skip_adjust)) {
    .assert_flag(git_skip_adjust)
    return(git_skip_adjust)
  }
  git_skip_adjust <- .projr_yml_dir_get_label(label, NULL)[["git-skip-adjust"]]
  if (is.null(git_skip_adjust)) {
    return(TRUE)
  }
  .assert_flag(git_skip_adjust)
  git_skip_adjust
}

.projr_ignore_check_git_skip_adjust_possible <- function() {
  # need to have git installed and in be in a git repo
  .projr_git_repo_check_exists() && .projr_git_system_check_git()
}

.projr_ignore_diryml_git_update_skip <- function(paths_to_ignore,
                                              paths_to_unignore) {
  # Adjust skip-worktree flags for ignored paths
  for (i in seq_along(paths_to_ignore)) {
    .projr_git_skip(paths_to_ignore[[i]])
  }
  
  # Adjust skip-worktree flags for unignored paths
  for (i in seq_along(paths_to_unignore)) {
    .projr_git_unskip(paths_to_unignore[[i]])
  }
  invisible(TRUE)
}

.projr_git_skip <- function(path) {
  # Set skip-worktree flag on all tracked files within the specified path
  fn_vec_tracked <- .projr_git_get_tracked(path)
  if (length(fn_vec_tracked) == 0L) {
    return(invisible(TRUE))
  }
  
  # Determine the appropriate bit bucket based on the operating system
  if (Sys.info()["sysname"] == "Windows") {
    path_bit_bucket <- "NUL"
  } else {
    path_bit_bucket <- "/dev/null"
  }
  
  # Wrap file paths in single quotes to handle spaces and special characters
  fn_vec_tracked <- paste0("'", fn_vec_tracked, "'")
  
  # Process files in batches of 50 to avoid command line length issues
  while (length(fn_vec_tracked) > 0L) {
    ind_vec <- seq_len(min(50L, length(fn_vec_tracked)))
    fn_vec_curr <- fn_vec_tracked[ind_vec]
    
    # Construct and execute the git command to set skip-worktree
    cmd <- paste0(
      "update-index --skip-worktree ",
      paste0(fn_vec_curr, collapse = " ")
    )
    system2("git", cmd, stdout = path_bit_bucket, stderr = path_bit_bucket)
    
    # Remove processed files from the list
    fn_vec_tracked <- fn_vec_tracked[-ind_vec]
  }
  
  invisible(TRUE)
}



.projr_git_unskip <- function(path) {
  # Remove skip-worktree flag from all tracked files within the specified path
  fn_vec_skipped <- .projr_git_get_skipped(path)
  if (length(fn_vec_skipped) == 0L) {
    return(invisible(TRUE))
  }
  
  # Determine the appropriate bit bucket based on the operating system
  if (Sys.info()["sysname"] == "Windows") {
    path_bit_bucket <- "NUL"
  } else {
    path_bit_bucket <- "/dev/null"
  }
  
  # Wrap file paths in single quotes to handle spaces and special characters
  fn_vec_skipped <- paste0("'", fn_vec_skipped, "'")
  
  # Process files in batches of 50 to avoid command line length issues
  while (length(fn_vec_skipped) > 0L) {
    ind_vec <- seq_len(min(50L, length(fn_vec_skipped)))
    fn_vec_curr <- fn_vec_skipped[ind_vec]
    
    # Construct and execute the git command to remove skip-worktree
    cmd <- paste0(
      "update-index --no-skip-worktree ",
      paste0(fn_vec_curr, collapse = " ")
    )
    system2("git", cmd, stdout = path_bit_bucket, stderr = path_bit_bucket)
    
    # Remove processed files from the list
    fn_vec_skipped <- fn_vec_skipped[-ind_vec]
  }
  
  invisible(TRUE)
}

.projr_git_get_tracked <- function(path) {
  # Retrieve a list of tracked files within the specified path
  fn_vec <- system2(
    "git",
    args = c("ls-files", "--", path),
    stdout = TRUE,
    stderr = FALSE
  )
  
  if (length(fn_vec) == 0L) {
    return(character(0L))
  }
  
  # Return the list of tracked file paths
  fn_vec
}

.projr_git_get_skipped <- function(path) {
  # Retrieve a list of files with skip-worktree set within the specified path
  fn_vec <- system2(
    "git",
    args = c("ls-files", "-v", "--", path),
    stdout = TRUE,
    stderr = FALSE
  )
  
  if (length(fn_vec) == 0L) {
    return(character(0L))
  }
  
  # Filter files with status 'S' indicating skip-worktree is set
  skipped_files <- fn_vec[grepl("^S", fn_vec)]
  
  # Extract the file paths from the output
  skipped_files <- sub("^S ", "", skipped_files)
  
  skipped_files
}


# .Rbuildignore Management
.projr_ignore_diryml_rbuild <- function() {
  if (!file.exists(.dir_proj_get("DESCRIPTION")) &&
        !file.exists(.dir_proj_get(".Rbuildignore"))) {
    return(invisible(FALSE))
  }
  # Remove all projr-managed entries from .Rbuildignore,
  # then add back applicable ones
  rbuildignore_list <- .projr_ignore_diryml_rbuildignore_get()
  
  yml_dir <- .projr_yml_dir_get(NULL)
  ignore_vec_dir <- character(0)
  
  for (i in seq_along(yml_dir)) {
    label <- names(yml_dir)[[i]]
    if (!.projr_ignore_get_rbuild(label)) {
      next
    }
    path <- .projr_ignore_diryml_path_get(label)
    if (length(path) == 0) {
      next
    }
    
    # Construct ignore patterns for .Rbuildignore
    patterns <- utils::glob2rx(path)
    patterns <- gsub("\\$$", "", patterns)
    patterns <- paste0(patterns, "/")
    patterns <- c(patterns, utils::glob2rx(path))
    
    ignore_vec_dir <- c(ignore_vec_dir, patterns)
  }
  
  rbuildignore_vec <- c(
    rbuildignore_list$start,
    ignore_vec_dir,
    rbuildignore_list$end
  )
  
  .projr_ignore_rbuild_write(rbuildignore_vec, append = FALSE)
  invisible(TRUE)
}

.projr_ignore_get_rbuild <- function(label) {
  ignore_rbuild <- .projr_yml_dir_get_label(label, NULL)[["ignore-rbuild"]]
  ignore <- .projr_yml_dir_get_label(label, NULL)[["ignore"]]
  if (is.null(ignore_rbuild) && is.null(ignore)) {
    return(TRUE)
  }
  if (.is_flag(ignore_rbuild) && .is_flag(ignore)) {
    no_match <- ignore_rbuild != ignore
    if (no_match) {
      stop(paste0(
        "Inconsistent ignore settings for label '", label, "': ",
        "ignore-git = ", ignore_rbuild, ", ignore = ", ignore
      ))
    }
  }
  if (is.null(ignore_rbuild)) {
    .assert_flag(ignore)
    return(ignore)
  }
  .assert_flag(ignore_rbuild)
  if (ignore_rbuild) TRUE else FALSE
}

.projr_ignore_diryml_rbuildignore_get <- function() {
  # Retrieve the current .Rbuildignore content and extract projr-managed sections
  rbuildignore_vec <- .projr_ignore_rbuild_read()
  
  if (length(rbuildignore_vec) == 0) {
    return(list(start = character(0), end = character(0)))
  }
  
  .projr_ignore_diryml_rbuildignore_get_check(
    match_str_top, match_str_bottom, rbuildignore_vec
  )
  
  projr_ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))

  if (length(projr_ignore_ind_top) == 0L) {
    return(list(
      start = c(
        rbuildignore_vec,
        "# Start of projr section: do not edit by hand (update with projr_ignore())"
      ),
      end = "# End of projr section"
    ))
  }
  
  list(
    start = rbuildignore_vec[seq_len(projr_ignore_ind_top)],
    end = rbuildignore_vec[seq(projr_ignore_ind_bot, length(rbuildignore_vec))]
  )
}

.projr_ignore_diryml_rbuildignore_get_check <- function(match_str_top,
                                                      match_str_bottom,
                                                      rbuildignore_vec) {
  # validate that the Projr-managed section in .Rbuildignore is well-formed
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))
  projr_ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
  
  if (length(projr_ignore_ind_top) > 1 ||
      length(projr_ignore_ind_bot) > 1) {
    stop("Multiple projr sections found in .Rbuildignore")
  }
  
  found_top <- length(projr_ignore_ind_top) == 1
  found_bottom <- length(projr_ignore_ind_bot) == 1
  
  if (found_top && !found_bottom) {
    stop("Found start of projr section but not end in .Rbuildignore")
  }
  
  if (!found_top && found_bottom) {
    stop("Found end of projr section but not start in .Rbuildignore")
  }
  
  if (found_top && found_bottom) {
    if (projr_ignore_ind_top > projr_ignore_ind_bot) {
      stop("Start of projr section found after end in .Rbuildignore")
    }
  }
  
  invisible(TRUE)
}

# Path Processing Function (Reused)
.projr_ignore_diryml_path_get <- function(label) {
  path_dir <- .projr_dir_get(label, safe = FALSE)
  if (identical(path_dir, ".")) {
    return(character(0))
  }
  
  within_wd <- fs::path_has_parent(path_dir, .dir_proj_get())
  if (!within_wd) {
    return(character(0))
  }
  
  path_dir_rel <- fs::path_rel(path_dir, .dir_proj_get())
  # Remove trailing slashes and spaces
  path_dir_rel <- gsub("\\s*/*\\s*$", "", path_dir_rel) 
  path_dir_rel
}

# Reading and Writing .Rbuildignore
.projr_ignore_rbuild_read <- function() {
  # Read the contents of .Rbuildignore, returning an empty
  # character vector if the file doesn't exist
  file_path <- .dir_proj_get(".Rbuildignore")
  if (!file.exists(file_path)) {
    return(character(0))
  }
  suppressWarnings(readLines(file_path, warn = FALSE))
}

.projr_ignore_rbuild_write <- function(buildignore, append = FALSE) {
  # Write the updated .Rbuildignore content, ensuring it ends with a newline
  file_path <- .dir_proj_get(".Rbuildignore")
  # Write the content to .Rbuildignore
  cat(
    buildignore,
    file = file_path,
    sep = "\n",
    append = append
  )
  # Ensure the file ends with a newline
  .projr_newline_append(file_path)
  invisible(file_path)
}
