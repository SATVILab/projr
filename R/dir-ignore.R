.ignore_diryml <- function(git_skip_adjust = NULL) {
  # Update `.gitignore` and `.Rbuildignore` Using Projr Configuration
  #
  # The .dir_ignore()` function updates the project’s `.gitignore` and
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
  # The .dir_ignore()` function updates the project’s `.gitignore` and
  # `.Rbuildignore` files to reflect the directories managed by Projr. It does so by:
  #
  # 1. Removing any previously established Projr-managed ignore entries.
  # 2. Re-inserting the correct ignores based on the current project configuration.
  #
  # As a result, directories specified in the Projr YAML configuration are correctly
  # excluded from version control (Git) and R package building processes.
  #
  # Optionally, you can control whether files within these directories should have
  # their Git skip-worktree flags adjusted. By default, .dir_ignore()` will
  # respect the settings derived from the project configuration. However, you may
  # override this behavior using the `git_skip_adjust` argument.
  #
  # **Key Effects:**
  #
  # - Ensures `.gitignore` includes Projr-managed directories, keeping them out of Git commits.
  # - Ensures `.Rbuildignore` excludes these directories from R build processes.
  # - Optionally adjusts Git skip-worktree flags for tracked files in these directories.
  .ignore_diryml_git(git_skip_adjust)
  .ignore_diryml_rbuild()
  invisible(TRUE)
}


.ignore_diryml_git <- function(git_skip_adjust = NULL) {

  if (!.git_repo_check_exists() &&
        !file.exists(.path_get(".gitignore"))) {
    return(invisible(FALSE))
  }

  # get paths to ignore, skip and/or unskip
  instr_list <- .ignore_diryml_git_get_instructions(git_skip_adjust)

  # ignore, skip and unskip paths
  .ignore_diryml_git_update_gitignore(instr_list$ignore)
  .ignore_diryml_git_update_skip(instr_list$skip, instr_list$unskip)

  invisible(TRUE)
}

# .gitignore Management (As previously defined)
.ignore_diryml_git_get_instructions <- function(git_skip_adjust = NULL) {
  # get which paths to ignore, skip, and/or unskip
  label_vec <- .ignore_diryml_git_get_instructions_labels()
  
  path_ignore <- path_skip <- path_unskip <- character(0)
  
  for (i in seq_along(label_vec)) {
    instr_list_label <- .ignore_diryml_git_get_instructions_label(
      label_vec[[i]], git_skip_adjust
    )
    path_ignore <- c(path_ignore, instr_list_label$ignore)
    path_skip <- c(path_skip, instr_list_label$skip)
    path_unskip <- c(path_unskip, instr_list_label$unskip)
  }
  list(ignore = path_ignore, skip = path_skip, unskip = path_unskip)
}

.ignore_diryml_git_get_instructions_labels <- function() {
  # Get directory labels and their paths,
  # ensuring directory types not specified are also included.
  yml_dir <- .yml_dir_get(NULL) # Fetch directory labels and paths
  label_vec <- names(yml_dir) # Extract the labels
  label_vec_strip <- .dir_label_strip(label_vec) # Strip unnecessary parts of the labels
  
  # Define patterns to match standard directory labels
  label_vec_match <- c("^docs", "^raw", "^cache", "^output")
  label_vec_rep <- c("docs", "raw-data", "cache", "output") # Standardized labels
  
  # Identify missing standard labels
  label_vec_missing_ind <- vapply(seq_along(label_vec_match), function(i) {
    !any(grepl(label_vec_match[[i]], label_vec_strip))
  }, logical(1))
  label_vec_missing <- label_vec_rep[label_vec_missing_ind] # Extract missing labels
  
  # Combine existing and missing labels
  c(label_vec, label_vec_missing)
}

.ignore_diryml_git_get_instructions_label <- function(label,
                                                         git_skip_adjust) {
  path <- .ignore_diryml_path_get(label)
  if (length(path) == 0) {
    c0 <- character(0L)
    return(list(ignore = c0, skip = c0, unskip = c0))
  }

  .ignore_diryml_git_get_instructions_label_impl(
    path, label, git_skip_adjust
  )
}

.ignore_diryml_git_get_instructions_label_impl <- function(path,
                                                                label,
                                                                git_skip_adjust) {
  # ignore
  ignore_git <- .ignore_get_git(label)
  path_ignore <- if (ignore_git) paste0(path, "/**") else character(0L)

  # skip
  git_skip_adjust <- .ignore_get_git_skip_adjust(
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

.ignore_diryml_git_update_gitignore <- function(ignore) {

  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  path_gitignore <- .path_get(".gitignore")

  file_vec <- .ignore_auto_path_add_get_updated(path_gitignore, ignore, TRUE)
  .ignore_path_write(file_vec, path_gitignore)
  invisible(TRUE)
}




.ignore_get_git <- function(label) {
  ignore_git <- .yml_dir_get_label(label, NULL)[["ignore-git"]]
  ignore <- .yml_dir_get_label(label, NULL)[["ignore"]]
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

.ignore_get_git_skip_adjust <- function(label, git_skip_adjust) {
  if (!.ignore_check_git_skip_adjust_possible()) {
    return(invisible(FALSE))
  }
  if (!is.null(git_skip_adjust)) {
    .assert_flag(git_skip_adjust)
    return(git_skip_adjust)
  }
  git_skip_adjust <- .yml_dir_get_label(label, NULL)[["git-skip-adjust"]]
  if (is.null(git_skip_adjust)) {
    return(TRUE)
  }
  .assert_flag(git_skip_adjust)
  git_skip_adjust
}

.ignore_check_git_skip_adjust_possible <- function() {
  # need to have git installed and in be in a git repo
  .git_repo_check_exists() && .git_system_check_git()
}

.ignore_diryml_git_update_skip <- function(paths_to_ignore,
                                              paths_to_unignore) {
  # Adjust skip-worktree flags for ignored paths
  for (i in seq_along(paths_to_ignore)) {
    .git_skip(paths_to_ignore[[i]])
  }
  
  # Adjust skip-worktree flags for unignored paths
  for (i in seq_along(paths_to_unignore)) {
    .git_unskip(paths_to_unignore[[i]])
  }
  invisible(TRUE)
}

.git_skip <- function(path) {
  # Set skip-worktree flag on all tracked files within the specified path
  fn_vec_tracked <- .git_get_tracked(path)
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



.git_unskip <- function(path) {
  # Remove skip-worktree flag from all tracked files within the specified path
  fn_vec_skipped <- .git_get_skipped(path)
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

.git_get_tracked <- function(path) {
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

.git_get_skipped <- function(path) {
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
.ignore_diryml_rbuild <- function() {
  if (!file.exists(.path_get("DESCRIPTION")) &&
        !file.exists(.path_get(".Rbuildignore"))) {
    return(invisible(FALSE))
  }
  # Remove all projr-managed entries from .Rbuildignore,
  # then add back applicable ones
  rbuildignore_list <- .ignore_diryml_rbuildignore_get()
  
  yml_dir <- .yml_dir_get(NULL)
  ignore_vec_dir <- character(0)
  
  for (i in seq_along(yml_dir)) {
    label <- names(yml_dir)[[i]]
    if (!.ignore_get_rbuild(label)) {
      next
    }
    path <- .ignore_diryml_path_get(label)
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
  
  .ignore_rbuild_write(rbuildignore_vec, append = FALSE)
  invisible(TRUE)
}

.ignore_get_rbuild <- function(label) {
  ignore_rbuild <- .yml_dir_get_label(label, NULL)[["ignore-rbuild"]]
  ignore <- .yml_dir_get_label(label, NULL)[["ignore"]]
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

.ignore_diryml_rbuildignore_get <- function() {
  # Retrieve the current .Rbuildignore content and extract projr-managed sections
  rbuildignore_vec <- .ignore_rbuild_read()
  
  if (length(rbuildignore_vec) == 0) {
    return(list(start = character(0), end = character(0)))
  }
  
  .ignore_diryml_rbuildignore_get_check(
    match_str_top, match_str_bottom, rbuildignore_vec
  )
  
 .ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
 .ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))

  if (length.ignore_ind_top) == 0L) {
    return(list(
      start = c(
        rbuildignore_vec,
        "# Start of projr section: do not edit by hand (update with.ignore_auto())"
      ),
      end = "# End of projr section"
    ))
  }
  
  list(
    start = rbuildignore_vec[seq_len.ignore_ind_top)],
    end = rbuildignore_vec[seq.ignore_ind_bot, length(rbuildignore_vec))]
  )
}

.ignore_diryml_rbuildignore_get_check <- function(match_str_top,
                                                      match_str_bottom,
                                                      rbuildignore_vec) {
  # validate that the Projr-managed section in .Rbuildignore is well-formed
 .ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))
 .ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
  
  if (length.ignore_ind_top) > 1 ||
      length.ignore_ind_bot) > 1) {
    stop("Multiple projr sections found in .Rbuildignore")
  }
  
  found_top <- length.ignore_ind_top) == 1
  found_bottom <- length.ignore_ind_bot) == 1
  
  if (found_top && !found_bottom) {
    stop("Found start of projr section but not end in .Rbuildignore")
  }
  
  if (!found_top && found_bottom) {
    stop("Found end of projr section but not start in .Rbuildignore")
  }
  
  if (found_top && found_bottom) {
    if .ignore_ind_top >.ignore_ind_bot) {
      stop("Start of projr section found after end in .Rbuildignore")
    }
  }
  
  invisible(TRUE)
}

# Path Processing Function (Reused)
.ignore_diryml_path_get <- function(label) {
  path_dir <- .dir_get(label, safe = FALSE)
  if (identical(path_dir, ".")) {
    return(character(0))
  }
  
  within_wd <- fs::path_has_parent(path_dir, .path_get())
  if (!within_wd) {
    return(character(0))
  }
  
  path_dir_rel <- fs::path_rel(path_dir, .path_get())
  # Remove trailing slashes and spaces
  path_dir_rel <- gsub("\\s*/*\\s*$", "", path_dir_rel) 
  path_dir_rel
}

# Reading and Writing .Rbuildignore
.ignore_rbuild_read <- function() {
  # Read the contents of .Rbuildignore, returning an empty
  # character vector if the file doesn't exist
  file_path <- .path_get(".Rbuildignore")
  if (!file.exists(file_path)) {
    return(character(0))
  }
  suppressWarnings(readLines(file_path, warn = FALSE))
}

.ignore_rbuild_write <- function(buildignore, append = FALSE) {
  # Write the updated .Rbuildignore content, ensuring it ends with a newline
  file_path <- .path_get(".Rbuildignore")
  # Write the content to .Rbuildignore
  cat(
    buildignore,
    file = file_path,
    sep = "\n",
    append = append
  )
  # Ensure the file ends with a newline
  .newline_append(file_path)
  invisible(file_path)
}
