#' Update .gitignore and .Rbuildignore with Projr-managed Directories
#'
#' The `projr_dir_ignore` function updates both `.gitignore` and `.Rbuildignore` files
#' by removing existing Projr-managed entries and adding back the relevant ones based on
#' the current project configuration. This ensures that directories specified in the
#' Projr configuration are appropriately ignored by Git and R build processes.
#'
#' @export
projr_dir_ignore <- function() {
  .projr_dir_ignore_git()
  .projr_dir_ignore_rbuild()
  invisible(TRUE)
}





.projr_dir_ignore_git <- function(git_skip_adjust = NULL) {

  instruction_list <- .projr_dir_ignore_git_get_instructions()

  .projr_dir_ignore_git_update_gitignore(
    instruction_list$ignore_vec_dir
  )
  .projr_dir_ignore_git_update_skip(
    instruction_list$paths_to_ignore,
    instruction_list$paths_to_unignore,
    git_skip_adjust
  )

  invisible(TRUE)
}

.projr_dir_ignore_git_update_gitignore <- function(ignore) {
  gitignore_list <- .projr_dir_ignore_gitignore_get()
  # Update the .gitignore content with new ignore patterns
  gitignore_vec <- c(
    gitignore_list$start,
    ignore,
    gitignore_list$end
  )
  .projr_ignore_git_write(gitignore_vec, FALSE)
  invisible(TRUE)
}

# .gitignore Management (As previously defined)
.projr_dir_ignore_git_get_instructions <- function() {
  # Initialize vectors to track paths to ignore and unignore
  yml_dir <- .projr_yml_dir_get(NULL)
  ignore_vec_dir <- character(0)
  paths_to_ignore <- character(0)
  paths_to_unignore <- character(0)
  
  for (i in seq_along(yml_dir)) {
    label <- names(yml_dir)[[i]]
    ignore_git <- .projr_ignore_get_git(label)
    

    path <- .projr_dir_ignore_path_get(label)

    if (length(path) == 0) {
      next
    }
    if (ignore_git == "ignore") {
      # Add to ignore vector
      ignore_pattern <- paste0(path, "/**")
      ignore_vec_dir <- c(ignore_vec_dir, ignore_pattern)
      paths_to_ignore <- c(paths_to_ignore, path)
    } else if (ignore_git == "no-ignore") {
      # Collect paths to unignore
      paths_to_unignore <- c(paths_to_unignore, path)
    }
  }
  list(
    ignore_vec_dir = ignore_vec_dir,
    paths_to_ignore = paths_to_ignore,
    paths_to_unignore = paths_to_unignore
  )
}

.projr_dir_ignore_gitignore_get <- function() {
  # Retrieve the current .gitignore content and extract projr-managed sections
  gitignore_vec <- .projr_ignore_git_read()
  if (length(gitignore_vec) == 0) {
    return(list(start = character(0), end = character(0)))
  }
  
  top_line <- "# Start of projr section: do not edit by hand (until `# End of projr section`)"
  bottom_line <- "# End of projr section"
  match_str_top <- "^# Start of projr section: do not edit by hand \\(until `# End of projr section`\\)"
  match_str_bottom <- paste0("^", bottom_line)
  
  .projr_dir_ignore_gitignore_get_check(
    match_str_top, match_str_bottom, gitignore_vec
  )

  projr_ignore_ind_top <- which(grepl(match_str_top, gitignore_vec))
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, gitignore_vec))

  if (length(projr_ignore_ind_top) == 0L) {
    return(list(start = character(0), end = character(0)))
  }
  
  list(
    start = gitignore_vec[seq_len(projr_ignore_ind_top)],
    end = gitignore_vec[seq(projr_ignore_ind_bot, length(gitignore_vec))]
  )
}

.projr_dir_ignore_gitignore_get_check <- function(match_str_top,
                                                  match_str_bottom,
                                                  gitignore_vec) {
  # Validate that the projr-managed section in .gitignore is well-formed                                                 
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, gitignore_vec))
  projr_ignore_ind_top <- which(grepl(match_str_top, gitignore_vec))
  
  if (length(projr_ignore_ind_top) > 1 ||
      length(projr_ignore_ind_bot) > 1) {
    stop("Multiple projr sections found in .gitignore")
  }
  
  found_top <- length(projr_ignore_ind_top) == 1
  found_bottom <- length(projr_ignore_ind_bot) == 1
  
  if (found_top && !found_bottom) {
    stop("Found start of projr section but not end in .gitignore")
  }
  
  if (!found_top && found_bottom) {
    stop("Found end of projr section but not start in .gitignore")
  }
  
  if (found_top && found_bottom) {
    if (projr_ignore_ind_top > projr_ignore_ind_bot) {
      stop("Start of projr section found after end in .gitignore")
    }
  }
  
  invisible(TRUE)
}

.projr_dir_ignore_git_update_skip <- function(paths_to_ignore,
                                              paths_to_unignore,
                                              git_skip_adjust) {
  if (isFALSE(git_skip_adjust)) {
    return(invisible(FALSE))
  }

  # Adjust skip-worktree flags for ignored paths
  for (i in seq_along(paths_to_ignore)) {
    path <- paths_to_ignore[[i]]
    if (git_skip_adjust) {
      .projr_git_skip(path)
    } else if (.projr_ignore_get_git_skip_adjust(path)) {
      .projr_git_skip(path)
    }
  }
  
  # Adjust skip-worktree flags for unignored paths
  for (i in seq_along(paths_to_unignore)) {
    path <- paths_to_unignore[[i]]
    if (git_skip_adjust) {
      .projr_git_unskip(path)
    } else if (.projr_ignore_get_git_skip_adjust(path)) {
      .projr_git_unskip(path)
    }
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
.projr_dir_ignore_rbuild <- function(k) {
  # Remove all projr-managed entries from .Rbuildignore,
  # then add back applicable ones
  rbuildignore_list <- .projr_dir_ignore_rbuildignore_get()
  
  yml_dir <- .projr_yml_dir_get(NULL)
  ignore_vec_dir <- character(0)
  
  for (i in seq_along(yml_dir)) {
    label <- names(yml_dir)[[i]]
    if (.projr_ignore_get_rbuild(label) != "ignore") {
      next
    }
    path <- .projr_dir_ignore_path_get(label)
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

.projr_dir_ignore_rbuildignore_get <- function() {
  # Retrieve the current .Rbuildignore content and extract projr-managed sections
  rbuildignore_vec <- .projr_ignore_rbuild_read()
  
  if (length(rbuildignore_vec) == 0) {
    return(list(start = character(0), end = character(0)))
  }
  
  top_line <- "# Start of projr section: do not edit by hand (until `# End of projr section`)"
  bottom_line <- "# End of projr section"
  match_str_top <- "^# Start of projr section: do not edit by hand \\(until `# End of projr section`\\)"
  match_str_bottom <- paste0("^", bottom_line)
  
  .projr_dir_ignore_rbuildignore_get_check(
    match_str_top, match_str_bottom, rbuildignore_vec
  )
  
  projr_ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
  projr_ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))

  if (length(projr_ignore_ind_top) == 0L) {
    return(list(start = character(0), end = character(0)))
  }
  
  list(
    start = rbuildignore_vec[seq_len(projr_ignore_ind_top)],
    end = rbuildignore_vec[seq(projr_ignore_ind_bot, length(rbuildignore_vec))]
  )
}

.projr_dir_ignore_rbuildignore_get_check <- function(match_str_top,
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
.projr_dir_ignore_path_get <- function(label) {
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
