.ignore_diryml <- function() {
  # Update `.gitignore` and `.Rbuildignore` Using Projr Configuration
  #
  # The .dir_ignore()` function updates the project’s `.gitignore` and
  # `.Rbuildignore` files to reflect the directories managed by Projr.
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
  # **Key Effects:**
  #
  # - Ensures `.gitignore` includes Projr-managed directories, keeping them out of Git commits.
  # - Ensures `.Rbuildignore` excludes these directories from R build processes.
  .ignore_diryml_git()
  .ignore_diryml_rbuild()
  invisible(TRUE)
}


.ignore_diryml_git <- function() {
  if (!.git_repo_check_exists() &&
    !file.exists(.path_get(".gitignore"))) {
    return(invisible(FALSE))
  }

  # get paths to ignore
  instr_list <- .ignore_diryml_git_get_instructions()

  # ignore paths
  .ignore_diryml_git_update_gitignore(instr_list$ignore)

  invisible(TRUE)
}

# .gitignore Management (As previously defined)
.ignore_diryml_git_get_instructions <- function() {
  # get which paths to ignore
  label_vec <- .ignore_diryml_git_get_instructions_labels()

  path_ignore <- character(0)

  for (i in seq_along(label_vec)) {
    instr_list_label <- .ignore_diryml_git_get_instructions_label(
      label_vec[[i]]
    )
    path_ignore <- c(path_ignore, instr_list_label$ignore)
  }
  list(ignore = path_ignore)
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

.ignore_diryml_git_get_instructions_label <- function(label) {
  path <- .ignore_diryml_path_get(label)
  if (length(path) == 0) {
    c0 <- character(0L)
    return(list(ignore = c0))
  }

  .ignore_diryml_git_get_instructions_label_impl(
    path, label
  )
}

.ignore_diryml_git_get_instructions_label_impl <- function(path,
                                                           label) {
  # ignore
  ignore_git <- .ignore_get_git(label)
  path_ignore <- if (ignore_git) paste0(path, "/**") else character(0L)

  list(ignore = path_ignore)
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

.git_get_tracked <- function(path) {
  # Retrieve a list of tracked files within the specified path
  fn_vec <- suppressWarnings(system2(
    "git",
    args = c("ls-files", "--", path),
    stdout = TRUE,
    stderr = FALSE
  ))

  if (length(fn_vec) == 0L) {
    return(character(0L))
  }

  # Return the list of tracked file paths
  fn_vec
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

  ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))
  ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))

  if (length(ignore_ind_top) == 0L) {
    return(list(
      start = c(
        rbuildignore_vec,
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())"
      ),
      end = "# End of projr section"
    ))
  }

  list(
    start = rbuildignore_vec[seq_len(ignore_ind_top)],
    end = rbuildignore_vec[seq(ignore_ind_bot, length(rbuildignore_vec))]
  )
}

.ignore_diryml_rbuildignore_get_check <- function(match_str_top,
                                                  match_str_bottom,
                                                  rbuildignore_vec) {
  # validate that the Projr-managed section in .Rbuildignore is well-formed
  ignore_ind_bot <- which(grepl(match_str_bottom, rbuildignore_vec))
  ignore_ind_top <- which(grepl(match_str_top, rbuildignore_vec))

  if (length(ignore_ind_top) > 1 ||
    length(ignore_ind_bot) > 1) {
    stop("Multiple projr sections found in .Rbuildignore")
  }

  found_top <- length(ignore_ind_top) == 1
  found_bottom <- length(ignore_ind_bot) == 1

  if (found_top && !found_bottom) {
    stop("Found start of projr section but not end in .Rbuildignore")
  }

  if (!found_top && found_bottom) {
    stop("Found end of projr section but not start in .Rbuildignore")
  }

  if (found_top && found_bottom) {
    if (ignore_ind_top > ignore_ind_bot) {
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
