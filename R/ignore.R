# break the specified file into three parts: the top, the projr-managed
# section, and the bottom. The projr-managed section is the content between
# the "Start of projr section" and "End of projr section" comments.
.ignore_path_get_list <- function(path, ignore, override) {
  file_vec <- .ignore_path_read(path)
  if (length(file_vec) == 0) {
    return(.ignore_get_list_empty())
  }

  ind_vec <- .ignore_path_get_ind(file_vec, path)

  start_end_list <- .ignore_path_get_startend(
    ind_vec[["top"]], ind_vec[["bot"]], file_vec
  )

  content <- .ignore_path_get_content(
    ind_vec[["top"]], ind_vec[["bot"]], file_vec
  )

  list(
    start = start_end_list[["start"]],
    content = content,
    end = start_end_list[["end"]]
  )
}

# read the content of the specified file
.ignore_path_read <- function(path) {
  if (!file.exists(path)) {
    return(character(0))
  }
  suppressWarnings(readLines(path))
}

# return an empty list
.ignore_get_list_empty <- function() {
  list(start = character(0), content = character(0), end = character(0))
}

# get the indices of the projr-managed section from the specified file
.ignore_path_get_ind <- function(file_vec, path) {
  .ignore_path_get_check(
    match_str_top, match_str_bottom, file_vec, path
  )

  ind_top <- which(grepl(match_str_top, file_vec))
  ind_bot <- which(grepl(match_str_bottom, file_vec))
  if (.is_len_0(ind_top)) {
    ind_top <- NA_integer_
  }
  if (.is_len_0(ind_bot)) {
    ind_bot <- NA_integer_
  }
  c("top" = ind_top, "bot" = ind_bot)
}

# check that the projr-managed section in the specified file is well-formed
.ignore_path_get_check <- function(match_str_top,
                                   match_str_bottom,
                                   file_vec,
                                   path) {
  # Validate that the projr-managed section in .gitignore is well-formed
  ignore_ind_bot <- which(grepl(match_str_bottom, file_vec))
  ignore_ind_top <- which(grepl(match_str_top, file_vec))

  if (length(ignore_ind_top) > 1 ||
    length(ignore_ind_bot) > 1) {
    stop(paste0(
      "Multiple projr sections found in ", basename(path)
    ))
  }

  found_top <- length(ignore_ind_top) == 1
  found_bottom <- length(ignore_ind_bot) == 1

  if (found_top && !found_bottom) {
    stop(paste0(
      "Found start of projr section but not end in ", basename(path)
    ))
  }

  if (!found_top && found_bottom) {
    stop(paste0(
      "Found end of projr section but not start in ", basename(path)
    ))
  }

  if (found_top && found_bottom) {
    if (ignore_ind_top > ignore_ind_bot) {
      stop(paste0(
        "Start of projr section found after end in ", basename(path)
      ))
    }
  }

  invisible(TRUE)
}

# get lines before and after the projr-managed section
.ignore_path_get_startend <- function(ind_top, ind_bot, file_vec) {
  if (is.na(ind_top)) {
    start <- c(
      file_vec,
      "# Start of projr section: do not edit by hand (update with projr_ignore_auto())"
    )
    end <- "# End of projr section"
  } else {
    start <- file_vec[seq_len(ind_top)]
    end <- file_vec[seq(ind_bot, length(file_vec))]
  }
  list(start = start, end = end)
}

# get lines in the projr-managed section
.ignore_path_get_content <- function(ind_top, ind_bot, file_vec) {
  if (is.na(ind_top)) {
    character(0)
  } else if (ind_top == ind_bot) {
    character(0)
  } else {
    file_vec[seq(ind_top + 1, ind_bot - 1)]
  }
}

# write to the specified file
.ignore_path_write <- function(file_vec, path) {
  writeLines(file_vec, path)
  .newline_append(path)
  invisible(path)
}

match_str_top <- "^# Start of projr section"
match_str_bottom <- "^# End of projr section"
