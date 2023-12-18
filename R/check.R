# directories
# -----------

.assert_dir_exists_single <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  if (!dir.exists(x)) {
    stop(paste0("The directory ", nm, " must exist"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_dir_exists <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!all(dir.exists(x))) {
    stop(paste0("The ", nm, " directories must exist"), call. = FALSE)
  }
  invisible(TRUE)
}

.is_dir <- fs::is_dir

# path
# --------------

.is_path_abs <- fs::is_absolute_path

.assert_path_not_file <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_path_not_file(x)) {
    stop(paste0(nm, " must not be a pre-existing file"), call. = FALSE)
  }
  invisible(TRUE)
}

.is_path_not_file <- function(x) {
  !(file.exists(x) && !dir.exists(x))
}

# files: package
# --------------

.is_file_exists_description <- function() {
  file.exists(.path_get_proj("DESCRIPTION"))
}

.path_get_proj <- function(...) {
  dir_proj <- tryCatch(
    rprojroot::is_r_package$find_file(),
    error = function(e) getwd()
  )
  list_sub <- list(...)
  if (length(list_sub) == 0L) {
    return(dir_proj)
  }
  do.call(
    file.path,
    args = list(dir_proj) |> append(list_sub)
  )
}

# files
# -----

.assert_file_abs <- function(x) {

}


.is_file_abs <- function(x) {
  file.exists(x)
}

.is_file <- fs::is_file


# options
# ---------------

.assert_opt_single_not <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  if (!all(.is_opt_not(x, opt))) {
    stop(
      paste0(nm, " must not be one of ", paste0(opt, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.assert_opt_not <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_pos(x = x, nm = nm)
  if (!all(.is_opt_not(x, opt))) {
    stop(
      paste0(nm, " must not be one of ", paste0(opt, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_opt_not <- function(x, opt) {
  !.is_opt(x, opt)
}

.assert_opt_single <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  .assert_opt(x = x, nm = nm, opt = opt)
}

.assert_opt <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_pos(x = x, nm = nm)
  if (!all(.is_opt(x, opt))) {
    stop(
      paste0(nm, " must be one of ", paste0(opt, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_opt <- function(x, opt) {
  .is_opt_min(x, opt) && .is_len_pos(x) && all(!is.na(x))
}

.is_opt_min <- function(x, opt) {
  x %in% opt
}


# logical
# -----------------

.assert_lgl <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_lgl(x)) {
    stop(
      paste0(nm, " must be a non-NA logical vector with positive length"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_lgl <- function(x) {
  is.logical(x) && .is_len_pos(x) && all(!is.na(x))
}

.assert_lgl_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!is.logical(x)) {
    stop(paste0(nm, " must be a logical vector"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_flag <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_flag(x)) {
    stop(
      paste0(nm, " must be a non-NA flag (TRUE or FALSE)"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


.assert_flag_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_flag_min(x)) {
    stop(paste0(nm, " must be a flag (TRUE or FALSE)"), call. = FALSE)
  }
  invisible(TRUE)
}



# numeric
# -----------------

.assert_num <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_num(x)) {
    stop(
      paste0(nm, " must be a non-NA numeric vector with positive length"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_num <- function(x) {
  is.numeric(x) && .is_len_pos(x) && all(!is.na(x))
}

.assert_num_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(x)) {
    stop(paste0(nm, " must be a numeric vector"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_number <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_number(x)) {
    stop(
      paste0(nm, " must be a non-NA number (a numeric vector of length one)"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.assert_number_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_number_min(x)) {
    stop(paste0(nm, " must be a number (a numeric vector of length one)"), call. = FALSE)
  }
  invisible(TRUE)
}

# character
# -----------------

.assert_chr_full <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_chr(x)) {
    stop(
      paste0(nm, " must be a non-empty character vector with no NA entries"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_chr <- function(x) {
  is.character(x) && .is_len_pos(x) && all(!is.na(x)) && all(nzchar(x))
}

.assert_chr <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!is.character(x)) {
    stop(paste0(nm, " must be character"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_string <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_string(x)) {
    stop(
      paste0(nm, " must be a non-empty string"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.assert_string_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_string_min(x)) {
    stop(
      paste0(nm, " must be a string (a length-one character vector)"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.assert_nz <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!.is_len_pos(x) || !all(nzchar(x))) {
    stop(paste0(nm, " must be non-empty"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_nchar_single <- function(x, nchar, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!.is_len_1(x) || nchar(x) != nchar) {
    stop(paste0(nm, " must be ", nchar, " characters long"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_nchar <- function(x, nchar, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!is.character(x) || any(nchar(x) != nchar)) {
    stop(paste0(nm, " must be non-empty"), call. = FALSE)
  }
  invisible(TRUE)
}


# scalars
# -------

# is
.is_string <- function(x) {
  .is_string_min(x) && nzchar(x) && !is.na(x)
}

.is_string_min <- function(x) {
  .is_len_1(x) && is.character(x)
}

.is_number_min <- function(x) {
  .is_len_1(x) && is.numeric(x)
}

.is_number <- function(x) {
  .is_number_min(x) && !is.na(x)
}

.is_flag <- function(x) {
  .is_flag_min(x) && all(!is.na(x))
}

.is_flag_min <- function(x) {
  is.logical(x) && .is_len_1(x)
}

# length
# -----------------

.assert_len_1 <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_len_1(x)) {
    stop(paste0(nm, " must have length one"), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_len_pos <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_len_pos(x)) {
    stop(paste0(nm, " must have positive length"), call. = FALSE)
  }
  invisible(TRUE)
}


# assert
.assert_len <- function(x, len, required = FALSE, nm = NULL) {
  if (is.null(nm)) {
    nm <- deparse(substitute(x))
  }
  if (required) {
    .assert_given(x = x, nm = nm)
  } else if (!.is_given_mid(x)) {
    return(invisible(TRUE))
  }
  if (!.is_len(x = x, len = len)) {
    stop(paste0(nm, " must be length ", len), call. = FALSE)
  }
  invisible(TRUE)
}

# is
.is_len <- function(x, len) {
  length(x) == len
}
.is_len_1 <- function(x) {
  .is_len(x, 1L)
}

.is_len_0 <- function(x) {
  .is_len(x, 0L)
}

.is_len_pos <- function(x) {
  length(x) > 0L
}

# miscellaneous checks
# --------------------

.is_try_error <- function(x) {
  inherits(x, "try-error")
}

# basic functions
# ---------------

# automatically get name
.assert_nm_get <- function(x, nm = NULL) {
  if (!is.null(nm)) {
    if (!.is_string(nm)) {
      stop("`nm` must be a string", call. = FALSE)
    }
    return(nm)
  }
  out <- tryCatch(
    deparse(substitute(x, env = parent.frame())),
    error = function(e) "`unknown_name`"
  ) |>
    paste0(collapse = "_")
  n_char_out <- nchar(out)
  substr(out, start = 1, min(n_char_out, 20))
}

# check that it's given
.assert_check <- function(x, required, nm) {
  nm <- .assert_nm_get(x, nm)
  if (required) {
    .assert_given_mid(x = x, nm = nm)
  } else if (!.is_given_mid(x)) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.assert_opt_single <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  .assert_opt(x = x, nm = nm, opt = opt)
}

.assert_given_full <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_full(x)) {
    stop(paste0(nm, " must be given"), call. = FALSE)
  }
  invisible(TRUE)
}

.is_given_full <- function(x) {
  .is_given_mid(x) && all(!is.na(x))
}

.assert_given_mid <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_mid(x)) {
    stop(paste0(nm, " must be given"), call. = FALSE)
  }
  invisible(TRUE)
}

.is_given_mid <- function(x) {
  .is_given(x) && !is.null(x)
}

.assert_given <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_mid(x)) {
    stop(paste0(nm, " must be given"), call. = FALSE)
  }
  invisible(TRUE)
}

.is_given <- function(x) {
  if (missing(x)) {
    return(FALSE)
  }
  TRUE
}
