# attributes
# ------------

.assert_attr_value <- function(x, attr, value, required = TRUE, nm = NULL) {
  .assert_given_mid(attr)
  .assert_given(value)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!identical(value, attr(x, attr))) {
    msg <- paste0(
      nm, " must have attribute ", attr, " with value ",
      value |> .string_create()
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_attr_exact <- function(x, attr, required = TRUE, nm = NULL) {
  .assert_given_mid(attr)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!identical(attr |> sort(), attributes(x) |> names() |> sort())) {
    msg <- paste0(
      nm, " must have exactly the following attribute(s): \n",
      attr |> .string_create()
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_attr <- function(x, attr, required = FALSE, nm = NULL) {
  if (required) {
    .assert_given(x = x, nm = nm)
  } else if (!.is_given_mid(x)) {
    return(invisible(TRUE))
  }
  .assert_given_mid(attr)
  .assert_string(attr, TRUE)
  nm <- .assert_nm_get(x, nm)

  if (.is_opt(attr, names(attributes(x)))) {
    return(invisible(TRUE))
  }

  msg <- paste0(nm, " must have attribute ", attr)
  .cli_debug("Validation failed: {msg}")
  stop(msg, call. = FALSE)
  invisible(TRUE)
}

.assert_has_not <- function(x, value, required = FALSE, nm = NULL) {
  .assert_given(value)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (any(value %in% x)) {
    msg <- paste0(
      nm, " must not contain any of the following value(s):\n",
      .string_create(value, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_has <- function(x, value, required = FALSE, nm = NULL) {
  .assert_given(value)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!all(value %in% x)) {
    msg <- paste0(
      nm, " must contain all the following value(s):\n",
      .string_create(value, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_detect <- function(x, pattern, required = FALSE, nm = NULL) {
  .assert_given(pattern)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!all(grepl(pattern, x))) {
    msg <- paste0(nm, " must all match ", pattern)
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_detect_any <- function(x, pattern, required = FALSE, nm = NULL) {
  .assert_given(pattern)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!any(grepl(pattern, x))) {
    msg <- paste0(nm, " must contain at least one match for ", pattern)
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_detect_single <- function(x, pattern, required = FALSE, nm = NULL) {
  .assert_given(pattern)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm, required = required)

  if (!grepl(pattern, x)) {
    msg <- paste0(nm, " must match ", pattern)
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

# class
# -----------

.assert_class_exact_unsorted <- function(x, class, required = TRUE, nm = NULL) {
  .assert_chr(class, TRUE)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!identical(class, class(x))) {
    msg <- paste0(
      nm, " must have exactly the following class(es) (without any sorting to help): ", # nolint
      .string_create(class, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_class_exact <- function(x, class, required = TRUE, nm = NULL) {
  .assert_chr(class, TRUE)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!identical(class |> sort(), class(x) |> sort())) {
    msg <- paste0(
      nm, " must have exactly the following class(es) (after sorting to help): ", # nolint: line_length_linter.
      .string_create(class, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}


.assert_class_all <- function(x, class, required = FALSE, nm = NULL) {
  .assert_chr(class, TRUE)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!all(vapply(class, function(cls) inherits(x, cls), logical(1)))) {
    msg <- paste0(
      nm, " must have all of the following class(es): ",
      .string_create(class, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_class <- .assert_class_all

.assert_class_any <- function(x, class, required = FALSE, nm = NULL) {
  .assert_chr(class, TRUE)
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!any(vapply(class, function(cls) inherits(x, cls), logical(1)))) {
    msg <- paste0(
      nm, " must have at least one of the following class(es): ",
      .string_create(class, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

# directories
# -----------

.assert_dir_exists_single <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  if (!dir.exists(x)) {
    msg <- paste0("The directory ", nm, " must exist")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_dir_exists <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!all(dir.exists(x))) {
    msg <- paste0("The ", nm, " directories must exist")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_dir <- fs::is_dir

# path
# --------------

.assert_path_not_sub <- function(x, sub, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_chr(sub, TRUE)
  if (!requireNamespace("fs", quietly = TRUE)) {
    msg <- "Package 'fs' is required but not installed.\nPlease install it using: install.packages(\"fs\")"
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }

  if (any(vapply(sub, function(y) fs::path_has_parent(x, y), logical(1)))) {
    msg <- paste0(nm, " must not be a subdirectory of ", sub)
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_path_not_file <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_path_not_file(x)) {
    msg <- paste0(nm, " must not be a pre-existing file")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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

# options
# ---------------

.assert_in_single_not <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  if (!all(.is_in_not(x, opt))) {
    msg <- paste0(
      nm, " must not be one of ", paste0(opt, collapse = ", "),
      "but is the following:\n",
      .string_create(x, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_in_not <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_pos(x = x, nm = nm)
  if (!all(.is_in_not(x, opt))) {
    msg <- paste0(
      nm, " must not be one of ", paste0(opt, collapse = ", "),
      "but is the following:\n",
      .string_create(x, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_in_not <- function(x, opt) {
  !.is_opt(x, opt)
}

.assert_in_single <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_1(x = x, nm = nm)
  .assert_in(x = x, nm = nm, opt = opt)
}

.assert_in <- function(x, opt, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  # check that it's neither missing nor NULL
  # if required
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  .assert_len_pos(x = x, nm = nm)
  if (!all(.is_opt(x, opt))) {
    msg <- paste0(
      nm, " must be one of ", paste0(opt, collapse = ", "), ",\n",
      "and not:\n", .string_create(x, sep = "\n")
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_opt <- function(x, opt) {
  all(.is_opt_min(x, opt)) && .is_len_pos(x) && all(!is.na(x))
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
    msg <- paste0(nm, " must be a non-NA logical vector with positive length")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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
    msg <- paste0(nm, " must be a logical vector")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_flag <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_flag(x)) {
    msg <- paste0(nm, " must be a non-NA flag (TRUE or FALSE)")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}


.assert_flag_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_flag_min(x)) {
    msg <- paste0(nm, " must be a flag (TRUE or FALSE)")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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
    msg <- paste0(nm, " must be a non-NA numeric vector with positive length")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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
    msg <- paste0(nm, " must be a numeric vector")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_number <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_number(x)) {
    msg <- paste0(nm, " must be a non-NA number (a numeric vector of length one)")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_number_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_number_min(x)) {
    msg <- paste0(nm, " must be a number (a numeric vector of length one)")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE) # nolint: line_length_linter.
  }
  invisible(TRUE)
}

# character
# -----------------

.assert_chr <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }
  if (!.is_chr(x)) {
    msg <- paste0(nm, " must be a non-empty character vector")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_chr <- function(x) {
  .is_chr_mid(x) & all(nzchar(x))
}

.assert_chr_mid <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_chr_mid(x)) {
    msg <- paste0(nm, " must be a non-empty character vector with no NA entries")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_chr_mid <- function(x) {
  is.character(x) && .is_len_pos(x) && all(!is.na(x))
}

.assert_chr_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!is.character(x)) {
    msg <- paste0(nm, " must be character")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_string <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }

  if (!.is_string(x)) {
    msg <- paste0(nm, " must be a non-empty string")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_string <- function(x) {
  .is_string_mid(x) && nzchar(x)
}

.assert_string_mid <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_string_mid(x)) {
    msg <- paste0(nm, " must be a non-empty string")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_string_min <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_string_min(x)) {
    msg <- paste0(nm, " must be a string (a length-one character vector)")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_nz <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!.is_len_pos(x) || !all(nzchar(x))) {
    msg <- paste0(nm, " must be non-empty")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_nchar_single <- function(x, nchar, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!.is_len_1(x) || nchar(x) != nchar) {
    msg <- paste0(nm, " must be ", nchar, " characters long")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_nchar <- function(x, nchar, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(FALSE))
  }

  if (!is.character(x) || any(nchar(x) != nchar)) {
    msg <- paste0(nm, " must be non-empty")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}


# scalars
# -------

# is
.is_string_mid <- function(x) {
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
    msg <- paste0(nm, " must have length one")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_len_pos <- function(x, required = FALSE, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.assert_check(x, required, nm)) {
    return(invisible(TRUE))
  }
  if (!.is_len_pos(x)) {
    msg <- paste0(nm, " must have positive length")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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
    msg <- paste0(
      nm, " must be length ", len, ",\n",
      "and not length ", length(x)
    )
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
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
    if (!.is_string_mid(nm)) {
      msg <- "`nm` must be a string"
      .cli_debug("Validation failed: {msg}")
      stop(msg, call. = FALSE)
    }
    return(nm)
  }
  tryCatch(
    deparse(substitute(x, env = parent.frame())),
    error = function(e) "`unknown_name`"
  ) |>
    .string_create()
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
  .assert_in(x = x, nm = nm, opt = opt)
}

.assert_given_full <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_full(x)) {
    msg <- paste0(nm, " must be given")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_given_full <- function(x) {
  .is_given_mid(x) && all(!is.na(x))
}

.assert_given_mid <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_mid(x)) {
    msg <- paste0(nm, " must be given")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_given_mid <- function(x) {
  .is_given(x) && !is.null(x)
}

.assert_given <- function(x, nm = NULL) {
  nm <- .assert_nm_get(x, nm)
  if (!.is_given_mid(x)) {
    msg <- paste0(nm, " must be given")
    .cli_debug("Validation failed: {msg}")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.is_given <- function(x) {
  if (missing(x)) {
    return(FALSE)
  }
  TRUE
}

.string_create <- function(x, n_char = 20, sep = ", ") {
  x |>
    paste0(collapse = ", ") |>
    substr(start = 1, stop = n_char)
}

.string_cap <- function(x, n = 20) {
  x |> substr(start = 1, stop = min(nchar(x), n))
}

.chr_cap <- function(x, n = 20) {
  x |> substr(start = 1, stop = pmin(nchar(x), n))
}
