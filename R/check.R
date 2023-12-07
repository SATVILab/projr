.projr_check_chr_single_opt <- function(x, nm, opt, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  .projr_check_chr(x = x, nm = nm)
  .projr_check_len(x = x, nm = nm, len = 1)
  .projr_check_opt(x = x, nm = nm, opt = opt)
}

.projr_check_chr_nz <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  .projr_check_chr(x = x, nm = nm)
  .projr_check_nz(x = x, nm = nm)
  invisible(TRUE)
}

.projr_state_chr_nz <- function(x, nm) {
  .projr_check_chr(x = x, nm = nm)
  .projr_check_nz(x = x, nm = nm)
  invisible(TRUE)
}

.projr_check_nz <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_nz(x)) {
    stop(paste0(nm, " must be non-zero length"))
  }
  invisible(TRUE)
}

.projr_state_nz <- function(x) {
  length(x) != 0L
}

.projr_check_chr_single <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  .projr_check_chr(x = x, nm = nm)
  .projr_check_len(x = x, nm = nm, len = 1)
  invisible(TRUE)
}

.projr_check_lgl_single <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  .projr_check_lgl(x = x, nm = nm)
  .projr_check_len(x = x, nm = nm, len = 1)
  invisible(TRUE)
}

.projr_check_opt <- function(x, nm, opt, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_opt(x = x, opt = opt)) {
    stop(paste0(nm, " must be one of ", paste0(opt, collapse = ", ")))
  }
  invisible(TRUE)
}

.projr_state_opt <- function(x, opt) {
  x %in% opt
}

.projr_check_chr <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_chr(x)) {
    stop("Must be character")
  }
  invisible(TRUE)
}

.projr_state_chr <- function(x) {
  all(is.character(x))
}

.projr_check_lgl <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_lgl(x)) {
    stop("Must be logical")
  }
  invisible(TRUE)
}

.projr_state_lgl <- function(x) {
  all(is.logical(x))
}

.projr_check_len <- function(x, nm, len, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_len(x = x, len = len)) {
    stop(paste0(nm, " must be length ", len))
  }
  invisible(TRUE)
}

.projr_state_len <- function(x, len) {
  length(x) == len
}

.projr_check_given <- function(x, nm) {
  if (!.projr_state_given(x)) {
    stop(paste0(nm, " must be given"))
  }
  invisible(TRUE)
}

.projr_state_given <- function(x) {
  if (missing(x)) {
    return(FALSE)
  }
  if (is.null(x)) {
    return(FALSE)
  }
  if (all(is.na(x))) {
    return(FALSE)
  }
  TRUE
}
