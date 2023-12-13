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

.projr_check_len_nz <- function(x, nm, required = FALSE, msg = NULL, msg_append = TRUE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_len_nz(x)) {
    stop(paste0(nm, " must be non-zero length"))
  }
  invisible(TRUE)
}

.projr_check_nz <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_nz(x)) {
    stop(paste0(nm, " must be non-zero"))
  }
  invisible(TRUE)
}

.projr_check_z <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_z(x)) {
    stop(paste0(nm, " must be zero"))
  }
  invisible(TRUE)
}

.projr_state_nz <- function(x) {
  nzchar(x)
}
.projr_state_z <- function(x) {
  !.projr_state_nz(x)
}

.projr_state_len_nz <- function(x) {
  !.projr_state_z(x)
}

.projr_state_len_z <- function(x) {
  .projr_state_len(x, 0L)
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

.projr_state_true <- function(x) {
  isTRUE(x)
}

.projr_check_true <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_true(x)) {
    stop(paste0(nm, " must be TRUE"))
  }
  invisible(TRUE)
}

.projr_state_false <- function(x) {
  isFALSE(x)
}

.projr_check_false <- function(x, nm, required = FALSE) {
  if (required) {
    .projr_check_given(x = x, nm = nm)
  } else if (!.projr_state_given(x)) {
    return(invisible(TRUE))
  }
  if (!.projr_state_false(x)) {
    stop(paste0(nm, " must be FALSE"))
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
    stop(paste0(nm, " must be character"))
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
    stop(paste0(nm, " must be logical"))
  }
  invisible(TRUE)
}

.projr_state_lgl <- function(x) {
  all(is.logical(x))
}

.projr_state_lgl_single <- function(x) {
  .projr_state_lgl(x) %% .projr_state_single(x)
}

.projr_state_single <- function(x) {
  .projr_state_len(x, 1)
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

.projr_state_abs <- function(x) {
  fs::is_absolute_path(x)
}

.projr_state_null <- function(x) {
  is.null(x)
}


.projr_state_cue <- function(cue, bump_component) {
  .projr_state_cue_check(cue, bump_component)

  if (.projr_state_null(cue)) {
    return(TRUE)
  }
  if (.projr_state_lgl_single(bump_component)) {
    return(.projr_state_opt(cue, c("build", "dev")))
  }
  # if cue is none, then we're saying nothing
  # must happen
  if (.projr_state_opt(cue, "none")) {
    return(FALSE)
  }
  if (.projr_state_opt(cue, c("build", "dev"))) {
    return(TRUE)
  }
  if (.projr_state_opt(cue, "major")) {
    return(.projr_state_opt(cue, bump_component))
  }
  if (.projr_state_opt(cue, "minor")) {
    return(.projr_state_opt(cue, c("major", "minor")))
  }
  .projr_state_opt(cue, c("major", "minor", "patch"))
}

.projr_state_cue_check <- function(cue, bump_component) {
  if (.projr_state_null(cue)) {
    return(invisble(TRUE))
  }
  .projr_check_len(cue, "cue", 1L)
  .projr_check_len(bump_component, "bump_component", 1L)
  if (.projr_state_null(bump_component)) {
    if (.projr_state_opt(cue, c("major", "minor", "patch"))) {
      stop("bump_component must be supplied if cue is major, minor or patch")
    }
  }
  if (!.projr_state_lgl(bump_component)) {
    .projr_check_opt(
      bump_component, "bump_component",
      c("major", "minor", "patch", "dev", "none")
    )
  }
  invisible(TRUE)
}

.projr_state_rel <- function(x) {
  !.projr_state_abs(x)
}
