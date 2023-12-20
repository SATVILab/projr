# projr-specific checks
.is_cue <- function(cue, bump_component) {
  .is_cue_check(cue, bump_component)

  if (is.null(cue)) {
    return(TRUE)
  }
  if (.is_flag(bump_component)) {
    return(.is_opt(cue, c("build", "dev")))
  }
  # if cue is none, then we're saying nothing
  # must happen
  if (.is_opt(cue, "none")) {
    return(FALSE)
  }
  if (.is_opt(cue, c("build", "dev"))) {
    return(TRUE)
  }
  if (.is_opt(cue, "major")) {
    return(.is_opt(bump_component, "major"))
  }
  if (.is_opt(cue, "minor")) {
    return(.is_opt(bump_component, c("major", "minor")))
  }
  .is_opt(bump_component, c("major", "minor", "patch"))
}

.is_cue_check <- function(cue, bump_component) {
  if (is.null(cue)) {
    return(invisible(TRUE))
  }
  .assert_len_1(cue)
  if (is.null(bump_component)) {
    if (.is_opt(cue, c("major", "minor", "patch"))) {
      stop(
        "bump_component must be supplied if cue is major, minor or patch",
        call. = FALSE
      )
    }
  }
  .assert_len_1(bump_component)
  if (.is_string(bump_component)) {
    .assert_in(bump_component, c("major", "minor", "patch", "dev", "none"))
  } else {
    .assert_flag(bump_component)
  }
  invisible(TRUE)
}
