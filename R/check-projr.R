# projr-specific checks
.is_cue <- function(cue, bump_component) {
  .is_cue_check(cue, bump_component)

  not_output_build <- is.null(bump_component) ||
    .is_opt(bump_component, c("none", "dev"))

  !not_output_build
}

.is_cue_check <- function(cue, bump_component) {
  .assert_in_single(cue, .opt_cue_get())
  if (is.character(bump_component)) {
    .assert_in_single(bump_component, c("major", "minor", "patch", "dev", "none"))
  } else {
    .assert_flag(bump_component)
  }
  invisible(TRUE)
}
