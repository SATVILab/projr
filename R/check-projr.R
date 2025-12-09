# projr-specific checks
.is_cue <- function(cue, bump_component) {
  .is_cue_check(cue, bump_component)

  not_output_build <- is.null(bump_component) ||
    .is_opt(bump_component, c("none", "dev"))

  !not_output_build
}

.is_cue_check <- function(cue, bump_component) {
  .assert_string(cue)
  .assert_len_1(cue)
  .assert_in(cue, c("always", "if-change"))
  .assert_len_1(bump_component)
  if (.is_string(bump_component)) {
    .assert_in(bump_component, c("major", "minor", "patch", "dev", "none"))
  } else {
    .assert_flag(bump_component)
  }
  invisible(TRUE)
}
