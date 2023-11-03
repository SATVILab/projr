.projr_cue_check <- function(cue, bump_component) {
  if (is.null(cue)) {
    return(TRUE)
  }
  # cue is set to FALSE when it's a dev build
  if (is.logical(bump_component)) {
    return(cue == "dev")
  }
  # if cue is none, then we're saying nothing
  # must happen
  if (cue == "none") {
    return(FALSE)
  }
  if (cue == "dev") {
    return(TRUE)
  }
  if (is.null(bump_component)) {
    stop("bump_component must be supplied if cue is major or minor")
  }
  cue_opt_vec <- c("major", "minor", "patch")
  if (cue == "major") {
    return(cue == bump_component)
  }
  if (cue == "minor") {
    return(bump_component %in% cue_opt_vec[1:2])
  }
  bump_component %in% cue_opt_vec
}
