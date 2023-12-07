.projr_build_script_run <- function(bump_component, stage) {
  yml_script <- .projr_yml_script_get(NULL)
  if (!.projr_state_nz(yml_script)) {
    return(invisible(FALSE))
  }
  for (x in yml_script) {
    .projr_build_script_run_title(x, bump_component, stage = stage)
  }
}

.projr_build_script_run_title <- function(x, bump_component, stage) {
  cue <- .projr_yml_script_complete_cue(x[["cue"]])
  if (!.projr_state_cue(cue, bump_component)) {
    return(invisible(FALSE))
  }
  if (!.projr_state_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .projr_script_run(x[["path"]][i])
  }
}

.projr_script_run <- function(path) {
  if (!.projr_file_state_exists(path)) {
    stop(paste0("Script '", path, "' does not exist."))
  }
  source(path)
}
