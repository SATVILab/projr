.projr_build_script_run <- function(stage) {
  yml_script <- .projr_yml_script_get(NULL)
  if (.is_len_0(yml_script)) {
    return(invisible(FALSE))
  }
  for (x in yml_script) {
    .projr_build_script_run_title(x, stage = stage)
  }
}

.projr_build_post_script_run <- function() {
  .projr_build_script_run("post")
}

.projr_build_pre_script_run <- function() {
  .projr_build_script_run("pre")
}

.projr_build_script_run_title <- function(x, stage) {
  if (!.is_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .projr_script_run(x[["path"]][i])
  }
}

.projr_script_run <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Script '", path, "' does not exist."))
  }
  source(path)
}
