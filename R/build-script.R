.build_script_run <- function(stage) {
  yml_script <- .yml_script_get(NULL)
  if (.is_len_0(yml_script)) {
    return(invisible(FALSE))
  }
  for (x in yml_script) {
    .build_script_run_title(x, stage = stage)
  }
}

.build_post_script_run <- function() {
  .build_script_run("post")
}

.build_pre_script_run <- function() {
  .build_script_run("pre")
}

.build_script_run_title <- function(x, stage) {
  if (!.is_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .script_run(x[["path"]][i])
  }
}

.script_run <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Script '", path, "' does not exist."))
  }
  # Run script in an isolated child environment of the global environment
  # This prevents scripts from cluttering the global environment or affecting each other
  .cli_step("Running R script {fs::path_rel(path)}")
  .build_source(path)
}

.build_source <- function(path) {
  .build_user_call(
    function(path) {
      source(path, local = new.env(parent = .GlobalEnv))
      invisible(NULL)
    },
    list(path)
  )
}

# Production executes in the caller. A replay can supply its own R session
# so that user code loads the analysis's locked packages, including projr.
.build_user_call <- function(fun, args) {
  session <- getOption("projr.reproduce.session")
  if (is.null(session)) {
    return(do.call(fun, args, envir = parent.frame()))
  }
  result <- session$run_with_output(fun, args)
  cat(result$stdout)
  cat(result$stderr, file = stderr())
  if (!is.null(result$error)) {
    stop(result$error)
  }
  result$result
}
