# ============================
# Pre/post # nolint: commented_code_linter.
# ============================

# renv
.build_renv_snapshot <- function(output_run) {
  if (!.build_renv_snapshot_check(output_run)) {
    return(invisible(FALSE))
  }
  renv::snapshot(prompt = FALSE)
  invisible(TRUE)
}

.build_renv_snapshot_check <- function(output_run) {
  output_run && !.is_test() && .renv_detect()
}

# commit
.build_git_commit <- function(output_run,
                              bump_component,
                              version_run_on_list,
                              stage,
                              msg) {
  # exit early if required
  if (!.build_git_check(output_run)) {
    .build_git_commit_pre_warn(stage, output_run)
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    stop("Git commits requested but no Git directory found")
  }

  msg <- .build_git_commit_get_msg(msg, stage)

  # commit
  .git_commit_all(
    msg = msg,
    add_untracked = .yml_git_get_add_untracked(NULL)
  )
}

.build_git_commit_pre_warn <- function(stage, output_run) {
  if (stage == "pre" && output_run && !.git_repo_check_exists()) {
    .build_git_commit_pre_warn_impl()
  }
  invisible(TRUE)
}

.build_git_commit_pre_warn_impl <- function() {
  # Use a persistent directory in the user's home folder.
  warning_dir <- file.path(dirname(tempdir()), ".projr")
  if (!dir.exists(warning_dir)) {
    dir.create(warning_dir, recursive = TRUE)
  }

  # Use the project name and today's date for unique persistent identification.
  project_id <- basename(getwd())
  today <- Sys.Date()
  warning_file <- file.path(
    warning_dir, paste0("git_warn_", project_id, "_", today, ".txt")
  )

  if (!file.exists(warning_file)) {
    # Create the file to indicate the warning for today has been issued.
    writeLines(as.character(Sys.time()), con = warning_file)
    return(FALSE) # Warning has not been issued before today.
  }
}

.bild_git_commit_pre_warn_impl_warn <- function() {
  warning(
    "Using Git with projr enhances version control and collaboration.\n",
    "Please store large files in the '_raw_data', '_output', and '_tmp' folders.\n", # nolint
    "Then run `projr::projr_init()` to initialize Git.\n"
  )
}

.build_git_commit_get_msg <- function(msg, stage) {
  if (stage == "pre") {
    "Snapshot pre-build"
  } else {
    paste0(
      "Build v", projr_version_get(), ": ",
      trimws(msg)
    )
  }
}

.build_git_check <- function(output_run) {
  output_run && .yml_git_get_commit(NULL)
}

# commit messages
.build_git_msg_get <- function(stage,
                               version_run_on_list,
                               bump_component,
                               msg) {
  switch(stage,
    "pre" = .build_git_msg_get_pre(bump_component),
    "post" = .build_git_msg_get_post(
      bump_component = bump_component,
      version_run_on_list = version_run_on_list,
      msg = msg
    ),
    "dev" = .build_git_msg_get_dev(version_run_on_list)
  )
}

.build_git_msg_get_pre <- function(bump_component) {
  paste0("Snapshot pre-", bump_component, " build")
}

.build_git_msg_get_post <- function(bump_component,
                                    version_run_on_list,
                                    msg) {
  msg_append <- ifelse(
    nzchar(msg),
    paste0(": ", msg),
    ""
  )
  paste0(
    "Record ", bump_component,
    " v", version_run_on_list[["desc"]][["success"]], " build",
    msg_append
  )
}

.build_git_msg_get_dev <- function(version_run_on_list) {
  paste0("Begin v", projr_version_get())
}

# commit
.build_git_push <- function(output_run) {
  if (!output_run || !.yml_git_get_push(NULL)) {
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    stop("No git repository detected but needed based on settings")
  }
  if (!.git_remote_check_upstream()) {
    warning("No upstream remote detected")
  }
  .git_push()
  invisible(TRUE)
}
