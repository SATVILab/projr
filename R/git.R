.projr_git_init <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_init_git(),
    "gert" = .projr_git_init_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    .projr_dep_install_only("gert")
    gert::git_config_set("user.name", "Darth Vader")
    gert::git_config_set("user.email", "number_one_fan@tellytubbies.com")
  }
}

.projr_git_repo_check_exists <- function() {
  dir.exists(.projr_dir_proj_get(".git"))
}

.projr_git_commit_file <- function(file, msg = NULL) {
  if (length(file) == 0L) {
    return(invisible(FALSE))
  }
  msg <- .projr_git_msg_get(msg = msg)
  switch(.projr_git_system_get(),
    "git" = .projr_git_commit_file_git(file, msg),
    "gert" = .projr_git_commit_file_gert(file, msg),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_commit_file_git <- function(file, msg) {
  .projr_git_add_file_git(file)
  system2(
    "git",
    args = paste0("commit -m '", msg, "' ", paste0(file, collapse = " ")),
    stdout = TRUE
  )
}

.projr_git_add_file_git <- function(file) {
  system2(
    "git",
    args = paste0("add ", paste0(file, collapse = " ")),
    stdout = TRUE
  )
}

.projr_git_commit_file_gert <- function(file, msg) {
  gert::git_add(file)
  gert::git_commit(msg)
}

.projr_git_commit_all <- function(msg = NULL, add_untracked = TRUE) {
  add_vec <- .projr_git_modified_get()
  if (add_untracked) {
    add_vec <- c(add_vec, .projr_git_new_get())
  }
  msg <- .projr_git_msg_get(msg = msg)
  .projr_git_commit_file(add_vec, msg)
}


# modified files
.projr_git_modified_get <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_modified_get_git(),
    "gert" = .projr_git_modified_get_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_modified_get_git <- function() {
  git_status_output <- system2(
    "git",
    args = "status --porcelain", stdout = TRUE
  )
  sub("^ M (.*)", "\\1", grep("^ M", git_status_output, value = TRUE))
}

.projr_git_modified_get_gert <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "modified"]
}

# new files
.projr_git_new_get <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_new_get_git(),
    "gert" = .projr_git_new_get_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_new_get_git <- function() {
  git_status_output <- system2(
    "git",
    args = "status --porcelain", stdout = TRUE
  )
  sub(
    "^\\?\\? (.*)", "\\1", grep("^\\?\\?", git_status_output, value = TRUE)
  )
}

.projr_git_new_get_gert <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "new"]
}

# message
.projr_git_msg_get <- function(msg) {
  if (!is.null(msg)) {
    return(msg)
  }
  cat("Please enter a one-line description of change", "\n")
  msg <- readline(prompt = ">> ")
  msg
}

# initialisation
.projr_git_init_git <- function() {
  system2("git", args = "init")
}

.projr_git_init_gert <- function() {
  gert::git_init(path = .projr_dir_proj_get())
}

# git or gert
.projr_git_system_get <- function() {
  if (.projr_git_system_check_git()) {
    return("git")
  }
  "gert"
}

# checking if git cli is available
.projr_git_system_check_git <- function() {
  git_version_try <- try(
    system2("git", args = "--version", stdout = TRUE),
    silent = TRUE
  )
  if (!inherits(git_version_try, "try-error")) {
    return(TRUE)
  }
  invisible(FALSE)
}

# set up system
.projr_git_system_setup <- function() {
  # do nothing for git, as it's already set up
  if (.projr_git_system_get() == "git") {
    return(invisible(FALSE))
  }
  .projr_git_system_setup_gert()
}

# install gert if not available
.projr_git_system_setup_gert <- function() {
  switch(!.projr_git_system_check_gert(),
    {
      .projr_dep_install("gert")
    }
  )
}

.projr_git_system_check_gert <- function() {
  requireNamespace("gert", quietly = TRUE)
}

# check if there's a remote
.projr_git_remote_check_exists <- function() {
  if (!.projr_git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  switch(.projr_git_system_get(),
    "git" = .projr_git_remote_check_exists_git(),
    "gert" = .projr_git_remote_check_exists_gert()
  )
}

.projr_git_remote_check_exists_git <- function() {
  length(system2("git", args = c("remote", "-v"), stdout = TRUE)) > 0L
}
.projr_git_remote_check_exists_gert <- function() {
  !inherits(try(gert::git_remote_ls(), silent = TRUE), "try-error")
}

.projr_git_remote_check_upstream <- function() {
  .projr_git_remote_check_exists()
  switch(.projr_git_system_get(),
    "git" = {
      if (.projr_git_remote_check_upstream_git()) {
        stop("No upstream remote detected")
      }
      invisible(TRUE)
    },
    # I think `gert` automatically sets to origin if none found
    "gert" = invisible(TRUE)
  )
  invisible(TRUE)
}

.projr_git_remote_check_upstream_git <- function() {
  remotes <- system2("git", args = c("remote", "-v"), stdout = TRUE)
  if (!any(grepl("upstream", remotes))) {
    stop("No upstream remote configured")
  }
}

# push
.projr_git_push <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_push_git(),
    "gert" = .projr_git_push_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
  invisible(TRUE)
}

.projr_git_push_git <- function() {
  system2("git", args = "push")
}

.projr_git_push_gert <- function() {
  gert::git_push()
}

# author
.projr_git_config_get_name <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_config_get_name_git(),
    "gert" = .projr_git_config_get_name_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_config_get_name_git <- function() {
  system2("git", args = "config user.name", stdout = TRUE)
}
.projr_git_config_get_name_gert <- function() {
  config_tbl <- gert::git_config()
  config_tbl[config_tbl[["name"]] == "user.name", ][["value"]][[1]]
}
