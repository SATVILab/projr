.projr_git_init <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_init_git(),
    "gert" = .projr_git_init_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    gert::git_config_set("user.name", "Darth Vader")
    gert::git_config_set("user.email", "number_one_fan@tellytubbies.com")
  }
}

.projr_git_check_repo <- function() {
  dir.exists(.projr_dir_proj_get(".git"))
}

.projr_git_commit_file <- function(file, msg = NULL) {
  msg <- .projr_git_msg_get(msg = msg)
  .projr_git_commit(file, msg)
}

.projr_git_commit_all <- function(msg = NULL, add_untracked = TRUE) {
  add_vec <- .projr_git_modified_get()
  if (add_untracked) {
    add_vec <- c(add_vec, .projr_git_new_get())
  }
  msg <- .projr_git_msg_get(msg = msg)
  .projr_git_commit(add_vec, msg)
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
.projr_git_msg_get <- function() {
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
  git_version_try <- try(system2("git", args = "--version"), silent = TRUE)
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

# github
# -----------------

# check if setup
.projr_init_gh_check_exists <- function() {
  .projr_git_helper_check_remote()
}

.projr_init_git_check_exists <- function() {
  dir.exists(.projr_dir_proj_get(".git"))
}

.projr_init_gh_check_exists <- function() {
  length(system2("git", args = "remote", stdout = TRUE)) == 0L
}

.projr_git_gh_check_auth <- function() {
  if (!nzchar(.projr_auth_get_github_pat())) {
    return(invisible(TRUE))
  }
  warning(
    "GITHUB_PAT environment variable not found.\n",
    "\n",
    "To allow creating a GitHub repository, please set it.\n",
    "\n",
    "To easily set it in less than two minutes, do the following:\n",
    "1. If you do not have a GitHub account, create one here: https://github.com\n",
    "2. In R, run usethis::create_github_token()\n",
    "3. In R, run gitcreds::gitcreds_set()\n",
    "4. Paste the token from step 1 into the R command line (terminal), and press enter\n",
    "For more details, see https://happygitwithr.com/https-pat#tldr\n",
    "\n",
    "After doing the above:\n",
    "1. In R, rerun projr::projr_init()\n",
    "It will skip what's been done already and try set up GitHub again.\n",
    call. = FALSE
  )
  invisible(FALSE)
}

.projr_git_gh_init <- function(username, public) {
  if (!is.null(username)) {
    if (.projr_git_gh_check_auth(username)) {
    }
  }
}

.projr_git_gh_init_actual <- function(username, public) {
  try({
    if (identical(username, gh::gh_whoami()$login)) {
      .projr_dep_install_only("usethis")
      usethis::use_github(private = !public)
    } else {
      .projr_dep_install_only("usethis")
      usethis::use_github(organisation = username, private = !public)
    }
  })
}
