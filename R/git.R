.git_init <- function() {
  if (.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  switch(.git_system_get(),
    "git" = .git_init_git(),
    "gert" = .git_init_gert(),
    stop(paste0(.git_system_get(), " not recognised"))
  )

  if (.is_test()) {
    .dep_install_only("gert")
    gert::git_config_set("user.name", "DarthVader")
    gert::git_config_set("user.email", "number_one_fan@tellytubbies.com")
  }
}

.git_repo_check_exists <- function() {
  # permit worktrees (files) or genuine Git repos (dirs)
  file.exists(.path_get(".git"))
}

.git_repo_rm <- function() {
  if (.git_repo_check_exists()) {
    unlink(.path_get(".git"), recursive = TRUE)
  }
}

.git_commit_file <- function(file, msg = NULL) {
  if (length(file) == 0L) {
    return(invisible(FALSE))
  }
  msg <- .git_msg_get(msg)
  switch(.git_system_get(),
    # suppress warnings from adding deleted files
    "git" = suppressWarnings(.git_commit_file_git(file, msg)),
    "gert" = suppressWarnings(.git_commit_file_gert(file, msg)),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_commit_file_git <- function(file, msg, ...) {
  .git_add_file_git(file)
  system2(
    "git",
    args = paste0(
      "commit -m ", shQuote(msg), " ",
      paste0(normalizePath(file, winslash = "/"), collapse = " ")
    ),
    ...
  )
}

.git_add_file_git <- function(file, ...) {
  system2(
    "git",
    args = paste0(
      "add ", paste0(normalizePath(file, winslash = "/"), collapse = " ")
    ),
    stdout = TRUE, ...
  )
}

.git_commit_file_gert <- function(file, msg, ...) {
  gert::git_add(file)
  gert::git_commit(msg, ...)
}

.git_commit_all <- function(msg = NULL, add_untracked = TRUE) {
  add_vec <- .git_modified_get()
  if (add_untracked) {
    add_vec <- c(add_vec, .git_new_get())
  }
  msg <- .git_msg_get(msg)
  .git_commit_file(add_vec, msg)
}


# modified files
.git_modified_get <- function() {
  switch(.git_system_get(),
    "git" = .git_modified_get_git(),
    "gert" = .git_modified_get_gert(),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_modified_get_git <- function() {
  git_status_output <- system2(
    "git",
    args = "status --porcelain", stdout = TRUE
  )
  match_vec <- grep(
    "^M.|^A.|^D.|^.M|^.A|^.D",
    git_status_output,
    value = TRUE
  )
  if (.is_len_0(match_vec)) {
    return(match_vec)
  }
  substr(
    match_vec,
    start = 4,
    stop = nchar(match_vec)
  )
}

.git_modified_get_gert <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "modified"]
}

# new files
.git_new_get <- function() {
  switch(.git_system_get(),
    "git" = .git_new_get_git(),
    "gert" = .git_new_get_gert(),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_new_get_git <- function() {
  git_status_output <- system2(
    "git",
    args = "status --porcelain", stdout = TRUE
  )
  sub(
    "^\\?\\? (.*)", "\\1",
    grep("^\\?\\?", git_status_output, value = TRUE)
  )
}

.git_new_get_gert <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "new"]
}

# message
.git_msg_get <- function(msg) {
  if (!is.null(msg)) {
    return(msg)
  }
  cat("Please enter a one-line description of change", "\n")
  msg <- readline(prompt = ">> ")
  msg
}

# just checked
.git_changed_filter <- function(path) {
  # check if specific file(s)
  # are untracked (and not ignore) or modified
  if (!.git_repo_check_exists()) {
    return(character(0L))
  }
  if (.is_len_0(path)) {
    return(character(0L))
  }
  path <- path[file.exists(path)]
  if (.is_len_0(path)) {
    return(character(0L))
  }
  path <- path[fs::path_has_parent(path, .path_get())]
  if (.is_len_0(path)) {
    return(character(0L))
  }
  path <- fs::path_rel(path, .path_get())
  if (!.is_chr(path)) {
    stop("path must be a character vector")
  }
  switch(.git_system_get(),
    "git" = .git_changed_filter_git(path),
    "gert" = .git_changed_filter_gert(path),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_changed_filter_git <- function(path) {
  path[vapply(path, .git_changed_filter_git_ind, logical(1L))]
}
.git_changed_filter_git_ind <- function(path) {
  # Run git status with porcelain output
  status <- system2("git", args = c("status", "--porcelain", path), stdout = TRUE)
  length(status) > 0
}

.git_changed_filter_gert <- function(path) {
  path_known <- path_rel[path_rel %in% git_ls()[["path"]]]
  git_status_tbl <- git_status(pathspec = path_known)
  # check if known files have been modified
  path_known_changed <- path_known[
    path_known %in% git_status_tbl[["file"]] &
      git_status_tbl[["status"]] %in% c("modified", "new")
  ]
  # unknown files are a changed
  path_unknown <- setdiff(path_rel, path_known)
  c(path_known_changed, path_unknown)
}

# initialisation
.git_init_git <- function() {
  system2("git", args = "init")
}

.git_init_gert <- function() {
  gert::git_init(path = .path_get())
}

# git or gert
.git_system_get <- function() {
  if (.git_system_check_git()) {
    return("git")
  }
  "gert"
}

# checking if git cli is available
.git_system_check_git <- function() {
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
.git_system_setup <- function() {
  # do nothing for git, as it's already set up
  if (.git_system_get() == "git") {
    return(invisible(FALSE))
  }
  .git_system_setup_gert()
}

# install gert if not available
.git_system_setup_gert <- function() {
  switch(!.git_system_check_gert(),
    {
      .dep_install("gert")
    }
  )
  if (!.git_system_check_gert()) {
    stop("Failed to install gert and Git executable not available")
  }
  invisible(TRUE)
}

.git_system_check_gert <- function() {
  requireNamespace("gert", quietly = TRUE)
}

# check if there's a remote
.git_remote_check_exists <- function() {
  if (!.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  switch(.git_system_get(),
    "git" = .git_remote_check_exists_git(),
    "gert" = .git_remote_check_exists_gert()
  )
}

.git_remote_check_exists_git <- function() {
  length(system2("git", args = c("remote", "-v"), stdout = TRUE)) > 0L
}
.git_remote_check_exists_gert <- function() {
  !inherits(try(gert::git_remote_ls(), silent = TRUE), "try-error")
}

.git_remote_check_upstream <- function() {
  if (!.git_remote_check_exists()) {
    return(FALSE)
  }
  switch(.git_system_get(),
    "git" = {
      if (!.git_remote_check_upstream_git()) {
        warning("No upstream remote detected")
        return(FALSE)
      }
      invisible(TRUE)
    },
    # I think `gert` automatically sets to origin if none found
    "gert" = invisible(TRUE)
  )
}

.git_remote_check_upstream_git <- function() {
  upstream_branch <- system2("git", args = c(
    "rev-parse", "--abbrev-ref", "--symbolic-full-name", paste0("HEAD", "@{upstream}")
  ), stdout = TRUE, stderr = TRUE)
  !grepl("no upstream configured", upstream_branch)
}

# push
.git_push <- function(...) {
  switch(.git_system_get(),
    "git" = .git_push_git(...),
    "gert" = .git_push_gert(...),
    stop(paste0(.git_system_get(), " not recognised"))
  )
  invisible(TRUE)
}

.git_push_git <- function(...) {
  system2("git", args = "push", ...)
  invisible(TRUE)
}

.git_push_gert <- function(...) {
  gert::git_push(...)
  invisible(TRUE)
}

# author
.git_config_get_name <- function() {
  switch(.git_system_get(),
    "git" = .git_config_get_name_git(),
    "gert" = .git_config_get_name_gert(),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_config_get_name_git <- function() {
  nm <- .git_config_get_name_git_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_name_git_global()
  if (.is_string(nm)) {
    return(nm)
  }
  .git_config_get_name_git_system()
}

.git_config_get_name_git_local <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --local user.name", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.git_config_get_name_git_global <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --global user.name", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.git_config_get_name_git_system <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --system user.name", stdout = TRUE)
    ),
    error = function(e) {
      "anonymous-user"
    }
  )
}

.git_config_get_name_gert <- function() {
  nm <- .git_config_get_name_gert_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_name_gert_global()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_name_gert_system()
  if (.is_string(nm)) {
    return(nm)
  }
  "anonymous-user"
}

.git_config_get_name_gert_local <- function() {
  config_tbl <- gert::git_config()
  local_vec_ind <- config_tbl[["level"]] == "local"
  config_tbl[
    config_tbl[["name"]] == "user.name" & local_vec_ind,
  ][["value"]]
}

.git_config_get_name_gert_global <- function() {
  config_tbl <- gert::git_config()
  global_vec_ind <- config_tbl[["level"]] == "global"
  config_tbl[
    config_tbl[["name"]] == "user.name" & global_vec_ind,
  ][["value"]]
}
.git_config_get_name_gert_system <- function() {
  config_tbl <- gert::git_config()
  system_vec_ind <- config_tbl[["level"]] == "system"
  config_tbl[
    config_tbl[["name"]] == "user.name" & system_vec_ind,
  ][["value"]]
}

# email
.git_config_get_email <- function() {
  switch(.git_system_get(),
    "git" = .git_config_get_email_git(),
    "gert" = .git_config_get_email_gert(),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_config_get_email_git <- function() {
  nm <- .git_config_get_email_git_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_email_git_global()
  if (.is_string(nm)) {
    return(nm)
  }
  .git_config_get_email_git_system()
}

.git_config_get_email_git_local <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --local user.email", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.git_config_get_email_git_global <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --global user.email", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.git_config_get_email_git_system <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --system user.email", stdout = TRUE)
    ),
    error = function(e) {
      "anonymous-user"
    }
  )
}

.git_config_get_email_gert <- function() {
  nm <- .git_config_get_email_gert_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_email_gert_global()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .git_config_get_email_gert_system()
  if (.is_string(nm)) {
    return(nm)
  }
  "anonymous-user"
}

.git_config_get_email_gert_local <- function() {
  config_tbl <- gert::git_config()
  local_vec_ind <- config_tbl[["level"]] == "local"
  config_tbl[
    config_tbl[["name"]] == "user.email" & local_vec_ind,
  ][["value"]]
}

.git_config_get_email_gert_global <- function() {
  config_tbl <- gert::git_config()
  global_vec_ind <- config_tbl[["level"]] == "global"
  config_tbl[
    config_tbl[["name"]] == "user.email" & global_vec_ind,
  ][["value"]]
}
.git_config_get_email_gert_system <- function() {
  config_tbl <- gert::git_config()
  system_vec_ind <- config_tbl[["level"]] == "system"
  config_tbl[
    config_tbl[["name"]] == "user.email" & system_vec_ind,
  ][["value"]]
}

# check if behind remote
.git_check_behind <- function() {
  if (!.git_remote_check_exists()) {
    return(invisible(FALSE))
  }
  switch(.git_system_get(),
    "git" = .git_check_behind_git(),
    "gert" = .git_check_behind_gert()
  )
}

.git_check_behind_git <- function() {
  commit_vec_local <- .git_get_commit_hash_local()
  commit_vec_remote <- .git_get_commit_hash_remote()
  length(setdiff(commit_vec_remote, commit_vec_local)) > 0L
}

.git_check_behind_gert <- function() {
  .git_fetch()
  gert::git_ahead_behind()$behind != 0L
}

.git_get_commit_hash_local <- function() {
  system2("git", args = c("log", "--pretty=format:'%H'"), stdout = TRUE)
}

.git_fetch <- function() {
  switch(.git_system_get(),
    "git" = .git_fetch_git(),
    "gert" = .git_fetch_gert()
  )
}

.git_fetch_git <- function() {
  system2("git", args = c("fetch"))
}
.git_fetch_gert <- function() {
  gert::git_fetch()
}

.git_get_commit_hash_remote <- function() {
  .git_fetch()
  remote_branch_name <- system2(
    "git",
    args = c("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"),
    stdout = TRUE
  )
  system2(
    "git",
    args = c("log", "--pretty=format:'%H'", remote_branch_name),
    stdout = TRUE
  )
}


.git_clone <- function(repo, path = NULL) {
  .assert_string(repo, TRUE)
  .assert_string(path)
  if (!grepl("/", repo)) {
    repo <- paste0(gh::gh_whoami()$login, "/", repo)
  }
  if (!is.null(path) && (path == "" || .is_len_0(path))) {
    path <- NULL
  }
  switch(.git_system_get(),
    "git" = .git_clone_git(repo, path),
    "gert" = .git_clone_gert(repo, path),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.git_clone_git <- function(repo, path = NULL) {
  url <- paste0("https://github.com/", repo, ".git")
  args <- c("clone", url, path)
  system2(
    "git",
    args = args,
    stdout = TRUE
  )
}

.git_clone_gert <- function(repo, path) {
  .dep_install("gert")
  url <- paste0("https://github.com/", repo, ".git")
  args_list <- list("url" = url, bare = FALSE)
  if (!is.null(path)) {
    args_list[["path"]] <- path
  }
  do.call(gert::git_clone, args_list)
}