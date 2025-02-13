.projr_git_init <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_init_git(),
    "gert" = .projr_git_init_gert(),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )

  if (.is_test()) {
    .projr_dep_install_only("gert")
    gert::git_config_set("user.name", "DarthVader")
    gert::git_config_set("user.email", "number_one_fan@tellytubbies.com")
  }
}

.projr_git_repo_check_exists <- function() {
  dir.exists(.path_get(".git"))
}

.projr_git_repo_rm <- function() {
  if (dir.exists(.path_get(".git"))) {
    unlink(.path_get(".git"), recursive = TRUE)
  }
}

.projr_git_commit_file <- function(file, msg = NULL) {
  if (length(file) == 0L) {
    return(invisible(FALSE))
  }
  msg <- .projr_git_msg_get(msg)
  switch(.projr_git_system_get(),
    # suppress warnings from adding deleted files
    "git" = suppressWarnings(.projr_git_commit_file_git(file, msg)),
    "gert" = suppressWarnings(.projr_git_commit_file_gert(file, msg)),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_commit_file_git <- function(file, msg, ...) {
  .projr_git_add_file_git(file)
  system2(
    "git",
    args = paste0(
      "commit -m ", shQuote(msg), " ",
      paste0(normalizePath(file, winslash = "/"), collapse = " ")
    ),
    ...
  )
}

.projr_git_add_file_git <- function(file, ...) {
  system2(
    "git",
    args = paste0(
      "add ", paste0(normalizePath(file, winslash = "/"), collapse = " ")
    ),
    stdout = TRUE, ...
  )
}

.projr_git_commit_file_gert <- function(file, msg, ...) {
  gert::git_add(file)
  gert::git_commit(msg, ...)
}

.projr_git_commit_all <- function(msg = NULL, add_untracked = TRUE) {
  add_vec <- .projr_git_modified_get()
  if (add_untracked) {
    add_vec <- c(add_vec, .projr_git_new_get())
  }
  msg <- .projr_git_msg_get(msg)
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
    "^\\?\\? (.*)", "\\1",
    grep("^\\?\\?", git_status_output, value = TRUE)
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

# just checked
.projr_git_changed_filter <- function(path) {
  # check if specific file(s)
  # are untracked (and not ignore) or modified
  if (!.projr_git_repo_check_exists()) {
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
  switch(.projr_git_system_get(),
    "git" = .projr_git_changed_filter_git(path),
    "gert" = .projr_git_changed_filter_gert(path),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
}

.projr_git_changed_filter_git <- function(path) {
  path[vapply(path, .projr_git_changed_filter_git_ind, logical(1L))]
}
.projr_git_changed_filter_git_ind <- function(path) {
  # Run git status with porcelain output
  status <- system2("git", args = c("status", "--porcelain", path), stdout = TRUE)
  length(status) > 0
}

.projr_git_changed_filter_gert <- function(path) {
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
.projr_git_init_git <- function() {
  system2("git", args = "init")
}

.projr_git_init_gert <- function() {
  gert::git_init(path = .path_get())
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
  if (!.projr_git_remote_check_exists()) {
    return(FALSE)
  }
  switch(.projr_git_system_get(),
    "git" = {
      if (!.projr_git_remote_check_upstream_git()) {
        warning("No upstream remote detected")
        return(FALSE)
      }
      invisible(TRUE)
    },
    # I think `gert` automatically sets to origin if none found
    "gert" = invisible(TRUE)
  )
}

.projr_git_remote_check_upstream_git <- function() {
  upstream_branch <- system2("git", args = c(
    "rev-parse", "--abbrev-ref", "--symbolic-full-name", paste0("HEAD", "@{upstream}")
  ), stdout = TRUE, stderr = TRUE)
  !grepl("no upstream configured", upstream_branch)
}

# push
.projr_git_push <- function(...) {
  switch(.projr_git_system_get(),
    "git" = .projr_git_push_git(...),
    "gert" = .projr_git_push_gert(...),
    stop(paste0(.projr_git_system_get(), " not recognised"))
  )
  invisible(TRUE)
}

.projr_git_push_git <- function(...) {
  system2("git", args = "push", ...)
  invisible(TRUE)
}

.projr_git_push_gert <- function(...) {
  gert::git_push(...)
  invisible(TRUE)
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
  nm <- .projr_git_config_get_name_git_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .projr_git_config_get_name_git_global()
  if (.is_string(nm)) {
    return(nm)
  }
  .projr_git_config_get_name_git_system()
}

.projr_git_config_get_name_git_local <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --local user.name", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.projr_git_config_get_name_git_global <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --global user.name", stdout = TRUE)
    ),
    error = function(e) character()
  )
}
.projr_git_config_get_name_git_system <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", args = "config --system user.name", stdout = TRUE)
    ),
    error = function(e) {
      "anonymous-user"
    }
  )
}

.projr_git_config_get_name_gert <- function() {
  nm <- .projr_git_config_get_name_gert_local()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .projr_git_config_get_name_gert_global()
  if (.is_string(nm)) {
    return(nm)
  }
  nm <- .projr_git_config_get_name_gert_system()
  if (.is_string(nm)) {
    return(nm)
  }
  "anonymous-user"
}

.projr_git_config_get_name_gert_local <- function() {
  config_tbl <- gert::git_config()
  local_vec_ind <- config_tbl[["level"]] == "local"
  config_tbl[
    config_tbl[["name"]] == "user.name" & local_vec_ind,
  ][["value"]]
}

.projr_git_config_get_name_gert_global <- function() {
  config_tbl <- gert::git_config()
  global_vec_ind <- config_tbl[["level"]] == "global"
  config_tbl[
    config_tbl[["name"]] == "user.name" & global_vec_ind,
  ][["value"]]
}
.projr_git_config_get_name_gert_system <- function() {
  config_tbl <- gert::git_config()
  system_vec_ind <- config_tbl[["level"]] == "system"
  config_tbl[
    config_tbl[["name"]] == "user.name" & system_vec_ind,
  ][["value"]]
}

.projr_git_check_behind <- function() {
  if (!.projr_git_remote_check_exists()) {
    return(invisible(FALSE))
  }
  switch(.projr_git_system_get(),
    "git" = .projr_git_check_behind_git(),
    "gert" = .projr_git_check_behind_gert()
  )
}

.projr_git_check_behind_git <- function() {
  commit_vec_local <- .projr_git_get_commit_hash_local()
  commit_vec_remote <- .projr_git_get_commit_hash_remote()
  length(setdiff(commit_vec_remote, commit_vec_local)) > 0L
}

.projr_git_check_behind_gert <- function() {
  .projr_git_fetch()
  gert::git_ahead_behind()$behind != 0L
}

.projr_git_get_commit_hash_local <- function() {
  system2("git", args = c("log", "--pretty=format:'%H'"), stdout = TRUE)
}

.projr_git_fetch <- function() {
  switch(.projr_git_system_get(),
    "git" = .projr_git_fetch_git(),
    "gert" = .projr_git_fetch_gert()
  )
}

.projr_git_fetch_git <- function() {
  system2("git", args = c("fetch"))
}
.projr_git_fetch_gert <- function() {
  gert::git_fetch()
}

.projr_git_get_commit_hash_remote <- function() {
  .projr_git_fetch()
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
