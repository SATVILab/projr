.pb_release_tbl_get <- function(pause_second = 3) {
  .dep_install("piggyback")
  gh_tbl_release <- .pb_release_tbl_get_attempt()
  if (!.pb_tbl_redo_check(gh_tbl_release)) {
    return(gh_tbl_release)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_release <- .pb_release_tbl_get_attempt()
  if (!.pb_tbl_redo_check(gh_tbl_release)) {
    return(gh_tbl_release)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  .pb_release_tbl_get_attempt()
}

.pb_repo_get <- function() {
  if (.git_repo_is_worktree()) {
    git_file <- readLines(.path_get(".git"), warn = FALSE)
    git_file <- gsub("^gitdir: ", "", git_file)
    if (!file.exists(git_file)) {
      stop("Cannot find gitdir: ", git_file)
    }
    .pb_guess_repo(git_file)
  } else {
    .pb_guess_repo()
  }
}

.pb_guess_repo <- function(path = ".") {
  gh_repo <- gh::gh_tree_remote(path)
  paste0(gh_repo[[1]], "/", gh_repo[[2]])
}

.pb_release_tbl_get_attempt <- function() {
  try(suppressWarnings(suppressMessages(
    piggyback::pb_releases(repo = .pb_repo_get()) #nolint
  ))) #nolint
}

.pb_asset_tbl_get <- function(tag, pause_second = 3) {
  .dep_install("piggyback")
  gh_tbl_asset <- .pb_asset_tbl_get_attempt(tag = tag)
  if (!.pb_tbl_redo_check(gh_tbl_asset)) {
    return(gh_tbl_asset)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_asset <- .pb_asset_tbl_get_attempt(tag = tag)
  if (!.pb_tbl_redo_check(gh_tbl_asset)) {
    return(gh_tbl_asset)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  .pb_asset_tbl_get_attempt(tag = tag)
}

.pb_tbl_redo_check <- function(tbl) {
  if (is.null(tbl)) {
    return(TRUE)
  }
  error_lgl <- inherits(tbl, "try-error")
  zero_row_lgl <- nrow(tbl) == 0L
  error_lgl || zero_row_lgl
}

.pb_asset_tbl_get_attempt <- function(tag) {
  try(suppressWarnings(suppressMessages(piggyback::pb_list(
    repo = .pb_repo_get(), tag = tag
  ))))
}

.pb_tag_format <- function(tag) {
  tag <- switch(tag,
    `@version` = .version_get_v(),
    tag
  )
  tag <- gsub("^ +", "", tag)
  tag < gsub(" +$", "", tag)
  gsub(" ", "-", tag)
}
