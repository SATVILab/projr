.projr_pb_release_tbl_get <- function(pause_second = 3) {
  gh_tbl_release <- .projr_pb_release_tbl_get_attempt()
  if (!.projr_pb_tbl_redo_check(gh_tbl_release)) {
    return(gh_tbl_release)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_release <- .projr_pb_release_tbl_get_attempt()
  if (!.projr_pb_tbl_redo_check(gh_tbl_release)) {
    return(gh_tbl_release)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  .projr_pb_release_tbl_get_attempt()
}

.projr_pb_release_tbl_get_attempt <- function() {
  try(suppressWarnings(suppressMessages(piggyback::pb_releases())))
}

.projr_pb_asset_tbl_get <- function(tag, pause_second = 3) {
  gh_tbl_asset <- .projr_pb_asset_tbl_get_attempt(tag = tag)
  if (!.projr_pb_tbl_redo_check(gh_tbl_asset)) {
    return(gh_tbl_asset)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_asset <- .projr_pb_asset_tbl_get_attempt(tag = tag)
  if (!.projr_pb_tbl_redo_check(gh_tbl_asset)) {
    return(gh_tbl_asset)
  }
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  .projr_pb_asset_tbl_get_attempt(tag = tag)
}

.projr_pb_tbl_redo_check <- function(tbl) {
  if (is.null(tbl)) {
    return(TRUE)
  }
  error_lgl <- inherits(tbl, "try-error")
  zero_row_lgl <- nrow(tbl) == 0L
  error_lgl || zero_row_lgl
}

.projr_pb_asset_tbl_get_attempt <- function(tag) {
  try(suppressWarnings(suppressMessages(piggyback::pb_list(tag = tag))))
}

.projr_pb_tag_format <- function(tag) {
  tag <- switch(tag,
    `@version` = .projr_version_get_v(),
    tag
  )
  tag <- gsub("^ +", "", tag)
  tag < gsub(" +$", "", tag)
  gsub(" ", "-", tag)
}
