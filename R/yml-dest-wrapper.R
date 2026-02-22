# =========================================
# local
# =========================================

#' @title Add a local directory as a destination
#'
#' @description
#' Add a local directory as a destination
#' to a _projr.yml file.
#'
#' @param title character.
#' The name of the directory.
#' Has no effect besides helping structure `_projr.yml`.
#' If not supplied, will be made equal to `content`.
#' @param content character vector.
#' Labels of directories to include in the upload.
#' Options are the labels of directories
#' in the active `projr` configuration.
#' @param path character.
#' Path to the directory.
#' If a relative path is given, then it is taken as
#' relative to the project home directory.
#' Must be supplied.
#' @param structure "latest" or "archive".
#' Structure of the remote.
#' If "latest", then `path` simply contains
#' the latest versions of the contents.
#' If "version", then `path` will contain
#' a directory for each version.
#' If not supplied, will be `archive`.
#' @param overwrite logical.
#' Whether to rewrite an existing entry of the same
#' title in the specified `projr` configuration file.
#' Default is TRUE.
#' @param send_cue "always", "if-change" or "never".
#' When to send to the remote. Default is NULL.
#' @param send_strategy "upload-all", "upload-missing",
#' "sync-purge" or "sync-diff".
#' How to synchronise to the remote. Default is NULL.
#' @param send_inspect "manifest" , "file" or "none".
#' What to look at to find what are the
#' files on the remote, and their versions. Default is NULL.
#' @param profile character.
#' Profile to write the settings to.
#' If "default", then written to `_projr.yml`,
#' otherwise written to `_projr-<profile>.yml`.
#' The default is "default".
#'
#' @export
#' @rdname projr_yml_dest_add
projr_yml_dest_add_local <- function(title,
                                     content,
                                     path,
                                     structure = NULL,
                                     overwrite = TRUE,
                                     #  get_strategy = NULL,
                                     #  get_conflict = NULL,
                                     send_cue = NULL,
                                     send_strategy = NULL,
                                     send_inspect = NULL,
                                     profile = "default") {
  .assert_string(title, TRUE)
  .assert_len_1(title, TRUE)
  .assert_chr(content, TRUE)
  .assert_in(content, .yml_dir_get(NULL) |> names())
  .assert_string(path, TRUE)

  .yml_dest_add(
    role = "destination",
    type = "local",
    title = title,
    content = content,
    path = path,
    path_append_label = NULL,
    structure = structure,
    get_strategy = NULL,
    get_conflict = NULL,
    send_cue = send_cue,
    send_strategy = send_strategy,
    send_inspect = send_inspect,
    profile = profile,
    overwrite = overwrite
  )
}

# =========================================
# github
# =========================================

#' @title Add a GitHub release as a destination
#'
#' @description
#' Add a GitHub release as a destination
#' to a _projr.yml file.
#'
#' @param title character.
#' Title of the GitHub release.
#' Can use title as `@version`,
#' in which case the release will be entitled
#' the version of the project at the time.
#' If not supplied, then will
#' automatically be generated from `content`.
#' @param content character vector.
#' Labels of directories to include in the upload.
#' Options are the labels of directories
#' in the active `projr` configuration,
#' as well as "docs", "data" and "code".
#' @param structure "latest" or "archive".
#' Structure of the remote. Default is NULL.
#' @param overwrite logical.
#' Whether to rewrite an existing entry. Default is TRUE.
#' @param send_cue "always", "if-change" or "never".
#' When to send to the remote. Default is NULL.
#' @param send_strategy "upload-all", "upload-missing",
#' "sync-purge" or "sync-diff".
#' How to synchronise to the remote. Default is NULL.
#' @param send_inspect "manifest" , "file" or "none".
#' What to look at to find files on the remote. Default is NULL.
#' @param profile character.
#' Profile to write the settings to. Default is "default".
#' @export
projr_yml_dest_add_github <- function(title,
                                      content,
                                      structure = NULL,
                                      overwrite = TRUE,
                                      # get_strategy = NULL,
                                      # get_conflict = NULL,
                                      send_cue = NULL,
                                      send_strategy = NULL,
                                      send_inspect = NULL,
                                      profile = "default") {
  .assert_string(title, TRUE)
  # as GitHub automatically does this anyway
  title <- gsub(" ", "-", title)
  .assert_len_1(title, TRUE)
  .assert_chr(content, TRUE)
  .assert_in(
    content, .yml_dir_get(NULL) |> names() |> c("code") |> unique()
  )

  .yml_dest_add(
    role = "destination",
    type = "github",
    title = title,
    content = content,
    path = NULL,
    structure = structure,
    get_strategy = NULL,
    get_conflict = NULL,
    send_cue = send_cue,
    send_strategy = send_strategy,
    send_inspect = send_inspect,
    profile = profile,
    overwrite = overwrite
  )
}
