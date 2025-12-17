# =========================================
# local
# =========================================

#' @title Add a local directory as a destination
#'
#' @description
#' Add a local directory as a destination
#' to a _projr.yml file.
#'
#'
#' @inheritParams projr_yml_dest_add_osf
#'
#' @param title character.
#' The name of the directory.
#' Has no effect besides helping structure `_projr.yml`.
#' If not supplied, will be made equal to `content`.
#' @param path character.
#' Path to the directory.
#' If a relative path is given, then it is taken as
#' relative to the project home directory.
#' Must be supplied.
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
#' @inheritParams projr_yml_dest_add_osf
#'
#' @param title character.
#' Title of the GitHub release.
#' Can use title as `@version`,
#' in which case the release will be entitled
#' the version of the project at the time.
#' If not supplied, then will
#' automatically be generated from `content`.
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
