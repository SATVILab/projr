# =========================================
# osf
# =========================================

#' @title Add an OSF node as a destination
#'
#' @description
#' Add an OSF node (project or component)
#' as a destination to a _projr.yml file.
#'
#' @param title character.
#' Title of the OSF node.
#' For GitHub releases, can use title as `@version`,
#' Note that this implies that a new tag will be created with each
#' new version, so do not use with large files.
#' If not supplied, then will
#' automatically be generated from `content`.
#' @param content character vector.
#' Labels of directories to include in the upload.
#' Options are the labels of directories
#' in the active `projr` configuration,
#' as well as "docs", "data" and "code".
#' "docs" means the directory where the documents are rendered to,
#' "data" means the files in the `"data"` directory,
#' and "code" means all files tracked by the Git repository.
#' @param path character.
#' Path to the directory on the OSF node.
#' @param structure "latest" or "version".
#' Structure of the remote.
#' If "latest", then `path` simply contains
#' the latest versions of the contents.
#' If "version", then `path` will contain
#' a directory for each version.
#' If not supplied, will be `version`.
#' @param overwrite logical.
#' Whether to rewrite an existing entry of the same
#' title in the specified `projr` configuration file.
#' Default is TRUE.
#' @param public logical.
#' Whether the OSF node is public.
#' Default is `FALSE`.
#' @param category character. The category of the project or component.
#' The following are valid options: `"project"`, `"analysis"`,
#' `"communication"`, `"data"`, `"hypothesis"`, `"instrumentation"`,
#' `"methods and measures"`, `"procedure"`, `"project"`, `"software"` and other
#' `"other"`. Default is `NULL.`
#' @param description character.
#' Description of the OSF node.
#' Default is `NULL`.
#' @param id character. The id of the project or component. Must be five
#' characters. Default is `NULL`.
#' @param id_parent character. The id of the parent project or component.
#' Must be five characters. Default is `NULL`.
#' @param send_cue "always", "if-change" or "never".
#' Only relevant if `structure` is `archive`
#' and `send_strategy` is `sync-diff` or `sync-purge`.
#' If `always`, then a new remote is created
#' every time, even if there is no change from the previous
#' build.
#' For example, if the contents of `raw-data` are the same
#' between builds `v0.0.1` and `v0.0.2`, then 
#' a local remote would have folders `raw-data/v0.0.1`
#' and `raw-data/v0.0.2`.
#' If `if-change`, then a new remote is created
#' only if there is a change from the previous build.
#' In the example above, a local remote would
#' only have the folder `raw-data/v0.0.1`.
#' If `never`, then a new remote is never created.
#' @param send_strategy "upload-all", "upload-missing",
#' "sync-purge" and "sync-diff".
#' How to synchronise to the remote.
#' If `upload-all`, then all files are uploaded.
#' If `upload-missing`, then only missing files are uploaded.
#' If `sync-purge`, then all files on the remote
#' are deleted before uploading all local files.
#' If `sync-diff`, then files
#' that have changed or been added locally are uploaded to the remote,
#' and files that have been removed locally are removed from the remote.
#' If not set, then "sync-diff" will be used.
#' @param send_inspect "manifest" , "file" or "none".
#' What to look at to find what are the
#' files on the remote, and their versions.
#' If `manifest`, then the manifest on the remote is used.
#' If `file`, then the files on the remote are downloaded
#' and their versions are determined.
#' If `none`, then no inspection is done (the remote
#' is typically treated as "empty" in that case).
#' If not set, then defaults to `"manifest"`.

#' @param profile character.
#' Profile to write the settings to.
#' If "default", then written to `_projr.yml`,
#' otherwise written to `_projr-<profile>.yml`.
#' The default is "default".
#' @export
#' @rdname.yml_dest_add_osf
projr_yml_dest_add_osf <- function(title,
                                   content,
                                   path = NULL,
                                   structure = NULL,
                                   overwrite = FALSE,
                                   public = FALSE,
                                   category = NULL,
                                   description = NULL,
                                   id = NULL,
                                   id_parent = NULL,
                                   # get_strategy = NULL,
                                   # get_conflict = NULL,
                                   send_cue = NULL,
                                   send_strategy = NULL,
                                   send_inspect = NULL,
                                   profile = "default") {
  .yml_dest_add(
    role = "destination",
    type = "osf",
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = NULL,
    overwrite = overwrite,
    public = public,
    category = category |> tolower(),
    description = description,
    id = id,
    id_parent = id_parent,
    get_strategy = NULL,
    get_conflict = NULL,
    send_cue = send_cue,
    send_strategy = send_strategy,
    send_inspect = send_inspect,
    profile = profile,
    overwrite = overwrite
  )
}

#' @rdname.yml_dest_add_osf
#' @export
projr_yml_dest_add_osf_proj <- function(title,
                                        description = NULL,
                                        content = NULL,
                                        public = FALSE,
                                        id = NULL) {
 .yml_dest_add_osf(
    title = title,
    description = description,
    content = content,
    public = public,
    category = "project",
    id = id
  )
}

#' @rdname.yml_dest_add_osf
#' @export
projr_yml_dest_add_osf_comp <- function(title,
                                        description = NULL,
                                        content = NULL,
                                        public = FALSE,
                                        category = NULL,
                                        id_parent = NULL,
                                        id = NULL) {
  if (missing(id_parent)) {
    stop("id_parent must be specified")
  }
 .yml_dest_add_osf(
    title = title,
    description = description,
    content = content,
    public = public,
    category = category,
    id_parent = id_parent,
    id = id
  )
}

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
#' @inheritParams.yml_dest_add_osf
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
#' @rdname.yml_dest_add
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
#' @inheritParams.yml_dest_add_osf
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
