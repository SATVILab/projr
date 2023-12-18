# =========================================
# osf
# =========================================

#' @title Add an OSF node as a destination
#'
#' @description
#' Add a project or component to the _projr.yml
#' specification for OSF.
#'
#' @param title character.
#' The title of the project or component.
#' Must be supplied.
#' @param body character. Description on OSF. Default is `NULL`.
#' @param content character vector. Keys to include in the OSF upload.
#' @param public logical. Whether the project or component is public on OSF.
#' Default is `FALSE`.
#' @param category character. The category of the project or component.
#' The following are valid options: `"project"`, `"analysis"`,
#' `"communication"`, `"data"`, `"hypothesis"`, `"instrumentation"`,
#' `"methods and measures"`, `"procedure"`, `"project"`, `"software"` and other
#' `"other"`. Default is `NULL.`
#' @param id character. The id of the project or component. Must be five
#' characters. Default is `NULL`.
#' @param id_parent character. The id of the parent project or component.
#' Must be five characters. Default is `NULL`.
#' @param title_parent character. The title of the parent project or component.
#' Default is `NULL`.
#' @param overwrite logical. If `TRUE`, then the project or component
#' will be overwitten if it exists.
#' Note that any components of the project/component
#' will be deleted from the YAML file. If `FALSE`, then an error will be thrown.
#' Default is `FALSE`.
#' element in
#' @export
#' @rdname projr_yml_dest_add
projr_yml_dest_add_osf <- function(title = NULL,
                                   content = NULL,
                                   structure = NULL,
                                   path = NULL,
                                   path_append_label = NULL,
                                   overwrite = FALSE,
                                   public = FALSE,
                                   category = NULL,
                                   description = NULL,
                                   id = NULL,
                                   id_parent = NULL,
                                   title_parent = NULL,
                                   get_sync_approach = NULL,
                                   get_conflict = NULL,
                                   send_cue = NULL,
                                   send_sync_approach = NULL,
                                   send_version_source = NULL,
                                   send_conflict = NULL) {
  .projr_yml_dest_add(
    role = "destination",
    type = "osf",
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    overwrite = overwrite,
    public = public,
    category = category,
    description = description,
    id = id,
    id_parent = id_parent,
    title_parent = title_parent,
    get_sync_approach = get_sync_approach,
    get_conflict = get_conflict,
    send_cue = send_cue,
    send_sync_approach = send_sync_approach,
    send_version_source = send_version_source,
    send_conflict = send_conflict
  )
}

#' @rdname projr_yml_dest_add_osf
#' @export
projr_yml_dest_add_osf_proj <- function(title,
                                        body = NULL,
                                        content = NULL,
                                        public = FALSE,
                                        id = NULL) {
  projr_yml_dest_add_osf(
    title = title,
    body = body,
    content = content,
    public = public,
    category = "project",
    id = id
  )
}

#' @rdname projr_yml_dest_add_osf
#' @export
projr_yml_dest_add_osf_comp <- function(title,
                                        body = NULL,
                                        content = NULL,
                                        public = FALSE,
                                        category = NULL,
                                        title_parent = NULL,
                                        id_parent = NULL,
                                        id = NULL) {
  if (missing(id_parent) && missing(title_parent)) {
    stop("either id_parent or title_parent must be specified")
  }
  projr_yml_dest_add_osf(
    title = title,
    body = body,
    content = content,
    public = public,
    category = category,
    id_parent = id_parent,
    title_parent = title_parent,
    id = id
  )
}

# =========================================
# local
# =========================================

#' @title Add an OSF node as a destination
#'
#' @description
#' Add a project or component to the _projr.yml
#' specification for OSF.
#'
#' Note: this will always add to `_projr.yml`.
#' If you want to add to another `projr` file, you can either
#' create that manually or copy the result of applying
#' this function to `_projr.yml` across.
#'
#' @param title character.
#' The name of the directory.
#' Has no effect besides helping structure `_projr.yml`.
#' If not supplied, will be made equal to `content`.
#' @param content character vector.
#' Labels to include in the OSF upload, e.g. `data-raw`, `cache`, `output`,
#' @param path character.
#' Path to the directory on the local system.
#' If a relative path is given, then it is taken as
#' relative to the project home directory.
#' If not supplied, then will be by default `_output`
#' if structure is `latest` and `_archive` if structure is
#' `version`.
#' @param path_append_label logical.
#' Whether to append the label to the path.
#' If `TRUE`, then the label will be appended to `path`.
#' If `FALSE`, then the path will be the path to the label.
#' @param ignore_git logical.
#' Whether to exclude the final path from Git.
#' If not supplied, then the path will be excluded.
#' @param ignore_rbuild logical.
#' Whether to exclude the final path in `.Rbuildignore`.
#' If not supplied, then the path will be excluded.
#' @param structure "latest" or "version".
#' Structure of the remote.
#' If "latest", then `path` simply contains
#' the latest versions of the contents.
#' If "version", then `path` will contain
#' a directory for each version.
#' If not supplied, will be `version`.
#' @param cue character.
#' When to cue the upload.
#'
#' @export
#' @rdname projr_yml_dest_add
projr_yml_dest_add_local <- function(title,
                                     content,
                                     path,
                                     path_append_label = NULL,
                                     structure = NULL,
                                     get_sync_approach = NULL,
                                     get_conflict = NULL,
                                     send_cue = NULL,
                                     send_sync_approach = NULL,
                                     send_version_source = NULL,
                                     send_conflict = NULL,
                                     overwrite = TRUE,
                                     profile = "default") {
  .assert_string(title, "title", required = TRUE)
  .assert_len(title, "title", 1L)
  ..assert_chr(content, "content", required = TRUE)
  .assert_opt(content, "content", .projr_yml_dir_get(profile) |> names())
  .assert_string(path, "path", required = TRUE)

  .projr_yml_dest_add(
    role = "destination",
    type = "local",
    title = title,
    content = content,
    path = path,
    path_append_label = path_append_label,
    structure = structure,
    get_sync_approach = get_sync_approach,
    get_conflict = get_conflict,
    send_cue = send_cue,
    send_sync_approach = send_sync_approach,
    send_version_source = send_version_source,
    send_conflict = send_conflict,
    profile = profile
  )
}
