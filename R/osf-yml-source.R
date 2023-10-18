#' @title Add an OSF node as a source
#'
#' @description
#' Add a project or component to the _projr.yml
#' specification for OSF.
#' Will create the node if it does not exist already.
#'
#' @param label character.
#' The directory label.
#' Must be supplied.
#' @param id character. The id of the project or component.
#' Must be five characters.
#' If not `NULL`, then it checks that the OSF
#' node exists and throws an error if it does not.
#' If `NULL`, then an OSF node is created, with
#' appropriate structure as specified by other parameters.
#' @param path character.
#' Path to the sub-directory within the OSF node
#' to which the contents will be saved.
#' @param path_append_label
#' Whether to append the label to the path.
#' For example, if `label` is `data-raw` and `path` is `Study234`,
#' then the path to the directory under which
#' the contents will be saved is be `data-raw`.
#' Default is `FALSE` if `path` is `NULL`, and `FALSE` otherwise.
#' @param category character.
#' The category of the project or component.
#' If `NULL`, then it is assumed that the node
#' is a component and will have category `"uncategorised"`.
#' If not `NULL`, then the following are valid options:
#' `"project"`, `"analysis"`,
#' `"communication"`, `"data"`, `"hypothesis"`, `"instrumentation"`,
#' `"methods and measures"`, `"procedure"`, `"project"`, `"software"` and other
#' `"other"`.
#' Only used if `id` is `NULL`.
#' Default is `NULL.`
#' @param parent_id character.
#' The id of the parent project or component.
#' Must be five characters (if supplied).
#' Only used if `id` is `NULL`.
#' Default is `NULL`.
#' @param title character.
#' The title of the project or component.
#' If not supplied, then will be set to `label`.
#' Only used if `id` is `NULL`.
#' @param body character vector.
#' Description on OSF.
#' Only used if `id` is `NULL`.
#' Default is `NULL`.
#' @param public logical.
#' Whether the project or component is public on OSF.
#' Only used if `id` is `NULL`.
#' Default is `FALSE`.

#' @param overwrite logical.
#' If `TRUE`, then any pre-existing OSF source will be overwritten
#' in the YAML file.
#' If `FALSE`, then a pre-existing source will throw an error.
#' Default is `FALSE`.
#'
#' @return
#' Invisibly returns the OSF id.
#'
#' @export
#' @examples
#'
#' # add a project as source:
#' projr_osf_source_add(
#'   label = "data-raw",
#'   category = "project"
#' )
#' # add a component as source:
#' projr_osf_source_add(
#'   label = "data-raw",
#'   category = "data",
#'   parent_id = "y235k"
#' )
projr_osf_source_add <- function(label,
                                 overwrite = FALSE,
                                 id = NULL,
                                 title = NULL,
                                 body = NULL,
                                 public = FALSE,
                                 category = NULL,
                                 parent_id = NULL,
                                 path = NULL,
                                 path_append_label = NULL,
                                 remote_structure = NULL,
                                 download_cue = NULL,
                                 download_sync_approach = NULL,
                                 download_conflict = NULL,
                                 upload_cue = NULL,
                                 upload_sync_approach = NULL,
                                 upload_version_source = NULL,
                                 upload_conflict = NULL) {
  # format inputs
  if (is.null(title)) {
    title <- label
  }
  yml_projr <- projr_yml_get_unchecked()

  download_list <- .projr_osf_yml_add_load_get_list(
    cue = download_cue,
    sync_approach = download_sync_approach,
    conflict = download_conflict
  )

  upload_list <- .projr_osf_yml_add_load_get_list(
    cue = upload_cue,
    sync_approach = upload_sync_approach,
    version_source = upload_version_source,
    conflict = upload_conflict
  )

  # check inputs
  .projr_osf_source_add_check(
    label = label, id = id, title = title, body = body,
    public = public, category = category, parent_id = parent_id,
    overwrite = overwrite,
    path = path, path_append_label = path_append_label,
    remote_structure = remote_structure,
    upload = upload_list, download = download_list
  )

  # check if going to overwrite
  if (!overwrite) {
    yml_projr_dir_label <- yml_projr[["directories"]][[label]]
    if ("osf" %in% names(yml_projr_dir_label)) {
      stop(paste0(
        "OSF source for label ", label, " already exists in _projr.yml"
      ))
    }
  }

  # get id
  # ------------------

  id <- .projr_osf_get_node_as_node(
    title = title,
    id = id,
    parent_id = parent_id,
    category = category,
    body = body,
    public = public
  )[["id"]][[1]]

  # create list to add
  # ------------------

  list_add <- .projr_osf_source_get_list_add(
    title = title,
    id = id,
    path = path,
    remote_structure = remote_structure,
    path_append_label = path_append_label,
    download_list = download_list,
    upload_list = upload_list
  )

  # add list
  # ----------------------------------
  yml_projr[["directories"]][[label]][["osf"]] <- list_add
  .projr_yml_set(yml_projr)

  id
}

.projr_osf_yml_add_load_get_list <- function(cue = NULL,
                                             sync_approach = NULL,
                                             version_source = NULL,
                                             conflict = NULL) {
  out_list <- list()
  param_vec <- c("cue", "sync_approach", "version_source", "conflict")
  for (x in param_vec) {
    if (!is.null(eval(parse(text = x)))) {
      nm_list <- switch(x,
        "sync_approach" = "sync-approach",
        "version_source" = "version-source",
        x
      )
      out_list[[nm_list]] <- eval(parse(text = x))
    }
  }
  out_list
}

.projr_osf_source_add_check <- function(label,
                                        id,
                                        title,
                                        body,
                                        public,
                                        category,
                                        path,
                                        path_append_label,
                                        remote_structure,
                                        parent_id,
                                        overwrite,
                                        download,
                                        upload) {
  # check inputs
  # ------------
  .projr_osf_yml_check_label(
    label = label, type_opt = c("data-raw", "cache")
  )
  .projr_osf_yml_check_body(body = body)

  .projr_osf_yml_check_id(id = id)
  .projr_osf_yml_check_parent_id(parent_id = parent_id)
  .projr_osf_yml_check_proj_with_parent(
    parent_id = parent_id, category = category
  )
  .projr_osf_yml_check_overwrite(overwrite = overwrite)
  .projr_osf_yml_check_path(path = path)
  .projr_osf_yml_check_path_append_label(
    path_append_label = path_append_label
  )
  .projr_osf_yml_check_remote_structure(
    remote_structure = remote_structure,
    nm_opt = c("latest", "version", "content")
  )

  # download element
  # -------------------------

  .projr_osf_yml_check_trans_list(trans_list = download)
  .projr_osf_yml_check_trans_names(
    trans_list = download,
    nm_opt = c("cue", "sync-approach", "conflict")
  )
  .projr_osf_yml_check_cue(
    trans_list = download,
    nm_opt = c("none", "build", "major", "minor", "patch")
  )
  .projr_osf_yml_check_sync_approach(
    trans_list = download,
    nm_opt = c(
      "download-all",
      "delete-then-download-all",
      "download-missing" # haven't implemented this one yet
    )
  )

  .projr_osf_yml_check_conflict(trans_list = download)

  # upload element
  # --------------------------------
  .projr_osf_yml_check_trans_list(trans_list = upload)
  .projr_osf_yml_check_trans_names(
    trans_list = upload,
    nm_opt = c("cue", "sync-approach", "version-source", "conflict")
  )
  .projr_osf_yml_check_cue(
    trans_list = upload,
    nm_opt = c("none", "build", "major", "minor", "patch")
  )
  .projr_osf_yml_check_sync_approach(
    trans_list = upload,
    nm_opt = c(
      "upload-all",
      "delete-then-upload-all",
      "upload-missing", # haven't implemented this one yet
      "upload-changed" # haven't implemented this one yet
    )
  )
  .projr_osf_yml_check_conflict(trans_list = upload)
}

.projr_osf_source_get_list_add <- function(title,
                                           id,
                                           path,
                                           path_append_label,
                                           remote_structure,
                                           download_list,
                                           upload_list) {
  list_add <- list(id = id)
  if (!is.null(remote_structure)) {
    list_add[["remote-structure"]] <- remote_structure
  }
  if (!is.null(path)) {
    list_add[["path"]] <- path
  }
  if (!is.null(path_append_label)) {
    list_add[["path-append-label"]] <- path_append_label
  }
  if (!length(download_list) == 0) {
    list_add <- list_add |> append(list(download = download_list))
  }
  if (!length(upload_list) == 0) {
    list_add <- list_add |> append(list(upload = upload_list))
  }
  list(list_add) |> stats::setNames(title)
}


.projr_osf_source_add_get_id <- function(id,
                                         parent_id,
                                         title,
                                         yml_param) {
  if (!is.null(id)) {
    osf_tbl <- .projr_osf_get_node_id(id)
    if (!inherits(osf_tbl, "osf_tbl_node")) {
      stop(paste0(
        "OSF node not found for id ", id
      ))
    }
    return(id)
  }
  # attempt to get node if it is already there
  osf_tbl <- .projr_osf_get_node_id_parent(
    title = title, parent_id = parent_id, parent_id_force = parent_id
  )
  if (!is.null(osf_tbl)) {
    return(osf_tbl[["id"]][[1]])
  }
  # create node if it isn't
  .projr_osf_create_node(
    title = title, yml_param = yml_param, parent_id = parent_id
  )[["id"]]
}
