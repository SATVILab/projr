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
#' If not `NULL`, then it assumed that an OSF node withthis ID exists.
#' If `NULL`, then an OSF node is created.
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
#' @param category character.
#' The category of the project or component.
#' The following are valid options: `"project"`, `"analysis"`,
#' `"communication"`, `"data"`, `"hypothesis"`, `"instrumentation"`,
#' `"methods and measures"`, `"procedure"`, `"project"`, `"software"` and other
#' `"other"`.
#' Only used if `id` is `NULL`.
#' Default is `NULL.`
#' @param path character.
#' Path to the sub-directory within the OSF node
#' to which the contents will be saved.
#' @param path_append_label
#' Whether to append the label to the path.
#' For example, if `label` is `data-raw` and `path` is `Study234`,
#' then the path to the directory under which
#' the contents will be saved is be `data-raw`.
#' Default is `FALSE` if `path` is `NULL`, and `FALSE` otherwise.
#' @
#' @param overwrite logical.
#' If `TRUE`, then any pre-existing OSF source will be overwritten
#' in the YAML file.
#' If `FALSE`, then a pre-existing source will throw an error.
#' Default is `TRUE`.
#' @param prefer
#'
#' @export
#' @examples
projr_osf_source_add <- function(label,
                                 overwrite = TRUE,
                                 id = NULL,
                                 title = NULL,
                                 body = NULL,
                                 public = FALSE,
                                 category = NULL,
                                 parent_id = NULL,
                                 parent_title = NULL,
                                 path = NULL,
                                 path_append_label = NULL,
                                 download_cue = NULL,
                                 download_sync_approach = NULL,
                                 download_version_source = NULL,
                                 download_conflict = NULL,
                                 upload_cue = NULL,
                                 upload_sync_approach = NULL,
                                 upload_version_source = NULL,
                                 upload_conflict = NULL,
                                 upload_archive = NULL) {
  # format inputs
  if (is.null(title)) {
    title <- label
  }

  download_list <- .projr_osf_yml_add_load_get_list(
    cue = download_cue,
    sync_approach = download_sync_approach,
    version_source = download_version_source,
    conflict = download_conflict
  )

  upload_list <- .projr_osf_yml_add_load_get_list(
    cue = upload_cue,
    sync_approach = upload_sync_approach,
    version_source = upload_version_source,
    conflict = upload_conflict,
    archive = upload_archive
  )

  yml_param <- list(
    category = category,
    description = body,
    public = public,
    category = category
  )
  yml_projr <- projr_yml_get_unchecked()

  # check inputs
  .projr_osf_source_add_check(
    label = label, id = id, title = title, body = body,
    public = public, category = category, parent_id = parent_id,
    parent_title = parent_title, overwrite = overwrite,
    upload = upload_list, download = download_list
  )

  # check if going to overwrite
  if (!overwrite) {
    yml_projr_dir_label <- yml_projr()[["directories"]][[label]]
    if ("osf" %in% names(yml_projr_dir_label)) {
      stop(paste0(
        "OSF source for label ", label, " already exists in _projr.yml"
      ))
    }
  }

  # get id
  # ------------------

  id <- .projr_osf_source_add_get_id(
    id = id, parent_id = parent_id,
    title = title, yml_param = yml_param
  )

  # create list to add
  # ------------------

  list_add <- .projr_osf_source_get_list_add(
    title = title,
    id = id,
    path = path,
    path_append_label = path_append_label,
    download_list = download_list,
    upload_list = upload_list
  )

  # add list
  # ----------------------------------
  yml_projr()[["directories"]][[label]][["osf"]] <- list_add
  .projr_yml_set(yml_projr)
}

.projr_osf_yml_add_load_get_list <- function(cue = NULL,
                                             sync_approach = NULL,
                                             version_source = NULL,
                                             conflict = NULL,
                                             archive = NULL) {
  out_list <- list()
  param_vec <- c("cue", "sync_approach", "version_source", "conflict")
  for (x in param_vec) {
    if (!is.null(eval(parse(text = x)))) {
      out_list[[x]] <- eval(parse(text = x))
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
                                        parent_id,
                                        parent_title,
                                        overwrite,
                                        download,
                                        upload) {
  # check inputs
  # ------------
  if (missing(label)) {
    stop("label must be supplied")
  }
  if (!is.logical(public)) {
    stop("public must be logical")
  }
  body_invalid <- (!is.null(body)) &&
    (!all(is.character(body)) || length(body) != 1)
  if (body_invalid) {
    stop("body must be a character vector (with one element)")
  }
  category_vec_valid <- c(
    "analysis",
    "communication",
    "data",
    "hypothesis",
    "instrumentation",
    "methods and measures",
    "procedure",
    "project",
    "software",
    "other"
  )
  category_invalid <- (!is.null(category)) &&
    !all(category %in% category_vec_valid)
  if (category_invalid) {
    stop("category must match a valid category")
  }
  id_invalid <- (!is.null(id)) &&
    (!all(is.character(id)) || all(nchar(id) == 5L) || length(id) != 1)
  if (id_invalid) {
    stop("id must be a string with five characters")
  }
  parent_id_invalid <- (!is.null(parent_id)) &&
    (!all(is.character(parent_id)) || all(nchar(parent_id) == 5L))
  if (parent_id_invalid) {
    stop("parent_id must be a string with five characters")
  }
  parent_title_invalid <- (!is.null(parent_title)) &&
    (!all(is.character(parent_title)) || length(parent_title) != 1)
  if (parent_title_invalid) {
    stop("parent_title must be a character of length one")
  }
  project_with_parent <- ((!is.null(category)) && category == "project") &&
    ((!is.null(parent_id)) || (!is.null(parent_title)))
  if (project_with_parent) {
    stop("OSF project cannot have a parent")
  }

  if (!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }

  # download element
  # -------------------------

  if (!is.list(download)) {
    stop("download must be a list")
  }
  dnld_diff_vec <- setdiff(
    names(download),
    c("cue", "sync_approach", "version_source", "conflict")
  )
  if (length(dnld_diff_vec) > 0) {
    stop(paste0(
      "download contains invalid names: ",
      paste0(dnld_diff_vec, collapse = ", ")
    ))
  }
  if ("cue" %in% names(download)) {
    if (!is.null(download[["cue"]])) {
      cue_diff_vec <- setdiff(
        download[["cue"]],
        c("none", "build", "major", "minor", "patch")
      )
      if (length(cue_diff_vec) > 0) {
        stop(paste0(
          "download$cue contains invalid names: ",
          paste0(cue_diff_vec, collapse = ", ")
        ))
      }
    }
  }
  if ("sync_approach" %in% names(download)) {
    if (!is.null(download[["sync_approach"]])) {
      selection_method_diff_vec <- setdiff(
        download[["sync_approach"]],
        c(
          "upload_all", "delete_then_upload_all", "upload_change"
        )
      )
      if (length(selection_method_diff_vec) > 0) {
        stop(paste0(
          "download$selection_method contains invalid names: ",
          paste0(selection_method_diff_vec, collapse = ", ")
        ))
      }
    }
  }
  if ("conflict" %in% names(download)) {
    if (!is.null(download[["conflict"]])) {
      conflict_diff_vec <- setdiff(
        download[["conflict"]], c("error", "overwrite", "skip")
      )
      if (length(conflict_diff_vec) > 0) {
        stop(paste0(
          "download$conflict contains invalid names: ",
          paste0(conflict_diff_vec, collapse = ", ")
        ))
      }
    }
  }
  if ("manifest_source" %in% names(download)) {
    if (!is.null(download[["manifest_source"]])) {
      manifest_source_diff_vec <- setdiff(
        download[["manifest_source"]], c("manifest", "download")
      )
      if (length(manifest_source_diff_vec) > 0) {
        stop(paste0(
          "download$manifest_source contains invalid names: ",
          paste0(manifest_source_diff_vec, collapse = ", ")
        ))
      }
    }
  }

  # upload element
  # --------------------------------
  upld_diff_vec <- setdiff(
    names(upload),
    c("cue", "sync_approach", "version_source", "conflict", "archive")
  )
  if (length(upld_diff_vec) > 0) {
    stop(paste0(
      "upload contains invalid names: ",
      paste0(upld_diff_vec, collapse = ", ")
    ))
  }
  if (!is.list(upload)) {
    stop("upload must be a list")
  }
}


.projr_osf_source_get_list_add <- function(title,
                                           id,
                                           path,
                                           path_append_label,
                                           download_list,
                                           upload_list) {
  list_add <- list(id = id)
  if (!is.null(path)) {
    list_add[["path"]] <- path
  }
  if (!is.null(path_append_label)) {
    list_add[["path_append_label"]] <- path_append_label
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
    return(id)
  }
  # attempt to get node if it is already there
  osf_tbl <- .projr_osf_get_node_id_parent(
    title = title, parent_id = parent_id
  )
  if (!is.null(osf_tbl)) {
    return(osf_tbl[["id"]][[1]])
  }
  # create node if it isn't
  .projr_osf_create_node(
    title = title, yml_param = yml_param, parent_id = parent_id
  )[["title"]]
}
