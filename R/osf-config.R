#' @rdname projr_osf_yml_up_add
#' @export
projr_osf_yml_up_add_proj <- function(title,
                                      body = NULL,
                                      content = NULL,
                                      public = FALSE,
                                      id = NULL) {
  projr_osf_yml_up_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = "project",
    id = id
  )
}

#' @rdname projr_osf_yml_up_add
#' @export
projr_osf_yml_up_add_comp <- function(title,
                                      body = NULL,
                                      content = NULL,
                                      public = FALSE,
                                      category = NULL,
                                      parent_title = NULL,
                                      parent_id = NULL,
                                      id = NULL) {
  if (missing(parent_id) && missing(parent_title)) {
    stop("either parent_id or parent_title must be specified")
  }
  projr_osf_yml_up_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = category,
    parent_id = parent_id,
    parent_title = parent_title,
    id = id
  )
}
