#' @rdname projr_osf_yml_up_add
#' @export
projr_osf_yml_down_add_proj <- function(title,
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
