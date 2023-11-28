# =================
# get
# =================

# title
.projr_yml_remote_title_get <- function(title, content) {
  if (is.null(title)) paste0(content, collapse = "-") else title
}

# content
.projr_yml_remote_content_get <- function(content) {
  if (is.null(content)) {
    stop("Content must be specified")
  }
  .projr_yml_remote_check_content(content = content)
  content
}

# public
.projr_yml_remote_public_get <- function(type, public) {
  # check
  .projr_yml_remote_check_public(type = type, public = public)
  public
}

# path
.projr_yml_remote_path_get <- function(type, path) {
  .projr_yml_remote_check_path(type = type, path = path)
  path
}

# append_label
.projr_yml_remote_path_append_label_get <- function(type, path) {
  .projr_yml_remote_check_path_append_label(type = type, path = path)
  path
}

# transfer list (get_list or send_list)
.projr_yml_remote_transfer_get <- function(cue = NULL,
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
