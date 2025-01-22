# =================
# get
# =================

# title
.projr_yml_remote_title_get <- function(title, content) {
  if (!.is_given_mid(title)) paste0(content, collapse = "-") else title
}


# transfer list (get_list or send_list)
.projr_yml_remote_transfer_get <- function(cue = NULL,
                                           strategy = NULL,
                                           version_source = NULL,
                                           conflict = NULL) {
  out_list <- list()
  param_vec <- c("cue", "strategy", "version_source", "conflict")
  for (x in param_vec) {
    if (!is.null(eval(parse(text = x)))) {
      nm_list <- switch(x,
        "strategy" = "strategy",
        "version_source" = "version-source",
        x
      )
      out_list[[nm_list]] <- eval(parse(text = x))
    }
  }
  out_list
}
