# =================
# get
# =================

# title
.yml_remote_title_get <- function(title, content) {
  if (!.is_given_mid(title)) paste0(content, collapse = "-") else title
}


# transfer list (get_list or send_list)
.yml_remote_transfer_get <- function(cue = NULL,
                                     strategy = NULL,
                                     inspect = NULL,
                                     conflict = NULL) {
  out_list <- list()
  param_vec <- c("cue", "strategy", "inspect", "conflict")
  for (x in param_vec) {
    if (!is.null(eval(parse(text = x)))) {
      nm_list <- switch(x,
        "strategy" = "strategy",
        "inspect" = "inspect",
        x
      )
      out_list[[nm_list]] <- eval(parse(text = x))
    }
  }
  out_list
}
