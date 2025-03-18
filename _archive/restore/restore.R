

.restore_label_get_source_spec_check <- function(label, type, title) {
  if (!is.null(pos) && !is.null(type) && !is.null(title)) {
    return(c("pos" = pos, "type" = type, "title" = title))
  }
}

.restore_get_source_source <- function(label, type, title) {
  yml_type <- .yml_dir_get_source_type(type, label, NULL)
  if (is.null(yml_type)) {
    return(NULL)
  }
  if (is.character(yml_type)) {
   .restore_get_source_source_chr(yml_type, title)
  }
  yml_type[[title]]
}

.restore_get_source_source_chr <- function(yml_type, title) {
  if (yml_type == title) list() else NULL
}

.restore_label_get_source <- function(label, pos, type, title) {
  # get source remote (type and title)
  if (!is.null(pos) && !is.null(type) && !is.null(title)) {
    return(c("pos" = pos, "type" = type, "title" = title))
  }
  yml_source_label <- .yml_dir_get_source(label, NULL)
  if (!is.null(yml_source_label)) {
    return(c(
      "pos" = "source",
      "type" = names(yml_source_label)[[1]],
      "title" = names(yml_source_label[[1]])[[1]]
    ))
  }
  if (is.null(type)) {
    nm_vec <- .yml_build_get(NULL) |>
      names()
    type <- nm_vec[grepl("^github$|^local$|^osf", nm_vec)]
    if (.is_len_0(type)) {
      if (.remote_check_exists("github", "archive")) {
        return(c("type" = "github", "title" = "archive"))
      } else {
        stop("No source found for ", label)
      }
      stop("No source found for ", label)
    }
  }
  yml_first <- NULL
  tp_first <- NULL
  tt_first <- NULL



  for (tp in type) {
    yml_type <- .yml_dest_get_type(tp, NULL)
    title <- names(yml_type)
    for (i in seq_along(title)) {
      tt <- title[[i]]
      yml_title <- .yml_dest_get_title_complete(
        tt, tp, NULL, FALSE, FALSE, FALSE
      )
      if (!label %in% yml_title[["content"]]) {
        next
      }
      if ("source" %in% names(yml_title)) {
        source_vec <- yml_title[["source"]]
        if (isTRUE(source_vec)) {
          return(c(type = tp, title = tt))
        } else if (isFALSE(source_vec)) {
          next
        } else if (label %in% source_vec) {
          return(c(type = tp, title = tt))
        }
      }
      if (is.null(tt_first)) {
        tp_first <- tp
        tt_first <- tt
      }
    }
  }

  c("yml" = yml_first,  "type" = tp_first, "title" = tt_first)
}
