.yml_dir_get_source_type <- function(type, label, profile) {
  .assert_string(type, TRUE)
  .assert_string(label, TRUE)
  .assert_string(profile)
  .assert_in(label, .opt_dir_get_label(profile))
  .assert_in(type, .opt_remote_get_type())
  init_list <- .yml_dir_get_source(label, profile)[[type]]
  if (length(init_list) == 0) {
    return(NULL)
  }
  init_list[[type]]
}

.yml_dir_get_source <- function(label, profile) {
  .assert_string(label, TRUE)
  .assert_string(profile)
  .assert_in(label, .opt_dir_get_label(profile))
  init_list <- .yml_dir_get_label(label, profile)[["source"]]
  if (length(init_list) == 0) {
    return(NULL)
  }
  init_list
}

.yml_dir_set_source <- function(yml_source, label, profile) {
  yml_label <- .yml_dir_get_label(label, profile)
  if (is.null(yml_source) || length(yml_source) == 0L) {
    yml_label[["source"]] <- NULL
  } else {
    yml_label[["source"]] <- yml_source
  }
  .yml_dir_set_label(yml_label, label, profile)
}

.yml_dir_get_source <- function(label, profile) {
  yml_label <- .yml_dir_get_label(label, profile)
  if (is.null(yml_label)) {
    return(NULL)
  }
  yml_label[["source"]]
}
