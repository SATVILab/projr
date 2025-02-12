projr_restore <- function(label) {
  
}

.projr_restore_get_label <- function(label) {
  if (!is.null(label)) {
    opt_vec <- .projr_yml_dir_get() |>
      names() |
    for (x in label ){
      .assert
    }
    .assert_in(label)
  }
}

