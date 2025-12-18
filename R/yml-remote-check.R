.yml_remote_check <- function(role,
                              type,
                              title = NULL,
                              content = NULL,
                              structure = NULL,
                              path = NULL,
                              path_append_label = NULL,
                              overwrite = FALSE,
                              description = NULL,
                              id = NULL,
                              id_parent = NULL,
                              get_list = NULL,
                              send_list = NULL) {
  .assert_string(title)
  .assert_string(role, TRUE)
  .assert_string(type, TRUE)
  content_opt <- switch(type,
    "github" = .opt_dir_get_label_send(NULL) |> c("code"),
    .opt_dir_get_label_send(NULL)
  ) |>
    unique()
  .assert_in(content, .opt_dir_get_label_send(NULL), TRUE)
  .assert_in(structure, .opt_remote_get_structure())
  .assert_string(path, type == "local")
  .assert_flag(path_append_label)
  .assert_flag(overwrite, TRUE)
  .assert_string(description)
  .assert_string(id)
  .assert_nchar_single(id, 5L)
  .assert_string(id_parent)
  .assert_nchar_single(id_parent, 5L)
  .assert_class_exact(get_list, "list")
  if (.is_len_pos(get_list)) {
    .assert_in(names(get_list), c("strategy", "conflict"))
    .assert_in(
      get_list[["strategy"]], .opt_remote_strategy_get()
    )
    .assert_in(
      get_list[["conflict"]], .opt_remote_conflict_get()
    )
  }
  .assert_class_exact(send_list, "list")
  if (.is_len_pos(send_list)) {
    .assert_in(names(send_list), .opt_remote_transfer_names_get())
    .assert_in(send_list[["cue"]], .opt_cue_get())
    .assert_in(
      send_list[["strategy"]], .opt_remote_strategy_get()
    )
    .assert_in(send_list[["conflict"]], .opt_remote_conflict_get())
    .assert_in(
      send_list[["inspect"]], .opt_remote_inspect_get()
    )
  }
  invisible(TRUE)
}
