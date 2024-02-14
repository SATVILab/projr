.projr_yml_remote_check <- function(role,
                                    type,
                                    title = NULL,
                                    content = NULL,
                                    structure = NULL,
                                    path = NULL,
                                    path_append_label = NULL,
                                    overwrite = FALSE,
                                    public = FALSE,
                                    category = NULL,
                                    description = NULL,
                                    id = NULL,
                                    id_parent = NULL,
                                    get_list = NULL,
                                    send_list = NULL) {
  .assert_string(title)
  .assert_string(role, TRUE)
  .assert_string(type, TRUE)
  content_opt <- switch(type,
    "github" = .projr_opt_dir_get_label_send(NULL) |> c("code"),
    .projr_opt_dir_get_label_send(NULL)
  ) |>
    unique()
  .assert_in(content, .projr_opt_dir_get_label_send(NULL), TRUE)
  .assert_in(structure, .projr_opt_remote_get_structure())
  .assert_string(path, type == "local")
  .assert_flag(path_append_label)
  .assert_flag(overwrite, TRUE)
  .assert_flag(public, type == "osf")
  .assert_in(category, .projr_opt_remote_get_osf_cat(), type == "osf")
  .assert_string(description)
  .assert_string(id)
  .assert_nchar_single(id, 5L)
  .assert_string(id_parent)
  .assert_nchar_single(id_parent, 5L)
  .assert_class_exact(get_list, "list")
  if (.is_len_pos(get_list)) {
    .assert_in(names(get_list), c("sync-approach", "conflict"))
    .assert_in(
      get_list[["sync-approach"]], .projr_opt_remote_sync_approach_get()
    )
    .assert_in(
      get_list[["conflict"]], .projr_opt_remote_conflict_get()
    )
  }
  .assert_class_exact(send_list, "list")
  if (.is_len_pos(send_list)) {
    .assert_in(names(send_list), .projr_opt_remote_transfer_names_get())
    .assert_in(send_list[["cue"]], .projr_opt_cue_get())
    .assert_in(
      send_list[["sync-approach"]], .projr_opt_remote_sync_approach_get()
    )
    .assert_in(send_list[["conflict"]], .projr_opt_remote_conflict_get())
    .assert_in(
      send_list[["version-source"]], .projr_opt_remote_version_source_get()
    )
  }
  invisible(TRUE)
}
