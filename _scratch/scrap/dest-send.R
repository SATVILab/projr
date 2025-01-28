# ================================
# send functions
# ================================

# for a single label
# ------------------------

.projr_dest_send_label <- function(label,
                                   title,
                                   type,
                                   output_run,
                                   upload_github,
                                   upload_force) {
  force(title)
  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run)
  yml_title <- .projr_yml_dest_get_title_complete(
    title, type, NULL, upload_github, upload_force
    )
  remote <- .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    label = label,
    structure = yml_title[["structure"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]]
  )

  # get overall type of plan
  plan <- .projr_dest_send_get_plan(
    inspect = yml_title[["send"]][["inspect"]],
    strategy = yml_title[["send"]][["strategy"]],
    type = type,
    structure = yml_title[["structure"]]
  )

  # get details of what files to remove and add
  plan_list_detail <- .projr_dest_send_get_plan_detail(
    plan = plan,
    label = label,
    path_dir_local = path_dir_local,
    remote = remote,
    type = type,
    inspect = yml_title[["send"]][["inspect"]]
  )

  # remove and upload files as stated
  .projr_plan_implement(
    plan = plan,
    plan_detail = plan_list_detail,
    remote = remote,
    type = type,
    structure = yml_title[["structure"]],
    path_dir_local = path_dir_local,
    conflict = yml_title[["send"]][["conflict"]]
  )
  
  # update manifest.csv and VERSION file
  .projr_dest_send_label_versioning_update(
    type = type, 
    remote = remote,
    label = label,
    plan_detail = plan_list_detail
  )
}

.projr_dest_send_label_get_remote_final_yml <- function(type,
                                                        label,
                                                        yml_title) {
  .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    label = label,
    structure = yml_title[["structure"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]]
  )
}