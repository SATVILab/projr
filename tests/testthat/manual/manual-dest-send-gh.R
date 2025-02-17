library(testthat)
devtools::load_all()
dir_test <- .test_setup_project(
  git = TRUE, github = TRUE, set_env_var = TRUE
)
usethis::with_project(
  path = dir_test,
  code = {
    .yml_dest_rm_type_all("default")
    file.create(projr_path_get("raw-data", "data.csv"))
   projr_init_git()
    projr::projr_build_dev()
    projr::projr_build_patch()
    # check that it runs at all
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      overwrite = TRUE
    )
   projr_yml_dest_add_github(
      title = "latest",
      content = "raw-data",
      structure = "latest"
    )
     # expect uploads
    projr::projr_build_patch()
    expect_true(.remote_check_exists("github", "latest"))
    expect_true(.remote_check_exists("github", "archive"))
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "archive", "fn" = "raw-data-v0.0.2.zip"),
      "github", "v0.0.2", "raw-data"
    )
    expect_identical(manifest_archive$fn[[1]], "data.csv")

    # expect no upload
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_false(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")


    # now force upload
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "always",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "archive", "fn" = "raw-data-v0.0.4.zip"),
      "github", "v0.0.4", "raw-data"
    )
    expect_identical(manifest_archive$fn[[1]], "data.csv")

    # use file hashing to check
   projr_yml_dest_add_github(
      title = "latest",
      content = "raw-data",
      structure = "latest",
      send_cue = "always"
    )

   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "always",
      send_inspect = "file",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .dest_send_label_get_manifest_remote_hash(
      c("tag" = "archive", "fn" = "raw-data-v0.0.5.zip"),
      "github", "v0.0.2", "raw-data"
    )
    expect_identical(manifest_archive$fn[[1]], "data.csv")

    # use file hashing to check, no upload for archive
    .yml_dest_rm_type_all("default")
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "if-change",
      send_inspect = "file",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_false(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))

    # inspect nothing, so always add
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "if-change",
      send_inspect = "none",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    expect_true(.remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))

    # handle an empty directory
    .yml_dest_rm_type_all("default")
    unlink("_raw_data", recursive = TRUE)
   projr_yml_dest_add_github(
      title = "latest",
      content = "raw-data",
      structure = "latest",
      send_cue = "if-change",
      overwrite = TRUE
    )
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "if-change",
      overwrite = TRUE
    )
    debugonce(.dest_send_label)
    projr::projr_build_patch()
    expect_false(.remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))

  }
)

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = gh::gh_whoami()$login,
  pkg = basename(dir_test)
)

# empty directories
# ---------------------

# --------------------------
# empty directory from the start, latest
# --------------------------

dir_test <- .test_setup_project(
  git = TRUE, github = TRUE, set_env_var = TRUE
)
usethis::with_project(
  path = dir_test,
  code = {
   projr_init_git()
    .yml_git_set_push(FALSE, TRUE, NULL)
    # remove github remote
    .yml_dest_rm_type_all("default")
    # add a local destination, that is never sent to
   projr_yml_dest_add_github(
      title = "latest",
      content = "raw-data",
      structure = "latest"
    )
    projr::projr_build_patch()
    # expect that the empty equivalent exists,
    # and that it only contains projr-empty
    expect_true(.remote_check_exists("github", "latest"))
    remote_pre_latest <- c("tag" = "latest")
    expect_true(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data-empty", NULL
    ))
    path_dir_save <-.dir_get_tmp_random_path()
    dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    .remote_file_get_ind_github(
      remote_pre_latest, "raw-data-empty", path_dir_save
    )
    expect_true(file.exists(file.path(path_dir_save, "projr-empty")))
    unlink(path_dir_save, recursive = TRUE)

    # check that we still have the same thing
    projr::projr_build_patch()
    expect_true(.remote_check_exists("github", "latest"))
    remote_pre_latest <- c("tag" = "latest")
    expect_true(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data-empty", NULL
    ))
    path_dir_save <-.dir_get_tmp_random_path()
    dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    .remote_file_get_ind_github(
      remote_pre_latest, "raw-data-empty", path_dir_save
    )
    unlink(path_dir_save, recursive = TRUE)

    # add a file, then we need to remove the empty directory
    file.create(projr_path_get("raw-data", "data.csv"))
    projr::projr_build_patch()
    expect_false(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data-empty", NULL
    ))
    expect_true(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data", NULL
    ))
    path_dir_save <- .dir_get_tmp_random_path()
    dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    .remote_file_get_ind_github(
      remote_pre_latest, "raw-data", path_dir_save
    )

    # now go from something to nothing,
    # so need to add raw-data-empty
    file.remove(projr_path_get("raw-data", "data.csv"))
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data-empty", NULL
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_latest, "latest", "raw-data", NULL
    ))
  }
)

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = gh::gh_whoami()$login,
  pkg = basename(dir_test)
)

# --------------------------
# empty directory from the start, archive
# --------------------------

dir_test <- .test_setup_project(
  git = TRUE, github = TRUE, set_env_var = TRUE
)
usethis::with_project(
  path = dir_test,
  code = {
   projr_init_git()
    .yml_git_set_push(FALSE, TRUE, NULL)
    # remove github remote
    .yml_dest_rm_type_all("default")
    # add a local destination, that is never sent to
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive"
    )
    projr::projr_build_patch()
    # expect that the empty equivalent exists,
    # and that it only contains projr-empty
    expect_true(.remote_check_exists("github", "archive"))
    remote_pre_archive <- c("tag" = "archive")
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.1-empty"
    ))
    path_dir_save <- .dir_get_tmp_random_path()
    dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    .remote_file_get_ind_github(
      remote_pre_archive, "raw-data-v0.0.1-empty", path_dir_save
    )
    expect_true(file.exists(file.path(path_dir_save, "projr-empty")))
    unlink(path_dir_save, recursive = TRUE)

    # check that we do not add v0.0.2, as cue is if-change

    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.1-empty"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.2-empty"
    ))

    # add a file, then we need to remove the empty directory
    file.create(projr_path_get("raw-data", "data.csv"))

    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.1-empty"
    ))
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.3"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.3-empty"
    ))

    # now go from something to nothing,
    # so need to add raw-data-empty
    file.remove(projr_path_get("raw-data", "data.csv"))
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.4-empty"
    ))
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.3"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.4"
    ))

    # try with cue: always
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "always",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.5-empty"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.5"
    ))

    # try with inspect: file, no change
   projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_inspect = "file",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.6-empty"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.6"
    ))

    # try with inspect: file, add a file

    file.create("_raw_data/data.csv")
    projr::projr_build_patch()
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.7-empty"
    ))
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.7"
    ))

    # try with inspect: file, remove a file
    file.remove("_raw_data/data.csv")
    projr::projr_build_patch()
    expect_true(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.8-empty"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.8"
    ))

    # try with inspect: file, no change from no file
    projr::projr_build_patch()
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.9-empty"
    ))
    expect_false(.remote_final_check_exists_github(
      remote_pre_archive, "archive", "raw-data", "v0.0.9"
    ))
  }
)

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = gh::gh_whoami()$login,
  pkg = basename(dir_test)
)
