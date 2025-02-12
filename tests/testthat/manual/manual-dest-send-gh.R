library(testthat)
devtools::load_all()
dir_test <- .projr_test_setup_project(
  git = TRUE, github = TRUE, set_env_var = TRUE
)
usethis::with_project(
  path = dir_test,
  code = {
    .projr_yml_dest_rm_type_all("default")
    file.create(projr::projr_path_get("raw-data", "data.csv"))
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
    expect_true(.projr_remote_check_exists("github", "latest"))
    expect_true(.projr_remote_check_exists("github", "archive"))
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.projr_remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .projr_dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .projr_dest_send_label_get_manifest_remote_hash(
      c("tag" = "archive", "fn" = "raw-data-v0.0.2.zip"),
      "github", "v0.0.2", "raw-data"
    )
    expect_identical(manifest_archive$fn[[1]], "data.csv")

    # expect no upload
    projr::projr_build_patch()
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_false(.projr_remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .projr_dest_send_label_get_manifest_remote_hash(
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
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.projr_remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .projr_dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .projr_dest_send_label_get_manifest_remote_hash(
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
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(.projr_remote_final_check_exists(
      "github", "archive", "raw-data", "archive", NULL, NULL, NULL
    ))
    manifest_latest <- .projr_dest_send_label_get_manifest_remote_hash(
      c("tag" = "latest", "fn" = "raw-data.zip"), "github", NULL, "raw-data"
    )
    expect_identical(manifest_latest$fn[[1]], "data.csv")
    manifest_archive <- .projr_dest_send_label_get_manifest_remote_hash(
      c("tag" = "archive", "fn" = "raw-data-v0.0.5.zip"),
      "github", "v0.0.2", "raw-data"
    )
    expect_identical(manifest_archive$fn[[1]], "data.csv")

    # use file hashing to check, no upload for archive
    .projr_yml_dest_rm_type_all("default")
    projr_yml_dest_add_github(
      title = "archive",
      content = "raw-data",
      structure = "archive",
      send_cue = "if-change",
      send_inspect = "file",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_false(.projr_remote_final_check_exists(
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
    expect_true(.projr_remote_final_check_exists(
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