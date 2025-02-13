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
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))

    browser()
    browser()

    # handle an empty directory
    .projr_yml_dest_rm_type_all("default")
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
    debugonce(.projr_dest_send_label)
    projr::projr_build_patch()
    expect_false(.projr_remote_final_check_exists(
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
# empty directory from the start
# --------------------------

dir_test <- .projr_test_setup_project(
  git = TRUE, github = TRUE, set_env_var = TRUE
)
usethis::with_project(
  path = dir_test,
  code = {
    browser()
    projr_init_git()
    .projr_yml_git_set_push(FALSE, TRUE, NULL)
    # remove github remote
    .projr_yml_dest_rm_type_all("default")
    # add a local destination, that is never sent to
    projr_yml_dest_add_github(
      title = "latest",
      content = "raw-data",
      structure = "latest"
    )
    debugonce(.projr_dest_send_label)
    projr::projr_build_patch()
    expect_true(.projr_remote_check_exists("github", "latest"))
    expect_true(.projr_remote_final_check_exists(
      "github", "latest", "raw-data", "latest", NULL, NULL, NULL
    ))
    expect_true(dir.exists("_latest/raw-data"))
    expect_false(file.exists("_latest/raw-data/data.csv"))
    # add a file
    file.create("_raw_data/data.csv")
    projr::projr_build_patch()
    expect_true(file.exists("_latest/raw-data/data.csv"))
    # remove that one file
    file.remove("_raw_data/data.csv")
    projr::projr_build_patch()
    expect_true(dir.exists("_latest/raw-data"))
    expect_false(file.exists("_latest/raw-data/data.csv"))

    # -------------------
    # structure: archive
    # -------------------

    .projr_yml_dest_rm_type_all("default")
    projr_yml_dest_add_local(
      title = "archive",
      content = "raw-data",
      path = "_archive",
      structure = "archive",
      overwrite = TRUE
    )
    projr::projr_build_patch()
    expect_true(dir.exists("_archive/raw-data/v0.0.4"))
    expect_false(file.exists("_archive/raw-data/v0.0.4/data.csv"))
    # add a file
    # browser()
    # debugonce(.projr_dest_send_label)
    # browser()
    file.create("_raw_data/data.csv")
    projr::projr_build_patch() # problem isn't here,
    # it's that manifest.csv for v0.0.4 
    # says that data.csv is there when it isn't
    expect_true(file.exists("_archive/raw-data/v0.0.5/data.csv"))
    # remove that one file
    file.remove("_raw_data/data.csv")
    projr::projr_build_patch()
    expect_true(dir.exists("_archive/raw-data/v0.0.6"))
    expect_false(file.exists("_archive/raw-data/v0.0.6/data.csv"))
    # keep it removed
    projr::projr_build_patch()
    expect_false(dir.exists("_archive/raw-data/v0.0.7"))
  }
)

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = gh::gh_whoami()$login,
  pkg = basename(dir_test)
)
