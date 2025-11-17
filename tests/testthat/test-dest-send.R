test_that("projr_yml_dest_add* functions work", {
  # skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive"
      )
      expect_identical(
        .yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )
      # add two
      projr_yml_dest_add_local(
        title = "archive second",
        content = "output",
        path = "_archive/second"
      )
      expect_identical(
        .yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          ),
          "archive second" = list(
            content = "output",
            path = "_archive/second"
          )
        )
      )
      # remove just one
      .yml_dest_rm_title("archive second", "local", "default")
      expect_identical(
        .yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )

      # ============
      # github
      # ============

      # test maniup
      .yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_github(
        title = "archive",
        content = "raw-data"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
      # add two
      projr_yml_dest_add_github(
        title = "archive second",
        content = "output"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          ),
          "archive-second" = list(
            content = "output"
          )
        )
      )
      # remove just one
      .yml_dest_rm_title("archive-second", "github", "default")
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
    }
  )
})

# --------------------------
# actually sending
# --------------------------

test_that("projr_dest_send works - local", {
  skip_if(.is_test_cran())
  # skip_if(.is_test_select()
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      file.create(projr_path_get("raw-data", "data.csv"))
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      projr::projr_build_dev()
      # remove github remote
      yml_projr <- projr_yml_get()
      github_ind <- which(names(yml_projr[["build"]]) == "github")
      if (length(github_ind) > 0) {
        yml_projr[["build"]] <- yml_projr[["build"]][
          -github_ind
        ]
        .yml_set(yml_projr)
      }
      projr::projr_build_patch()
      # add a local destination, that is never sent to
      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))


      # expect no upload
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))

      # now force upload
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        overwrite = TRUE
      )

      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.5/data.csv"))

      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest",
        send_cue = "always"
      )

      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        send_inspect = "file",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.6/data.csv"))

      # new upload, despite no change in project and cue: if-change
      # as remote was changed
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "file",
        overwrite = TRUE
      )
      file.create("_archive/raw-data/v0.0.6/extra.txt")
      projr::projr_build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.7/data.csv"))
      expect_true(!file.exists("_archive/raw-data/v0.0.7/extra.txt"))

      # inspect nothing, so always add
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "none",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.8/data.csv"))

      # handle an empty directory
      unlink("_raw_data", recursive = TRUE)
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(dir.exists("_archive/raw-data/v0.0.9"))
      expect_true(.is_len_0(list.files("_archive/raw-data/v0.0.9")))
    }
  )
})


# --------------------------
# empty directory from the start
# --------------------------

test_that("projr_dest_send works - local - empty dirs", {
  skip_if(.is_test_cran())
  # skip_if(.is_test_select()
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      # remove github remote
      .yml_dest_rm_type_all("default")
      # add a local destination, that is never sent to
      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr::projr_build_patch()
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

      .yml_dest_rm_type_all("default")
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
      # debugonce(.dest_send_label)
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
})

# --------------------------
# debug output test
# --------------------------

test_that("dest_send debug output works correctly", {
  skip_if(.is_test_cran())
  # skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      file.create(projr_path_get("raw-data", "data.csv"))
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      
      # Remove github remote
      .yml_dest_rm_type_all("default")
      
      # Add a local destination
      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      
      # Capture output with debug level
      old_output_level <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = NA)
      old_log_detailed <- Sys.getenv("PROJR_LOG_DETAILED", unset = NA)
      
      Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
      Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
      
      output <- capture.output({
        projr::projr_build_patch()
      }, type = "message")
      
      # Restore env vars
      if (is.na(old_output_level)) {
        Sys.unsetenv("PROJR_OUTPUT_LEVEL")
      } else {
        Sys.setenv(PROJR_OUTPUT_LEVEL = old_output_level)
      }
      if (is.na(old_log_detailed)) {
        Sys.unsetenv("PROJR_LOG_DETAILED")
      } else {
        Sys.setenv(PROJR_LOG_DETAILED = old_log_detailed)
      }
      
      # Join output into a single string for easier searching
      output_text <- paste(output, collapse = "\n")
      
      # Verify debug output contains expected messages
      expect_true(
        grepl("Starting destination send process", output_text, fixed = TRUE),
        info = "Should log start of destination send"
      )
      expect_true(
        grepl("remote type", output_text, ignore.case = TRUE),
        info = "Should log remote type being processed"
      )
      expect_true(
        grepl("Destination", output_text, fixed = TRUE),
        info = "Should log destination processing"
      )
      expect_true(
        grepl("Content", output_text, fixed = TRUE),
        info = "Should log content processing"
      )
      
      # Verify file was created
      expect_true(file.exists("_latest/raw-data/data.csv"))
    }
  )
})
