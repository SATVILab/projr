test_that("projr_source_add_osf works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_orig <- projr_yml_get_unchecked()
      label <- .projr_osf_label_get_random("DataRawProjrOSFTest")

      # create a new project as a source
      id_proj <- projr_source_add_osf(label = label, category = "project")
      # extract id, defer the node's deletion and test id
      .projr_osf_rm_node_id_defer(id_proj)
      expect_true(is.character(id_proj))
      expect_true(nchar(id_proj) == 5L)

      # check that duplication throws an error
      expect_error(projr_source_add_osf(label = label, category))

      # check ID added
      expect_error(projr_source_add_osf(label = "data-raw"))
      expect_error(
        projr_source_add_osf(label = "data-raw", category = "analysis")
      )
      id_comp <- projr_source_add_osf(
        label = "data-raw", category = "data", parent_id = id_proj
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)

      # add with more things
      id_comp_overwrite <- projr_source_add_osf(
        label = "data-raw",
        overwrite = TRUE,
        id = NULL,
        title = "Titular",
        body = "Lavish",
        public = FALSE,
        category = "communication",
        parent_id = id_proj,
        path = "sub-dir",
        path_append_label = FALSE,
        remote_structure = "latest",
        download_cue = "build",
        download_sync_approach = "download-all",
        download_conflict = "overwrite",
        upload_cue = "major",
        upload_sync_approach = "upload-all",
        upload_version_source = "manifest",
        upload_conflict = "overwrite"
      )
      yml_proj <- projr_yml_get_unchecked()
      yml_projr_dr_osf <- yml_proj[["directories"]][["data-raw"]][["osf"]]
      expect_true(names(yml_projr_dr_osf) == "Titular")
      expect_true(
        all(c("id", "path", "path-append-label", "download", "upload") %in%
          names(yml_projr_dr_osf[[1]]))
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_dest_add_osf works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_orig <- projr_yml_get_unchecked()
      title <- .projr_osf_label_get_random("ProjectProjrOSFTest")

      # setup
      # ----------------------

      # create files
      .projr_test_setup_content("data-raw")

      # test adding a project
      # ------------------------

      # create a new project as a destination
      id_proj <- projr_dest_add_osf(
        title = title, category = "project"
      )

      # extract id, defer the node's deletion and test id
      .projr_osf_rm_node_id_defer(id_proj)
      expect_true(is.character(id_proj))
      expect_true(nchar(id_proj) == 5L)

      yml_projr <- projr_yml_get_unchecked()

      # test basic, top-level add
      expect_true("osf" %in% names(yml_projr[["build"]]))
      expect_true(title %in% names(yml_projr[["build"]][["osf"]]))

      # test overwriting project (same title)
      # ------------------------

      # overwrite not allowed
      expect_error(
        projr_dest_add_osf(
          title = title, category = "project",
          overwrite = FALSE
        )
      )
      # overwriting allowed
      id_proj <- expect_no_error(
        projr_dest_add_osf(
          title = title, overwrite = TRUE, category = "project"
        )
      )
      yml_projr <- projr_yml_get_unchecked()
      expect_true(title %in% names(yml_projr[["build"]][["osf"]]))
      expect_identical(
        id_proj,
        yml_projr[["build"]][["osf"]][[title]][["id"]]
      )

      # test adding a component to a project
      # ----------------------------


      # error when parent_id not specified
      expect_error(
        projr_dest_add_osf(content = "data-raw", category = "data")
      )

      # parent_id specified
      id_comp <- projr_dest_add_osf(
        title = "Test", content = "data-raw",
        category = "data", parent_id = id_proj
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        id_comp,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test"]][["id"]]
      )

      # parent_title specified
      id_comp <- projr_dest_add_osf(
        title = "Test2", content = "data-raw",
        category = "data", parent_title = title
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        id_comp,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["id"]]
      )

      # test adding a component to a project
      # ----------------------------

      # parent_id specified
      id_comp_sub <- projr_dest_add_osf(
        title = "TestSub", content = "data-raw",
        category = "data", parent_id = id_comp
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        id_comp_sub,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSub"]][["id"]] # nolint
      )

      # parent_title specified
      id_comp_sub <- projr_dest_add_osf(
        title = "TestSubTitle", content = "data-raw",
        category = "data", parent_title = "Test2"
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        id_comp_sub,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSubTitle"]][["id"]] # nolint
      )
      expect_identical(
        length(yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]]), # nolint
        2L
      )

      # test adding a component to a sub-component
      # ----------------------------

      # parent_id specified
      # debugonce(projr_dest_add_osf)
      # debugonce(.projr_osf_yml_get_parent_vec)
      # debugonce(.projr_osf_yml_find_parent)
      # debugonce(.projr_osf_yml_find_parent_rec)
      id_comp_sub_sub <- projr_dest_add_osf(
        title = "TestSubSubId", content = "data-raw",
        category = "data", parent_id = id_comp_sub
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSubTitle"]][["component"]][["TestSubSubId"]][["id"]], # nolint
        id_comp_sub_sub
      )

      id_comp_sub_sub <- projr_dest_add_osf(
        title = "TestSubSubTitle", content = "data-raw",
        category = "data", parent_title = "TestSubTitle"
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- projr_yml_get_unchecked()
      expect_identical(
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSubTitle"]][["component"]][["TestSubSubTitle"]][["id"]], # nolint
        id_comp_sub_sub
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
