test_that("projr_source_add_osf works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml.orig <- .yml_get()
      label <- .osf_label_get_random("DataRawProjrOSFTest")

      # create a new project as a source
      id_proj <-.source_add_osf(label = label, category = "project")
      # extract id, defer the node's deletion and test id
      .osf_rm_node_id_defer(id_proj)
      expect_true(is.character(id_proj))
      expect_true(nchar(id_proj) == 5L)

      # check that duplication throws an error
      expect_error.source_add_osf(label = label, category))

      # check ID added
      expect_error.source_add_osf(label = "data-raw"))
      expect_error(
       .source_add_osf(label = "data-raw", category = "analysis")
      )
      id_comp <-.source_add_osf(
        label = "data-raw", category = "data", id_parent = id_proj
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)

      # add with more things
      id_comp_overwrite <-.source_add_osf(
        label = "data-raw",
        overwrite = TRUE,
        id = NULL,
        title = "Titular",
        body = "Lavish",
        public = FALSE,
        category = "communication",
        id_parent = id_proj,
        path = "sub-dir",
        path_append_label = FALSE,
        structure = "latest",
        download_cue = "never",
        download_strategy = "download-all",
        download_conflict = "overwrite",
        upload_cue = "if-change",
        upload_strategy = "upload-all",
        upload_inspect = "manifest",
        upload_conflict = "overwrite"
      )
      yml_proj <- .yml_get()
      yml.dr_osf <- yml_proj[["directories"]][["data-raw"]][["osf"]]
      expect_true(names(yml.dr_osf) == "Titular")
      expect_true(
        all(c("id", "path", "path-append-label", "download", "upload") %in%
          names(yml.dr_osf[[1]]))
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_dest_add_osf works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml.orig <- .yml_get()
      title <- .osf_label_get_random("ProjectProjrOSFTest")

      # setup
      # ----------------------

      # create files
      .test_setup_content("data-raw")

      # test adding a project
      # ------------------------

      # create a new project as a destination
      id_proj <-.dest_add_osf(
        title = title, category = "project"
      )

      # extract id, defer the node's deletion and test id
      .osf_rm_node_id_defer(id_proj)
      expect_true(is.character(id_proj))
      expect_true(nchar(id_proj) == 5L)

      yml_projr <- .yml_get()

      # test basic, top-level add
      expect_true("osf" %in% names(yml_projr[["build"]]))
      expect_true(title %in% names(yml_projr[["build"]][["osf"]]))

      # test overwriting project (same title)
      # ------------------------

      # overwrite not allowed
      expect_error(
       .dest_add_osf(
          title = title, category = "project",
          overwrite = FALSE
        )
      )
      # overwriting allowed
      id_proj <- expect_no_error(
       .dest_add_osf(
          title = title, overwrite = TRUE, category = "project"
        )
      )
      yml_projr <- .yml_get()
      expect_true(title %in% names(yml_projr[["build"]][["osf"]]))
      expect_identical(
        id_proj,
        yml_projr[["build"]][["osf"]][[title]][["id"]]
      )

      # test adding a component to a project
      # ----------------------------


      # error when id_parent not specified
      expect_error(
       .dest_add_osf(content = "data-raw", category = "data")
      )

      # id_parent specified
      id_comp <-.dest_add_osf(
        title = "Test", content = "data-raw",
        category = "data", id_parent = id_proj
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
      expect_identical(
        id_comp,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test"]][["id"]]
      )

      # parent_title specified
      id_comp <-.dest_add_osf(
        title = "Test2", content = "data-raw",
        category = "data", parent_title = title
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
      expect_identical(
        id_comp,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["id"]]
      )

      # test adding a component to a project
      # ----------------------------

      # id_parent specified
      id_comp_sub <-.dest_add_osf(
        title = "TestSub", content = "data-raw",
        category = "data", id_parent = id_comp
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
      expect_identical(
        id_comp_sub,
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSub"]][["id"]] # nolint
      )

      # parent_title specified
      id_comp_sub <-.dest_add_osf(
        title = "TestSubTitle", content = "data-raw",
        category = "data", parent_title = "Test2"
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
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

      # id_parent specified
      # debugonce.dest_add_osf)
      # debugonce(.osf_yml_get_parent_vec)
      # debugonce(.osf_yml_find_parent)
      # debugonce(.osf_yml_find_parent_rec)
      id_comp_sub_sub <-.dest_add_osf(
        title = "TestSubSubId", content = "data-raw",
        category = "data", id_parent = id_comp_sub
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
      expect_identical(
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSubTitle"]][["component"]][["TestSubSubId"]][["id"]], # nolint
        id_comp_sub_sub
      )

      id_comp_sub_sub <-.dest_add_osf(
        title = "TestSubSubTitle", content = "data-raw",
        category = "data", parent_title = "TestSubTitle"
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)
      yml_projr <- .yml_get()
      expect_identical(
        yml_projr[["build"]][["osf"]][[title]][["component"]][["Test2"]][["component"]][["TestSubTitle"]][["component"]][["TestSubSubTitle"]][["id"]], # nolint
        id_comp_sub_sub
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
