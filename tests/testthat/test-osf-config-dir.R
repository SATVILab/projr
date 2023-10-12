test_that("projr_osf_source_add works", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_orig <- projr_yml_get_unchecked()
      label <- .projr_osf_label_get_random("DataRawProjrOSFTest")

      # create a new project as a source
      id_proj <- projr_osf_source_add(label = label, category = "project")
      # extract id, defer the node's deletion and test id
      .projr_osf_rm_node_id_defer(id_proj)
      expect_true(is.character(id_proj))
      expect_true(nchar(id_proj) == 5L)

      # check that duplication throws an error
      expect_error(projr_osf_source_add(label = label, category))

      # check ID added
      expect_error(projr_osf_source_add(label = "data-raw"))
      expect_error(
        projr_osf_source_add(label = "data-raw", category = "analysis")
      )
      id_comp <- projr_osf_source_add(
        label = "data-raw", category = "data", parent_id = id_proj
      )
      expect_true(is.character(id_comp))
      expect_true(nchar(id_comp) == 5L)

      # add with more things
      id_comp_overwrite <- projr_osf_source_add(
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
