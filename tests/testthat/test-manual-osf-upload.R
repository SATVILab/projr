test_that(".projr_osf_upload_node_add", {
  # skips
  skip_if_offline()
  skip_if(TRUE)

  # setup
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  gert::git_init(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      # create previous upload
      # ---------------------------

      yml_projr_orig <- projr_yml_get_unchecked()

      # create files
      file.create(
        projr_path_get("output", "abc", output_safe = FALSE)
      )
      file.create(
        projr_path_get("output", "subdir", "def.txt", output_safe = FALSE)
      )
      file.create(
        projr_path_get(
          "output", "subdir", "subdir2", "ghi.txt",
          output_safe = FALSE
        )
      )

      # create manifest
      manifest <- .projr_build_manifest_hash_post(TRUE)
      .projr_manifest_write(
        manifest,
        output_run = TRUE
      )

      # configure osf upload
      yml_projr <- yml_projr_orig
      yml_projr[["osf"]] <- (
        list(public = FALSE, category = "project")
      ) |>
        stats::setNames("Test")
      .projr_yml_set(yml_projr)

      # upload
      .projr_osf_upload(output_run = TRUE)
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
