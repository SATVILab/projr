test_that("projr_osf_dest_add works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))
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
      yml_projr_orig <- projr_yml_get_unchecked()
      # test basic, top-level add
      projr_osf_dest_add(title = "Test", create = FALSE)
      yml_projr <- projr_yml_get_unchecked()
      expect_true("osf" %in% names(yml_projr[["build"]]))
      expect_true("Test" %in% names(yml_projr[["build"]][["osf"]]))
      # test overwrite
      expect_error(projr_osf_dest_add(title = "Test", create = FALSE))
      expect_no_error(
        projr_osf_dest_add(title = "Test", create = FALSE, overwrite = TRUE)
      )
      # test add of an addition at top-level
      projr_osf_dest_add(
        title = "Test2",
        create = FALSE
      )
      yml_projr <- projr_yml_get_unchecked()
      expect_true(
        identical(c("Test", "Test2"), names(yml_projr[["build"]][["osf"]]))
      )
      expect_error(projr_osf_dest_add(title = "Test2", create = FALSE))

      # test adding a project with a parent
      expect_error(projr_osf_dest_add(
        title = "Test3", create = FALSE,
        category = "project", parent_id = "123"
      ))

      # now add a sub-category
      projr_osf_dest_add(
        title = "Test3", create = FALSE, parent_title = "Test"
      )
      yml_projr_osf <- projr_yml_get_unchecked()[["build"]][["osf"]]
      expect_true("Test3" %in%
        names(yml_projr_osf[["Test"]][["component"]]))
      projr_osf_dest_add(
        title = "Test4", create = FALSE, parent_title = "Test3"
      )
      yml_projr_osf <- projr_yml_get_unchecked()[["build"]][["osf"]]
      expect_true("Test4" %in%
        names(yml_projr_osf[["Test"]][["component"]][["Test3"]][["component"]]))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".projr_osf_get_node works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      # get node id directly
      # --------------------
      expect_null(.projr_osf_get_node_id(NULL))
      expect_error(.projr_osf_get_node_id("aawkjlera"))
      # get node id from parent
      # -----------------------
      yml_param <- list(parent_id = NULL)
      expect_null(.projr_osf_get_node_id_parent(
        "TestSub",
        yml_param = yml_param, NULL
      ))
      # create node because not found
      # -----------------------------
      expect_error(.projr_osf_create_node(
        "abc",
        yml_param = list(category = "comp"), parent_id = NULL
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})
