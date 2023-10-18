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
