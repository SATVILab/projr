test_that(".projr_osf_get_node works", {
  usethis::with_project(
    path = dir_test,
    code = {
      # get node id directly
      # --------------------
      expect_true(inherits(.projr_osf_get_node_id("yhv5k"), "osf_tbl_node"))

      # get node id from parent
      # -----------------------

      # no sub-component
      expect_null(.projr_osf_get_node_id_parent(title = "ABC", list(), "q26c9"))
      # sub-components but not one looking for
      expect_null(
        .projr_osf_get_node_id_parent("TestSubAlt", yml_param = list(), "yhv5k")
      )
      # find sub-component
      expect_true(inherits(
        .projr_osf_get_node_id_parent("TestSub", yml_param = list(), "yhv5k"),
        "osf_tbl_node"
      ))

      # create node because not found
      # -----------------------------

      # project
      yml_param <- list(
        category = "project", description = "desc", public = FALSE
      )
      osf_tbl <- try(.projr_osf_create_node(
        title = "abc", yml_param = yml_param, id_parent = NULL
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))

      # component
      yml_param <- list(
        category = "data", description = "desc", public = FALSE
      )
      osf_tbl <- try(.projr_osf_create_node(
        title = "abc", yml_param = yml_param, id_parent = "q26c9"
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))

      # test end to end
      osf_tbl <- try(.projr_osf_get_node(
        title = "Test", yml_param = yml_param, id_parent = "q26c9"
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".projr_osf_get_node works", {
  usethis::with_project(
    path = dir_test,
    code = {
      # get node id directly
      # --------------------
      expect_true(inherits(.projr_osf_get_node_id("yhv5k"), "osf_tbl_node"))

      # get node id from parent
      # -----------------------

      # no sub-component
      expect_null(.projr_osf_get_node_id_parent(title = "ABC", list(), "q26c9"))
      # sub-components but not one looking for
      expect_null(
        .projr_osf_get_node_id_parent("TestSubAlt", yml_param = list(), "yhv5k")
      )
      # find sub-component
      expect_true(inherits(
        .projr_osf_get_node_id_parent("TestSub", yml_param = list(), "yhv5k"),
        "osf_tbl_node"
      ))

      # create node because not found
      # -----------------------------

      # project
      yml_param <- list(
        category = "project", description = "desc", public = FALSE
      )
      osf_tbl <- try(.projr_osf_create_node(
        title = "abc", yml_param = yml_param, id_parent = NULL
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))

      # component
      yml_param <- list(
        category = "data", description = "desc", public = FALSE
      )
      osf_tbl <- try(.projr_osf_create_node(
        title = "abc", yml_param = yml_param, id_parent = "q26c9"
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))

      # test end to end
      osf_tbl <- try(.projr_osf_get_node(
        title = "Test", yml_param = yml_param, id_parent = "q26c9"
      ))
      expect_true(inherits(osf_tbl, "osf_tbl_node"))
      try(osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE))
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that(".projr_osf_send_node_label works", {
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
  if (!requireNamespace("gert", quietly = TRUE)) {
    utils::install.packages("gert")
  }
  gert::git_init(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      osf_tbl <- .projr_osf_get_node_id("q26c9")
      file.create(projr_path_get("output", "test.txt", output_safe = FALSE))
      osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)

      .projr_osf_send_node_label(
        osf_tbl = osf_tbl, label = "output"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
