# bookdown
# ------------------------
test_that("projr_build_dev works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_build_dev()
      yml_bd <- .yml_bd_get()
      # it's set in _bookdown.yml
      # and not in _projr.yml, so use _bookdown.yml
      # as basename
      expect_identical(basename(yml_bd$output_dir), "docs")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
      # debugonce(.build_output_get_msg)
      # debugonce(.git_msg_get)
      projr_build("patch", msg = "test")
      .version_get()
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "docs")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build("minor", msg = "test")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# rmarkdown
test_that("projr_build_ works with rmarkdown", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("rmarkdown")
      # dev build
      projr_build_dev()
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs"))
      expect_true(file.exists("_tmp/projr/v0.0.0-1/docs/index.html"))
      expect_false(file.exists("index.html"))
      # output build
      if (dir.exists("docs")) {
        unlink("docs", recursive = TRUE)
      }
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# quarto projects
test_that("projr_build_ works with quarto projects", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("quarto_project")
      # dev build
      # browser()
      projr_build_dev()
      expect_true(
        dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(
        length(.file_ls("_tmp/projr/v0.0.0-1/docs")) > 5
      )

      # output build
      # debugonce(.build_copy_docs_quarto_project)
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(length(.file_ls("docs")) > 5)
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# quarto 
test_that("projr_build_ works with quarto", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("quarto")
      # dev build
      projr_build_dev()
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs"))
      expect_true(file.exists("_tmp/projr/v0.0.0-1/docs/index.html"))
      expect_false(file.exists("index.html"))
      # output build
      if (dir.exists("docs")) {
        unlink("docs", recursive = TRUE)
      }
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})