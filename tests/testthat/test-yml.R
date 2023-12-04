test_that("getting and setting metadata files works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))
  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- fn_vec

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
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_int <- .projr_yml_get_root_full()
      expect_identical(class(.projr_yml_get_root_full()), "list")
      expect_identical(class(.projr_yml_bd_get()), "list")
      expect_identical(class(.projr_desc_get()), c("matrix", "array"))
      yml_projr_min <- list(
        "directories" = NULL, "build" = list()
      )
      .projr_yml_set(yml_projr_min)
      expect_identical(.projr_yml_get_root_full(), yml_projr_min)
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_error(.projr_yml_get_root_full())
      expect_identical(.projr_yml_bd_get(), list())
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_yml_check works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- fn_vec

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
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_correct <- projr_yml_get_unchecked()
      expect_true(projr_yml_check())

      # contains build and data-raw
      # ---------------------
      expect_error(projr_yml_check(yml_projr_correct["build"]))
      expect_error(projr_yml_check(yml_projr_correct["directories"]))

      # directories section
      # ---------------------

      # naming is correct
      yml_projr_error <- yml_projr_correct
      yml_projr_error[["directories"]] <- list(
        yml_projr_correct[["directories"]][[1]]
      )
      expect_error(projr_yml_check(yml_projr_error))
      yml_projr_error[["directories"]] <- list(
        "data-raw" = yml_projr_correct[["directories"]][[1]],
        "data-raw" = yml_projr_correct[["directories"]][[1]]
      )
      expect_error(projr_yml_check(yml_projr_error))

      # required elements
      yml_projr_error[["directories"]] <- list(
        "cache" = yml_projr_correct[["directories"]][[1]],
        "data-raw" = yml_projr_correct[["directories"]][[1]],
        "output" = yml_projr_correct[["directories"]][[2]]
      )
      expect_error(projr_yml_check(yml_projr_error))
      # required elements
      yml_projr_error[["directories"]] <- list(
        "cache" = yml_projr_correct[["directories"]][[1]],
        "data-raw" = yml_projr_correct[["directories"]][[1]],
        "archive" = yml_projr_correct[["directories"]][[2]]
      )
      expect_error(projr_yml_check(yml_projr_error))
      # required elements
      yml_projr_error[["directories"]] <- list(
        "cache" = yml_projr_correct[["directories"]][[1]],
        "output" = yml_projr_correct[["directories"]][[1]],
        "archive" = yml_projr_correct[["directories"]][[2]]
      )
      expect_error(projr_yml_check(yml_projr_error))
      # required elements
      yml_projr_error[["directories"]] <- list(
        "cache" = yml_projr_correct[["directories"]][[1]],
        "output" = yml_projr_correct[["directories"]][[1]],
        "archive" = yml_projr_correct[["directories"]][[2]]
      )
      expect_error(projr_yml_check(yml_projr_error))

      # directories section
      # ---------------------

      # naming is correct
      yml_projr_error <- yml_projr_correct
      yml_projr_build_correct <- yml_projr_correct[["build"]]
      yml_projr_error[["build"]] <- yml_projr_build_correct[
        c("git", "github")
      ]
      expect_error(projr_yml_check(yml_projr_error))
      yml_projr_error[["build"]] <- yml_projr_correct[["build"]]
      yml_projr_error[["build"]][["dev-output"]] <- numeric(1)
      expect_error(projr_yml_check(yml_projr_error))
      yml_projr_error[["build"]] <- yml_projr_build_correct[
        "dev-output"
      ] |>
        append(list("a" = 1))
      expect_error(projr_yml_check(yml_projr_error))

      # basic checks
      yml <- list("directories" = list("data-raw" = list("path")))
      path_yml <- .projr_dir_proj_get("_projr.yml")
      yml_projr_init <- yaml::read_yaml(path_yml)
      unlink(path_yml)
      expect_error(projr_yml_check())
      .projr_yml_set(yml_projr_init)
      expect_identical(class(.projr_yml_get_root_full()), "list")
      expect_identical(class(.projr_yml_bd_get()), "list")
      expect_identical(class(.projr_desc_get()), c("matrix", "array"))
      .projr_yml_set(list("directories" = NULL))
      expect_identical(.projr_yml_get_root_full(), list("directories" = NULL))
      .projr_yml_bd_set(list())
      expect_identical(.projr_yml_bd_get(), list())
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_error(.projr_yml_get_root_full())
      expect_identical(.projr_yml_bd_get(), list())
      yml_projr_local <- list("directories" = "bleh", "acd" = "WRONG")
      yaml::write_yaml(yml_projr_local, file.path(dir_test, "_projr-local.yml"))
      expect_identical(.projr_yml_get_local(), list("directories" = "bleh"))
    },
    quiet = TRUE,
    force = TRUE
  )
})



test_that(".projr_yml_check_dir_elem works", {
  # each element is correctly specified
  # --------------------------
  key_vec <- c("data-raw", "cache", "output", "archive")
  # invalid element
  yml_projr_dir_elem <- list(
    "smg" = "b",
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      key_vec[1],
      key_vec
    )
  )
  # doubled-up element
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      key_vec[1],
      key_vec
    )
  )
  # path: missing
  yml_projr_dir_elem <- list(
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      key_vec[1],
      key_vec
    )
  )
  key_vec <- c("data-raw", "cache", "output", "archive")
  # path: to a restricted folder
  yml_projr_dir_elem <- list(
    "path" = "R",
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      key_vec[1],
      key_vec
    )
  )
  # path: to a restricted sub-folder
  yml_projr_dir_elem <- list(
    "path" = "data/abc",
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # path: length greater than 1
  yml_projr_dir_elem <- list(
    "path" = c("abc", "def"),
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # path: not character
  yml_projr_dir_elem <- list(
    "path" = 1,
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # ignore: not logical or character
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = 1,
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # ignore: not correct character type
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = "def",
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # ignore: length greater than one
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = c(TRUE, FALSE),
    "output" = TRUE,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # output: either logical or character
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = 1,
    "archive" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # archive: either logical or character
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = TRUE,
    "archive" = 1
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  # data-raw and cache: output and archive not found
  # output
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = "output-share",
    "archive" = "archive"
  )
  key_vec <- c("data-raw", "cache", "outputting", "archive")
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "daTa_----RAW",
      key_vec
    )
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "CACHE",
      key_vec
    )
  )
  yml_projr_dir_elem[["output"]] <- "outputting"
  expect_true(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "daTa_----RAW",
      key_vec
    )
  )
  expect_true(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "CACHE",
      key_vec
    )
  )
  # archive
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = "outputting",
    "archive" = "archivist"
  )
  key_vec <- c("data-raw", "cache", "outputting", "archival")
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "daTa_----RAW",
      key_vec
    )
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "CACHE",
      key_vec
    )
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "outputting",
      key_vec
    )
  )
  yml_projr_dir_elem[["archive"]] <- "archival"
  expect_true(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "daTa_----RAW",
      key_vec
    )
  )
  expect_true(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "CACHE",
      key_vec
    )
  )
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "archive" = "archival"
  )
  expect_true(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "outputting",
      key_vec
    )
  )
  # outputting from output and archive
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = "output"
  )
  key_vec <- c("data-raw", "cache", "output", "archive")
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "output",
      key_vec
    )
  )
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "archive",
      key_vec
    )
  )
  # archiving
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = "output",
    "archive" = "archive"
  )
  key_vec <- c("data-raw", "cache", "output", "archive")
  expect_error(
    .projr_yml_check_dir_elem(
      yml_projr_dir_elem,
      "archive",
      key_vec
    )
  )
  # manifest to data-raw or cache
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "output" = "output",
    "archive" = "archive",
    "manifest" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "data-raw",
    keys = key_vec
  ))
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "cache",
    keys = key_vec
  ))
  # hash to archive
  yml_projr_dir_elem <- list(
    "path" = "abc",
    "ignore-git" = TRUE,
    "hash" = TRUE
  )
  expect_error(.projr_yml_check_dir_elem(
    yml_projr_dir_elem,
    key = "archive",
    keys = key_vec
  ))
})

test_that("Check that .projr_yml_check_build_git works", {
  yml_git_correct <- list(
    "commit" = TRUE,
    "add-untracked" = TRUE,
    "push" = TRUE
  )
  expect_false(is.null(.projr_yml_check_build_git(NULL)))
  expect_error(.projr_yml_check_build_git(list("a" = 1)))
  yml_git_error <- yml_git_correct[-1]
  expect_error(.projr_yml_check_build_git(yml_git_error))
  expect_error(.projr_yml_check_build_git(list("commit" = "a")))
})

test_that("Check that .projr_yml_check_build_gh_release works", {
  yml_git_correct <- list(
    "commit" = TRUE,
    "add-untracked" = TRUE,
    "push" = TRUE
  )
  expect_false(is.null(.projr_yml_check_build_gh_release(NULL)))
  expect_error(.projr_yml_check_gh_release_ind(tag, elem = list()))
  expect_error(.projr_yml_check_gh_release_ind(tag = "abc"))
  expect_error(.projr_yml_check_gh_release_ind(
    tag = "abc", elem = list("content" = 1)
  ))
  expect_error(.projr_yml_check_gh_release_ind(
    tag = "abc", elem = list("body" = 1)
  ))
  expect_error(.projr_yml_check_gh_release_ind(
    tag = "abc", elem = list("content" = c("schmoogle"), "body" = "abc"),
    directories = "anti-schmoogle"
  ))
})

test_that(".projr_yml_merge works", {
  # check simple overrides
  # ---------------------

  # local used
  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list("a" = 2)
  yml_projr_local <- list("a" = 3)
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = 3)
  )

  # profile used
  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list("a" = 2)
  yml_projr_local <- list()
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = 2)
  )

  # default used
  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list()
  yml_projr_local <- list()
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = 1)
  )

  # check that we use more detailed list
  # when it takes precedence
  # ----------------------

  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list("a" = 2)
  yml_projr_local <- list("a" = list("b" = 3))
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = list("b" = 3))
  )
  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list("a" = list("b" = 2))
  yml_projr_local <- list()
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = list("b" = 2))
  )
  yml_projr_root_default <- list("a" = 1)
  yml_projr_profile <- list("a" = list("b" = 2))
  yml_projr_local <- list("a" = 3)
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = 3)
  )


  # merging one level down
  # ----------------------

  # local used
  yml_projr_root_default <- list("b" = list("a" = 1))
  yml_projr_profile <- list("b" = list("a" = 2))
  yml_projr_local <- list("b" = list("a" = 3))
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("b" = list("a" = 3))
  )
  # profile used
  yml_projr_root_default <- list("b" = list("a" = 1))
  yml_projr_profile <- list("b" = list("a" = 2))
  yml_projr_local <- list("b" = list())
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("b" = list("a" = 2))
  )
  # default used
  yml_projr_root_default <- list("b" = list("a" = 1))
  yml_projr_profile <- list("b" = list())
  yml_projr_local <- list("b" = list())
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("b" = list("a" = 1))
  )

  # multiple merges, merging one level down
  # ----------------------

  # local used once, profile used once
  yml_projr_root_default <- list(
    "b" = list("a" = 1),
    "c" = list("d" = list("e" = 1))
  )
  yml_projr_profile <- list(
    "b" = list("a" = 2),
    "c" = list("d" = list("e" = 2))
  )
  yml_projr_local <- list(
    "c" = list("d" = list("e" = 3))
  )
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list(
      "b" = list("a" = 2),
      "c" = list("d" = list("e" = 3))
    )
  )
  # check that we can add onto lists
  # -------------------------------
  yml_projr_root_default <- list("a" = list("b" = 1), "f" = 1)
  yml_projr_profile <- list("a" = list("b" = 2), "d" = 2)
  yml_projr_local <- list("a" = list("c" = 3), "e" = 3)
  expect_identical(
    .projr_yml_merge(
      yml_projr_root_default,
      yml_projr_profile,
      yml_projr_local
    ),
    list("a" = list("b" = 2, "c" = 3), "f" = 1, "d" = 2, "e" = 3)
  )
})
