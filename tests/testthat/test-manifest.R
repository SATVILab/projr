test_that("projr_manifest_hash_dir works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

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
      yml_projr_init <- .projr_yml_get_root_full()
      # test getting hashes
      dir.create("_data_raw/sub", recursive = TRUE)
      invisible(file.create("_data_raw/abc.csv"))
      invisible(file.create("_data_raw/sub/def.csv"))
      dir.create("_tmp")
      expect_identical(
        nrow(.projr_manifest_hash_label("cache", output_run = TRUE)), 0L
      )
      expect_identical(
        ncol(.projr_manifest_hash_label("cache", output_run = TRUE)), 4L
      )
      invisible(file.create("_data_raw/sub/def.csv"))

      # test getting manifest
      # ----------------------------

      # pre:
      dir.create("_tmp/abc", recursive = TRUE)
      file.create("_tmp/test.txt")
      file.create("_tmp/abc/test.txt")

      manifest <- .projr_build_manifest_hash_pre(TRUE)
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_pre(TRUE)
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- TRUE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_pre(TRUE)
      expect_identical(nrow(manifest), 2L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))

      # post:
      if (dir.exists("_data_raw")) {
        unlink("_data_raw", recursive = TRUE)
      }
      if (dir.exists("_tmp")) {
        unlink("_tmp", recursive = TRUE)
      }
      dir.create("_tmp/projr/v0.0.0-1/output/abc", recursive = TRUE)
      invisible(file.create("_tmp/projr/v0.0.0-1/output/test.txt"))
      invisible(file.create("_tmp/projr/v0.0.0-1/output/abc/test.txt"))
      dir.create("_data_raw/abc", recursive = TRUE)
      invisible(file.create("_data_raw/test.txt"))
      invisible(file.create("_data_raw/abc/test.txt"))
      # to fix:
      # - why did data-raw sub-directory not appear?
      # - what is going on with the output directory? It seems
      #   like the test is assuming the files will be in the cache directory,
      #   but they should actually be in the unsafe directory.
      #   well, I guess it doesn't matter.
      # seems like both are now fixed

      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 4L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["data-raw"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 2L)
      yml_projr[["directories"]][["output"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post(FALSE)
      expect_identical(nrow(manifest), 0L)
      expect_identical(colnames(manifest), c("label", "fn", "version", "hash"))

      # test saving manifest
      # -------------------------
      manifest_tbl <- data.frame(x = 1)
      .projr_manifest_write(manifest_tbl, output_run = TRUE)
      expect_true(file.exists("manifest.csv"))
      .projr_manifest_write(manifest_tbl, output_run = FALSE)
      expect_true(
        file.exists(projr_dir_get("output", "manifest.csv", output_run = FALSE))
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_manifest_compare works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

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
      # clean up
      if (dir.exists(projr_dir_get("data-raw"))) {
        unlink(projr_dir_get("data-raw"), recursive = TRUE)
      }
      # create files
      path_output_kept_unchanged <- projr_path_get(
        "output", "kept_unchanged.txt",
        output_safe = FALSE
      )
      path_output_kept_changed <- projr_path_get(
        "output", "kept_changed.txt",
        output_safe = FALSE
      )
      path_output_removed <- projr_path_get(
        "output", "removed.txt",
        output_safe = FALSE
      )
      invisible(file.create(path_output_kept_unchanged))
      invisible(file.create(path_output_kept_changed))
      invisible(file.create(path_output_removed))
      # get manifest beforehand
      manifest_tbl_pre <- .projr_build_manifest_hash_post(output_run = TRUE)
      # add a file, and change a file
      path_output_add <- projr_path_get(
        "output", "added.txt",
        output_safe = FALSE
      )
      invisible(file.create(path_output_add))
      cat("add", file = path_output_kept_changed, append = TRUE)
      unlink(path_output_removed)
      # get manifest afterwards
      manifest_tbl_post <- .projr_build_manifest_hash_post(output_run = TRUE)
      .projr_manifest_compare(manifest_tbl_pre, manifest_tbl_post)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre, manifest_tbl_post
      )
      expect_identical(
        "kept_unchanged.txt", manifest_compare_list$kept_unchanged$fn
      )
      expect_identical(
        "kept_changed.txt", manifest_compare_list$kept_changed$fn
      )
      expect_identical("removed.txt", manifest_compare_list$removed$fn)
      expect_identical("added.txt", manifest_compare_list$added$fn)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre[rep(FALSE, nrow(manifest_tbl_pre))], manifest_tbl_post
      )

      zero_row_tbl <- .projr_zero_tbl_get_manifest()
      expect_identical(manifest_compare_list$kept_unchanged, zero_row_tbl)
      expect_identical(manifest_compare_list$added, manifest_tbl_post)
      manifest_compare_list <- .projr_manifest_compare(
        manifest_tbl_pre,
        manifest_tbl_post[rep(FALSE, nrow(manifest_tbl_pre)), ]
      )
      expect_identical(manifest_compare_list$kept_unchanged, zero_row_tbl)
      expect_identical(manifest_compare_list$removed, manifest_tbl_pre)
    },
    force = TRUE,
    quiet = TRUE
  )
})
