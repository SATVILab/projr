test_that("projr_manifest_hash_dir works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
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
      expect_identical(nrow(.projr_manifest_hash_label("cache")), 0L)
      expect_identical(ncol(.projr_manifest_hash_label("cache")), 3L)
      invisible(file.create("_data_raw/sub/def.csv"))

      # test getting manifest
      # ----------------------------

      # pre:
      dir.create("_tmp/abc", recursive = TRUE)
      file.create("_tmp/test.txt")
      file.create("_tmp/abc/test.txt")

      manifest <- .projr_build_manifest_hash_pre()
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_pre()
      expect_identical(nrow(manifest), 0L)
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["cache"]][["hash"]] <- TRUE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_pre()
      expect_identical(nrow(manifest), 2L)
      expect_identical(colnames(manifest), c("label", "fn", "hash"))

      # post:
      if (dir.exists("_data_raw")) {
        unlink("_data_raw", recursive = TRUE)
      }
      if (dir.exists("_tmp")) {
        unlink("_tmp", recursive = TRUE)
      }
      dir.create("_tmp/projr-output/0.0.0-1/abc", recursive = TRUE)
      invisible(file.create("_tmp/projr-output/0.0.0-1/test.txt"))
      invisible(file.create("_tmp/projr-output/0.0.0-1/abc/test.txt"))
      dir.create("_data_raw/abc", recursive = TRUE)
      invisible(file.create("_data_raw/test.txt"))
      invisible(file.create("_data_raw/abc/test.txt"))

      manifest <- .projr_build_manifest_hash_post()
      expect_identical(nrow(manifest), 4L)
      expect_identical(colnames(manifest), c("label", "fn", "hash"))
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["data-raw"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post()
      expect_identical(nrow(manifest), 2L)
      yml_projr[["directories"]][["output"]][["hash"]] <- FALSE
      .projr_yml_set(yml_projr)
      manifest <- .projr_build_manifest_hash_post()
      expect_identical(nrow(manifest), 0L)
      expect_identical(colnames(manifest), c("label", "fn", "hash"))

      # test saving manifest
      # -------------------------
      manifest_tbl <- data.frame(x = 1)
      .projr_build_manifest_save(manifest_tbl)
      expect_true(file.exists("_output/manifest.csv"))
      expect_true(file.exists("_archive/v0.0.0-1/manifest.csv"))
      expect_true(file.exists("_output/manifest.rds"))
      expect_true(file.exists("_archive/v0.0.0-1/manifest.rds"))
      invisible(file.remove("_output/manifest.csv"))
      invisible(file.remove("_output/manifest.rds"))
      invisible(file.remove("_archive/v0.0.0-1/manifest.csv"))
      invisible(file.remove("_archive/v0.0.0-1/manifest.rds"))
      projr_version_set("0.0.1")
      .projr_build_manifest_save(manifest_tbl)
      expect_true(file.exists("_output/manifest.csv"))
      expect_true(file.exists("_output/manifest.rds"))
      expect_true(file.exists("_archive/v0.0.1/manifest.csv"))
      expect_true(file.exists("_archive/v0.0.1/manifest.rds"))
      invisible(file.remove("_output/manifest.csv"))
      invisible(file.remove("_output/manifest.rds"))
      invisible(file.remove("_archive/v0.0.1/manifest.csv"))
      invisible(file.remove("_archive/v0.0.1/manifest.rds"))
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["archive"]][["manifest"]] <- FALSE
      .projr_yml_set(yml_projr)
      .projr_build_manifest_save(manifest_tbl)
      expect_true(file.exists("_output/manifest.csv"))
      expect_false(file.exists("_archive/manifest.csv"))
      expect_true(file.exists("_output/manifest.rds"))
      expect_false(file.exists("_archive/manifest.rds"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
