test_that(".manifest_hash_label works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test hashing empty directory
      path_dir_empty <- projr_path_get_dir("raw-data")
      .dir_rm(path_dir_empty)
      dir.create(path_dir_empty)
      manifest <- .manifest_hash_label("raw-data", FALSE)
      expect_identical(nrow(manifest), 0L)
      # test hashing empty directory with a sub-directory
      dir.create(file.path(path_dir_empty, "def"))
      manifest <- .manifest_hash_label("raw-data", TRUE)
      expect_identical(nrow(manifest), 0L)

      # test hashing non-empty directories
      path_dir <- .test_setup_content("output", safe = FALSE)
      manifest <- .manifest_hash_label("output", TRUE)
      expect_identical(nrow(manifest), 3L)
      expect_identical(length(unique(manifest$hash)), 1L)

      # test hashing non-empty directories - non-output run
      path_dir <- .test_setup_content("output", safe = TRUE)
      manifest <- .manifest_hash_label("output", FALSE)
      expect_identical(nrow(manifest), 3L)
      expect_identical(length(unique(manifest$hash)), 1L)
    }
  )
})

test_that(".build_manifest_* works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # pre
      # --------------------------

      # no content, except for an ignored label (output)
      label_vec <- c("cache", "raw-data", "output")[-3]
      for (x in label_vec) {
        .dir_rm(projr_path_get_dir(x, safe = TRUE, create = FALSE))
        .dir_create(x, safe = TRUE)
      }
      .test_setup_content("output", safe = TRUE)
      expect_false(.build_manifest_pre(FALSE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 0L)

      # content, but ignore cache by default
      invisible(.test_setup_content(label_vec, safe = TRUE))
      expect_false(.build_manifest_pre(FALSE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 3L)

      # content, but now explicitly version cache
      .yml_dir_nm_set_hash(TRUE, "cache", "default")
      invisible(.test_setup_content(label_vec, safe = TRUE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 6L)

      # post
      # --------------------------
      expect_false(.build_manifest_post(FALSE))
      path_manifest <- .build_manifest_post(TRUE)
      expect_identical(nrow(.manifest_read(path_manifest)), 6L)

      # now add output content
      .test_setup_content("output", safe = FALSE)
      path_manifest <- .build_manifest_post(TRUE)
      expect_identical(nrow(.manifest_read(path_manifest)), 9L)

      # now add doc content
      .test_setup_content("docs", safe = FALSE)
      path_manifest <- .build_manifest_post(TRUE)
      expect_identical(nrow(.manifest_read(path_manifest)), 12L)

      # return zero table
      invisible(.file_rm(.build_manifest_pre_path_get()))
      .dir_rm(projr_path_get_dir("docs", safe = FALSE))
      .dir_rm(projr_path_get_dir("output", safe = FALSE))
      .file_rm(.path_get("manifest.csv"))
      path_manifest <- .build_manifest_post(TRUE)
      expect_identical(nrow(.manifest_read(path_manifest)), 0L)
    }
  )
})
