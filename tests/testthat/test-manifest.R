test_that(".projr_manifest_hash_label works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test hashing empty directory
      path_dir_empty <- projr_path_get_dir("raw-data")
      .dir_rm(path_dir_empty)
      dir.create(path_dir_empty)
      manifest <- .projr_manifest_hash_label("raw-data", FALSE)
      expect_identical(nrow(manifest), 0L)
      # test hashing empty directory with a sub-directory
      dir.create(file.path(path_dir_empty, "def"))
      manifest <- .projr_manifest_hash_label("raw-data", TRUE)
      expect_identical(nrow(manifest), 0L)

      # test hashing non-empty directories
      path_dir <- .projr_test_setup_content("output", safe = FALSE)
      manifest <- .projr_manifest_hash_label("output", TRUE)
      expect_identical(nrow(manifest), 3L)
      expect_identical(length(unique(manifest$hash)), 1L)

      # test hashing non-empty directories - non-output run
      path_dir <- .projr_test_setup_content("output", safe = TRUE)
      manifest <- .projr_manifest_hash_label("output", FALSE)
      expect_identical(nrow(manifest), 3L)
      expect_identical(length(unique(manifest$hash)), 1L)
    }
  )
})

test_that(".projr_build_manifest_* works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # pre
      # --------------------------

      # no content, except for an ignored label (output)
      label_vec <- c("cache", "raw-data", "output")[-3]
      for (x in label_vec) {
        .dir_rm(projr_path_get_dir(x, safe = TRUE, create = FALSE))
        projr_dir_create(x, safe = TRUE)
      }
      .projr_test_setup_content("output", safe = TRUE)
      expect_false(.projr_build_manifest_pre(FALSE))
      path_manifest <- .projr_build_manifest_pre(TRUE)
      manifest <- .projr_manifest_read(path_manifest)
      expect_identical(nrow(manifest), 0L)

      # content, but ignore cache by default
      invisible(.projr_test_setup_content(label_vec, safe = TRUE))
      expect_false(.projr_build_manifest_pre(FALSE))
      path_manifest <- .projr_build_manifest_pre(TRUE)
      manifest <- .projr_manifest_read(path_manifest)
      expect_identical(nrow(manifest), 3L)

      # content, but now explicitly version cache
      .projr_yml_dir_nm_set_hash(TRUE, "cache", "default")
      invisible(.projr_test_setup_content(label_vec, safe = TRUE))
      path_manifest <- .projr_build_manifest_pre(TRUE)
      manifest <- .projr_manifest_read(path_manifest)
      expect_identical(nrow(manifest), 6L)

      # post
      # --------------------------
      expect_false(.projr_build_manifest_post(FALSE))
      path_manifest <- .projr_build_manifest_post(TRUE)
      expect_identical(nrow(.projr_manifest_read(path_manifest)), 6L)

      # now add output content
      .projr_test_setup_content("output", safe = FALSE)
      path_manifest <- .projr_build_manifest_post(TRUE)
      expect_identical(nrow(.projr_manifest_read(path_manifest)), 9L)

      # now add doc content
      .projr_test_setup_content("docs", safe = FALSE)
      path_manifest <- .projr_build_manifest_post(TRUE)
      expect_identical(nrow(.projr_manifest_read(path_manifest)), 12L)

      # return zero table
      invisible(.file_rm(.projr_build_manifest_pre_path_get()))
      .dir_rm(projr_path_get_dir("docs", safe = FALSE))
      .dir_rm(projr_path_get_dir("output", safe = FALSE))
      .file_rm(.dir_proj_get("manifest.csv"))
      path_manifest <- .projr_build_manifest_post(TRUE)
      expect_identical(nrow(.projr_manifest_read(path_manifest)), 0L)
    }
  )
})
