test_that("unlabeled local destinations keep manifests separate from their payload", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    source <- projr_path_get_dir("raw-data", safe = FALSE)
    fs::dir_create(source)
    latest <- withr::local_tempdir()
    archive <- withr::local_tempdir()
    for (layout in c("latest", "archive")) {
      path <- if (layout == "latest") latest else archive
      projr_yml_dest_add_local(layout, "raw-data", path, structure = layout,
        send_cue = "always", send_strategy = "sync-purge", send_inspect = "file"
      )
      destination <- .yml_dest_get_title(layout, "local", "default")
      destination[["path-append-label"]] <- FALSE
      .yml_dest_set_title(destination, layout, "local", TRUE, "default")
    }
    send <- function(layout) .dest_send_label("raw-data", layout, "local", TRUE, FALSE, FALSE, FALSE)
    for (version in c("1.0.0", "1.0.1-2")) {
      projr_version_set(version)
      writeLines(version, file.path(source, "result.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      send("latest")
      send("archive")
      if (version == "1.0.0" && .Platform$OS.type != "windows") {
        # Purging this literal filename must preserve the neighboring manifests directory.
        file.create(file.path(latest, "manif*"))
      }
    }
    expect_false(file.exists(file.path(latest, "manif*")))
    for (path in c(latest, archive)) {
      index <- .manifest_store_index(file.path(path, "manifests"), "raw-data")
      expect_identical(index$version, c("v1.0.0", "v1.0.1-2"))
    }
    expect_identical(readLines(file.path(archive, "v1.0.0", "result.txt")), "1.0.0")
    expect_identical(readLines(file.path(latest, "result.txt")), "1.0.1-2")
    for (layout in c("latest", "archive")) {
      writeLines("stale", file.path(source, "other.txt"))
      projr_content_update(label = "raw-data", type = "local", title = layout, clear = TRUE)
      expect_identical(list.files(source, all.files = TRUE, no.. = TRUE), "result.txt")
      expect_identical(readLines(file.path(source, "result.txt")), "1.0.1-2")
    }

    writeLines("user-supplied", file.path(source, "VERSION"))
    expect_error(send("latest"), "reserves these paths.*VERSION")
    expect_identical(readLines(file.path(latest, "result.txt")), "1.0.1-2")
    unlink(file.path(source, "VERSION"))
    unmanaged <- withr::local_tempdir()
    dir.create(file.path(unmanaged, "manifests"))
    root <- .remote_final_get_local(unmanaged, FALSE, "raw-data", "latest", pre = TRUE, empty = FALSE)
    expect_error(.remote_local_check_payload(source, root), "reserves these paths.*manifests")
  })
})
