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
    writeLines("importing", file.path(unmanaged, "manifests", "FORMAT"))
    expect_no_error(.remote_local_check_payload(source, root))
  })
})

test_that("flat local destinations preserve history through an empty build", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    label <- "raw-data"
    config$build$scripts <- character()
    .yml_set(config)
    projr_version_set("1.0.0-1")
    parent <- withr::local_tempdir()
    destinations <- c(latest = file.path(parent, "bundle-empty"), archive = file.path(parent, "by-version"),
                      labeled = file.path(parent, "labeled-empty"))
    for (layout in names(destinations)) {
      structure <- if (layout == "archive") "archive" else "latest"
      projr_yml_dest_add_local(layout, label, destinations[[layout]], structure = structure,
        send_cue = "always", send_strategy = "sync-purge", send_inspect = "file"
      )
      destination <- .yml_dest_get_title(layout, "local", "default")
      destination[["path-append-label"]] <- layout == "labeled"
      .yml_dest_set_title(destination, layout, "local", TRUE, "default")
    }
    sibling <- paste0(destinations[["latest"]], "-empty")
    dir.create(sibling)
    sentinel <- file.path(sibling, "owned-by-someone-else.txt")
    writeLines("preserve this", sentinel)
    source <- projr_path_get_dir(label, safe = FALSE)
    fs::dir_create(source)
    file <- file.path(source, "result.txt")
    for (i in 1:3) {
      if (i == 2L) unlink(file) else writeLines("original", file)
      projr_build_patch(msg = paste("Archive", i))
      for (layout in names(destinations)) {
        # An explicitly empty build must clear a stale copy during restore.
        writeLines("stale", file)
        if (i == 2L) {
          expect_false(projr_content_update(label, pos = "dest", type = "local", title = layout, clear = FALSE))
          expect_identical(readLines(file), "stale")
        }
        expect_true(projr_content_update(label, pos = "dest", type = "local", title = layout, clear = TRUE))
        if (i == 2L) {
          expect_false(file.exists(file))
        } else {
          expect_identical(readLines(file), "original")
        }
        index <- .manifest_store_index(file.path(destinations[[layout]], "manifests"), label)
        expect_identical(index$version, paste0("v1.0.", seq_len(i)))
        expect_identical(index$directory_version, c("v1.0.1", "v1.0.2", "v1.0.1")[seq_len(i)])
      }
      expect_true(file.exists(sentinel))
      if (i == 1L) {
        # Missing files from a populated snapshot do not confirm an empty build.
        remote_file <- file.path(destinations[["latest"]], "result.txt")
        unlink(remote_file)
        expect_false(projr_content_update(label, pos = "dest", type = "local", title = "latest", clear = TRUE))
        expect_identical(readLines(file), "original")
        writeLines("original", remote_file)
      }
    }
    expect_identical(readLines(sentinel), "preserve this")
    expect_identical(readLines(file.path(destinations[["archive"]], "v1.0.1", "result.txt")), "original")
  })
})
