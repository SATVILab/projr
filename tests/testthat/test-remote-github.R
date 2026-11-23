test_that("GitHub remote names include the selected release and archive version", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE)
  usethis::with_project(dir_test, {
    expect_identical(.remote_get("github", "example"), c(tag = "example"))
    expect_identical(
      .remote_final_get("github", id = "example", label = "raw-data", structure = "archive", version = "v1.2.3"),
      c(tag = "example", fn = "raw-data-v1.2.3.zip")
    )
    expect_identical(
      .remote_final_get("github", id = "example", label = "raw-data", structure = "archive", version = "v1.2.3", empty = TRUE),
      c(tag = "example", fn = "raw-data-v1.2.3-empty.zip")
    )
  })
})

test_that("same-named GitHub assets stay independent throughout cleanup", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  tags <- basename(c(tempfile("fixture-a-"), tempfile("fixture-b-")))
  .test_local_github_releases(tags)
  remotes <- lapply(tags, function(tag) c(tag = tag, fn = "raw-data.zip"))
  projects <- c(.test_setup_project(), .test_setup_project())
  for (i in seq_along(tags)) {
    usethis::with_project(projects[i], {
      writeBin(as.raw(i), "sample.dat")
      .remote_file_add("github", remotes[[i]], fn = "sample.dat", path_dir_local = ".")
      expect_identical(.remote_file_ls("github", remotes[[i]]), "sample.dat")
    })
  }
  for (i in seq_along(tags)) {
    usethis::with_project(projects[i], {
      .remote_file_get("github", remotes[[i]], "sample.dat", "download")
      expect_identical(readBin("download/sample.dat", "raw", 8L), as.raw(i))
    })
  }

  usethis::with_project(projects[1], {
    dir.create("reports")
    writeLines("extra", "reports/extra.txt")
    .remote_file_add("github", remotes[[1]], fn = "reports/extra.txt", path_dir_local = ".")
    expect_setequal(.remote_file_ls("github", remotes[[1]]), c("reports/extra.txt", "sample.dat"))
    .remote_file_rm("github", fn = "reports/extra.txt", remote = remotes[[1]])
    expect_identical(.remote_file_ls("github", remotes[[1]]), "sample.dat")
    expect_true(.remote_final_rm_github_httr("fixture/project", tags[1], "raw-data.zip"))
    expect_true(.remote_rm_github_httr("fixture/project", tags[1]))
    expect_false(.remote_check_exists_github_httr("fixture/project", tags[1]))
  })
  usethis::with_project(projects[2], {
    expect_true(.remote_check_exists_github_httr("fixture/project", tags[2]))
    expect_identical(.remote_file_ls("github", remotes[[2]]), "sample.dat")
    .remote_file_get("github", remotes[[2]], "sample.dat", "after-cleanup")
    expect_identical(readBin("after-cleanup/sample.dat", "raw", 8L), as.raw(2))
  })
})

test_that("GitHub manifest and VERSION archives round-trip offline", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  tags <- basename(c(tempfile("manifest-"), tempfile("version-")))
  fail_next_upload <- .test_local_github_releases(tags)
  dir_test <- .test_setup_project(git = FALSE)
  usethis::with_project(dir_test, {
    remote <- c(tag = tags[1])
    manifest <- data.frame(label = "raw-data", fn = "sample", version = "v1.0.0", hash = "abc")
    stage <- withr::local_tempdir()
    .manifest_write(manifest, file.path(stage, "manifest.csv"))
    archive <- file.path(stage, "legacy.zip")
    withr::with_dir(stage, utils::zip(archive, "manifest.csv", flags = "-q"))
    .gh_release_asset_upload_httr(.gh_repo_get(), tags[1], archive, asset_name = "manifest.zip")
    expect_identical(.remote_get_manifest("github", remote), manifest)

    # Updating one label preserves the legacy label and all earlier build versions.
    .remote_write_manifest("github", remote, transform(manifest, version = "v1.0.1"))
    output <- data.frame(label = "output", fn = "result", version = "v1.0.1", hash = "def")
    .remote_write_manifest("github", remote, output)
    expected <- rbind(output, manifest, transform(manifest, version = "v1.0.1"))
    expect_identical(.remote_get_manifest("github", remote), expected)
    expect_identical(.remote_get_manifest("github", remote, "raw-data", "1.0.0"), manifest)
    fail_next_upload("manifest.zip")
    expect_error(.remote_write_manifest("github", remote, transform(output, hash = "changed")), "503")
    expect_identical(.remote_get_manifest("github", remote), expected)
    expect_identical(.remote_ls_final_github_httr(.gh_repo_get(), tags[1]), "manifest.zip")
    zip_path <- .remote_file_get_all_github_httr(.gh_repo_get(), tags[1], "manifest.zip", stage)
    # The two raw-data builds share the same content-addressed file list.
    snapshot <- function(label, fn, hash) {
      csv <- paste0('"fn","hash"\n"', fn, '","', hash, '"\n')
      file.path(label, paste0("files-", digest::digest(csv, "sha256", serialize = FALSE), ".csv"))
    }
    expect_setequal(utils::unzip(zip_path, list = TRUE)$Name, c(
      "FORMAT", "output/versions.csv", "raw-data/versions.csv",
      snapshot("output", "result", "def"), snapshot("raw-data", "sample", "abc")
    ))
    # A provisional local import cannot serve as a published release history.
    writeLines("importing", file.path(stage, "FORMAT"))
    incomplete <- file.path(stage, "partial.zip")
    withr::with_dir(stage, utils::zip(incomplete, "FORMAT", flags = "-q"))
    .gh_release_asset_upload_httr(.gh_repo_get(), tags[1], incomplete, asset_name = "manifest.zip")
    expect_error(.remote_get_manifest("github", remote), "Incomplete manifest.zip")
    expect_error(.remote_write_manifest("github", remote, output), "Incomplete manifest.zip")
    retained <- .remote_file_get_all_github_httr(.gh_repo_get(), tags[1], "manifest.zip", stage)
    expect_identical(digest::digest(file = retained), digest::digest(file = incomplete))

    .remote_write_version_file("github", c(tag = tags[2]), "v1.2.3")
    expect_identical(.remote_get_version_file("github", c(tag = tags[2])), "v1.2.3")
  })
})

test_that("overlapping writes to one release preserve both directory histories on retry", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  tags <- basename(c(tempfile("shared-"), tempfile("independent-")))
  .test_local_github_releases(tags)
  shared <- c(tag = tags[1])
  independent <- c(tag = tags[2])
  first <- data.frame(label = "raw-data", fn = "sample", version = "v1.0.0", hash = "a")
  second <- transform(first, label = "output", hash = "b")
  original_download <- .remote_manifest_download
  overlap <- TRUE
  testthat::local_mocked_bindings(.remote_manifest_download = function(...) {
    if (overlap) {
      overlap <<- FALSE
      expect_error(.remote_write_manifest("github", shared, second), "Could not claim projr-manifest.lock")
      expect_identical(.remote_ls_final_github_httr(.gh_repo_get(), tags[1]), "projr-manifest.lock")
      .remote_write_manifest("github", independent, second)
    }
    original_download(...)
  })
  .remote_write_manifest("github", shared, first)
  .remote_write_manifest("github", shared, second)
  expect_identical(.remote_get_manifest("github", shared), rbind(second, first))
  expect_identical(.remote_get_manifest("github", independent), second)
  expect_identical(.remote_ls_final_github_httr(.gh_repo_get(), tags[1]), "manifest.zip")
})

test_that("detailed metadata logs retain the read error and omit project no-ops", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  .test_local_github_releases("archive")
  log <- withr::local_tempfile()
  file.create(log)
  withr::local_envvar(c(PROJR_LOG_DETAILED = "true", PROJR_BUILD_LOG = log, PROJR_OUTPUT_LEVEL = "none"))
  expect_true(.remote_write_version_file("project", NULL, "v1.0.0"))
  expect_true(.remote_write_changelog("project", NULL))
  expect_length(.remote_get_version_file("project", NULL), 0L)
  expect_length(readLines(log), 0L)

  asset <- withr::local_tempfile()
  writeLines("available", asset)
  .gh_release_asset_upload_httr(.gh_repo_get(), "archive", asset, asset_name = "available.txt")
  expect_length(.remote_get_version_file("github", c(tag = "archive")), 0L)
  messages <- readLines(log)
  expect_true(any(grepl("Could not read VERSION.*VERSION.zip.*archive", messages)))
  expect_false(any(grepl("VERSION is absent|No VERSION file found", messages)))
})
