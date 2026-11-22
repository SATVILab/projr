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
  .test_local_github_releases(tags)
  dir_test <- .test_setup_project(git = FALSE)
  usethis::with_project(dir_test, {
    manifest <- data.frame(label = "output", fn = "sample", version = "v1.0.0", hash = "abc")
    .remote_write_manifest("github", c(tag = tags[1]), manifest)
    expect_equal(.remote_get_manifest("github", c(tag = tags[1])), manifest)
    .remote_write_version_file("github", c(tag = tags[2]), "v1.2.3")
    expect_identical(.remote_get_version_file("github", c(tag = tags[2])), "v1.2.3")
  })
})
