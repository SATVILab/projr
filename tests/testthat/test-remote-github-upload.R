.test_upload_file <- function(bytes = as.raw(1:8)) {
  path <- withr::local_tempfile(.local_envir = parent.frame())
  writeBin(bytes, path)
  path
}

test_that("a GitHub upload stored despite a server error succeeds", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  fail_upload <- .test_local_github_releases("archive")
  path <- .test_upload_file()
  fail_upload("data.zip", stored = TRUE)
  asset <- .gh_release_asset_upload_httr(
    "fixture/project",
    "archive",
    path,
    "data.zip"
  )
  expect_identical(asset$name, "data.zip")
  expect_identical(
    .remote_ls_final_github_httr("fixture/project", "archive"),
    "data.zip"
  )
  # The manifest lock is an exclusive claim and is verified the same way.
  fail_upload("projr-manifest.lock", stored = TRUE)
  claim <- .gh_release_asset_upload_httr(
    "fixture/project",
    "archive",
    path,
    "projr-manifest.lock",
    overwrite = FALSE
  )
  expect_identical(claim$name, "projr-manifest.lock")
})

test_that("a GitHub upload retries once and then reports a server error", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  fail_upload <- .test_local_github_releases("archive")
  path <- .test_upload_file()
  fail_upload("data.zip")
  expect_identical(
    .gh_release_asset_upload_httr(
      "fixture/project",
      "archive",
      path,
      "data.zip"
    )$name,
    "data.zip"
  )
  fail_upload("other.zip", times = 2L)
  expect_error(
    .gh_release_asset_upload_httr(
      "fixture/project",
      "archive",
      path,
      "other.zip"
    ),
    "HTTP 503"
  )
  expect_identical(
    .remote_ls_final_github_httr("fixture/project", "archive"),
    "data.zip"
  )
})

test_that("a GitHub retry accepts an already stored matching asset", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  fail_upload <- .test_local_github_releases("archive")
  path <- .test_upload_file()
  # The listing lags behind the stored asset, so the retry receives HTTP 422.
  checked <- 0L
  verify <- .gh_release_asset_uploaded
  local_mocked_bindings(
    .gh_release_asset_uploaded = function(...) {
      checked <<- checked + 1L
      if (checked == 1L) NULL else verify(...)
    }
  )
  fail_upload("data.zip", stored = TRUE)
  asset <- .gh_release_asset_upload_httr(
    "fixture/project",
    "archive",
    path,
    "data.zip"
  )
  expect_identical(asset$name, "data.zip")
  expect_identical(checked, 2L)
})

test_that("a GitHub retry rejects another writer's claim of equal size", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  .test_local_github_releases("archive")
  ours <- .test_upload_file(as.raw(1:8))
  theirs <- .test_upload_file(as.raw(8:1))
  .gh_release_asset_upload_httr("fixture/project", "archive", theirs, "lock")
  expect_true(isTRUE(
    .gh_release_asset_uploaded(
      "fixture/project",
      "archive",
      "lock",
      ours,
      FALSE
    )
  ))
  expect_identical(
    .gh_release_asset_uploaded(
      "fixture/project",
      "archive",
      "lock",
      theirs,
      FALSE
    )$name,
    "lock"
  )
})
