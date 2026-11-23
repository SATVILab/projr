test_that("a clone restores the checked-out build from its implicit GitHub archive", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  skip_if_not_installed("httr")
  .test_local_github_releases("archive")
  project <- .test_setup_project(rm_engine = TRUE)
  clone <- file.path(withr::local_tempdir(), "clone")
  usethis::with_project(project, {
    yml <- .yml_get("default")
    yml$directories <- list(
      "raw-data" = list(path = "_raw_data"),
      "raw-reference" = list(path = "_reference"),
      output = list(path = "_output"),
      cache = list(path = "_tmp")
    )
    yml$build <- list(scripts = "analysis.R", git = list(commit = TRUE, push = FALSE))
    .yml_set(yml, "default")
    writeLines(c(
      'data <- readLines(projr::projr_path_get("raw-data", "data.txt"))',
      'ref <- readLines(projr::projr_path_get("raw-reference", "key.txt"))',
      'writeLines(paste(ref, data), projr::projr_path_get("output", "result.txt"))'
    ), "analysis.R")
    input <- projr_path_get("raw-data", "data.txt")
    writeLines("first", input)
    writeLines("constant", projr_path_get("raw-reference", "key.txt"))
    projr_build_patch("First analysis", archive_github = TRUE)
    first_build <- gert::git_log(max = 2)$commit[[2L]]
    expected <- readBin("_output/result.txt", "raw", n = file.size("_output/result.txt"))
    writeLines("second", input)
    projr_build_patch("Second analysis", archive_github = TRUE)
    expect_identical(readLines("_output/result.txt"), "constant second")
    gert::git_clone(project, clone)
  })
  usethis::with_project(clone, {
    # The development commit after publishing points back to the latest completed build.
    expect_false(file.exists("_raw_data/data.txt"))
    projr_restore()
    expect_identical(readLines("_raw_data/data.txt"), "second")
    expect_identical(readLines("_reference/key.txt"), "constant")
    unlink(c("_raw_data", "_reference"), recursive = TRUE)
    expect_equal(system2("git", c("checkout", "--quiet", "--detach", first_build)), 0L)
    unlink(c(".projr/manifests", "manifest.csv", "_output"), recursive = TRUE)
    projr_restore()
    expect_identical(readLines("_raw_data/data.txt"), "first")
    expect_identical(readLines("_reference/key.txt"), "constant")
    expect_false(file.exists("_output/result.txt"))
    projr_build_dev(project = clone)
    result <- projr_path_get("output", "result.txt", safe = TRUE)
    expect_identical(readBin(result, "raw", n = file.size(result)), expected)
  })
})

test_that("restore verifies an earlier matching archive before replacing local files", {
  skip_if(.is_test_select())
  .test_local_github_releases("artifacts")
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project,
    {
      stage <- withr::local_tempdir()
      writeLines("original", file.path(stage, "sample.txt"))
      rows <- data.frame(
        label = "raw-data", fn = "sample.txt", version = paste0("v0.0.", 1:3),
        hash = c(
          unname(tools::md5sum(file.path(stage, "sample.txt"))),
          digest::digest("changed", algo = "md5"), unname(tools::md5sum(file.path(stage, "sample.txt")))
        )
      )
      .manifest_store_write(rows)
      legacy <- tempfile()
      utils::write.csv(rows, legacy, row.names = FALSE)
      legacy_snapshot <- .restore_build_snapshot(tempfile(), legacy, "raw-data", "v0.0.3")
      current_snapshot <- .restore_build_snapshot(.path_get(".projr", "manifests"), legacy, "raw-data", "v0.0.3")
      expect_identical(legacy_snapshot$id, current_snapshot$id)
      expect_identical(legacy_snapshot$versions, c("v0.0.1", "v0.0.3"))
      archive <- tempfile(fileext = ".zip")
      upload <- function(files) {
        unlink(archive)
        expect_equal(with_dir(stage, utils::zip(archive, files, flags = "-q")), 0L)
        .gh_release_asset_upload_httr(.gh_repo_get(), "artifacts", archive, asset_name = "raw-data-v0.0.1.zip")
      }
      upload("sample.txt")
      input <- projr_path_get("raw-data", "sample.txt")
      writeLines("local edits", input)
      expect_error(projr_restore("raw-data", "0.0.3", "artifacts"), "differ")
      expect_identical(readLines(input), "local edits")
      expect_true(projr_restore("raw-data", "0.0.3", "artifacts", clear = TRUE))
      expect_identical(readLines(input), "original")

      writeLines("local edits", input)
      writeLines("damaged", file.path(stage, "sample.txt"))
      upload("sample.txt")
      expect_error(projr_restore("raw-data", "0.0.3", "artifacts", clear = TRUE), "hash")
      expect_identical(readLines(input), "local edits")

      writeLines("extra", file.path(stage, "other.txt"))
      upload(c("sample.txt", "other.txt"))
      expect_error(projr_restore("raw-data", "0.0.3", "artifacts", clear = TRUE), "file list")
      expect_identical(readLines(input), "local edits")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("restore recovers a missing immutable file list and VERSION from recorded metadata", {
  skip_if(.is_test_select())
  .test_local_github_releases("archive")
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project,
    {
      version <- "v0.0.2"
      yml <- .yml_get("default")
      yml$directories[["raw-data"]]$path <- "inputs/@version"
      .yml_set(yml, "default")
      rows <- .empty_tbl_get_manifest("raw-data", version)
      .manifest_store_write(rows)
      earlier <- .empty_tbl_get_manifest("raw-data", "v0.0.1")
      .remote_write_manifest("github", c(tag = "archive"), rbind(earlier, rows))
      unlink(c(
        .path_get("VERSION"), .path_get("DESCRIPTION"),
        Sys.glob(.path_get(".projr", "manifests", "raw-data", "files-*.csv"))
      ))
      writeLines(c("# BUILDLOG", "", "#### v0.0.2: Published"), .path_get("BUILDLOG.md"))
      expect_true(projr_restore("raw-data"))
      expect_identical(readLines(.path_get("VERSION")), version)
      expect_identical(.manifest_read_project("raw-data", version), rows)
      history <- .manifest_store_index(label = "raw-data")
      expect_identical(history$directory_version, c("v0.0.1", "v0.0.1"))
      expect_true(dir.exists("inputs/v0.0.2"))
      unlink("inputs", recursive = TRUE)
      # The build log also supplies the version for a dependency-only DESCRIPTION.
      unlink("VERSION")
      writeBin(charToRaw("Imports: stats"), "DESCRIPTION")
      # Empty committed snapshots require no release access.
      expect_true(projr_restore("raw-data", destination = "absent-release"))
      expect_true(dir.exists("inputs/v0.0.2"))
      expect_identical(readLines("VERSION"), version)
      expect_identical(readBin("DESCRIPTION", "raw", 100L), charToRaw("Imports: stats"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("a first build archives literal starter paths and restores them into a clone", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  skip_if_not_installed("httr")
  .test_local_github_releases("archive")
  project <- withr::local_tempdir()
  clone <- file.path(withr::local_tempdir(), "clone")
  upstream <- withr::local_tempdir()
  gert::git_init(upstream, bare = TRUE)
  gert::git_init(project)
  gert::git_remote_add(upstream, repo = project)
  .test_setup_project_git_config(project)
  usethis::with_project(project, {
    dir.create("_raw_data")
    writeLines("21", "_raw_data/number.txt")
    writeLines(c(
      'number <- as.integer(readLines("_raw_data/number.txt"))',
      'writeLines(as.character(2L * number), "_output/result.txt")'
    ), "analysis.R")
    gert::git_add("analysis.R")
    gert::git_commit("Initial analysis")
    gert::git_push("origin", set_upstream = TRUE)
    expect_false(file.exists("_projr.yml"))
    expect_false(dir.exists("_output"))
    projr_build(archive_github = TRUE, project = project)
    expect_identical(readLines("_output/result.txt"), "42")
    assets <- .remote_ls_final_github_httr(.gh_repo_get(), "archive")
    version <- .restore_build_version(NULL)
    output_asset <- paste0("output-", version, ".zip")
    expect_true(all(c(paste0("raw-data-", version, ".zip"), output_asset, "manifest.zip") %in% assets))
    archive <- .remote_file_get_all_github_httr(.gh_repo_get(), "archive", output_asset, tempdir())
    expect_identical(readLines(unz(archive, "result.txt")), "42")
    gert::git_clone(upstream, clone, branch = gert::git_branch())
    .test_setup_project_git_config(clone)
  }, quiet = TRUE, force = TRUE)
  usethis::with_project(clone, {
    expect_false(file.exists("_raw_data/number.txt"))
    expect_true(projr_restore())
    expect_identical(readLines("_raw_data/number.txt"), "21")
    expect_false(file.exists("_output/result.txt"))
    projr_build(project = clone)
    expect_identical(readLines("_output/result.txt"), "42")
  }, quiet = TRUE, force = TRUE)
})
