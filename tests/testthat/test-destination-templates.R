test_that("a new release targets the remote branch commit and never moves a tag", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project()
  usethis::with_project(dir_test, {
    api <- "https://fixture.invalid"
    repo <- "fixture/project"
    withr::local_envvar(c(GITHUB_API_URL = api, GITHUB_PAT = "fixture"))
    testthat::local_mocked_bindings(.gh_repo_get = function() repo)
    gert::git_branch_create("feature/reports")
    sha <- strrep("a", 40)
    created <- NULL
    requests <- character()
    branch_exists <- TRUE
    release_exists <- FALSE
    tag_sha <- NULL
    handle <- function(req) {
      route <- utils::URLdecode(sub(paste0(api, "/repos/", repo), "", req$url, fixed = TRUE))
      requests <<- c(requests, paste(req$method, route))
      if (startsWith(route, "/releases/tags/")) {
        return(.test_github_response(if (release_exists) 200 else 404))
      }
      if (identical(route, "/git/ref/heads/feature/reports")) {
        return(.test_github_response(
          if (branch_exists) 200 else 404,
          list(object = list(type = "commit", sha = sha))
        ))
      }
      if (startsWith(route, "/git/ref/tags/")) {
        return(.test_github_response(
          if (is.null(tag_sha)) 404 else 200,
          list(object = list(type = "commit", sha = tag_sha))
        ))
      }
      if (identical(route, "/releases") && req$method == "POST") {
        created <<- jsonlite::fromJSON(rawToChar(req$options$postfields))
        return(.test_github_response(201))
      }
      stop("Unexpected GitHub request ", req$method, " ", route)
    }
    previous <- httr::set_callback("request", handle)
    withr::defer(httr::set_callback("request", previous))
    .yml_dest_rm_type_all("default")
    projr_version_set("1.2.3")
    projr_yml_dest_add_github("archive-@version", "output", branch = "@branch")
    prepare <- function() .dest_prepare_github_releases("patch", FALSE, FALSE, FALSE, TRUE)
    prepare()
    expect_identical(created$tag_name, "archive-v1.2.3")
    expect_identical(created$name, created$tag_name)
    expect_identical(created$target_commitish, sha)

    created <- NULL
    branch_exists <- FALSE
    expect_error(prepare(), "heads/feature")
    expect_null(created)

    branch_exists <- TRUE
    tag_sha <- strrep("b", 40)
    expect_error(prepare(), "Existing Git tag")
    expect_null(created)

    tag_sha <- sha
    prepare()
    expect_identical(created$target_commitish, tag_sha)

    requests <- character()
    release_exists <- TRUE
    prepare()
    expect_identical(requests, "GET /releases/tags/archive-v1.2.3")

    .yml_dest_rm_type_all("default")
    projr_yml_dest_add_github("archive", "output")
    requests <- character()
    prepare()
    projr_version_set("1.2.4")
    prepare()
    expect_identical(requests, rep("GET /releases/tags/archive", 2L))
  })
})

test_that("a local build records its templated archive and restores from the recorded path", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    .yml_dest_rm_type_all("default")
    gert::git_branch_create("analysis/report")
    projr_version_set("1.2.2")
    .test_content_setup_label("raw-data")
    writeLines("Archived specimen data", file.path("_raw_data", "sample.txt"))
    title <- "backup-@branch"
    path <- "archives/@branch/@version-@uuid"
    projr_yml_dest_add_local(title, "raw-data", path = path, structure = "latest")

    projr_build_patch(msg = "Recorded archive")
    version_dir <- list.dirs("archives", recursive = TRUE, full.names = TRUE)
    archived <- version_dir[dirname(version_dir) %in% version_dir & grepl("/v1[.]2[.]3-[^/]+$", version_dir)]
    expect_length(archived, 1L)
    expect_true(any(grepl(archived, readLines(".projr/BUILDLOG.md"), fixed = TRUE)))
    expect_identical(.yml_dest_get_title(title, "local", NULL)[["path"]], path)
    original <- list.files("_raw_data", recursive = TRUE)
    expected <- tools::md5sum(file.path("_raw_data", original))
    expect_false(projr_content_update("raw-data",
      pos = "dest", type = "local", title = title,
      destination = "archives/missing", clear = TRUE
    ))
    expect_identical(readLines("_raw_data/sample.txt"), "Archived specimen data")
    unlink("_raw_data", recursive = TRUE)
    dir.create("nested")
    result <- withr::with_dir(
      "nested",
      projr_content_update("raw-data", pos = "dest", type = "local", title = title, destination = archived)
    )
    expect_true(result)
    expect_identical(tools::md5sum(file.path("_raw_data", original)), expected)
  })
})
