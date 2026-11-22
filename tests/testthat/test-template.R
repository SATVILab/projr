test_that("templates resolve complete names and preserve literal text", {
  skip_if(.is_test_select())
  withr::local_envvar(c(PROJR_BUILD_HEAD = "ref: refs/heads/feat/A"))
  expect_identical(
    .template_expand(c(
      "@version_report.csv", "@branch", "out-@version/@branch",
      "@version\\report", "@version report", "@versionasdf", "archive-@branching",
      "@version+extra", NA_character_
    ), version = "v1.2"),
    c(
      "v1.2_report.csv", "feat_2f_41", "out-v1.2/feat_2f_41",
      "v1.2\\report", "v1.2 report", "@versionasdf", "archive-@branching",
      "@version+extra", NA_character_
    )
  )
  expect_null(.template_expand(NULL))
  expect_identical(.template_substitute("a+@uuid.txt", identity, toupper), "A+@uuid.TXT")
})

test_that("branch names stay distinct as portable path components", {
  skip_if(.is_test_select())
  branches <- c(
    "topic/a", "topic_2fa", "topic-a", "Topic/a", "con", "_63on",
    "release.1", "\u00e9tude", "main"
  )
  encoded <- vapply(paste0("ref: refs/heads/", branches), .template_branch, character(1))
  expect_false(anyDuplicated(tolower(encoded)) > 0L)
  expect_true(all(grepl("^[a-z0-9_-]+$", encoded)))
  expect_identical(unname(tail(encoded, 1)), "main")
  expect_identical(.template_branch("ref: refs/heads/con"), "_63on")
  sha <- strrep("a", 40L)
  expect_identical(.template_branch(sha), paste0("__detached-", sha))
  expect_error(.template_branch_raw(sha), "named Git branch")
  expect_error(.template_branch(""), "named Git branch")
})

test_that("each build has its own UUID and passes its context to child processes", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(dir_test,
    {
      withr::local_envvar(c(PROJR_BUILD_HEAD = NA, PROJR_BUILD_VERSION = NA, PROJR_BUILD_UUID = NA))
      session_uuid <- .template_expand("@uuid")
      expect_identical(.template_expand("@uuid"), session_uuid)

      restore_outer <- .template_context_start("v2.1")
      withr::defer(restore_outer())
      outer_uuid <- .template_expand("@uuid")
      expect_match(outer_uuid, "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
      expect_false(identical(outer_uuid, session_uuid))
      expect_identical(.template_expand("@uuid/@uuid"), paste(outer_uuid, outer_uuid, sep = "/"))

      child <- system2(file.path(R.home("bin"), "Rscript"), c(
        "--vanilla", "-e",
        shQuote('cat(Sys.getenv(c("PROJR_BUILD_VERSION","PROJR_BUILD_UUID","PROJR_BUILD_HEAD")), sep="\\n")')
      ),
      stdout = TRUE
      )
      expect_identical(child, c("v2.1", outer_uuid, .template_checkout_head()))

      expect_error((function() {
        restore_inner <- .template_context_start("v2.2")
        on.exit(restore_inner())
        expect_false(identical(.template_expand("@uuid"), outer_uuid))
        expect_identical(.template_expand("@version"), "v2.2")
        stop("sample failure")
      })(), "sample failure")
      expect_identical(.template_expand("@version/@uuid"), paste0("v2.1/", outer_uuid))
      restore_outer()
      expect_identical(.template_expand("@uuid"), session_uuid)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("release preparation and rendering use the same bumped version and UUID", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(dir_test,
    {
      config <- .yml_get("default")
      config$build$scripts <- "render.R"
      .yml_set(config)
      writeLines('writeLines(projr:::.template_expand("@version/@uuid"), "render.txt")', "render.R")
      preparations <- character()
      testthat::local_mocked_bindings(
        .dest_prepare_github_releases = function(...) {
          preparations <<- c(preparations, .template_expand("@version/@uuid"))
        }
      )
      expect_identical(projr_version_get(), "0.0.0-1")
      projr_build_patch(msg = "Test build templates")
      rendered <- readLines("render.txt")
      expect_match(rendered, "^v0\\.0\\.1/")
      expect_identical(preparations, rep(rendered, 2L))
      expect_identical(projr_version_get(), "0.0.1-1")
      expect_false(identical(.template_expand("@version/@uuid"), rendered))
    },
    force = TRUE,
    quiet = TRUE
  )
})
