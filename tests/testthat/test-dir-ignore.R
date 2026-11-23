test_that("directory Git rules respect file creation and preserve manual entries", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE)

  usethis::with_project(dir_test, {
    unlink(".gitignore")
    expect_false(.ignore_diryml_git())
    expect_false(file.exists(".gitignore"))

    expect_true(.ignore_diryml_git(create = TRUE))
    lines <- readLines(".gitignore")
    expect_equal(sum(grepl(match_str_top, lines)), 1L)
    expect_equal(sum(grepl(match_str_bottom, lines)), 1L)

    writeLines(c("# Manual content", "*.tmp"), ".gitignore")
    expect_true(.ignore_diryml_git())
    lines <- readLines(".gitignore")
    expect_identical(head(lines, 2L), c("# Manual content", "*.tmp"))
    expect_true(any(grepl("_output", lines, fixed = TRUE)))
  })
})

test_that("directory ignore settings resolve defaults and reject conflicts", {
  skip_if(.is_test_select())
  for (type in c("git", "rbuild")) {
    expect_false(.ignore_get_setting("data", type, list()))
    expect_true(.ignore_get_setting("raw-data", type, list()))
    expect_true(.ignore_get_setting("DATA", type, list()))
    expect_true(.ignore_get_setting("data", type, list(ignore = TRUE)))
    expect_true(.ignore_get_setting("output", type, list()))
    expect_true(.ignore_get_setting("output", type, list(ignore = TRUE)))
    expect_false(.ignore_get_setting("output", type, list(ignore = FALSE)))
    expect_true(.ignore_get_setting("output", type, list(ignore = "ignore")))
    expect_false(.ignore_get_setting(
      "output",
      type,
      list(ignore = "no-ignore")
    ))
    expect_false(.ignore_get_setting("output", type, list(ignore = "manual")))

    setting <- paste0("ignore-", type)
    directory <- list()
    directory[[setting]] <- TRUE
    expect_true(.ignore_get_setting("output", type, directory))
    directory[[setting]] <- FALSE
    expect_false(.ignore_get_setting("output", type, directory))

    directory$ignore <- FALSE
    expect_false(.ignore_get_setting("output", type, directory))
    directory[[setting]] <- TRUE
    expect_error(
      .ignore_get_setting("output", type, directory),
      "Inconsistent ignore settings"
    )
    directory$ignore <- TRUE
    expect_true(.ignore_get_setting("output", type, directory))
  }
})

test_that("directory ignore paths are relative to the project root", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE)
  usethis::with_project(dir_test, {
    paths <- c("reports/archive/", file.path(dir_test, "reports/archive"))
    for (path in paths) {
      expect_equal(
        as.character(.ignore_diryml_path_get("raw", list(path = path))),
        "reports/archive"
      )
    }
    expect_identical(
      .ignore_diryml_path_get("DATA", list(path = "other")),
      "other"
    )
    for (path in c(
      ".",
      dir_test,
      "../outside",
      "Z:/foreign/data",
      "//foreign.example/share/data"
    )) {
      expect_length(.ignore_diryml_path_get("raw", list(path = path)), 0L)
    }
    expect_false(dir.exists("reports/archive"))
  })
})

test_that("absolute ignore paths may use an alias for the checkout's parent", {
  skip_if(.is_test_select())
  scratch <- withr::local_tempdir()
  parent <- file.path(scratch, "source")
  project <- file.path(parent, "study")
  fs::dir_create(project)
  alias <- file.path(scratch, "linked")
  skip_if_not(
    suppressWarnings(file.symlink(parent, alias)),
    "Directory symlink unavailable"
  )
  withr::defer(fs::link_delete(alias))
  path <- file.path(alias, "study", "future", "output-@uuid")
  expect_identical(
    .ignore_project_relative(path, normalizePath(project, winslash = "/")),
    "future/output-@uuid"
  )
  expect_false(dir.exists(path))
})

test_that("directory ignore rules cover configured paths and respect each ignore type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())
  dir_test <- .test_setup_project()
  usethis::with_project(dir_test, {
    yaml::write_yaml(
      list(
        directories = list(
          "output" = list(path = "results", "ignore-rbuild" = FALSE),
          "archive" = list(path = "archives+old", "ignore-git" = FALSE)
        )
      ),
      "_projr.yml"
    )
    unlink(c(".gitignore", ".Rbuildignore"))
    expect_true(.ignore_diryml_git())
    expect_true(.ignore_diryml_rbuild())

    git_ignored <- function(path) {
      system2("git", c("check-ignore", "--quiet", "--", path)) == 0L
    }
    expect_true(git_ignored("results/output"))
    expect_false(git_ignored("_raw_data/input"))
    expect_false(git_ignored("_tmp/cache"))
    expect_false(git_ignored(file.path(.yml_bd_get_output_dir(), "index.html")))
    expect_false(git_ignored("archives+old/copy"))

    rules <- readLines(".Rbuildignore")
    rbuild_ignored <- function(path) {
      any(vapply(
        rules[nzchar(rules)],
        grepl,
        logical(1),
        x = path,
        perl = TRUE
      ))
    }
    expect_true(rbuild_ignored("archives+old"))
    expect_true(rbuild_ignored("archives+old/copy"))
    expect_false(rbuild_ignored("archives+older/copy"))
    expect_false(rbuild_ignored("archivesold/copy"))
    expect_false(rbuild_ignored("results/output"))
    expect_false(dir.exists("archives+old"))
  })
})

test_that("package data stays visible in Git and in an R source package", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if_not_installed("pkgbuild")
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    config <- list(
      directories = list(
        data = list(path = "data"),
        `raw-data` = list(path = "_raw_data")
      )
    )
    yaml::write_yaml(config, "_projr.yml")
    desc::desc_set(
      Version = "0.1.0",
      License = "GPL-3",
      `Authors@R` = 'person("Test", "Author", email = "test@example.org", role = c("aut", "cre"))',
      Maintainer = "Test Author <test@example.org>"
    )
    package_sample <- c(3, 1, 4)
    projr_use_data(package_sample, safe = FALSE)
    dir.create("_raw_data", showWarnings = FALSE)
    writeLines("private", "_raw_data/observations.txt")
    writeLines("private", "data/private.txt")
    start <- "# Start of projr section"
    marker <- "# End of configured directory rules"
    end <- "# End of projr section"
    writeLines(
      c("/data/private.txt", start, "/data/**", marker, end),
      ".gitignore"
    )
    writeLines(
      c("^data/private[.]txt$", start, "^data(/|$)", marker, end),
      ".Rbuildignore"
    )

    projr_ignore_auto()
    ignored <- function(path) {
      system2("git", c("check-ignore", "--quiet", "--", path)) == 0L
    }
    expect_false(ignored("data/package_sample.rda"))
    expect_true(ignored("data/private.txt"))
    expect_true(ignored("_raw_data/observations.txt"))
    for (path in c(".gitignore", ".Rbuildignore")) {
      expect_false(marker %in% readLines(path))
    }
    tarball <- pkgbuild::build(
      ".",
      dest_path = withr::local_tempdir(),
      vignettes = FALSE,
      manual = FALSE,
      quiet = TRUE
    )
    files <- utils::untar(tarball, list = TRUE)
    expect_true("testProjr2/data/package_sample.rda" %in% files)
    expect_false(any(
      endsWith(files, "/private.txt") | endsWith(files, "/observations.txt")
    ))

    config$directories$data <- list(path = "unused", `ignore-git` = TRUE)
    yaml::write_yaml(config, "_projr.yml")
    projr_ignore_file("new-rule")
    expect_true(ignored("data/package_sample.rda"))
    expect_false(ignored("unused/example.rda"))
    expect_false("^data(/|$)" %in% readLines(".Rbuildignore"))

    config$directories$data <- list(path = "data", `ignore-rbuild` = TRUE)
    yaml::write_yaml(config, "_projr.yml")
    projr_ignore_file("another-rule")
    expect_false(ignored("data/package_sample.rda"))
    expect_true("^data(/|$)" %in% readLines(".Rbuildignore"))
  })
})

test_that("refresh preserves manual R build rules and validates the managed section", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE)
  usethis::with_project(dir_test, {
    start <- "# Start of projr section"
    end <- "# End of projr section"
    writeLines(c("^before$", start, end, "^after$"), ".Rbuildignore")
    expect_true(.ignore_diryml_rbuild())
    lines <- readLines(".Rbuildignore")
    expect_identical(lines[[1L]], "^before$")
    expect_identical(tail(lines[nzchar(lines)], 1L), "^after$")
    expect_true(match(start, lines) < match(end, lines))
    expect_equal(
      tail(readBin(".Rbuildignore", "raw", file.size(".Rbuildignore")), 1L),
      charToRaw("\n")
    )

    for (markers in list(c(start, start, end), c(start, end, end))) {
      writeLines(markers, ".Rbuildignore")
      expect_error(.ignore_diryml_rbuild(), "Multiple projr sections")
    }
    writeLines(start, ".Rbuildignore")
    expect_error(
      .ignore_diryml_rbuild(),
      "Found start of projr section but not end"
    )
    writeLines(end, ".Rbuildignore")
    expect_error(
      .ignore_diryml_rbuild(),
      "Found end of projr section but not start"
    )
    writeLines(c(end, start), ".Rbuildignore")
    expect_error(
      .ignore_diryml_rbuild(),
      "Start of projr section found after end"
    )
  })
})

test_that(".git_get_tracked returns tracked files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create and track files
      dir.create("tracked_dir")
      writeLines("content", "tracked_dir/tracked.txt")
      .git_commit_file("tracked_dir/tracked.txt", "Add tracked file")

      # Get tracked files
      tracked <- .git_get_tracked("tracked_dir")
      expect_true(length(tracked) > 0)
      expect_true(any(grepl("tracked.txt", tracked)))
    }
  )
})

test_that(".git_get_tracked returns empty for untracked directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directory but don't track files
      dir.create("untracked_dir")
      writeLines("content", "untracked_dir/untracked.txt")

      # Should return empty
      tracked <- .git_get_tracked("untracked_dir")
      expect_equal(length(tracked), 0)
    }
  )
})
