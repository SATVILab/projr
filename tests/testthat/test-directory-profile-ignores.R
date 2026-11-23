test_that("directory ignores are combined per profile, path and ignore type", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  withr::local_envvar(PROJR_PROFILE = "lab")

  usethis::with_project(
    dir_test,
    {
      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(
              path = "shared/measurements",
              "ignore-git" = TRUE,
              "ignore-rbuild" = FALSE
            )
          )
        ),
        "_projr.yml"
      )
      yaml::write_yaml(
        list(
          directories = list(
            "raw-lab" = list(
              path = "local/scratch",
              "ignore-even-if-inactive" = FALSE
            )
          )
        ),
        "_projr-lab.yml"
      )
      yaml::write_yaml(
        list(
          directories = list(
            "raw-hpc" = list(
              path = "shared/measurements",
              "ignore-git" = FALSE,
              "ignore-rbuild" = TRUE
            ),
            "raw-archive" = list(
              path = "archive/assays",
              "ignore-rbuild" = FALSE
            ),
            "raw-study" = list(path = "study+[x]"),
            "raw-private" = list(
              path = "hpc/private",
              "ignore-even-if-inactive" = FALSE
            )
          )
        ),
        "_projr-hpc.yml"
      )
      yaml::write_yaml(
        list(
          directories = list(
            "raw-foreign" = list(path = "Z:/unmounted/archive")
          )
        ),
        "_projr-foreign.yml"
      )

      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(path = "desktop/measurements")
          )
        ),
        "_projr-local.yml"
      )

      git_ignored <- function(path) {
        system2("git", c("check-ignore", "--quiet", "--", shQuote(path))) == 0L
      }
      rbuild_ignored <- function(path) {
        patterns <- readLines(".Rbuildignore")
        patterns <- patterns[nzchar(patterns) & !startsWith(patterns, "#")]
        any(vapply(patterns, grepl, logical(1), x = path, perl = TRUE))
      }

      dir.create("analysis")
      file.remove(".gitignore", ".Rbuildignore")
      withr::with_dir("analysis", projr_ignore_file("private-notes.txt"))
      expect_true(git_ignored("private-notes.txt"))
      expect_true(rbuild_ignored("private-notes.txt"))
      expect_true(git_ignored("shared/measurements/sample"))
      expect_true(rbuild_ignored("shared/measurements/sample"))
      expect_true(git_ignored("desktop/measurements/sample"))
      expect_false(rbuild_ignored("desktop/measurements/sample"))
      expect_true(git_ignored("archive/assays/sample"))
      expect_false(rbuild_ignored("archive/assays/sample"))
      expect_true(git_ignored("local/scratch/sample"))
      expect_true(rbuild_ignored("local/scratch/sample"))
      expect_false(git_ignored("hpc/private/sample"))
      expect_false(rbuild_ignored("hpc/private/sample"))
      expect_false(git_ignored("analysis/archive/assays/sample"))
      expect_true(git_ignored("study+[x]/sample"))
      expect_true(rbuild_ignored("study+[x]/sample"))
      expect_false(git_ignored("study+x/sample"))
      expect_false(rbuild_ignored("studyyx/sample"))
      expect_false(any(grepl(
        "unmounted",
        c(readLines(".gitignore"), readLines(".Rbuildignore"))
      )))

      Sys.setenv(PROJR_PROFILE = "hpc")
      withr::with_dir("analysis", projr_ignore_auto())
      expect_true(git_ignored("shared/measurements/sample"))
      expect_true(rbuild_ignored("shared/measurements/sample"))
      expect_false(git_ignored("local/scratch/sample"))
      expect_false(rbuild_ignored("local/scratch/sample"))
      expect_true(git_ignored("hpc/private/sample"))
      expect_true(rbuild_ignored("hpc/private/sample"))
      expect_true(git_ignored("private-notes.txt"))
      expect_true(rbuild_ignored("private-notes.txt"))

      rules <- lapply(c(".gitignore", ".Rbuildignore"), readLines)
      projr_ignore_auto()
      expect_identical(
        lapply(c(".gitignore", ".Rbuildignore"), readLines),
        rules
      )
    },
    force = TRUE
  )
})

test_that("profile directory rules follow current paths and preserve manual ignores", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  withr::local_envvar(PROJR_PROFILE = NA_character_)

  usethis::with_project(
    dir_test,
    {
      file.remove("DESCRIPTION")
      writeLines("# Keep existing manual rules", ".Rbuildignore")
      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(path = "private/old")
          )
        ),
        "_projr-desktop.yml"
      )

      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(path = "hpc/data"),
            "raw-shared" = list(path = "private/old")
          )
        ),
        "_projr-hpc.yml"
      )
      projr_ignore_file_git("private/old/notes.txt")
      projr_ignore_auto()
      git_ignored <- function(path) {
        system2("git", c("check-ignore", "--quiet", "--", path)) == 0L
      }
      expect_true(git_ignored("private/old/data"))
      expect_true(git_ignored("hpc/data/sample"))
      expect_false(any(grepl(
        "private/old",
        readLines(".Rbuildignore"),
        fixed = TRUE
      )))

      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(path = "new/location")
          )
        ),
        "_projr-desktop.yml"
      )
      projr_ignore_auto()
      expect_true(git_ignored("new/location/data"))
      expect_true(git_ignored("private/old/data"))
      expect_false(any(grepl(
        "new/location",
        readLines(".Rbuildignore"),
        fixed = TRUE
      )))
      expect_true(
        "# Keep existing manual rules" %in% readLines(".Rbuildignore")
      )

      yaml::write_yaml(
        list(
          directories = list(
            "raw-data" = list(path = "hpc/data")
          )
        ),
        "_projr-hpc.yml"
      )
      projr_ignore_auto()
      expect_false(git_ignored("private/old/data"))
      expect_true(git_ignored("private/old/notes.txt"))
      expect_true(git_ignored("new/location/data"))
      expect_true(git_ignored("hpc/data/sample"))
    },
    force = TRUE
  )
})

test_that("a manual addition refreshes an older projr-managed ignore block", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    yaml::write_yaml(
      list(directories = list("raw-data" = list(path = "current"))),
      "_projr.yml"
    )
    writeLines("generated", "render.log")
    start <- "# Start of projr section"
    end <- "# End of projr section"
    writeLines(
      c("before", start, "old/**", "render.log", end, "after"),
      ".gitignore"
    )
    projr_ignore_file_git("new-manual")

    writeLines(
      c("^before$", start, "^old/", "^old$", "^render\\.log$", end, "^after$"),
      ".Rbuildignore"
    )
    projr_ignore_file_rbuild("new-manual")

    git <- readLines(".gitignore")
    rbuild <- readLines(".Rbuildignore")
    expect_false("old/**" %in% git)
    expect_false(any(c("^old/", "^old$") %in% rbuild))
    expect_true(all(
      c("/current/**", "render.log", "new-manual", "before", "after") %in% git
    ))
    expect_true(all(
      c(
        "^current(/|$)",
        "^render\\.log$",
        "^new-manual$",
        "^before$",
        "^after$"
      ) %in%
        rbuild
    ))
    expect_lt(match("before", git), match(start, git))
    expect_gt(match("after", git), match(end, git))
    expect_lt(match("^before$", rbuild), match(start, rbuild))
    expect_gt(match("^after$", rbuild), match(end, rbuild))
  })
})

test_that("manual ignore edits keep a build's nested render and failure rules", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if_not_installed("rmarkdown")
  project <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(project, {
    dir.create("reports")
    writeLines(
      c("---", "title: Report", "output: html_document", "---"),
      "reports/report.Rmd"
    )
    writeLines(
      c(
        'projr::projr_ignore_file("local-note.txt")',
        'writeLines("intermediate", "reports/unexpected.tex")',
        'stop("deliberate build failure")'
      ),
      "fail.R"
    )
    expect_error(
      projr_build_dev(
        project = project,
        scripts = c("fail.R", "reports/report.Rmd")
      ),
      "deliberate build failure"
    )
    rbuild <- readLines(".Rbuildignore")
    for (file in c("report.tex", "unexpected.tex")) {
      path <- file.path("reports", file)
      expect_equal(system2("git", c("check-ignore", "--quiet", "--", path)), 0L)
      expect_true(any(vapply(
        rbuild[nzchar(rbuild)],
        grepl,
        logical(1),
        x = path
      )))
    }
    projr_ignore_auto()
    expect_true(
      "/reports/unexpected.tex" %in% .ignore_path_get_list(".gitignore")$start
    )
  })
})

test_that("templated directories and destinations cover parallel builds from the current YAML", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    .yml_set(list(
      directories = list(
        "raw-data" = list(path = "profiles/lab"),
        output = list(path = "results/@branch/build-@uuid"),
        cache = list(path = "cache-@branch"),
        "raw-literal" = list(path = "@branching")
      ),
      build = list(local = list(archive = list(path = "out+put/export-@uuid")))
    ))
    .yml_set(
      list(directories = list("raw-data" = list(path = "local/lab"))),
      "local"
    )
    .yml_set(
      list(directories = list("raw-data" = list(path = "profiles/cluster"))),
      "cluster"
    )

    projr_ignore_auto()
    git_ignored <- function(path) {
      system2("git", c("check-ignore", "--quiet", "--", shQuote(path))) == 0L
    }
    rbuild_ignored <- function(path) {
      rules <- readLines(".Rbuildignore")
      any(vapply(
        rules[nzchar(rules)],
        grepl,
        logical(1),
        x = path,
        perl = TRUE
      ))
    }
    for (ignored in list(git_ignored, rbuild_ignored)) {
      for (path in c("profiles/lab", "profiles/cluster", "local/lab")) {
        expect_true(ignored(file.path(path, "sample")))
      }
      for (uuid in c(
        "12345678-1234-4123-8123-000000000001",
        "12345678-1234-4123-b123-000000000002"
      )) {
        expect_true(ignored(paste0(
          "results/feature_2fa/build-",
          uuid,
          "/plot"
        )))
        expect_true(ignored(paste0("out+put/export-", uuid, "/plot")))
      }
      expect_true(ignored("@branching/input"))
      expect_false(ignored("sibling/input"))
      expect_false(ignored("results/feature_2fa/build-author-draft/plot"))
      expect_false(ignored(
        "results/deep/nested/build-12345678-1234-4123-8123-000000000001/plot"
      ))
    }
    # The same generated path can belong to an archive and a directory.
    # Moving the directory on a manual refresh must keep the archive rule.
    settings <- projr_yml_get("default")
    settings$directories$output$path <- settings$build$local$archive$path
    .yml_set(settings)
    projr_ignore_auto()
    settings$directories$output$path <- "new-results"
    .yml_set(settings)
    projr_ignore_file("notes.txt")
    for (ignored in list(git_ignored, rbuild_ignored)) {
      expect_true(ignored("new-results/plot"))
      expect_true(ignored(
        "out+put/export-12345678-1234-4123-8123-000000000001/plot"
      ))
    }

    settings$directories$output$path <- "results/@branch/build-@uuid"
    .yml_set(settings)
    restore <- .template_context_start()
    on.exit(restore(), add = TRUE)
    path <- projr_path_get_dir("output", create = FALSE, safe = FALSE)
    expect_identical(path, .template_expand("results/@branch/build-@uuid"))
    expect_identical(
      .dir_get_cache_auto_path(NULL),
      .template_expand("cache-@branch")
    )
    expect_false(grepl("@", path, fixed = TRUE))
    expect_identical(
      .yml_dir_get_path_raw("output", NULL),
      "results/@branch/build-@uuid"
    )
  })
})
