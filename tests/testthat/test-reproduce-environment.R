test_that("renv and pak install the selected analysis packages before rendering", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if_not_installed("pak")
  root <- withr::local_tempdir()
  repo <- file.path(root, "repository")
  contrib <- file.path(repo, "src", "contrib")
  fs::dir_create(contrib)
  withr::local_dir(root)
  make_package <- function(name, version, destination) {
    fs::dir_create(file.path(name, "R"))
    writeLines(
      c(
        paste("Package:", name),
        paste("Version:", version),
        "Title: Replay Fixture",
        "Description: A test package for offline replay.",
        "License: MIT",
        "Author: Test",
        "Maintainer: Test <test@example.org>"
      ),
      file.path(name, "DESCRIPTION")
    )
    writeLines("export(replay_value)", file.path(name, "NAMESPACE"))
    writeLines(
      paste0('replay_value <- function() "', version, '"'),
      file.path(name, "R", "value.R")
    )
    utils::tar(
      file.path(destination, paste0(name, "_", version, ".tar.gz")),
      name,
      compression = "gzip",
      tar = "internal"
    )
  }
  # This small stand-in for a locked projr exposes a function absent in the
  # current replay driver. Its value must reach renderers in fresh R sessions.
  make_package("projr", "0.0.1", contrib)
  make_package("replayprobe", "1.2.3", contrib)
  tools::write_PACKAGES(contrib, type = "source")
  repo_path <- normalizePath(repo, winslash = "/")
  repos <- c(
    CRAN = paste0(
      "file://",
      if (!startsWith(repo_path, "/")) "/",
      utils::URLencode(repo_path)
    )
  )
  withr::local_options(list(repos = repos))
  caller <- file.path(root, "caller")
  fs::dir_create(c(caller, file.path(caller, "lib")))
  profile <- file.path(caller, ".Rprofile")
  writeLines("stop('used caller profile')", profile)
  withr::local_envvar(c(
    R_PROFILE_USER = profile,
    RENV_PROJECT = caller,
    RENV_PATHS_LIBRARY_ROOT = file.path(caller, "lib"),
    RENV_PATHS_LOCKFILE = file.path(caller, "wrong.lock"),
    PKG_CRAN_MIRROR = unname(repos),
    # pkgcache refuses the user cache during R CMD check.
    R_USER_CACHE_DIR = file.path(root, "user-cache"),
    PKG_USE_BIOCONDUCTOR = "false",
    PKG_SYSREQS_DB_UPDATE = "false",
    http_proxy = "http://127.0.0.1:9",
    https_proxy = "http://127.0.0.1:9",
    ALL_PROXY = "http://127.0.0.1:9",
    NO_PROXY = "",
    no_proxy = ""
  ))
  for (method in c("renv", "pak")) {
    project <- file.path(root, method)
    scratch <- file.path(root, paste0("stage-", method))
    fs::dir_create(c(project, scratch))
    usethis::with_project(
      project,
      {
        writeLines(
          c(
            "directories:",
            "  cache: {path: scratch}",
            "  output: {path: artifacts}",
            "build: {scripts: analyze.R, git: false}"
          ),
          "_projr.yml"
        )
        writeLines("v1.0.0", "VERSION")
        writeLines(
          c(
            "Package: replayfixture",
            "Version: 1.0.0",
            "Title: Replay Fixture",
            "Description: Validate an archived project.",
            "License: MIT",
            "Author: Test",
            "Maintainer: Test <test@example.org>",
            "Imports: replayprobe"
          ),
          "DESCRIPTION"
        )
        if (method == "renv") {
          fs::dir_create("renv/cellar")
          file.copy(
            list.files(contrib, pattern = "[.]tar[.]gz$", full.names = TRUE),
            "renv/cellar"
          )
          jsonlite::write_json(
            list(
              R = list(
                Version = as.character(getRversion()),
                Repositories = list(list(Name = "CRAN", URL = unname(repos)))
              ),
              Packages = list(
                projr = list(
                  Package = "projr",
                  Version = "0.0.1",
                  Source = "Local"
                ),
                replayprobe = list(
                  Package = "replayprobe",
                  Version = "1.2.3",
                  Source = "Local"
                )
              )
            ),
            "renv.lock",
            auto_unbox = TRUE
          )
        }
        read_value <- if (method == "renv") {
          "projr::replay_value()"
        } else {
          "replayprobe::replay_value()"
        }
        child <- paste0("cat(", read_value, ")")
        writeLines(
          c(
            paste0("writeLines(", read_value, ", 'artifacts/value.txt')"),
            paste0(
              "child <- system2(file.path(R.home('bin'), 'Rscript'), c('-e', shQuote(",
              deparse(child),
              ")), stdout = TRUE)"
            ),
            "writeLines(child, 'artifacts/child.txt')"
          ),
          "analyze.R"
        )
        info <- .reproduce_run("v1.0.0", scratch)
        expected <- if (method == "renv") "0.0.1" else "1.2.3"
        expect_identical(readLines("artifacts/value.txt"), expected)
        expect_identical(readLines("artifacts/child.txt"), expected)
        expect_identical(info$method, method)
        expect_identical(info$pinned, method == "renv")
        expect_identical(
          info$packages$version[info$packages$package == "replayprobe"],
          "1.2.3"
        )
        expect_identical(Sys.getenv("R_PROFILE_USER"), profile)
        expect_identical(Sys.getenv("RENV_PROJECT"), caller)
        expect_length(
          list.files(file.path(caller, "lib"), all.files = TRUE, no.. = TRUE),
          0L
        )
      },
      quiet = TRUE,
      force = TRUE
    )
  }
})
