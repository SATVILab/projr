test_that("a fresh clone selects a recorded build and its code on the requested branch", {
  skip_if(.is_test_select())
  skip_if(!nzchar(Sys.which("git")))
  sandbox <- withr::local_tempdir()
  producer <- file.path(sandbox, "producer")
  upstream <- file.path(sandbox, "upstream.git")
  git <- function(...) {
    .git_run_write(shQuote(c(...)), stdout = TRUE, stderr = TRUE)
  }
  git("init", "--quiet", "-b", "analysis", producer)
  config <- withr::local_tempfile()
  withr::local_envvar(c(
    GIT_AUTHOR_NAME = "projr test",
    GIT_COMMITTER_NAME = "projr test",
    GIT_AUTHOR_EMAIL = "fixture@example.org",
    GIT_COMMITTER_EMAIL = "fixture@example.org",
    GIT_ALLOW_PROTOCOL = "file",
    GIT_CONFIG_GLOBAL = config
  ))
  usethis::with_project(
    producer,
    {
      save <- function(subject) {
        git("add", "-A")
        git("commit", "--quiet", "-m", subject)
        git("rev-parse", "HEAD")
      }
      record <- function(version) {
        .buildlog_add(
          "Analysis",
          "patch",
          list(desc = list(success = version)),
          1,
          summary = character(),
          destinations = character()
        )
      }
      writeLines("build: {git: false}", "_projr.yml")
      writeLines("print(1)", "analysis.R")
      writeLines("v0.0.1", "VERSION")
      before <- save("Snapshot pre-build")
      record("0.0.1")
      # Old projects kept this log at the root.
      expect_true(file.rename(".projr/BUILDLOG.md", "BUILDLOG.md"))
      first <- save("Build v0.0.1: analysis")
      writeLines("print(2)", "analysis.R")
      writeLines("v0.0.2", "VERSION")
      before_second <- save("Snapshot pre-build")
      .project_state_upgrade(producer)
      record("0.0.2")
      second <- save("Build v0.0.2: analysis")
      writeLines("print(3)", "analysis.R")
      tip <- save("Edit analysis after publishing")
      git("checkout", "-q", "-b", "feature/extra", first)
      writeLines("print('extra')", "analysis.R")
      other <- save("Edit feature code")
      git("checkout", "-q", "analysis")
      git("clone", "--quiet", "--bare", "--", producer, upstream)
      git(
        "config",
        "--file",
        config,
        paste0("url.", upstream, ".insteadOf"),
        "https://github.com/fixture/analysis.git"
      )

      reproduce <- function(
        ...,
        repo = upstream,
        path = tempfile(tmpdir = sandbox)
      ) {
        .reproduce_checkout(repo, ..., path = path)
      }
      empty <- file.path(sandbox, "fresh checkout")
      dir.create(empty)
      latest <- reproduce(path = empty)
      expect_identical(latest$version, "v0.0.2")
      expect_identical(latest$branch, "analysis")
      expect_identical(latest$build_commit, second)
      expect_identical(latest$code_commit, second)
      expect_identical(git("-C", empty, "rev-parse", "HEAD"), second)
      expect_identical(readLines(file.path(empty, "analysis.R")), "print(2)")
      expect_identical(reproduce(version = "0.0.1")$build_commit, first)
      expect_identical(
        reproduce(commit = substr(before_second, 1, 10))$code_commit,
        first
      )
      expect_identical(reproduce(commit = second)$build_commit, second)

      mixed <- reproduce(commit = tip, version = "v0.0.1")
      expect_identical(mixed$build_commit, first)
      expect_identical(mixed$code_commit, tip)
      expect_identical(
        readLines(file.path(mixed$path, "analysis.R")),
        "print(3)"
      )
      expect_identical(
        reproduce(commit = first, version = "0.0.2")$build_commit,
        second
      )
      expect_identical(
        reproduce(repo = "fixture/analysis@feature/extra")$build_commit,
        first
      )
      expect_error(reproduce(commit = before), "No completed build")
      expect_error(reproduce(version = "0.1.0"), "No completed build")
      expect_error(reproduce(commit = other), "outside branch")
      expect_error(reproduce(commit = "deadbeef"), "failed")
      expect_error(
        reproduce(repo = "fixture/analysis@feature/extra", branch = "analysis"),
        "disagree"
      )
      expect_error(reproduce(path = producer), "new or empty")
      withr::with_envvar(
        c(GIT_DIR = file.path(producer, ".git"), GIT_WORK_TREE = producer),
        {
          copy <- local({
            previous <- .reproduce_git_environment()
            on.exit(xfun::set_envvar(previous))
            expect_identical(Sys.getenv("GIT_ALLOW_PROTOCOL"), "file")
            expect_identical(Sys.getenv("GIT_CONFIG_GLOBAL"), config)
            reproduce(version = "0.0.1")
          })
          expect_identical(copy$code_commit, first)
          expect_identical(Sys.getenv("GIT_DIR"), file.path(producer, ".git"))
          expect_identical(Sys.getenv("GIT_WORK_TREE"), producer)
        }
      )
      expect_identical(git("rev-parse", "HEAD"), tip)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("a build logged at the next pre-build uses its own earlier code", {
  skip_if(.is_test_select())
  skip_if(!nzchar(Sys.which("git")))
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  git <- function(...) {
    .git_run_write(shQuote(c(...)), stdout = TRUE, stderr = TRUE)
  }
  git("init", "--quiet", "-b", "main", source)
  withr::local_envvar(c(
    GIT_AUTHOR_NAME = "projr test",
    GIT_COMMITTER_NAME = "projr test",
    GIT_AUTHOR_EMAIL = "fixture@example.org",
    GIT_COMMITTER_EMAIL = "fixture@example.org"
  ))
  usethis::with_project(
    source,
    {
      writeLines("build: {git: false}", "_projr.yml")
      writeLines("v1.0.0", "VERSION")
      writeLines("first", "input.R")
      git("add", "-A")
      git("commit", "-qm", "Snapshot pre-build")
      code <- git("rev-parse", "HEAD")
      writeLines("next", "input.R")
      writeLines("v1.1.0", "VERSION")
      .buildlog_add(
        "First analysis",
        "major",
        list(desc = list(success = "1.0.0")),
        1,
        summary = character(),
        destinations = character()
      )
      git("add", "-A")
      git("commit", "-qm", "Snapshot pre-build")
      baseline <- git("rev-parse", "HEAD")
      checkout <- .reproduce_checkout(".", path = file.path(root, "copy"))
      expect_identical(checkout$version, "v1.0.0")
      expect_identical(checkout$build_commit, baseline)
      expect_identical(checkout$code_commit, code)
      expect_identical(readLines(file.path(checkout$path, "input.R")), "first")

      # This older build appears with no corresponding pre-build code snapshot.
      .buildlog_add(
        "Imported analysis",
        "minor",
        list(desc = list(success = "1.2.0")),
        1,
        summary = character(),
        destinations = character()
      )
      git("add", "-A")
      git("commit", "-qm", "Snapshot pre-build")
      expect_error(
        .reproduce_checkout(".", path = file.path(root, "missing")),
        "Supply commit and version"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
