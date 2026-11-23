test_that("reproduction restores the selected inputs and compares an independent code commit", {
  skip_if(.is_test_select())
  skip_if(!nzchar(Sys.which("git")))
  base <- withr::local_tempdir()
  producer <- file.path(base, "analysis")
  fs::dir_create(producer)
  git <- function(...) {
    .git_run_write(
      shQuote(c("-C", producer, ...)),
      stdout = TRUE,
      stderr = TRUE
    )
  }
  git("init", "-q", "-b", "analysis")
  git("config", "user.name", "projr fixture")
  git("config", "user.email", "projr@example.org")
  usethis::with_project(
    producer,
    {
      dir.create("old-results")
      writeLines(
        c(
          "directories:",
          "  raw-data: {path: incoming}",
          "  output: {path: old-results}",
          "  cache: {path: scratch}",
          "remotes: {local: {storage: {path: history}}}",
          "build: {scripts: calculate.R, git: false}",
          "dev: {scripts: skip.R}"
        ),
        "_projr.yml"
      )
      writeLines("stop('dev scripts ran')", "skip.R")
      writeLines(
        c(
          "stopifnot(!file.exists('old-results/answer.txt'))",
          "writeLines(readLines('incoming/data.csv'), 'old-results/answer.txt')"
        ),
        "calculate.R"
      )
      writeLines("v1.0.0", "VERSION")
      fs::dir_create("history/raw-data/v1.0.0")
      writeLines("actual data", "history/raw-data/v1.0.0/data.csv")
      writeLines("stale committed result", "old-results/answer.txt")
      rows <- data.frame(
        label = c("raw-data", "output"),
        version = "v1.0.0",
        fn = c("data.csv", "answer.txt"),
        hash = unname(tools::md5sum("history/raw-data/v1.0.0/data.csv"))
      )
      .manifest_store_write(rows)
      .manifest_store_write(rows[1, ], "history/manifests")
      .buildlog_add(
        "First build",
        "major",
        list(desc = list(success = "1.0.0")),
        1,
        summary = character(),
        destinations = character()
      )
      git("add", ".")
      git("commit", "-qm", "Completed first build")
      first <- git("rev-parse", "HEAD")
      writeLines(
        "writeLines('new code', 'old-results/answer.txt')",
        "calculate.R"
      )
      git("commit", "-qam", "Change analysis after the build")
      changed <- git("rev-parse", "HEAD")

      replay <- projr_reproduce(
        producer,
        commit = changed,
        path = file.path(base, "first")
      )
      expect_true(replay$exact)
      expect_identical(replay$code_commit, first)
      expect_identical(replay$files$status, "identical")
      expect_identical(replay$dependencies$method, "session")
      expect_identical(
        readLines(file.path(replay$path, "incoming", "data.csv")),
        "actual data"
      )

      alternate <- projr_reproduce(
        producer,
        commit = changed,
        version = "1.0.0",
        path = file.path(base, "other")
      )
      expect_false(alternate$exact)
      expect_identical(alternate$build_commit, first)
      expect_identical(alternate$code_commit, changed)
      expect_identical(alternate$files$status, "changed")
      expect_identical(alternate$directories$changed, 1L)
      expect_identical(
        readLines("old-results/answer.txt"),
        "stale committed result"
      )
      expect_identical(git("rev-parse", "HEAD"), changed)
    },
    quiet = TRUE,
    force = TRUE
  )
})
