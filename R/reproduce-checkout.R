# A Git hook may set GIT_DIR or GIT_WORK_TREE. Keep operations in the clone
# even when the caller is already inside another repository's Git process.
.reproduce_git_environment <- function() {
  if (!nzchar(Sys.which("git"))) {
    stop(
      "Install Git to reproduce a build from its commit history.",
      call. = FALSE
    )
  }

  local_vars <- .git_run_write(
    c("rev-parse", "--local-env-vars"),
    stdout = TRUE
  )
  xfun::set_envvar(stats::setNames(
    rep(NA_character_, length(local_vars)),
    local_vars
  ))
}

# Clone first so a later push cannot change the branch or build being selected.
.reproduce_checkout <- function(
  repo,
  branch = NULL,
  commit = NULL,
  version = NULL,
  path
) {
  .assert_string(repo, TRUE)
  .assert_string(branch)
  .assert_string(path, TRUE)
  .assert_string(commit)
  .assert_string(version)
  .assert_detect_single(commit, "^[a-fA-F0-9]{4,64}$")
  .assert_detect_single(version, "^v?[0-9]+([.-][0-9]+)*$")
  path <- as.character(fs::path_abs(fs::path_expand(path)))
  if (
    fs::link_exists(path) ||
      (file.exists(path) &&
        (!dir.exists(path) ||
          length(list.files(path, all.files = TRUE, no.. = TRUE))))
  ) {
    stop(
      "Choose a new or empty directory for the reproduction checkout: ",
      path,
      call. = FALSE
    )
  }
  if (dir.exists(repo)) {
    source <- as.character(fs::path_real(repo))
  } else {
    .assert_detect_single(
      repo,
      "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+(@[^[:space:]]+)?$",
      TRUE
    )
    source <- sub("@.*$", "", repo)
    if (basename(source) %in% c(".", "..")) {
      stop("Invalid GitHub repository.", call. = FALSE)
    }
    if (repo != source) {
      inline <- substring(repo, nchar(source) + 2L)
      if (!is.null(branch) && branch != inline) {
        stop(
          "The branch in repo and the branch argument disagree.",
          call. = FALSE
        )
      }
      branch <- inline
    }
    source <- paste0("https://github.com/", source, ".git")
  }

  git <- function(...) {
    .git_run_write(shQuote(c(...)), stdout = TRUE, stderr = TRUE)
  }
  if (!is.null(branch)) {
    git("check-ref-format", paste0("refs/heads/", branch))
  }
  git("clone", "--quiet", "--no-checkout", "--no-local", "--", source, path)
  run <- function(...) git("-C", path, ...)
  if (is.null(branch)) {
    branch <- sub(
      "^refs/remotes/origin/",
      "",
      run("symbolic-ref", "refs/remotes/origin/HEAD")
    )
  }
  tip <- run(
    "rev-parse",
    "--verify",
    paste0("refs/remotes/origin/", branch, "^{commit}")
  )
  if (!is.null(commit)) {
    commit <- run("rev-parse", "--verify", paste0(commit, "^{commit}"))
    if (!identical(run("merge-base", commit, tip), commit)) {
      stop("Commit ", commit, " is outside branch ", branch, ".", call. = FALSE)
    }
  }

  # With both selectors, the version may be newer than the requested code.
  explicit_code <- !is.null(commit) && !is.null(version)
  limit <- if (is.null(version)) commit %||% tip else tip
  pattern <- if (is.null(version)) {
    "^#{2,4} v[0-9]+([.-][0-9]+)*:"
  } else {
    paste0(
      "^#{2,4} ",
      gsub(".", "[.]", .version_v_add(version), fixed = TRUE),
      ":"
    )
  }
  paths <- c(".projr/BUILDLOG.md", "BUILDLOG.md")
  header_at <- function(revision) {
    available <- run("ls-tree", "--name-only", revision, "--", paths)
    logfile <- intersect(paths, available)[1L]
    if (is.na(logfile)) {
      return(NA_character_)
    }
    lines <- run("show", paste0(revision, ":", logfile))
    grep(pattern, lines, perl = TRUE, value = TRUE)[1L]
  }
  history <- function(...) {
    run(
      "log",
      "--topo-order",
      "--format=%H",
      "--no-renames",
      "--diff-filter=AM",
      ...,
      limit,
      "--",
      paths
    )
  }
  header <- header_at(limit)
  if (is.na(header)) {
    # The selected revision may have removed the log or a particular entry.
    for (change in history(paste0("-G", pattern))) {
      header <- header_at(change)
      if (!is.na(header)) break
    }
  }
  if (is.na(header)) {
    stop(
      "No completed build",
      if (!is.null(version)) paste0(" ", version),
      " recorded on ",
      branch,
      " at or before ",
      limit,
      ".",
      call. = FALSE
    )
  }
  # Match the full dated header across both log locations. File moves and
  # later edits to the log do not turn those commits into the original build.
  build_commit <- history("--reverse", paste0("-S", header))[1L]
  version <- sub("^#{2,4} (v[0-9.-]+):.*", "\\1", header)

  code_commit <- if (explicit_code) commit else build_commit
  if (
    !explicit_code &&
      grepl(
        "^Snapshot pre-.*build$",
        run("show", "-s", "--format=%s", build_commit)
      )
  ) {
    # Some older builds first committed the previous build's log and manifest
    # when preparing the next one. That later commit can already have changed R code.
    candidates <- run(
      "log",
      "--first-parent",
      "--format=%H",
      "--grep=^Snapshot pre-.*build$",
      build_commit
    )
    code_commit <- NULL
    for (candidate in setdiff(candidates, build_commit)) {
      if (
        "VERSION" %in%
          run("ls-tree", "--name-only", candidate, "--", "VERSION") &&
          identical(
            .version_v_add(run("show", paste0(candidate, ":VERSION"))[1L]),
            version
          )
      ) {
        code_commit <- candidate
        break
      }
    }
    if (is.null(code_commit)) {
      stop(
        "The record for ",
        version,
        " first appears during a later pre-build. ",
        "Supply commit and version to select the code explicitly.",
        call. = FALSE
      )
    }
  }
  run("checkout", "--quiet", "--detach", code_commit)
  list(
    path = path,
    branch = branch,
    version = version,
    build_commit = build_commit,
    code_commit = code_commit
  )
}
