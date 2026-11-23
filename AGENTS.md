# AGENTS.md

Configuration for autonomous coding agents (e.g., Google Jules, GitHub Copilot) working on the `projr` R package.

---

## Core Philosophy / Project Context

`projr` is an R package that facilitates reproducible and archived research projects. Its primary goal is to automate the full lifecycle of a research build: rendering documents, bumping versions, committing to Git, and archiving outputs to remote destinations (GitHub Releases, OSF, or local directories).

Key architectural patterns:

- **Single build entry point**: `projr_build_patch/minor/major()` for production; `projr_build_dev()` for development.
- **YAML-driven configuration**: Create `_projr.yml` at the project root by default. Also read default, named and local configuration files in `_projr/config/`. A root file takes precedence over a nested file of the same name. Update the selected file in place and interpret configured paths relative to the project root.
- **Manifest tracking**: Production builds record each hashed directory independently under `_projr/manifests/<label>/`. Its `versions.csv` maps project versions to the earliest matching directory version and the SHA-256 of a file list. Each distinct list of relative paths and MD5 hashes is stored once.
- **Remote destinations**: A unified dispatcher pattern (`R/remote.R`) routes file operations to local, GitHub, or OSF backends.
- **Internal vs. exported functions**: Internal helpers use a `.` prefix (e.g., `.hash_file()`); exported functions use the `projr_` prefix (e.g., `projr_build_patch()`).

A project with a legacy root `manifest.csv` can be queried and restored before migration. The next production build imports its history. Local destinations store directory histories in `manifests/`. GitHub Releases use one `manifest.zip` asset. A local latest destination with `path-append-label: false` shares its root with release metadata. Preserve that metadata across payload purges. Confirm an empty build from the destination's `VERSION` and matching directory snapshot before clearing local files on restore.

Set the file hashing approach and worker limit under `settings.hashing`. The defaults are `approach: cpp` and `cores: 1`. A positive whole-number core count can parallelize file hashing with either the C++ or historical `legacy` approach. Both preserve file ordering, identical MD5s and per-directory histories. Existing `settings.hash-engine` configurations remain readable.

Production builds use `build.renv: auto` by default. Snapshot when the project has its renv lockfile or activation script. `true` can create the first lockfile using installed packages, and `false` disables snapshots. Check dependencies before remote preparation and rendering. Take the post-render snapshot noninteractively and report missing package names before modifying the lockfile.

### Build script selection and rendering

- `projr_build_dev(scripts = ...)` takes precedence over `dev.scripts`, then `build.scripts`. Production builds use `build.scripts`. Both use the merged active configuration. An explicit empty selection (`character()` in R or `[]` in YAML) skips rendering and script execution.
- Resolve paths and glob patterns in `_projr.yml` from the project root. Keep each renderer's default code execution directory and honor its native options, including Quarto's `execute-dir` and R Markdown's `knit_root_dir`. Run Quarto synchronously so output collection and post-build hooks see the completed render in RStudio.
- Run explicitly selected scripts in the listed order. Sort matches alphabetically within each glob, apply `!` exclusions across the selection, and keep repeated literal R scripts and standalone documents in place. Overlapping glob matches run once. Quarto and Bookdown projects reject explicit repeats of a project document because they render each member only once.
- Build a Quarto or Bookdown project once at the position of its last selected project document. Store selected documents in Quarto's `project.render` or `book.chapters`, or Bookdown's `rmd_files`. Standalone documents and R scripts retain their positions around that build.
- An explicit R-only selection runs those scripts without rendering the project. A document for the other engine renders as a standalone document. A directory with both project configurations is ambiguous and should report an error.
- With automatic selection, render the project first and run top-level R scripts alphabetically afterward. With no project, run top-level documents and R scripts alphabetically. Automatic R discovery skips configured hooks, underscore-prefixed files, and `renv_repos.R`.
- Save the original project YAML bytes before preparation, apply the selection after the pre-build commit, and keep it through output collection and post-build hooks. Restore the saved bytes immediately before switching to the dev version and on failure. Render Bookdown in a fresh R session so a preceding standalone render cannot supply another book's configuration.
- Keep source documents and their directories trackable. Ignore renderer-generated output and intermediates at their project-relative paths, including beside nested documents. Install predictable rules before rendering and retain ignore rules for newly created intermediates after a failed render.
- Collect project outputs from the configured safe documentation directory. Collect standalone outputs and their companion assets together in the configured documentation directory. When names collide, keep the affected documents in their project-relative subdirectories and report any remaining conflicts before collecting outputs.

### Including directories in an output

- Set `directories.<receiving output>.include` to declared raw, docs or output labels. Copy each complete directory to a subdirectory named for its source label, including nested outputs in dependency order.
- Collect development copies in the build cache and production copies in the configured output before hashing or archiving it. Preserve existing source-side `output` settings. A request made in both places copies once.
- Reserve the receiving subdirectory for the included label. Reject cycles, conflicting files or links, and directory paths that would copy recursively or clear the source.

### Directory ignores across profiles

- When updating `.gitignore`, combine directory rules from active and inactive profiles per literal project-relative path. Apply the same logic to `.Rbuildignore` when the project root contains `DESCRIPTION`. Include a path for each ignore type enabled by any contributing profile.
- `directories.<label>.ignore-even-if-inactive` defaults to `true` and extends the directory's existing `ignore`, `ignore-git`, and `ignore-rbuild` settings. With `false`, that profile contributes the directory's rules only while active.
- Generate rules only for paths inside the checkout. Skip foreign absolute paths in inactive profiles without accessing or validating their directories.
- Refresh directory rules from the current configuration. When the last qualifying profile reference to a path is removed, drop its generated rule on the next refresh. On the first manual edit to a legacy managed block, regenerate its contents and preserve rules outside the block.

### Destination and directory templates

- Expand `@version`, `@branch`, and `@uuid` in configured directory paths, local destination names and paths, and GitHub release names. A token ends at the end of the string or before a hyphen, underscore, period, ordinary space, or path separator. Preserve unknown names literally.
- Use the build version with one `v` prefix. Capture one UUIDv4 and Git HEAD per build and share them with child render processes and retries. Encode branch names as one portable path component, preserving the distinction between `feature/foo` and `feature-foo`. Detached HEAD uses the commit SHA.
- Generate directory ignore patterns from the current YAML templates, including local archives. Keep the template in YAML and record the concrete archive destination in `BUILDLOG.md` for restore.
- Store local release histories in `<destination>/manifests/` for every layout. With `path-append-label: false` and `structure: latest`, keep the root's `manifests/`, `manifest.csv`, `VERSION`, and `CHANGELOG.md` separate from payload listing, hashing, purge, and restore. Report a conflicting payload name before writing.
- Keep a GitHub release's title and tag identical. Only an explicitly configured target branch selects the commit for a new tag. Resolve that branch in the destination repository and leave existing release tags in place.

---

## Tech Stack & Tooling

### Language

- **R** (≥ 4.1.0) in `R/`, with native manifest code in `src/` registered through `cpp11`

### Core R package dependencies (Imports)

| Package | Purpose |
|---------|---------|
| `renv` | Reproducible dependency management |
| `yaml` | Read/write `_projr.yml` |
| `jsonlite` | JSON serialisation |
| `rprojroot` | Reliable project-root detection |
| `desc` | Read/write `DESCRIPTION` |
| `fs` | File-system operations |
| `digest` | SHA-256 snapshot IDs and historical MD5 hashing |
| `uuid` | Random UUIDv4 identifiers for build templates |
| `filelock` | Serialize concurrent updates to project manifests |
| `cli` | Console output |
| `glue` | String interpolation |

### Suggested / optional dependencies

`gert`, `gh`, `gitcreds`, `credentials` (Git/GitHub), `httr` (GitHub Releases API), `osfr` (OSF), `rmarkdown`, `quarto`, `bookdown`, `knitr` (document rendering), `testthat` ≥ 3.0 (testing), `devtools`, `usethis`, `styler`, `roxygen2` (development).

### What NOT to use

- **Do not use `piggyback`** for GitHub Release operations — all GitHub API calls go through `httr` directly (see `R/remote-github-httr.R`).
- **Do not use the `magrittr` pipe (`%>%`)** — use the native R pipe `|>` instead.
- **Do not edit files in `man/`** — they are auto-generated by `roxygen2`.

### Document rendering

- Quarto (`quarto-dev/quarto-actions`)
- R Markdown / Bookdown

---

## Setup Commands

Run these commands once to configure a local development environment.

### 1. Install system dependencies (Ubuntu / Debian)

```bash
sudo apt-get update -y
sudo apt-get install --no-install-recommends -y \
  libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev \
  libgit2-dev libjpeg-dev libpng-dev libx11-dev pandoc
```

### 2. Install Quarto

Follow the instructions at <https://quarto.org/docs/get-started/> or use:

```bash
# Via quarto-dev/quarto-actions (CI)
# Locally: download installer from https://quarto.org
```

### 3. Restore R dependencies with renv

```r
# Inside an R session at the project root:
renv::restore()
```

### 4. Load the package for interactive development

```r
devtools::load_all()
```

---

## Build & Test Instructions

### Run the test suite (LITE mode — recommended for development)

```r
devtools::load_all()
.test_set_lite()
devtools::test()
```

LITE mode runs ~364 core tests in ~2.5 minutes. It must be explicitly enabled with `.test_set_lite()` before each session; without it, `devtools::test()` runs the full suite.

### Run the full test suite

```r
devtools::load_all()
devtools::test()
```

Full mode runs ~452 tests and is used for pre-release validation.

### Debug a specific test

```r
devtools::load_all()
.test_unset_lite()   # Disable LITE mode so the target test runs
.test_set_select()   # Skip all other tests

# Temporarily comment out skip_if(.is_test_select()) in the target test file
devtools::test()

# Restore when done:
.test_unset_select()
```

### R CMD check

```r
devtools::check()
```

### Lint / format code

```r
styler::style_pkg()
```

### Update documentation

```r
devtools::document()   # Regenerates man/ and NAMESPACE from roxygen2 comments
```

### Verify pkgdown reference index

```r
pkgdown::check_pkgdown()
```

---

## Coding Style & Conventions

### Naming

- **Exported functions**: `projr_` prefix, `snake_case` (e.g., `projr_build_patch()`).
- **Internal functions**: `.` prefix, `snake_case` (e.g., `.hash_file()`).
- **Variables**: descriptive `snake_case`; avoid single-letter or highly abbreviated names.
- **Temporal pairs**: prefer `before`/`after`, `previous`/`current`, `old`/`new` over generic `_pre`/`_post` suffixes.
- **File paths**: use `filename` (name only) or `filepath` (full path); use plural (`filenames`, `filepaths`) for vectors.

### Code style

- Indent with **2 spaces**.
- Use the **native pipe** `|>`, not `%>%`.
- Keep functions focused and modular.
- Validate public inputs before modifying files or contacting a remote. Use the shared `.assert_*()` helpers from `R/check.R`. Use `.assert_flag()` for scalar switches, `.assert_in_single()` for a single choice and `.assert_count()` for whole-number counts. Optional NULL arguments pass validation.

### Documentation (roxygen2)

All **exported** functions must include:

```r
#' @title Short title
#' @description Detailed description.
#' @param x character. Description of x.
#' @return Describe the return value.
#' @export
#' @examples
#' \dontrun{
#'   projr_example()
#' }
```

Internal functions (`.` prefix) must **not** have `@export`.

### Logging

- Use `.cli_debug()`, `.cli_info()`, `.cli_success()`, `.cli_step()` — they automatically read the `PROJR_OUTPUT_LEVEL` environment variable.
- Do **not** pass `output_level` as a function parameter.
- Do **not** log inside tight loops over many files; log summaries instead.

### Tests

- Framework: `testthat` edition 3 (`Config/testthat/edition: 3` in `DESCRIPTION`).
- Test files: `tests/testthat/test-{feature}.R`.
- Helper functions: `tests/testthat/helper-*.R` (not in `R/`).
- Add `skip_if(.is_test_select())` to every test to enable selective debugging.
- Add `skip_if(.is_test_lite())` to comprehensive / exhaustive-parameter tests.
- Add `skip_if(.is_test_cran())` to any test that is slow or hits the network.
- Wrap tests that need a temporary project in `usethis::with_project()`.

### Commits / PRs

- Make **minimal, surgical changes** — change as few lines as possible.
- Run `devtools::document()`, `devtools::test()` (LITE), and `styler::style_pkg()` before committing.
- Update `_pkgdown.yml` when adding or removing exported functions, then verify with `pkgdown::check_pkgdown()`.
- Ensure files end with a **single newline** and have **no trailing whitespace**.
- Never leave `browser()` or unguarded interactive debugging calls in committed code.

### Environment variables

| Variable | Values | Purpose |
|----------|--------|---------|
| `PROJR_AUTO_INSTALL` | `TRUE` / `FALSE` | Auto-install missing R packages without prompting |
| `PROJR_OUTPUT_LEVEL` | `none` / `std` / `debug` | Console verbosity |
| `PROJR_CLEAR_OUTPUT` | `pre` / `post` / `never` | When to clear output dirs |
| `PROJR_LOG_DETAILED` | `TRUE` / `FALSE` | Create detailed log files |
| `R_PKG_TEST_LITE` | set / unset | Enable LITE test mode |
| `R_PKG_TEST_CRAN` | set / unset | Enable CRAN test mode |
| `R_PKG_TEST_SELECT` | set / unset | Enable SELECT test mode |
| `GITHUB_PAT` | token | GitHub authentication (highest priority) |
| `OSF_PAT` | token | OSF authentication |


## Ponytail

For coding, refactoring, bug-fixing, review and implementation design, read
`.agents/skills/ponytail/SKILL.md` and apply Ponytail in **full** mode by
default. Use **ultra** only when explicitly requested. Repository-specific
correctness, security, architecture and validation requirements take precedence
over generic simplification guidance.

## GitHub issues and Projects

For issue and Project administration, use the canonical
[MiguelRodo/github-projects-skill](https://github.com/MiguelRodo/github-projects-skill/tree/main/skills/github-projects)
skill and read `.projects/project.md` for this repository's routing contract.
The central issue store owns planning; this repository is its implementation mirror.
