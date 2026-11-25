# AGENTS.md

Configuration for autonomous coding agents (e.g., Google Jules, GitHub
Copilot) working on the `projr` R package.

------------------------------------------------------------------------

## Core Philosophy / Project Context

`projr` is an R package that facilitates reproducible and archived
research projects. Its primary goal is to automate the full lifecycle of
a research build: rendering documents, bumping versions, committing to
Git, and archiving outputs to remote destinations (GitHub Releases, OSF,
or local directories).

Key architectural patterns:

- **Single build entry point**: `projr_build_patch/minor/major()` for
  production;
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  for development.
- **YAML-driven configuration**: Create `_projr.yml` at the project root
  by default. Also read default, named and local configuration files in
  `.projr/config/`. A root file takes precedence over a nested file of
  the same name. Update the selected file in place and interpret
  configured paths relative to the project root.
  - A `profile` key in the default or local file names projr profiles
    (string or sequence) or `projr`/`quarto` profiles (mapping).
    Precedence: build `profile` argument or `args_engine`, then
    `PROJR_PROFILE`/`QUARTO_PROFILE`, then the local file, then the
    default file. A named profile file must not contain `profile`;
    `profile.renv` is an error until renv profiles are supported.
- **Directory paths**: Require a default configuration at the root or in
  `.projr/config/`. On the first build, write the starter at the project
  root if both supported locations lack a base YAML. The starter
  disables Git outside a repository and disables pushing for a
  repository with no upstream. Keep existing configurations intact. The
  reserved `data` label resolves to the package’s root `data/` directory
  (or its safe build cache). Other directory labels require an explicit
  path in the merged active configuration. The initialization template
  declares conventional paths explicitly. Require a cache when staging
  outputs and a docs directory when rendering documents. Resolve
  configured paths, safe build paths and ignore rules from the project
  root, including when called from a nested renderer working directory.
- **Manifest tracking**: Production builds record each hashed directory
  independently under `.projr/manifests/<label>/`. Its `versions.csv`
  maps project versions to the earliest matching directory version and
  the SHA-256 of a file list. Each distinct list of relative paths and
  MD5 hashes is stored once. In file history queries, an unchanged file
  keeps the version where it first appeared or most recently changed.
- **Remote destinations**: A unified dispatcher pattern (`R/remote.R`)
  routes file operations to local, GitHub, or OSF backends.
- **Internal vs. exported functions**: Internal helpers use a `.` prefix
  (e.g., `.hash_file()`); exported functions use the `projr_` prefix
  (e.g.,
  [`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md)).

The public `projr_manifest_*()` functions read the current checkout’s
history. A destination’s history reflects the builds it received. The
same list of files can therefore have a different first recorded version
in the checkout and in an archive. Compare the snapshot identifiers and
verify the files when retrieving a build. Keep manifest queries local
and use the retrieval and restore APIs for archive reads.

A project with a legacy root `manifest.csv` can be queried and restored
before migration. The next production build imports its history. Local
destinations store directory histories in `manifests/`. GitHub Releases
use one `manifest.zip` asset. A local latest destination with
`path-append-label: false` shares its root with release metadata.
Preserve that metadata across payload purges. Confirm an empty build
from the destination’s `VERSION` and matching directory snapshot before
clearing local files on restore.

Use
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
inside an existing checkout to restore its recorded inputs from a GitHub
archive release. Resolve the completed production version from the
checkout, fetch missing directory snapshots from the release, and verify
every filename and hash in a staging directory before installing it. Try
the exact version and earlier matching assets first. When they are
unavailable, a later upload may supply the files only if the release
index records the same directory snapshot and its downloaded file list
and hashes match the requested build. Preserve user changes unless
`clear = TRUE`.
[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md)
is a deprecated wrapper for
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).

`build.source` selects registered remotes (same forms as `build.dest`)
to supply raw inputs, defaulting to every declared raw label. Fetch
settings live under `remotes.<type>.<title>.get` and can be overridden
for all of `build.source` or per remote: `version`, `cue` (`if-empty` in
builds), `inspect`, `strategy` and `conflict`.

- Plan every fetch before any hook or script, downloading only into
  staging. Stop on conflicts before changing files, then install
  verified files before hashing raw inputs. `if-empty` and a pinned
  version already in place need no network.
- A conflict is a local edit since the last fetch. Keep fetch records in
  the Git-ignored, machine-local `.projr/source/<label>.json`; without
  one, differing files conflict. A projr-written raw `LICENSE` is not a
  local change. Production builds record each label’s remote, version
  and snapshot under “Raw-input sources” in `.projr/BUILDLOG.md`.
- [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
  fetches outside a build (default `cue: if-change`), using
  `build.source` or the single registered remote publishing each label.
  Interactive conflicts ask keep/replace/cancel. `version = "current"`
  delegates to
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md).
  [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md),
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  and `projr_get_*()` share the get backend.
- `directories.<label>.source` and a remote’s `source:` field are
  errors; use `build.source`.

Set the file hashing approach and worker limit under `settings.hashing`.
The defaults are `approach: cpp` and `cores: 1`. A positive whole-number
core count can parallelize file hashing with either the C++ or
historical `legacy` approach. Both preserve file ordering, identical
MD5s and per-directory histories. Existing `settings.hash-engine`
configurations remain readable.

Production builds use `build.renv: auto` by default. Snapshot when the
project has its renv lockfile or activation script. `true` can create
the first lockfile using installed packages, and `false` disables
snapshots. Check dependencies before remote preparation and rendering.
Take the post-render snapshot noninteractively and report missing
package names before modifying the lockfile. When adding DESCRIPTION
makes renv choose an empty package library, builds may reuse the same
checkout’s populated analysis library. Keep parent checks and render
processes on the chosen project library and restore the calling session
on exit.

### Build script selection and rendering

- `projr_build_dev(scripts = ...)` takes precedence over `dev.scripts`,
  then `build.scripts`. Production builds use `build.scripts`. Both use
  the merged active configuration. An explicit empty selection
  ([`character()`](https://rdrr.io/r/base/character.html) in R or `[]`
  in YAML) skips rendering and script execution.
- A selection made up entirely of quoted exclusions (for example,
  `["!scratch.R"]`) filters automatic discovery, retaining the
  project-first order and its native membership for untouched documents.
- Resolve paths and glob patterns in `_projr.yml` from the project root.
  Keep each renderer’s default code execution directory and honor its
  native options, including Quarto’s `execute-dir` and R Markdown’s
  `knit_root_dir`. Run Quarto synchronously so output collection and
  post-build hooks see the completed render in RStudio.
- Keep `args_engine` as the public build argument. Count distinct
  selected build engines. One engine accepts flat options even across
  repeated documents. With several engines, require named option lists
  for any overrides and list the selected engine names on an ambiguous
  call before build work. Omitted engines and an empty list use
  defaults. Use the matching options for rendering, Quarto profile
  selection and output collection. Plain R scripts have no render
  options.
- In `build.scripts` and `dev.scripts`, allow `- exclude: path-or-glob`
  among path entries. Each exclusion takes one string. Quoted
  `"!path-or-glob"` entries remain supported. Apply exclusions to the
  whole selection, including automatic discovery when only exclusions
  are supplied.
- Define named pipelines under top-level `pipelines.<name>` with
  `scripts` (same entries as `build.scripts`) and `hooks` (`pre`,
  `post`, `both`). Reject names containing `:` or `,`. Select with
  `build.pipeline`; `dev.pipeline` falls back to `build.pipeline`. The
  `pipeline` argument of
  [`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md),
  the bump helpers and
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  overrides the configured selection for one build.
- Accept one pipeline name or a list, with glob entries. Run entries in
  the given order and a glob’s matches in their order under `pipelines`;
  run each pipeline once, at its first position. Concatenate scripts and
  each phase’s hooks in that order. Reject an entry matching no pipeline
  before any hook, script or file change.
- Combine inline `build`/`dev` `scripts` and `hooks` with the whole
  group of pipelines. Entries keyed before `pipeline` run first; entries
  keyed after are appended. Keep each hook in its phase, apply `both` in
  each phase and run a repeated hook once, at its first position. Dev
  builds keep `dev.scripts` (falling back to `build.scripts`) and
  `dev.hooks` only. With a pipeline, the
  `projr_build_dev(scripts = ...)` argument takes the place of the
  inline scripts; without one, it overrides `dev.scripts` and
  `build.scripts`.
- Read key position from the highest-precedence file that sets
  `pipeline` (local file, profiles in order, then `_projr.yml`; `dev`
  before `build` for dev builds). Run inline entries first when that
  mapping lacks the inline key. An argument-only selection runs the
  pipelines after inline entries; an argument replacing a configured
  selection keeps its position. Document this as a projr convention, as
  YAML mappings are formally unordered.
- `pipeline: false`, `pipeline: []` or `pipeline = FALSE` selects no
  pipeline; a higher layer’s value replaces lower ones whole. Inline
  scripts and hooks and the `scripts` argument still run.
- An explicit empty inline list adds nothing and does not suppress the
  pipelines. Without a pipeline, keep existing selection, discovery and
  empty-list behaviour. A profile’s `pipelines.<name>.scripts` replaces
  the default list. Keep remotes, versioning, renv, Git and publication
  under `build`.
- Report the selected pipelines and their scripts in the console, their
  hooks in the detailed log, and their names, scripts and hooks in a
  **Pipeline** section of production `.projr/BUILDLOG.md`.
- Run explicitly selected scripts in the listed order. Sort matches
  alphabetically within each glob and keep repeated literal R scripts
  and standalone documents in place. Overlapping glob matches run once.
  Quarto and Bookdown projects reject explicit repeats of a project
  document because they render each member only once.
- Build a Quarto or Bookdown project once at the position of its last
  selected project document. Store selected documents in Quarto’s
  `project.render` or `book.chapters`, or Bookdown’s `rmd_files`.
  Standalone documents and R scripts retain their positions around that
  build.
- An explicit R-only selection runs those scripts without rendering the
  project. A document for the other engine renders as a standalone
  document. A directory with both project configurations is ambiguous
  and should report an error.
- With automatic selection, render the project first and run top-level R
  scripts alphabetically afterward. With no project, run top-level
  documents and R scripts alphabetically. Automatic R discovery skips
  configured hooks, underscore-prefixed files, and `renv_repos.R`.
- Save the original project YAML bytes before preparation, apply the
  selection after the pre-build commit, and keep it through output
  collection and post-build hooks. Restore the saved bytes immediately
  before switching to the dev version and on failure. Render Bookdown in
  a fresh R session so a preceding standalone render cannot supply
  another book’s configuration.
- Keep path lookups with `create = FALSE` read-only. Stage the
  renderer’s output directory only when the build includes that project.
  Preserve its original YAML bytes for both automatic and explicit
  project selections.
- Development builds must leave `VERSION` and `DESCRIPTION` version
  fields untouched on success and failure. They may use an existing
  version in `@version` templates and must be able to run before the
  first version exists.
- Production package checks include `rmarkdown` for a root `README.Rmd`,
  and `pkgbuild` when it uses its own project package. Run those checks
  before the analysis.
- Package startup must leave files alone. Before a development or
  production build first mutates a checkout, obtain approval for its
  full root path and the build’s file, script, hook, Git and archive
  operations. Ask once interactively for the checkout as a whole. Save
  positive interactive approval in the user’s projr configuration
  directory, outside the checkout. Existing project files do not imply
  approval, and separate checkout paths have separate approvals.
- Accept `project` as an explicit checkout root for an unattended build.
  This authorizes that invocation without saving approval. A
  subdirectory never grants consent for a different root. Stop an
  unapproved unattended build that omits `project` before writing files.
  Remote operations still require their configured credentials.
- Keep ordinary build writes in the project, its configured paths or the
  R session’s temporary directory. User-wide R settings belong to
  explicit setup commands.
- Keep source documents and their directories trackable. Ignore
  renderer-generated output and intermediates at their project-relative
  paths, including beside nested documents. Install predictable rules
  before rendering and retain ignore rules for newly created
  intermediates after a failed render. The root `README.Rmd` renders to
  `README.md`. Keep that Markdown and its `README_files/` figures
  trackable, and predict `README.html` only when another document
  produces it.
- Collect project outputs from the configured safe documentation
  directory. Collect standalone outputs and their companion assets
  together in the configured documentation directory. When names
  collide, keep the affected documents in their project-relative
  subdirectories and report any remaining conflicts before collecting
  outputs.

### Including directories in an output

- Set `directories.<receiving output>.include` to declared raw, docs or
  output labels. Copy each complete directory to a subdirectory named
  for its source label, including nested outputs in dependency order.
- Create declared output directories before production scripts run so
  scripts can write directly to their configured paths on the first
  build.
- Collect development copies in the build cache and production copies in
  the configured output before hashing or archiving it. Preserve
  existing source-side `output` settings. A request made in both places
  copies once.
- [`projr_use_data()`](https://satvilab.github.io/projr/reference/projr_use_data.md)
  checks the active configuration when called. If another label resolves
  to root `data/` or a descendant, name the conflicting label before
  saving anything. Analyses may use those paths freely until they call
  the helper. Collect staged public datasets into `data/` and staged
  internal datasets into `R/sysdata.rda` during production. Preserve
  other package datasets and R sources during cleanup.
- Reserve the receiving subdirectory for the included label. Reject
  cycles, conflicting files or links, and directory paths that would
  copy recursively or clear the source.

Store package bookkeeping under `.projr/` (optional configuration in
`config/`, versioned directory histories in `manifests/`, and untracked
build diagnostics and history in `logs/`). Keep the first-build starter
at the project root as `_projr.yml`. Keep the tracked production history
in `.projr/BUILDLOG.md` and the changelog in `CHANGELOG.md` at the root.
A build or restore may move recognized legacy state from `_projr/` and
an older root `BUILDLOG.md`. Ordinary reads must work without moving
files.

### Directory ignores across profiles

- Keep user rules outside the managed block intact, including their
  ordering, duplicates and line endings. Seed editable `.vdoc.*` ignores
  during initialization and respect their removal on refresh.

- When updating `.gitignore`, combine directory rules from active and
  inactive profiles per literal project-relative path. Apply the same
  logic to `.Rbuildignore` when the project root contains `DESCRIPTION`.
  Include a path for each ignore type enabled by any contributing
  profile.

- `directories.<label>.ignore-even-if-inactive` defaults to `true` and
  extends the directory’s existing `ignore`, `ignore-git`, and
  `ignore-rbuild` settings. With `false`, that profile contributes the
  directory’s rules only while active.

- Generate rules only for paths inside the checkout. Skip foreign
  absolute paths in inactive profiles without accessing or validating
  their directories.

- Refresh directory rules from the current configuration. When the last
  qualifying profile reference disappears, drop its generated rule on
  refresh. Keep one managed block in each ignore file. Regenerate rules
  from their sources and preserve user entries outside the block,
  including during migration. Keep package datasets under `data/`
  available to Git and R source packages by default. A `data` label can
  explicitly select either ignore type for that folder.

### Named archives and retrieving files

- Before a production build changes its version, renders, or creates a
  release, compare the local last completed build and proposed version
  with the selected archives. Report each destination’s version and
  outcome. `build.restrictions.not_behind_remote` defaults to the
  planned publishing destinations, including one-off archives. A list
  selects specific remote names (`<type>:<title>` when a title is
  ambiguous), including read-only destinations. `false` skips the check.
  `build.restrictions.rule` defaults to `base_version_not_behind`
  (`base`), requiring local base \>= remote and proposed \> remote.
  `target_version_not_behind` (`target`) requires only proposed \>
  remote. Empty archives pass. Keep Git’s upstream check separate under
  `not_behind_git`, accepting the historical `not_behind` spelling.
  Development builds skip both checks.
- Advance the archive’s project version only after publishing its final
  selected directory and changelog. An earlier successful label in an
  interrupted upload must preserve the last completed archive version.
- Define local and GitHub archives under `remotes.<type>.<title>`. Store
  the address, layout and sending options there. Select archives with
  `build.dest`: a name, a list of names, or a mapping whose option keys
  (`content`, `cue`, `inspect`, `strategy`) apply to every listed
  archive and whose archive keys take per-archive overrides. Per-archive
  settings take precedence over `build.dest` options, then
  `remotes.<type>.<title>.send`, then defaults. A bare selection
  publishes all eligible raw, output and docs directory labels. An
  explicit `content` list selects the labels to publish and can include
  cache. A title omitted from `build.dest` remains readable. Reject
  inline `build.local` and `build.github` with a pointer to `remotes`
  and `build.dest`.
- Name an archive by its title when unique across types, otherwise as
  `<type>:<title>`. Report unknown and ambiguous names. `content`,
  `cue`, `inspect`, `strategy`, `conflict` and `version` cannot be
  titles.
- A one-off `archive_github` or `archive_local` build registers
  `remotes.github.archive` or `remotes.local.archive` (path `_archive`)
  when absent, without selecting it in `build.dest`.
- For GitHub, default the destination repository to the producer’s
  repository and the tag and title to the configured name. An explicit
  `repo` or `tag` overrides its respective default. Expand templates
  using the producer’s build context. Keep the source branch,
  destination repository, concrete release tag and requested build
  version distinct.
- [`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md)
  accepts a local analysis path or a current-project remote name.
  [`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md)
  accepts a producer repository (optionally qualified with `@branch`) or
  a registered remote name, and an optional release tag. Direct reads
  work outside a project. Local reads require neither Git nor a network
  connection.
- Use the source manifest and available files to retrieve labels from a
  completed build, independently of the current publishing selection.
  Default to the latest completed build, accept an exact version and
  verify filenames and hashes before installing files under
  `out/<label>/`. Require `overwrite = TRUE` to replace conflicting
  contents. Return the absolute output paths named by label.

### Reproducing a completed build

- [`projr_reproduce()`](https://satvilab.github.io/projr/reference/projr_reproduce.md)
  clones into a new or empty directory. Use the latest completed build
  on the selected branch, an exact version, or the last completed build
  reachable as of a commit. When both a commit and version are
  specified, use that commit’s code with the named version’s inputs and
  expected outputs.
- Restore dependencies from the code checkout’s lockfile. With only a
  DESCRIPTION, use pak and report the unpinned versions. Restore
  verified raw inputs, clear the clone’s outputs and run its production
  scripts and hooks. Run the analysis without bumping its version,
  committing, pushing or publishing artifacts.
- Compare the complete file sets and hashes for output and documentation
  directories. Return overall and per-directory matches plus per-file
  statuses. A recorded empty directory can match. Missing hashes prevent
  a successful comparison.

### Destination and directory templates

- Expand `@version`, `@branch`, and `@uuid` in configured directory
  paths, local destination names and paths, and GitHub release names. A
  token ends at the end of the string or before a hyphen, underscore,
  period, ordinary space, or path separator. Preserve unknown names
  literally.
- Use the build version with one `v` prefix. Capture one UUIDv4 and Git
  HEAD per build and share them with child render processes and retries.
  Encode branch names as one portable path component, preserving the
  distinction between `feature/foo` and `feature-foo`. Detached HEAD
  uses the commit SHA.
- Generate directory ignore patterns from the current YAML templates,
  including local archives. Keep the template in YAML and record the
  concrete archive destination in `.projr/BUILDLOG.md` for restore.
- Store local release histories in `<destination>/manifests/` for every
  layout. With `path-append-label: false` and `structure: latest`, keep
  the root’s `manifests/`, `manifest.csv`, `VERSION`, and `CHANGELOG.md`
  separate from payload listing, hashing, purge, and restore. Report a
  conflicting payload name before writing.
- Keep a GitHub release’s title and tag identical. Only an explicitly
  configured target branch selects the commit for a new tag. Resolve
  that branch in the destination repository and leave existing release
  tags in place.

------------------------------------------------------------------------

## Tech Stack & Tooling

### Language

- **R** (≥ 4.1.0) in `R/`, with native manifest code in `src/`
  registered through `cpp11`

### Core R package dependencies (Imports)

| Package | Purpose |
|----|----|
| `renv` | Reproducible dependency management |
| `callr` | Run replayed scripts and hooks in an R session with the analysis libraries |
| `yaml` | Read/write `_projr.yml` |
| `jsonlite` | JSON serialisation |
| `httr` | GitHub release downloads and uploads, including reads from a restored renv library |
| `rprojroot` | Reliable project-root detection |
| `desc` | Read/write `DESCRIPTION` |
| `fs` | File-system operations |
| `digest` | SHA-256 snapshot IDs and historical MD5 hashing |
| `uuid` | Random UUIDv4 identifiers for build templates |
| `filelock` | Serialize concurrent updates to project manifests |
| `cli` | Console output |
| `glue` | String interpolation |

### Suggested / optional dependencies

`gert`, `gh`, `gitcreds`, `credentials` (Git/GitHub), `osfr` (OSF),
`rmarkdown`, `quarto`, `bookdown`, `knitr` (document rendering),
`testthat` ≥ 3.0 (testing), `devtools`, `usethis`, `roxygen2`
(development).

### What NOT to use

- **Do not use `piggyback`** for GitHub Release operations — all GitHub
  API calls go through `httr` directly (see `R/remote-github-httr.R`).
- **Do not use the `magrittr` pipe (`%>%`)** — use the native R pipe
  `|>` instead.
- **Do not edit files in `man/`** — they are auto-generated by
  `roxygen2`.

### Document rendering

- Quarto (`quarto-dev/quarto-actions`)
- R Markdown / Bookdown

------------------------------------------------------------------------

## Setup Commands

Run these commands once to configure a local development environment.

### 1. Install system dependencies (Ubuntu / Debian)

``` bash
sudo apt-get update -y
sudo apt-get install --no-install-recommends -y \
  libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev \
  libgit2-dev libjpeg-dev libpng-dev libx11-dev pandoc
```

### 2. Install Quarto

Follow the instructions at <https://quarto.org/docs/get-started/> or
use:

``` bash
# Via quarto-dev/quarto-actions (CI)
# Locally: download installer from https://quarto.org
```

### 3. Restore R dependencies with renv

``` r

# Inside an R session at the project root:
renv::restore()
```

### 4. Load the package for interactive development

``` r

devtools::load_all()
```

------------------------------------------------------------------------

## Build & Test Instructions

### Run the test suite (LITE mode — recommended for development)

``` r

devtools::load_all()
.test_set_lite()
devtools::test()
```

LITE mode runs ~364 core tests in ~2.5 minutes. It must be explicitly
enabled with `.test_set_lite()` before each session; without it,
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
runs the full suite.

### Run the full test suite

``` r

devtools::load_all()
devtools::test()
```

Full mode runs ~452 tests and is used for pre-release validation.

### Debug a specific test

``` r

devtools::load_all()
.test_unset_lite()   # Disable LITE mode so the target test runs
.test_set_select()   # Skip all other tests

# Temporarily comment out skip_if(.is_test_select()) in the target test file
devtools::test()

# Restore when done:
.test_unset_select()
```

### R CMD check

``` r

devtools::check()
```

### Format R code

Use Air for package code and tests. From the repository root, run the
first command to format and the second to check for pending changes.

``` sh
air format R tests
air format --check R tests
```

Positron includes Air. In Positron or VS Code, use **Format Document**
on an R file. For the terminal command, see the [Air CLI installation
guide](https://posit-dev.github.io/air/cli.html).

### Update documentation

``` r

devtools::document()   # Regenerates man/ and NAMESPACE from roxygen2 comments
```

### Website examples

Use the conventional `_raw_data/` and `_output/` folders in the first
example so a new user can publish with
`projr_build(archive_github = TRUE)` before writing any YAML. Show how
to run
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
in a fresh checkout. Keep the homepage and getting-started example in
sync, and put the complete list of options and conventions in the
configuration and function references.

Keep each introductory diagram’s Mermaid source beside its rendered SVG
in `vignettes/figures/`. Update both when the workflow changes. Use the
folder paths readers see on disk and give each figure a text
alternative.

### Verify pkgdown reference index

``` r

pkgdown::check_pkgdown()
```

------------------------------------------------------------------------

## Coding Style & Conventions

### Naming

- **Exported functions**: `projr_` prefix, `snake_case` (e.g.,
  [`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md)).
- **Internal functions**: `.` prefix, `snake_case` (e.g.,
  `.hash_file()`).
- **Variables**: descriptive `snake_case`; avoid single-letter or highly
  abbreviated names.
- **Temporal pairs**: prefer `before`/`after`, `previous`/`current`,
  `old`/`new` over generic `_pre`/`_post` suffixes.
- **File paths**: use `filename` (name only) or `filepath` (full path);
  use plural (`filenames`, `filepaths`) for vectors.

### Code style

- Indent with **2 spaces**.
- Use the **native pipe** `|>`, not `%>%`.
- Keep functions focused and modular.
- Validate public inputs before modifying files or contacting a remote.
  Use the shared `.assert_*()` helpers from `R/check.R`. Use
  `.assert_flag()` for scalar switches, `.assert_in_single()` for a
  single choice and `.assert_count()` for whole-number counts. Optional
  NULL arguments pass validation.

### Documentation (roxygen2)

All **exported** functions must include:

``` r

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

- Use
  [`.cli_debug()`](https://satvilab.github.io/projr/reference/dot-cli_debug.md),
  `.cli_info()`,
  [`.cli_success()`](https://satvilab.github.io/projr/reference/dot-cli_success.md),
  [`.cli_step()`](https://satvilab.github.io/projr/reference/dot-cli_step.md)
  — they automatically read the `PROJR_OUTPUT_LEVEL` environment
  variable.
- Do **not** pass `output_level` as a function parameter.
- Announce steps at the point the build attempts them. Reserve success
  messages for confirmed outcomes and keep routine skips out of standard
  output.
- Record the build version, active profiles, script and hook paths,
  destination decisions, and failure reason in the detailed log.
  Summarize file operations per directory or batch. Preserve prior
  history and the current log through cache cleanup.

### Tests

- Framework: `testthat` edition 3 (`Config/testthat/edition: 3` in
  `DESCRIPTION`).
- Test files: `tests/testthat/test-{feature}.R`.
- Helper functions: `tests/testthat/helper-*.R` (not in `R/`).
- Add `skip_if(.is_test_select())` to every test to enable selective
  debugging.
- Add `skip_if(.is_test_lite())` to comprehensive / exhaustive-parameter
  tests.
- Add `skip_if(.is_test_cran())` to any test that is slow or hits the
  network.
- Keep Git test identities in temporary fixture repositories and scope
  author and committer environment variables to the test session. When
  project initialization needs a Git identity, write it to the project
  repository.
- Wrap tests that need a temporary project in
  [`usethis::with_project()`](https://usethis.r-lib.org/reference/proj_utils.html).

### Commits / PRs

- For a stable package release, merge its three-part `DESCRIPTION`
  version into `main`. The release workflow publishes the matching
  `vX.Y.Z` GitHub Release with the `R CMD build` source tarball. Tag
  pushes and the manual version/bump workflow also support publication.
  Keep released version tags fixed.

- Make **minimal, surgical changes** — change as few lines as possible.

- Run
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html),
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  (LITE), and `air format --check R tests` before committing.

- Update `_pkgdown.yml` when adding or removing exported functions, then
  verify with
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html).

- Ensure files end with a **single newline** and have **no trailing
  whitespace**.

- Never leave [`browser()`](https://rdrr.io/r/base/browser.html) or
  unguarded interactive debugging calls in committed code.

### Environment variables

| Variable | Values | Purpose |
|----|----|----|
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

For coding, refactoring, bug-fixing, review and implementation design,
read `.agents/skills/ponytail/SKILL.md` and apply Ponytail in **full**
mode by default. Use **ultra** only when explicitly requested.
Repository-specific correctness, security, architecture and validation
requirements take precedence over generic simplification guidance.

## GitHub issues and Projects

For issue and Project administration, use the canonical
[MiguelRodo/github-projects-skill](https://github.com/MiguelRodo/github-projects-skill/tree/main/skills/github-projects)
skill and read `.projects/project.md` for this repository’s routing
contract. The central issue store owns planning; this repository is its
implementation mirror.
