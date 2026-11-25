# Project configuration reference

Use this reference to look up a YAML setting, understand which files a
build will touch, or restore a saved result. For a first build, follow
the [getting started
guide](https://satvilab.github.io/projr/articles/get-started.md).

## The project root and profiles

Run the first build from the analysis project. If it has no base
configuration, projr creates `_projr.yml` with explicit paths for
`raw-data` (`_raw_data`), `output` (`_output`), `cache` (`_tmp`), and
`docs` (`docs`). Existing scripts using those folders can keep their
paths. If your files are already elsewhere, point the corresponding
label at the existing folder. Once a base file exists, its directory
declarations determine which labels are available.

| File                   | When it applies                            |
|------------------------|--------------------------------------------|
| `_projr.yml`           | Base configuration shared by the project.  |
| `_projr-<profile>.yml` | Overrides for a named analysis or machine. |
| `_projr-local.yml`     | Local overrides with highest precedence.   |

Each file can live at the project root or in `.projr/config/`. When both
locations contain the same filename, the root copy wins. All relative
paths in either location start at the project root. A nested script can
therefore use the same directory labels when run in RStudio or during a
build.

Select profiles with a build’s `profile` argument or `PROJR_PROFILE`.
For example, `Sys.setenv(PROJR_PROFILE = "site-a,cluster")` applies both
named profiles, with the earlier name taking precedence when both
specify a value. Local overrides still win. Use
[`projr_yml_get()`](https://satvilab.github.io/projr/reference/projr_yml_get.md)
to inspect the merged settings, `projr_yml_get("default")` to inspect
the base file, and
[`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
to validate the active configuration.

### Choosing the checkout for a build

The first interactive build names the full project root and asks once
for permission to run its scripts and hooks, write its configured files,
and perform the requested Git and archive operations. Configured
directories can be outside the checkout. Approval is remembered for that
checkout’s path in the user’s projr configuration directory. A fresh
clone at another path prompts again, even if it contains the same
`_projr.yml`.

For unattended builds, pass the root of the checkout as `project`. Use
`projr_build(project = ".", archive_github = TRUE)` when the job starts
at that root, or pass the path to the root from another working
directory. The explicit path authorizes this invocation. Git and remote
destinations still use their own credentials.

### Files projr keeps

The `.projr/` folder belongs to the project. Git can track its
configuration and manifests so a fresh checkout knows which files
belonged to a build. The production build history goes in
`.projr/BUILDLOG.md` and can be committed with the analysis. Detailed
logs for individual runs stay local in `.projr/logs/`.

A build or restore moves recognized configuration files and manifests
from the earlier `_projr/` layout. Read-only queries still work on older
checkouts. If both locations contain the same configuration filename or
a manifest store, projr stops before moving anything so you can choose
which history to keep. Existing logs in a former cache directory remain
readable with `projr_log_view(log_file = "...")`.

## Directory declarations

This example keeps incoming data and generated figures where they
already live. It also creates a deliverable containing the figures and
the rendered documents.

``` yaml
directories:
  raw-data:
    path: incoming/site-a
  cache:
    path: scratch
  docs:
    path: reports/rendered
  output-figures:
    path: results/figures
  output-delivery:
    path: deliverables
    include: [output-figures, docs]
```

After a production build, the included copies are under
`deliverables/output-figures/` and `deliverables/docs/`. Each copy
reflects the current source, including file removals. Other files in the
receiving directory are separate from these copies. An output can
include another assembled output. Cycles, paths that would copy a
directory inside itself, and overlapping paths that could erase an input
are rejected.

For R package datasets,
`projr_path_get("data", "sample.rda", safe = FALSE)` resolves to
`data/sample.rda` in the project root. The `data` label is built in and
its location is fixed. A production build copies datasets staged by
`projr_use_data(sample)` into `data/` and internal datasets into
`R/sysdata.rda`. Development builds keep staged datasets under the
configured cache. If an active directory label already uses `data/` or a
subdirectory such as `data/site-a/`, the helper reports that label
before saving. Projects that store their own files there can still build
and look up those paths.

| `directories.<label>` field | Meaning |
|----|----|
| `path` | Required for configurable labels. An absolute path can point outside the checkout. |
| `hash` | Include the directory in the version history. Defaults to true for raw data, outputs and docs, and false for cache. |
| `include` | On an output label, list declared raw, docs or output labels to copy under their label names. An empty list requests no includes. |
| `output` | Older source-side form. Use an output label or list to copy into those outputs, `true` for all other outputs, or `false` to skip. |
| `package` | On an output label, `true` also builds the R source package and copies it under `pkg/`. |
| `license` | A license identifier such as `CC-BY`, or a mapping containing `type`, `authors` and `year`. See [`vignette("directory-licenses")`](https://satvilab.github.io/projr/articles/directory-licenses.md). |
| `ignore` | Controls both Git and R build rules. Defaults to false for package datasets (`data`) and true for other declared directories, including `raw-data`. Accepts YAML booleans, `ignore`, `no-ignore`, or `manual`. |
| `ignore-git`, `ignore-rbuild` | Set either type separately. When `ignore` is also supplied, the choices must agree. |
| `ignore-even-if-inactive` | Defaults to true. Applies that profile’s enabled ignore types while a different profile is active. |

Use a raw, cache or output prefix when a label should participate in the
corresponding build stage, for example `raw-site-a` and `output-tables`.
Declare a cache for builds that need staging, and `docs` for collected
documents. If several cache labels are configured, the first supplies
the build cache. Labels used as directory lookups need a declared path,
including `code` and `data` if your scripts use those names. The suffix
`-empty` is reserved for archive metadata.

`projr_path_get("output-figures", "plot.png")` returns a filename and
creates its parent if needed. `projr_path_get_dir("output-figures")`
returns a directory. Use `create = FALSE` for lookup only. Both accept
`format = "absolute"`, `"relative"`, or `"auto"` (the default follows
the YAML path’s format). Relative results are usable from the caller’s
working directory. The default `safe = TRUE` allows projr to route
output and document writes to the build’s working area. Use
`safe = FALSE` to address the configured final location directly.
Literal paths in your own code use R’s working directory.

When refreshing ignores, projr considers each profile’s configured
paths. If any eligible profile requests a Git ignore, the project keeps
that rule. R package build ignores follow the same rule when the root
has a `DESCRIPTION`. Changing a path removes the old generated rule once
its last qualifying reference disappears. User-written rules outside the
managed section stay in place. Only paths inside the checkout generate
rules, so a Windows path in an inactive profile can coexist with a Linux
checkout.

Initialization keeps the contents of an existing `.gitignore` outside
the projr-managed section, including comments, duplicate rules and
exceptions. It also adds editable defaults for R session files,
`.vdoc.*`, `.DS_Store` and `Thumbs.db`. You can delete those defaults
after initialization.

## Documents, R scripts and hooks

| Setting | Selection |
|----|----|
| `build.scripts` | Ordered inputs for a production build. Also used by dev builds when `dev.scripts` is absent. |
| `dev.scripts` | Ordered inputs for a development build. The `scripts` argument to [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md) takes precedence. |
| `build.hooks`, `dev.hooks` | Each accepts `pre`, `post`, and `both`. Supply R script paths under the stage. Dev and production hooks are configured independently. |

A script selection accepts `.R`, `.Rmd` and `.qmd` paths or glob
patterns. To skip files, add an item such as
`- exclude: reports/draft.Rmd` to either scripts list. Each `exclude`
takes one path or glob pattern. Quoted strings such as
`"!reports/draft.Rmd"` also work. Matches within a glob are
alphabetical. Exclusions apply to the selection as a whole. If every
entry is an exclusion, projr uses automatic discovery and skips the
matching files. For example, `scripts: [{exclude: scratch.R}]` builds
the project first and then runs the other top-level R scripts. In a
directory without a rendering project, the remaining documents and
scripts run together alphabetically. Overlapping globs select each file
once. Repeating a literal R script or standalone document runs it again.
An explicit `scripts: []`, or `scripts = character()` in R, skips
document rendering and script execution.

For example, suppose a Quarto project has the following selection.

``` yaml
build:
  scripts: [prepare.R, intro.qmd, check-data.R, results.qmd, summarise.R]
  hooks:
    post: hooks/record-build.R
```

Here `prepare.R` and `check-data.R` run first. Quarto builds `intro.qmd`
and `results.qmd` together when the selection reaches `results.qmd`.
Then `summarise.R` runs. Projr temporarily sets Quarto’s
`project.render` (and `book.chapters` for a book), or Bookdown’s
`rmd_files`. It keeps the selected configuration through post-build
hooks, then restores the original YAML, including on failure. For Quarto
document exclusions this also covers active profile and included
configuration files inside the checkout. A Quarto book needs its index
page whenever it renders any remaining chapters. An R-only selection
runs just those scripts. Documents for a different engine render
individually in their listed positions. Choose one project configuration
when both `_quarto.yml` and `_bookdown.yml` are present.

When script selection is omitted, projr builds the Quarto or Bookdown
project using its own settings and then runs top-level R scripts
alphabetically. In a directory without a rendering project it runs
top-level documents and scripts together in alphabetical order.
Automatic R discovery skips configured hooks, underscore-prefixed
scripts and `renv_repos.R`.

Renderers retain their own working-directory conventions, including
Quarto’s `execute-dir` and R Markdown’s `knit_root_dir`. To pass options
from R, use `args_engine` in the build call. Group options by engine
when the selection mixes engines. See [options for a build
engine](https://satvilab.github.io/projr/articles/build-process.html#options-for-a-build-engine).

The collected document and its companion assets stay together. Where
basenames collide, projr preserves the affected documents’
project-relative subdirectories. Renderer intermediates such as
generated TeX files receive project-relative ignore rules, including for
nested source documents.

## Builds, dependencies and versions

| Setting | Values and effect |
|----|----|
| `build.renv` | `auto` (default) snapshots when the project has `renv.lock` or `renv/activate.R`. `true` requests snapshots even for a first lockfile. `false` disables build snapshots. A production build checks dependencies before rendering and snapshots again without prompting afterward. |
| `build.git` | A boolean, or a mapping with booleans `commit`, `add-untracked`, and `push`. The first build writes `false` when there is no Git checkout, or disables push when the branch has no upstream. For an existing configuration with these settings omitted the normal production defaults enable all three. |
| `build.restrictions.branch` | `true` allows any branch (default), a name or list allows those branches, and `false` or `[]` allows none. |
| `build.restrictions.not_behind_git` | Defaults to `true`. Checks whether the local Git branch is behind its upstream. Existing configurations can use `not_behind`. |
| `build.restrictions.not_behind_remote` | Defaults to `true`. Checks the archives selected for publication, including one-off archives. Set `false` to skip archive version checks or supply the titles of the archives to check. |
| `build.restrictions.rule` | `base_version_not_behind` (or `base`, the default) checks the last completed local version and the proposed version. `target_version_not_behind` (or `target`) checks just the proposed version. |
| `build.cite` | A boolean or a mapping of booleans `cff`, `inst-citation`, and `codemeta`. Selects citation files for production build updates. |
| `metadata.version-format` | Defaults to `major.minor.patch-dev`. Supported formats also allow `major.minor` or `major` before the development component, a dot separator before it, and a final `1` or `9000` in place of `dev`. |
| `parameters` | Project-specific values available inside scripts, for example `parameters: {seed: 42}` and `projr_par_get("seed")`. |

Use
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
to iterate. It preserves the current project version and can run before
the project has one. An unversioned project stages its working files in
`<cache>/projr/dev/`. The `@version` placeholder requires a version set
by a production build or by
[`projr_version_set()`](https://satvilab.github.io/projr/reference/projr_version_set.md).
Use `old_dev_remove = FALSE` to keep older working directories. Use
[`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md),
[`projr_build_minor()`](https://satvilab.github.io/projr/reference/projr_build.md)
or
[`projr_build_major()`](https://satvilab.github.io/projr/reference/projr_build.md)
to choose the component for a versioned build. `msg` records a build
description. `clear_output` accepts `"pre"`, `"post"` or `"never"`.
Production builds default to `"pre"` and development builds to
`"never"`. Pass `clear_output = NULL` to read `PROJR_CLEAR_OUTPUT`. For
console and file logging, profile environment files, and authentication,
see
[`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md).

If you add a package `DESCRIPTION` to an existing renv analysis, renv
can choose a new library when the next R session starts. When that
library is empty apart from renv, builds reuse the populated analysis
library for this checkout. The build and its renderers use the same
packages. A populated new library takes precedence, and explicit renv
library settings are respected. A missing renderer dependency stops the
build with the package names and the installation command for the
project library.

### Archive version checks

Before a production build publishes, projr shows the last completed
version in the checkout, the proposed build version, and the version
recorded by each selected archive. A new or empty archive passes. For an
existing archive the proposed version must be newer than its last
successful upload. By default the checkout must also be at least as
recent as that upload.

The local base comes from the latest build recorded in
`.projr/BUILDLOG.md`. For an older checkout without a recorded build,
projr can use its stable project version. A development version alone
does not establish a completed build.

The archive records its completed project version after all selected
directories have uploaded. If a populated archive has missing or
unreadable version information, the build stops so you can inspect it
before publishing.

For example, suppose your checkout last built `0.1.0` and the archive
already contains `0.2.0`. The default rule stops even if you request a
major build to `1.0.0`. Choose `rule: target` when you intend to publish
that version from the older checkout. Both rules stop a proposed build
of `0.2.0`, since the archive already has that version.

By default projr checks the destinations this build is scheduled to
publish to, including those requested with `archive_github` or
`archive_local`. To check specific archives on every production build,
list their configured titles. This also checks registered archives that
the build only reads.

``` yaml
build:
  restrictions:
    not_behind_remote: [reports, backup]
    rule: target
```

If the same title appears under both `remotes.github` and
`remotes.local`, write it as `<type>:<title>`, for example
`not_behind_remote: [github:reports]`. Set `not_behind_remote: false` to
skip archive checks. Git upstream checks have their own `not_behind_git`
setting. Development builds skip both checks.

## Hashing and history

``` yaml
settings:
  hashing:
    approach: cpp
    cores: 1
```

`cpp` streams files through the native implementation. `legacy` uses the
historical R hashing method. Both produce the same file MD5s and use the
same directory histories. `cores` is a positive integer. Increase it to
distribute files across workers during a hashing phase. The default uses
a single worker. The earlier `settings.hash-engine` spelling is accepted
for compatibility.

Each production build records its selected inputs before rendering and
its outputs afterward. Each directory has its own history. For example,
a change in `output` adds a new file list for that directory while
`raw-data` can keep using its existing list. Development builds leave
these histories alone.

| Where to look | Where the history for `raw-data` lives |
|----|----|
| Project checkout | `.projr/manifests/raw-data/` |
| Local archive | `manifests/raw-data/` inside the archive folder |
| GitHub Release | The `raw-data/` directory inside the `manifest.zip` asset |

Open `versions.csv` in any of those directories to find a build’s
version, the earliest version that recorded identical contents there,
and the snapshot identifier. The matching `files-<snapshot>.csv` lists
relative filenames and their MD5 hashes. A directory that returns to an
earlier state reuses the same list. projr can also read a legacy root
`manifest.csv` and imports its history on the next production build.

Run
[`projr_manifest_changes()`](https://satvilab.github.io/projr/reference/projr_manifest_query.md)
in the checkout to compare two builds, or
[`projr_manifest_file_history()`](https://satvilab.github.io/projr/reference/projr_manifest_file_query.md)
to see when an individual file changed. An archive records the builds it
received, so its first copy of a file list can have a different version.
For a worked example with a published archive, see [restoring an earlier
build](https://satvilab.github.io/projr/articles/restore-artifacts.html#when-builds-share-the-same-inputs).

## Saving artifacts

Use `remotes` to give an archive a name and record where it lives. Use
`build.dest` to select which archives a production build writes to. This
example keeps a complete local archive called `shared` and sends just
the figures to a GitHub repository. The project declares
`output-figures` under `directories`.

``` yaml
remotes:
  local:
    shared:
      path: ../team-archives/study
  github:
    reports:
      repo: research-team/study-results
      tag: "figures-@branch"
      structure: archive
      send:
        cue: if-change
        inspect: manifest
        strategy: sync-diff
build:
  dest:
    shared:
    reports:
      content: [output-figures]
```

`build.dest` takes one of three forms.

| `build.dest` | What a production build sends |
|----|----|
| `dest: shared` | All declared raw, output and docs labels to one archive. Cache is included only when explicitly selected. |
| `dest: [shared, reports]` | All of those labels to each listed archive. |
| A mapping | Each key is an archive name or a setting. A setting (`content`, `cue`, `inspect` or `strategy`) applies to every listed archive. Settings under an archive’s name apply to that archive only. |

A per-archive setting takes precedence over a setting for the whole of
`build.dest`, which takes precedence over `remotes.<type>.<title>.send`.
An archive left out of `build.dest` receives nothing, but its saved
files remain available to readers. Readers can ask for any label
actually saved for a build, even after its publishing selection changes.
A profile’s `build.dest` replaces the default selection, so `dest: []`
in a profile turns uploads off.

Name an archive by its title when no other type uses that title, or as
`<type>:<title>`, for example `github:reports`. The words `content`,
`cue`, `inspect`, `strategy`, `conflict` and `version` cannot be archive
titles.

For GitHub, `repo` defaults to the repository that produced the build.
The name under `remotes.github` supplies the release tag and title
unless `tag` supplies a different name. For example, `reports: {}` under
`remotes.github` registers a release named `reports` in the producer’s
repository. `repo: research-team/study-results` sends the files to a
separate repository. A release such as `figures-main` can hold files
from several project versions.

| Field under `remotes.<type>.<title>` | Values and effect |
|----|----|
| `path` | Local archive path, relative to the project root or absolute. |
| `repo` | GitHub repository in `owner/repo` form. Defaults to the repository where the analysis is built. |
| `structure` | `archive` (default) stores versioned payloads. `latest` keeps the current payload for each label. |
| `path-append-label` | Defaults to true. On a local destination, false lays out artifacts without the label subdirectory. A dedicated path per label avoids overlapping payloads. |
| `tag` | GitHub release name, used for both the Git tag and the displayed release title. Defaults to the registered name. A literal `archive` shares one release across versions. |
| `branch` | Optional GitHub target branch for a new tag. The branch must exist in the destination repository. `@branch` selects the checkout’s named branch. Reusing a release preserves its tag target. |
| `send.cue` | `if-change` (default) or `always`. |
| `send.inspect` | `manifest` (default) compares recorded hashes, `file` inspects the files themselves, and `none` treats the destination as empty for comparison. |
| `send.strategy` | `sync-diff` sends changes and removes obsolete files. `sync-purge` replaces the remote contents, `upload-all` sends every local file, and `upload-missing` sends only absent names. Defaults to `sync-diff`, or `upload-all` with `inspect: none`. |

For a one-off production archive, pass labels to `archive_local` or
`archive_github`. `TRUE` chooses the available archive labels.
`always_archive` controls whether those one-off archives are sent even
when unchanged. The first one-off archive also registers
`remotes.github.archive: {}` or
`remotes.local.archive: {path: _archive}` in `_projr.yml`, so projr
knows where the files went. It does not add the archive to `build.dest`.

### Naming a destination for a branch or build

Directory paths, local destination names and paths, and GitHub release
names can contain the following placeholders.

| Placeholder | Example |
|----|----|
| `@version` | `archive-@version` becomes `archive-v1.2.3` for build 1.2.3. |
| `@branch` | `archive-@branch` on `feature/foo` becomes `archive-feature_2ffoo`. A detached checkout uses `__detached-` followed by the full commit SHA. |
| `@uuid` | A fresh UUIDv4 for each build, shared by that build’s scripts, renderers, paths and release operations. |

The build supplies the values for all three placeholders. For example,
`figures-@branch` uses the branch of the analysis being built even when
the release is hosted in a separate repository. The optional `branch`
setting selects the commit for a new GitHub tag in that destination
repository.

Quote a YAML string that begins with `@`. A placeholder ends at the end
of the string or before a hyphen, underscore, period, ordinary space or
path separator. For instance, `"@branch/@version"` expands both tokens,
and `archive-@branching` stays literal. Ignore rules cover the
configured path template across runs.

### Archive layout and restore

Local destinations keep per-label histories under `manifests/`. GitHub
Releases carry those histories in a single `manifest.zip` alongside the
artifact ZIPs. In an archive structure, a GitHub artifact is named, for
example, `output-figures-v1.2.3.zip`. An empty directory version can
have an explicit empty marker. In a flat local latest destination the
names `manifests`, `manifest.csv`, `VERSION`, and `CHANGELOG.md` at its
root belong to projr.

To reproduce a checked-out build published to GitHub, run
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
in the checkout. It uses the release named `archive` by default and
restores the declared raw directory labels. It reads the checkout’s
production version, or the most recent completed build in its
`.projr/BUILDLOG.md` when on the follow-up development commit. Supply
`version = "1.2.3"` for a specific build or `destination` for another
release tag. It verifies every selected directory’s paths and MD5s
before installing any downloads. Local changes and extra files stop the
restore unless you explicitly allow replacement with `clear = TRUE`.

Run
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
to fetch the raw labels from their latest builds on the remotes
registered under `remotes`. Supply `label` to fetch other directories
and `remote` (a title, or `<type>:<title>`) to choose the remote.
Without `remote`, projr uses `build.source`, or else the single
registered remote that published each label. `conflict = "remote"`
replaces local changes, including when the archive records an empty
version.
[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md)
is deprecated and now calls
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).

If a destination contained `@uuid`, find its concrete path or release
tag in the **Resolved archive destinations** section of that build’s
`.projr/BUILDLOG.md`. In an older checkout, projr can read a root
`BUILDLOG.md`. The next build or restore moves that history into
`.projr/`. The changelog stays at the root.

``` r

projr::projr_source(label = "output-delivery", remote = "local:shared")
```

`projr_restore_repo("owner/repo")` clones a repository and fetches its
directories with
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).
Use `projr_restore_repo_wd("owner/repo")` to clone into the current
directory. Both accept `label`, and `type` and `title` to name the
remote.

## Fetching raw inputs

Use `build.source` to fetch raw inputs from registered remotes before a
build runs. It takes the same three forms as `build.dest`. By default
each listed remote supplies every declared raw label. This example takes
the latest survey files from a colleague’s GitHub release and pins the
codebook to their build 0.6.3 in a local archive.

``` yaml
remotes:
  github:
    survey-team:
      repo: example/survey
      tag: archive
      get:
        cue: if-change
  local:
    shared:
      path: ../team-archives/survey

directories:
  raw-survey:
    path: _raw_data/survey
  raw-codebook:
    path: _raw_data/codebook

build:
  source:
    survey-team:
      content: [raw-survey]
    local:shared:
      content: [raw-codebook]
      version: "0.6.3"
```

Set defaults under `remotes.<type>.<title>.get`. A setting under
`build.source` applies to every listed remote, and a setting under a
remote’s name applies to that remote only.

| `get` setting | Values and effect |
|----|----|
| `version` | `latest` (default), `x` for the latest build within major version `x`, `x.y` for the latest within that minor version, or an exact `x.y.z`. |
| `cue` | `if-empty` (default in builds) fetches only into empty folders, without contacting the remote. `if-change` fetches when the remote’s snapshot differs. `always` fetches every time. `never` skips the remote. |
| `inspect` | `manifest` (default) trusts the remote’s recorded hashes. `file` downloads and hashes the files. `none` skips the comparison. |
| `strategy` | `sync-diff` (default) and `sync-purge` mirror the remote and remove local files it lacks. `download-all` never deletes. `download-missing` never overwrites. |
| `conflict` | `error` (default) stops the build. `local` keeps local edits and `remote` replaces them. |

Before any hook or script runs, the build plans every fetch, downloading
only into a temporary staging folder. It stops on a conflict before
changing any files, then installs the verified files before hashing raw
inputs. A pinned version that is already in place needs no network
access.

A conflict is a local file edited since the last fetch that the fetch
would overwrite or delete. Fetch records are kept in the Git-ignored
`.projr/source/<label>.json` and stay on that machine. A fresh clone has
no record, so local files that differ from the remote count as conflicts
until resolved. A `LICENSE` that projr writes into a licensed raw folder
is not a local change. Production builds record each label’s remote,
version and snapshot under **Raw-input sources** in
`.projr/BUILDLOG.md`, and the console reports each update.

`directories.<label>.source` and a `source` field under `remotes` are no
longer supported. Use `build.source` instead.

Run
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
to fetch outside a build or to resolve a conflict. Its arguments
(`label`, `remote`, `version`, `cue`, `strategy`, `inspect` and
`conflict`) override the configuration, and `cue` defaults to
`if-change`. In an interactive session, a conflict asks whether to keep
the local files, replace them or cancel. Otherwise `conflict` decides.
If two registered remotes publish the same label and `build.source` does
not choose one, name it with `remote`. `version = "current"` restores
this checkout’s recorded build, as
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
does.

``` r

projr::projr_source(label = "raw-survey", conflict = "remote")
```
