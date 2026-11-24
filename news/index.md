# Changelog

## projr (development version)

- Production builds check the versions already published to their
  archives before rendering. By default they stop if the checkout is
  behind an archive or the proposed version already exists there. Set
  `build.restrictions.rule` to `target` to publish a newer version from
  an older checkout.

- Builds that mix document types can pass different options to each
  build engine through `args_engine`. For example, use
  `list(quarto_project = list(profile = "print"), rmd = list(quiet = TRUE))`
  to build a Quarto project with its print profile and render a
  standalone R Markdown document quietly. A single engine still accepts
  a flat list.

## projr 0.7.0

- [`projr_reproduce()`](https://satvilab.github.io/projr/reference/projr_reproduce.md)
  runs a completed build in a fresh clone and shows which output files
  matched, changed, were missing or were added.
- Download reports or inputs from a completed build with
  [`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md),
  or copy them from a local analysis with
  [`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md).
  Choose the labels and version you need. projr verifies the files
  before copying them into the folder you choose.
- Installing projr now brings in `httr`, which it uses to download and
  upload GitHub release files. An older project pinned to projr 0.6.1
  can add the missing package with `renv::install("httr")`.
- [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  can recover missing files for an older build from a later upload when
  the release records an identical directory snapshot. It tries the
  requested version and earlier matches first and verifies every file.
- Name archives under `remotes` in `_projr.yml`. Select them under
  `build` when you want to publish. You can keep an archive available to
  readers after you stop uploading new builds to it.

## projr 0.6.1

- Development builds leave `VERSION` and the package version in
  `DESCRIPTION` alone, including after a failed render. You can run a
  development build before creating either file. A dev build run inside
  a production README also keeps the outer build’s archive at its
  production version.
- Use `- exclude: scratch.R` in a `build.scripts` or `dev.scripts` list
  to skip a file or glob. With only exclusions, projr discovers the
  usual scripts and documents and skips the matches. Quoted
  `"!scratch.R"` also works.
- Manual rules in `.gitignore` and `.Rbuildignore` survive a refresh.
  Initialization keeps the original ignore file outside the managed
  section and adds editable rules for `.vdoc.*`, `.DS_Store` and
  `Thumbs.db`.
- Package datasets use the standard `data/` folder without a YAML entry.
  A production build collects staged datasets into the package and
  preserves other datasets already there. Declaring `data: {path: data}`
  also keeps datasets available to Git and R source packages by default.
- Refreshing the managed ignore block removes its old directory heading
  and keeps figures linked from the root `README.md` visible to Git.
- Production builds check the packages needed for a root `README.Rmd`
  before running analysis scripts.
- If adding a package `DESCRIPTION` makes renv select an empty library,
  builds can still use the packages already installed in this checkout’s
  analysis library. Quarto and its pre-build checks use the same project
  library.
- The tracked build history is now `.projr/BUILDLOG.md`. Older checkouts
  keep their entries when the log moves. `CHANGELOG.md` stays at the
  root.
- When a file has stayed the same since its first build,
  [`projr_manifest_file_last_change()`](https://satvilab.github.io/projr/reference/projr_manifest_file_query.md)
  now reports the version where it first appeared.

## projr 0.6.0

### Changes for existing projects

- The first interactive build of a checkout asks once for permission to
  run its scripts and hooks, write configured files, and perform the
  requested Git or archive operations. For an unattended build, pass the
  checkout root as `project`. From that root, use
  `projr_build(project = ".", archive_github = TRUE)` to publish.
- Use `build.scripts` and `dev.scripts` to select scripts in project
  YAML. In
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md),
  replace `file =` with `scripts =`. An empty selection skips scripts
  and rendering.
- Declare a path under `directories` for each label your project uses.
  Configured paths resolve from the project root, including when code
  runs in a subdirectory. A first build creates a starter `_projr.yml`
  if needed. It uses `_raw_data/` for inputs and `_output/` for results.
- Project metadata now lives under `.projr/`. You can keep the base
  `_projr.yml` at the project root or put it in `.projr/config/`
  alongside named profiles. Directory histories live in
  `.projr/manifests/`, and detailed build logs live in `.projr/logs/`.
- A production build imports history from an existing root
  `manifest.csv`. Each directory now has its own human-readable version
  index and file lists. Repeated directory contents reuse the earliest
  matching snapshot.

### Building and restoring an analysis

- Use `projr_build(archive_github = TRUE)` to publish versioned inputs
  and outputs to a GitHub release. In a fresh copy of the project, check
  out the desired build and call
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  to recover its raw inputs. Restore verifies file paths and hashes
  before replacing local files.
- Combine R scripts, standalone documents and one Quarto or Bookdown
  project in a single build. An explicit selection renders the project
  at its last selected document. With automatic selection, the project
  renders first, followed by top-level R scripts in alphabetical order.
- Native hashing streams file contents and defaults to one worker. Set
  `settings.hashing.cores` to increase parallelism or select
  `settings.hashing.approach: legacy` to use the previous hashing
  method. Both approaches produce compatible file hashes.
- A receiving output directory can include other declared directories. A
  destination name can use `@version`, `@branch` or `@uuid` to separate
  archived builds.
- Production builds detect an existing `renv` project and record package
  versions before and after rendering. Set `build.renv` to `true` to
  start a lockfile or `false` to skip snapshots.
- Build progress shows the steps projr actually attempts. When a render
  or Git push fails, the detailed build log records where it stopped.

## projr 0.0.8.9002

- Builds began checking for missing optional packages before rendering
  and reporting the installation commands for them.
- GitHub Releases gained diagnostic messages for uploads, downloads and
  retries.
