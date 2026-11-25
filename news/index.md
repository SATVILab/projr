# Changelog

## projr (development version)

- Breaking:
  [`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore_repo.md)
  and
  [`projr_restore_repo_wd()`](https://satvilab.github.io/projr/reference/projr_restore_repo.md)
  take `remote` (a title, or `<type>:<title>`), as
  [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
  does, in place of `pos`, `type`, `title` and `destination`.

- GitHub release uploads that return a server error after storing the
  file no longer fail. projr checks the release, accepts a fully stored
  asset, and otherwise retries once.

## projr 0.9.0

- Fetch raw inputs before a build runs. Register a remote under
  `remotes` and list it under `build.source`. Before any hook or script,
  the build checks the remote, stops if you have changed files locally
  since the last fetch, and then installs verified files. Pin a build
  with `version: "1.2.3"`, or follow the latest build within a major or
  minor version with `"1"` or `"1.2"`. Folders that already hold files
  are left alone by default, so offline builds keep working.

- [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
  fetches inputs outside a build. It uses `build.source`, or finds the
  registered remote that has published each directory, and asks whether
  to keep or replace files you have changed.
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md),
  [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
  and
  [`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md)/[`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md)
  now share the same verified download code.

- Choose where a production build publishes with `build.dest`, as a name
  (`dest: archive`), a list, or a mapping with per-archive settings.
  Name a remote as `github:archive` when the same title is used for a
  GitHub and a local archive.

- Breaking: archives can no longer be defined inline under
  `build.github` or `build.local`. Move each one to
  `remotes.github.<title>` or `remotes.local.<title>` and list it under
  `build.dest`. The error message says what to change.
  `directories.<label>.source` and a remote’s `source:` field are also
  removed; use `build.source` instead.

- Breaking:
  [`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md)
  is deprecated in favour of
  [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).
  Its `destination` argument now errors, and `clear = FALSE` stops when
  local files differ instead of overwriting them.

- Run named pipelines. Define ordered scripts and hooks under
  `pipelines` and select them with `build.pipeline` or `dev.pipeline`,
  or for one build with `projr_build_patch(pipeline = "sensitivity")`.
  Select several pipelines or a glob such as `model-*`. Inline `scripts`
  and `hooks` run before or after the pipeline depending on where you
  place them relative to `pipeline`. `pipeline = FALSE` runs only the
  inline scripts.

- Choose profiles in configuration. A `profile` key in `_projr.yml` or
  `_projr-local.yml` selects projr profiles, or projr and Quarto
  profiles separately. The build’s `profile` argument and the
  `PROJR_PROFILE` and `QUARTO_PROFILE` environment variables still take
  precedence.

- Configuration files in `.projr/config/` can use short names such as
  `analysis.yml`, `default.yml` and `local.yml`.

- A one-off `archive_github = TRUE` or `archive_local = TRUE` build
  records the archive under `remotes`, so later restores know where it
  went.

- `build.restrictions.not_behind_remote` lists remote names, using
  `github:reports` when a title is ambiguous.

## projr 0.8.0

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
