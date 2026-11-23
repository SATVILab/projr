# projr 0.6.0

## Changes for existing projects

* The first interactive build of a checkout asks once for permission to run
  its scripts and hooks, write configured files, and perform the requested
  Git or archive operations. For an unattended build, pass the checkout
  root as `project`. From that root, use
  `projr_build(project = ".", archive_github = TRUE)` to publish.
* Use `build.scripts` and `dev.scripts` to select scripts in project YAML.
  In `projr_build_dev()`, replace `file =` with `scripts =`. An empty
  selection skips scripts and rendering.
* Declare a path under `directories` for each label your project uses.
  Configured paths resolve from the project root, including when code runs
  in a subdirectory. A first build creates a starter `_projr.yml` if needed.
  It uses `_raw_data/` for inputs and `_output/` for results.
* Project metadata now lives under `.projr/`. You can keep the base
  `_projr.yml` at the project root or put it in `.projr/config/` alongside
  named profiles. Directory histories live in `.projr/manifests/`, and
  detailed build logs live in `.projr/logs/`.
* A production build imports history from an existing root `manifest.csv`.
  Each directory now has its own human-readable version index and file lists.
  Repeated directory contents reuse the earliest matching snapshot.

## Building and restoring an analysis

* Use `projr_build(archive_github = TRUE)` to publish versioned inputs and
  outputs to a GitHub release. In a fresh copy of the project, check out
  the desired build and call `projr_restore()` to recover its raw inputs.
  Restore verifies file paths and hashes before replacing local files.
* Combine R scripts, standalone documents and one Quarto or Bookdown project
  in a single build. An explicit selection renders the project at its last
  selected document. With automatic selection, the project renders first,
  followed by top-level R scripts in alphabetical order.
* Native hashing streams file contents and defaults to one worker. Set
  `settings.hashing.cores` to increase parallelism or select
  `settings.hashing.approach: legacy` to use the previous hashing method.
  Both approaches produce compatible file hashes.
* A receiving output directory can include other declared directories.
  A destination name can use `@version`, `@branch` or `@uuid` to separate
  archived builds.

# projr 0.0.8.9002

* Builds began checking for missing optional packages before rendering and
  reporting the installation commands for them.
* GitHub Releases gained diagnostic messages for uploads, downloads and retries.
