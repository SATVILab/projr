# Technical changelog

## Development version

* Add `projr_reproduce()` to select a build by branch, version or commit,
  restore its inputs and compare a fresh production render against its
  recorded output and documentation snapshots.

* Resolve registered GitHub and local destinations from `remotes`. A build
  selects a registered destination by name and can restrict its `content`.
  A GitHub destination can specify its own repository and release tag.
* Add `projr_get()`, `projr_get_local()` and `projr_get_github()` for
  versioned reads into label-specific folders. Stage the files and verify
  the directory snapshot before replacing any local contents.

## 0.6.1

* Reserve version initialization, version transitions and rollback for production
  builds. Use a development cache when no version exists.
* Accept `exclude: pattern` items alongside paths in YAML script lists. Apply
  exclusions across the selection. An exclusion-only list filters the renderer’s
  automatic project inputs and the automatically discovered R scripts.
* Preserve manual ignore patterns and their line endings during managed updates.
  Seed editable ignore rules when initializing a project.
* Make `data` a reserved path for package datasets. Check a conflicting
  directory label only when `projr_use_data()` is called. Collect staged
  datasets into `data/` and `R/sysdata.rda` before building the package.
* Default configured package data to inclusion in Git and source packages.
  Migrate the ignore files to one outer block. Keep the published README’s
  linked figures and hand-written HTML trackable.
* Include the README renderer and, for a README that loads its own package,
  `pkgbuild` in the production prerequisite check.
* Use the checkout’s populated analysis library for render subprocesses when
  a late package DESCRIPTION redirects renv to an empty project library.
  Restore the caller’s library settings on exit.
* Migrate the root build log into `.projr/` using the same conflict checks
  as other project bookkeeping. Older checkouts support read-only lookups.
* Return the earliest version in the final run of matching file hashes
  from `projr_manifest_file_last_change()`, including when a file has the same
  hash in every build.

## 0.6.0

* Development and production build functions accept `project`. Interactive
  approval is stored per canonical checkout path in the user’s projr
  configuration directory. An explicit root in an unattended build grants
  approval for that invocation. The check precedes the first project write.
* The build records input and output directory snapshots in separate phases.
  Each label's `versions.csv` maps project versions to shared
  `files-<snapshot>.csv` lists. Empty directory versions are representable.
  Local archives store these histories in `manifests/`. GitHub releases
  store them together in `manifest.zip`.
* Script selection is applied to the merged active profile. Each glob expands
  alphabetically, and exclusions apply across the selection. Repeating an
  explicit R script or standalone document repeats its execution. The build
  temporarily updates the Quarto or Bookdown project YAML and restores it on
  completion or failure.
* Managed Git and R package ignore rules combine eligible directory paths
  across profiles. They are regenerated from current project configuration.
  Rendered documents and companion assets are collected together, including
  documents in nested directories.
* GitHub restore checks all selected directories against the requested build
  before installing downloaded files. Release templates resolve once per build.
* Directory indexes and file lists keep their original bytes when checked out
  with Git line-ending conversion enabled. Project-relative paths also work
  when the operating system uses an alias for a checkout directory.
