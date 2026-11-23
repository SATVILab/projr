# Technical changelog

## 0.6.0

- Development and production build functions accept `project`.
  Interactive approval is stored per canonical checkout path in the
  user’s projr configuration directory. An explicit root in an
  unattended build grants approval for that invocation. The check
  precedes the first project write.
- The build records input and output directory snapshots in separate
  phases. Each label’s `versions.csv` maps project versions to shared
  `files-<snapshot>.csv` lists. Empty directory versions are
  representable. Local archives store these histories in `manifests/`.
  GitHub releases store them together in `manifest.zip`.
- Script selection is applied to the merged active profile. Each glob
  expands alphabetically, and exclusions apply across the selection.
  Repeating an explicit R script or standalone document repeats its
  execution. The build temporarily updates the Quarto or Bookdown
  project YAML and restores it on completion or failure.
- Managed Git and R package ignore rules combine eligible directory
  paths across profiles. They are regenerated from current project
  configuration. Rendered documents and companion assets are collected
  together, including documents in nested directories.
- GitHub restore checks all selected directories against the requested
  build before installing downloaded files. Release templates resolve
  once per build.
- Directory indexes and file lists keep their original bytes when
  checked out with Git line-ending conversion enabled. Project-relative
  paths also work when the operating system uses an alias for a checkout
  directory.
