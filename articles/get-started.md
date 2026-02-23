# Get started

## Get started with projr

### Installation

``` r
# Install from GitHub:
remotes::install_github("SATVILab/projr")
```

### Quick start

Set up a project and run your first build:

``` r
library(projr)

# Initialise with sensible defaults
projr_init()

# Run a dev build to test
projr_build_dev()
```

[`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md)
creates project directories, a `_projr.yml` config file,
`.gitignore`/`.Rbuildignore` entries, and a `DESCRIPTION` file.

For an interactive walkthrough, use
[`projr_init_prompt()`](https://satvilab.github.io/projr/reference/projr_init_prompt.md).
For a full setup including Git, GitHub, and renv, use
[`projr_init_all()`](https://satvilab.github.io/projr/reference/projr_init.md).

### Builds

#### Development builds

Use
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
while iterating. Dev builds:

- Route outputs to the cache directory (`_tmp/projr/v<version>/`)
- Do not bump the version or upload to archives
- Leave `_output` untouched

``` r
# Build everything
projr_build_dev()

# Build a specific document
projr_build_dev("analysis.Rmd")
```

#### Production builds

Use production builds when you are ready to release a new version:

``` r
# Patch bump (0.1.0 -> 0.1.1)
projr_build()
projr_build_patch() # equivalent

# Minor bump (0.1.0 -> 0.2.0)
projr_build_minor()

# Major bump (0.1.0 -> 1.0.0)
projr_build_major()
```

Production builds clear and populate `_output`, bump the version, record
a manifest of inputs and outputs, archive to configured destinations
(GitHub Releases, OSF, or local), and commit/push to Git if configured.

### Archiving outputs

Add a destination so production builds automatically archive outputs:

``` r
# Archive to GitHub Releases
projr_yml_dest_add_github(
  title = "default",
  content = "output",
  structure = "archive"
)

# Or archive to a local directory
projr_yml_dest_add_local(
  title = "backup",
  content = "output",
  path = "/path/to/archive",
  structure = "latest"
)
```

### Restoring a project

Clone and restore raw data on a new machine:

``` r
projr_restore_repo("owner/repo")
```

This clones the repo, restores raw data from configured archives, and
sets up the project structure.

To update content from archives in an existing project:

``` r
projr_content_update()
```

### Default directories

| Label      | Path        | Purpose                               |
|------------|-------------|---------------------------------------|
| `raw-data` | `_raw_data` | Source data                           |
| `cache`    | `_tmp`      | Temporary files and dev build outputs |
| `output`   | `_output`   | Final outputs (figures, tables)       |
| `docs`     | `docs`      | Rendered documents (HTML, PDF)        |

Access directories programmatically:

``` r
projr_path_get_dir("output")             # actual path: _output
projr_path_get_dir("output", safe = TRUE) # cache path: _tmp/projr/v0.1.0/output
```

### Manifest

Each build records file hashes in `manifest.csv`, linking outputs to
exact input versions. Query it with:

``` r
projr_manifest_changes("0.0.1", "0.0.2")
projr_manifest_last_change()
```

### Configuration

All settings live in `_projr.yml`. Inspect and validate with:

``` r
projr_yml_get()
projr_yml_check()
```

Configure Git behaviour:

``` r
projr_yml_git_set(commit = TRUE, push = TRUE, add_untracked = TRUE)
```

### Next steps

- [How-to
  guides](https://satvilab.github.io/projr/articles/how-to-guides.md)
  for specific tasks
- [Concepts](https://satvilab.github.io/projr/articles/concepts.md)
  behind projr’s design
- [Design](https://satvilab.github.io/projr/articles/design.md)
  philosophy
- [Reference](https://satvilab.github.io/projr/reference/index.md) for
  all functions
