# intro

## Introduction

`projr` helps you set up and maintain reproducible, versioned and
shareable R projects. The overall workflow is:

1.  Run
    [`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md)
    to set up the project.
2.  Run
    [`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md)
    (or
    [`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md))
    to build, version and share the project.
3.  Use
    [`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
    to reference project directories in your code.

Project structure and build actions are configured in `_projr.yml`.

### Installation

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("SATVILab/projr")
```

### Workflow

#### Initialise the project

1.  Open R in the directory you want to use (in RStudio: File \> New
    Project).
2.  Run
    [`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md):

``` r
projr::projr_init()
```

This prompts you for project metadata (title, description, author) and
optionally sets up a Git repository connected to GitHub.

#### Build the project

**Production builds** render your documents, bump the version, commit to
Git and upload outputs to configured destinations:

``` r
projr::projr_build_patch()  # bump patch version (0.0.1 -> 0.0.2)
projr::projr_build_minor()  # bump minor version (0.0.1 -> 0.1.0)
projr::projr_build_major()  # bump major version (0.0.1 -> 1.0.0)
```

[`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md)
is an alias for
[`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md).

**Development builds** render documents but save outputs to a temporary
cache directory, protecting your last successful build:

``` r
projr::projr_build_dev()
projr::projr_build_dev("analysis.Rmd")  # render specific file(s)
```

Both detect your document type automatically (Rmd, Qmd, Quarto projects,
bookdown).

#### Versioning

The project version lives in `DESCRIPTION` and follows **semantic
versioning** (`major.minor.patch`). Use
[`projr_version_get()`](https://satvilab.github.io/projr/reference/projr_version_get.md)
and
[`projr_version_set()`](https://satvilab.github.io/projr/reference/projr_version_set.md)
to inspect or change it directly.

After a production build, the version is bumped to a development suffix
(e.g. `0.0.2` becomes `0.0.2-1`) so that development work is always
distinguishable from released versions.

#### Sharing outputs

After each production build, `projr` can upload project components (raw
data, outputs, docs) to GitHub releases, OSF or a local folder. This is
configured under the `build` key in `_projr.yml` and happens
automatically.

### `_projr.yml` configuration

#### `directories`

Specifies where project files live:

``` yaml
directories:
  raw-data:
    path: _raw_data
  cache:
    path: _tmp
  output:
    path: _output
  docs:
    path: docs
```

- **raw-data**: raw input data
- **cache**: intermediate files you do not want to share
- **output**: shareable build outputs (figures, tables)
- **docs**: rendered documents (HTML, PDF)

Use
[`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
to construct paths:

``` r
projr_path_get("output")
# "_output"

projr_path_get("output", "figure", "plot.png")
# "_output/figure/plot.png"

# Use in code:
png(filename = projr_path_get("output", "figure", "plot.png"))
# ... plotting code ...
dev.off()
```

If you later change the output path in `_projr.yml`,
[`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
adapts automatically. It replaces hard-coded paths and
[`file.path()`](https://rdrr.io/r/base/file.path.html)/`here::here()`
calls.

#### `build`

Controls what happens after each production build. For example, with
GitHub configured:

``` yaml
build:
  github:
    input:
      content: [raw-data]
      description: "Project inputs"
```

This uploads the `raw-data` directory to a GitHub release after each
build. Uploads are incremental by default (only when content changes).
Git commit, push and other settings are also configured here.

Destinations include GitHub releases, OSF nodes and local directories.

### Setting up GitHub

You do not strictly need GitHub, but it enables easy sharing and version
control. `projr` handles Git operations for you. If Git is not
installed, `projr` falls back to the `gert` R package.

#### Steps

**1. Create a GitHub account** at [github.com](https://github.com).

**2. Generate a personal access token (PAT):**

``` r
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
usethis::create_github_token()
```

This opens GitHub in your browser. Click “Generate token” and copy it.

**3. Save the token:**

``` r
gitcreds::gitcreds_set()
```

Paste your token when prompted. You can also run
[`projr::projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for detailed instructions.
