# Get started

## Get started with projr

This guide shows you the minimal setup path to start using projr for
reproducible research projects.

### Installation

``` r
# Once on CRAN:
# install.packages("projr")

# For now, install from GitHub:
remotes::install_github("SATVILab/projr")
```

### Minimal setup

The quickest way to start is to create the essential directories and run
your first build:

``` r
library(projr)

# Create core directories
dir.create("_raw_data", showWarnings = FALSE)
dir.create("_output",   showWarnings = FALSE)

# Run your first build
projr_build()
```

This will: - Clear `_output` directory - Render any R Markdown or Quarto
documents - Version raw data and outputs - Create a manifest linking
inputs to outputs

### Full initialisation (optional)

For a complete project setup with metadata, Git, and GitHub:

#### Option 1: Sensible defaults

``` r
projr_init()  # Uses sensible defaults
```

#### Option 2: Step-by-step prompts

``` r
projr_init_prompt()  # Guides you through each option
```

#### Option 3: Include extras by default

``` r
projr_init_full()  # Includes additional features
```

These initialisation functions set up: - Project directories
(`_raw_data`, `_output`, `_tmp`, `docs`) - `_projr.yml` configuration
file - `DESCRIPTION` file with project metadata - README template - Git
repository (optional) - GitHub repository (optional) - `.gitignore` and
`.Rbuildignore` files - renv for dependency management (optional)

### Development vs final builds

#### Development builds: `projr_build_dev()`

Use this whilst iterating on your code:

``` r
projr_build_dev()
```

Development builds: - Route documents and outputs to the cache directory
(`_tmp/projr/v<version>/`) - **Do not** touch your `_output` directory
(unless `clear_output = "pre"`) - **Do not** bump the version number -
**Do not** upload to archives - Perfect for testing changes without
overwriting your latest release

**Example**: Inspecting cached outputs

``` r
# After a dev build, find your outputs in:
# _tmp/projr/v0.1.0/output/
# _tmp/projr/v0.1.0/docs/

# You can specify which docs to render:
projr_build_dev("analysis.Rmd")
```

#### Final builds: `projr_build()`

Use this when you’re ready to create a new version:

``` r
# Patch version bump (0.1.0 -> 0.1.1)
projr_build()
# or explicitly:
projr_build_patch()

# Minor version bump (0.1.0 -> 0.2.0)
projr_build_minor()

# Major version bump (0.1.0 -> 1.0.0)
projr_build_major()
```

Final builds: - Clear and populate `_output` directory - Bump the
version number - Version all project components - Create manifest
linking inputs to outputs - Archive to configured destinations (GitHub
Releases, OSF, local) - Commit and push to Git (if configured)

### Restoring on a new machine

Clone the repository and restore raw data:

``` r
projr_restore_repo("owner/repo")
```

This will: - Clone the repository from GitHub - Restore raw data from
configured archives (GitHub Releases, OSF, or local) - Set up the
project structure - (Optionally) restore renv dependencies with
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)

### Understanding defaults

When you initialise a project, projr sets up these defaults:

#### Directory labels

- `raw-data`: `_raw_data` - Your source data
- `cache`: `_tmp` - Temporary files and dev build outputs
- `output`: `_output` - Final outputs (figures, tables)
- `docs`: `docs` - Rendered documents (HTML, PDF)

#### Auto-ignore rules

projr automatically configures: - `.gitignore` to exclude large files
and cache directories - `.Rbuildignore` for R package development (if
applicable)

#### Manifest

After each build, projr creates a manifest (`_output/manifest.csv`) that
records: - Which version of raw data was used - Which version of code
was used - Which outputs were generated - SHA-256 checksums for
verification

This ensures you can always trace outputs back to their exact inputs.

#### Changelog and build log

- `NEWS.md` - Manual changelog for human-readable release notes
- `BUILDLOG.md` - Automated build log (configure via YAML)

### Next steps

- Read [How-to
  guides](https://satvilab.github.io/projr/articles/how-to-guides.md)
  for specific tasks
- Understand
  [Concepts](https://satvilab.github.io/projr/articles/concepts.md)
  behind projr’s design
- Explore [Design](https://satvilab.github.io/projr/articles/design.md)
  philosophy
- Browse the
  [Reference](https://satvilab.github.io/projr/reference/index.md) for
  all functions
