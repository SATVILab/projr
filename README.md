
# projr

<!-- badges: start -->
[![R-CMD-check](https://github.com/SATVILab/projr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SATVILab/projr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/SATVILab/projr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SATVILab/projr?branch=main)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

`projr` is an R package that facilitates reproducible and archived projects. It provides a streamlined workflow for managing project structure, versioning, building, and sharing research outputs.

## Key Features

- **Easy initialization**: Set up projects with metadata, configuration, and version control via `projr_init()`
- **Project versioning**: Semantic versioning (x.y.z) for the entire project
- **Component versioning**: Individual versioning of code, data, figures, and documents
- **Automated builds**: Render literate programming documents (R Markdown, Quarto, Bookdown)
- **Flexible sharing**: Upload project components to GitHub Releases, OSF, or local folders
- **Structured workflow**: Manage project directories through `_projr.yml` configuration
- **Path helpers**: Use `projr_path_get()` to maintain portable, configuration-aware file paths

## Installation

You can install the development version of projr from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("SATVILab/projr")
```

## Quick Start

### Initialize a new project

```r
library(projr)

# Interactive setup with prompts for metadata, Git, and GitHub
projr_init()
```

This sets up:
- Project metadata (DESCRIPTION file)
- `projr` configuration (`_projr.yml`)
- Git repository (optional)
- GitHub connection (optional)
- Directory structure for data, outputs, and documents

### Build your project

```r
# Test build (outputs to temporary directory)
projr_build_dev()

# Production build with versioning and uploads
projr_build()  # or projr_build_patch(), projr_build_minor(), projr_build_major()
```

Building automatically:
- Renders all literate programming documents
- Updates project version
- Versions project components
- Uploads to configured destinations (GitHub, OSF, local folders)

### Use portable paths

```r
# Get project-relative paths defined in _projr.yml
projr_path_get("output")
# "_output"

projr_path_get("output", "figures", "plot.png")
# "_output/figures/plot.png"
```

## Configuration

The `_projr.yml` file controls project behavior:

```yaml
directories:
  raw-data:
    path: _raw_data
  cache: 
    path: _tmp
  output: 
    path: _output
  docs:
    path: docs

build:
  # Configure versioning, uploads to GitHub/OSF, etc.
```

## Documentation

- [Introduction vignette](https://satvilab.github.io/projr/articles/intro.html) - comprehensive guide to `projr` workflow
- [Package website](https://satvilab.github.io/projr/) - full documentation with function reference

## Getting Help

If you encounter a bug or have a feature request, please [file an issue](https://github.com/SATVILab/projr/issues).

## License

See [LICENSE](LICENSE) file for details.
