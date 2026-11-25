# projr

<!-- badges: start -->
[![R-CMD-check](https://github.com/SATVILab/projr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SATVILab/projr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/SATVILab/projr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SATVILab/projr?branch=main)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/486647841.svg)](https://doi.org/10.5281/zenodo.18402379)
<!-- badges: end -->

Run an R analysis, save the files for each version, and restore the inputs
when you want to reproduce a result.

## Build and save a version

In a GitHub project, put the original inputs in `_raw_data/` and have your
analysis save its results in `_output/`. Run the build from the project folder.

```r
library(projr)
projr_build(archive_github = TRUE)
```

On the first interactive build, projr shows the checkout’s full path and
asks once for permission to run its scripts and hooks, write configured
files, and publish the requested archive. It remembers that checkout.
In an unattended job started at the checkout root, use
`projr_build(project = ".", archive_github = TRUE)`.

The first build writes `_projr.yml` with those folder names, records the
version, and uploads the files to the repository’s `archive` release.
You need GitHub credentials with permission to create releases. People
who can read the repository can also access the archived inputs.

If your project uses other folder names, you can keep them and set their
paths in `_projr.yml`.

## Reproduce a result

Clone the repository and check out the build you want. In R, restore its
raw inputs and run a local preview.

```r
library(projr)
projr_restore()
projr_build_dev()
```

To check whether the production build produces the same files, run
`projr_reproduce("owner/repo")` with your repository name.

To download finished reports or results into a folder of your choice, see the
[guide to retrieving files](https://satvilab.github.io/projr/articles/restore-artifacts.html#get-files-from-a-completed-build).

## Install

```r
install.packages(c("remotes", "gh"))
remotes::install_github("SATVILab/projr@*release")
```

To install version 0.9.0 specifically, run `remotes::install_github("SATVILab/projr@v0.9.0")`.
To install the current development code, use `SATVILab/projr@main`.
[Downloadable source packages](https://github.com/SATVILab/projr/releases)

Follow the [worked example](https://satvilab.github.io/projr/articles/get-started.html)
to publish and reproduce a small analysis. The [configuration reference](https://satvilab.github.io/projr/articles/configuration.html)
lists the available settings and the [function reference](https://satvilab.github.io/projr/reference/index.html)
documents the R functions.

## Getting help

If you encounter a bug or have a feature request, please [file an issue](https://github.com/SATVILab/projr/issues).

## Licence

See [LICENSE](LICENSE) file for details.
