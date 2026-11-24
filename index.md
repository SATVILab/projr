# Build and restore *an R analysis*

Keep inputs in `_raw_data/` and write results to `_output/`. With these
folder names you can publish a versioned build to GitHub Releases
without writing a configuration file, then restore the inputs in a fresh
copy of the project.

[Get started
→](https://satvilab.github.io/projr/articles/get-started.md)[Function
reference](https://satvilab.github.io/projr/reference/index.md)

```
├── _raw_data/
│   └── measurements.csv
├── analysis.R
└── _output/
    └── mean.txt
```

› `projr_build(archive_github = TRUE)`

analysis/

## Run your first build

Open a local clone of a GitHub repository where you can push commits and
create releases. Install projr and its publishing tools once, then load
the package in R.

``` r

install.packages(c("remotes", "gh"))
remotes::install_github("SATVILab/projr@*release")
library(projr)
```

To install version 0.8.0 specifically, run
`remotes::install_github("SATVILab/projr@v0.8.0")`. To install the
current development code, use `SATVILab/projr@main`. [Downloadable
source packages](https://github.com/SATVILab/projr/releases)

### Read an input and write a result

Create `_raw_data/measurements.csv` with a single column named `value`
and the values `2`, `4` and `6`. Save the following script as
`analysis.R` in the project root. A production build writes the mean,
`4`, to `_output/mean.txt`. The output path helper also lets a
development build stage the result in the build cache.

``` r

measurements <- read.csv("_raw_data/measurements.csv")
result <- mean(measurements$value)
output <- projr::projr_path_get("output", "mean.txt")
writeLines(as.character(result), output)
```

### Publish a version

Run this in the R console. On the first build, projr shows the
checkout’s full path and asks once before running the analysis, writing
files and publishing the archive.

``` r

projr_build(archive_github = TRUE)
```

The build creates the starter `_projr.yml` and uploads inputs and
outputs to the `archive` release. Use a GitHub token with write access
to the repository’s contents. People with read access can download the
raw data.

For an unattended job started at the checkout root, use
`projr_build(project = ".", archive_github = TRUE)`.

### Restore the inputs

Clone the same repository into a new folder and open it in R. Run the
command below. projr finds the completed build for the checkout and
verifies the restored inputs against its recorded hashes.

``` r

projr::projr_restore()
```

To run the production build in a fresh clone and compare the results
with the saved files, see [Check whether a build
reproduces](https://satvilab.github.io/projr/articles/restore-artifacts.html#check-whether-a-build-reproduces).

## Adapt the workflow

[Project configuration ↗Keep your existing folder names by declaring
their paths in
`_projr.yml`.](https://satvilab.github.io/projr/articles/configuration.md)

[Scripts and documents ↗Choose R, Quarto and R Markdown files and set
their build
order.](https://satvilab.github.io/projr/articles/scripts-and-hooks.md)

[Getting files ↗Download finished reports or restore the inputs for a
specific
build.](https://satvilab.github.io/projr/articles/restore-artifacts.md)
