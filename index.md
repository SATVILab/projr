<div class="projr-hero">
<div class="projr-intro">
<h1>Build and restore <em>an R analysis</em></h1>
<p>Keep inputs in <code>_raw_data/</code> and write results to <code>_output/</code>. With these folder names you can publish a versioned build to GitHub Releases without writing a configuration file, then restore the inputs in a fresh copy of the project.</p>
<div class="projr-actions"><a class="btn btn-primary" href="articles/get-started.html">Get started <span aria-hidden="true">→</span></a><a class="projr-quiet-link" href="reference/index.html">Function reference</a></div>
</div>
<figure class="projr-project">
<figcaption><span class="fa-regular fa-folder-open" aria-hidden="true"></span> analysis/</figcaption>
<pre aria-label="Example project directory"><code><span class="projr-lines">├──</span> <strong>_raw_data/</strong>
<span class="projr-lines">│   └──</span> measurements.csv
<span class="projr-lines">├──</span> analysis.R
<span class="projr-lines">└──</span> <strong>_output/</strong>
<span class="projr-lines">    └──</span> mean.txt</code></pre>
<div class="projr-console"><span aria-hidden="true">›</span> <code>projr_build(archive_github = TRUE)</code></div>
</figure>
</div>

## Run your first build

Open a local clone of a GitHub repository where you can push commits
and create releases. Install projr and its publishing tools once, then
load the package in R.

```r
install.packages(c("remotes", "gh"))
remotes::install_github("SATVILab/projr@*release")
library(projr)
```

To install version 0.7.0 specifically, run `remotes::install_github("SATVILab/projr@v0.7.0")`.
To install the current development code, use `SATVILab/projr@main`.
[Downloadable source packages](https://github.com/SATVILab/projr/releases)

<div class="projr-workflow">

### Read an input and write a result

Create `_raw_data/measurements.csv` with a single column named `value` and
the values `2`, `4` and `6`. Save the following script as `analysis.R` in
the project root. A production build writes the mean, `4`, to
`_output/mean.txt`. The output path helper also lets a development
build stage the result in the build cache.

```r
measurements <- read.csv("_raw_data/measurements.csv")
result <- mean(measurements$value)
output <- projr::projr_path_get("output", "mean.txt")
writeLines(as.character(result), output)
```

### Publish a version

Run this in the R console. On the first build, projr shows the checkout’s
full path and asks once before running the analysis, writing files and
publishing the archive.

```r
projr_build(archive_github = TRUE)
```

The build creates the starter `_projr.yml` and uploads inputs and outputs
to the `archive` release. Use a GitHub token with write access to the
repository’s contents. People with read access can download the raw data.

For an unattended job started at the checkout root, use
`projr_build(project = ".", archive_github = TRUE)`.

### Restore the inputs

Clone the same repository into a new folder and open it in R. Run the command below.
projr finds the completed build for the checkout and verifies the restored
inputs against its recorded hashes.

```r
projr::projr_restore()
```

To run the production build in a fresh clone and compare the results with
the saved files, see [Check whether a build reproduces](articles/restore-artifacts.html#check-whether-a-build-reproduces).

</div>

## Adapt the workflow

<div class="projr-guides">
<div><a class="projr-guide" href="articles/configuration.html"><span class="projr-guide-title">Project configuration <span aria-hidden="true">↗</span></span><span>Keep your existing folder names by declaring their paths in <code>_projr.yml</code>.</span></a></div>
<div><a class="projr-guide" href="articles/scripts-and-hooks.html"><span class="projr-guide-title">Scripts and documents <span aria-hidden="true">↗</span></span><span>Choose R, Quarto and R Markdown files and set their build order.</span></a></div>
<div><a class="projr-guide" href="articles/restore-artifacts.html"><span class="projr-guide-title">Getting files <span aria-hidden="true">↗</span></span><span>Download finished reports or restore the inputs for a specific build.</span></a></div>
</div>
