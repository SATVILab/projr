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

Start in an R project linked to your GitHub repository. In the R console,
install the package with `remotes::install_github("SATVILab/projr")` and
load it with `library(projr)`. Install `remotes` first with
`install.packages("remotes")` if you need it.

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

Run this in the R console. The first build creates the starter
`_projr.yml`, runs your script, records the file hashes, and uploads raw
inputs and outputs to the `archive` release.

```r
projr_build(archive_github = TRUE)
```

Authenticate with a GitHub account that can write releases in the repository.
People with read access to the repository can download the uploaded raw data.
For a public repository, that includes anyone.

### Restore the inputs

Clone the same repository into a new folder and open it in R. Run the command below.
projr finds the completed build for the checkout and verifies the restored
inputs against its recorded hashes.

```r
projr::projr_restore()
```

</div>

## Adapt the workflow

<div class="projr-guides">
<div><a class="projr-guide" href="articles/configuration.html"><span class="projr-guide-title">Project configuration <span aria-hidden="true">↗</span></span><span>Keep your existing folder names by declaring their paths in <code>_projr.yml</code>.</span></a></div>
<div><a class="projr-guide" href="articles/scripts-and-hooks.html"><span class="projr-guide-title">Scripts and documents <span aria-hidden="true">↗</span></span><span>Choose R, Quarto and R Markdown files and set their build order.</span></a></div>
<div><a class="projr-guide" href="articles/restore-artifacts.html"><span class="projr-guide-title">Restoring files <span aria-hidden="true">↗</span></span><span>Select a version or retrieve data from another archive.</span></a></div>
</div>
