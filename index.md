# projr

## What is projr?

`projr` provides a single intuitive build function that automates
rendering, versioning, archiving, and restoration for reproducible
research projects. With projr, you can focus on your research whilst it
handles the infrastructure.

## The whole game

``` r
# Put raw data in _raw_data/, outputs in _output/, analysis docs in project root:
projr_build()
```

This one function:

- Clears `_output` directory
- Renders all documents (R Markdown, Quarto, Bookdown)
- Versions raw data, outputs, and project
- Manages Git/GitHub commits (if configured)
- Archives to GitHub Releases, OSF, or local storage (optional)

## Try a development build

``` r
projr_build_dev()
```

Development builds:

- Route outputs to cache (`_tmp/`) for inspection
- Don’t bump version numbers
- Don’t upload to archives
- Let you iterate safely without overwriting released outputs

## Restore later

``` r
projr_restore_repo("owner/my-amazing-repo")
```

Restoration:

- Clones the repository from GitHub
- Restores raw data from configured archives
- Reconstructs the full project structure
- Ready to reproduce the analysis

## Why it helps

1.  **Code and data availability**: Automatic archiving to GitHub
    Releases or OSF makes sharing complete research projects effortless

2.  **Correctness via version-linked inputs/outputs**: Manifests link
    every output to the exact version of raw data and code that created
    it, ensuring traceability

3.  **Dependency capture**: Optional renv integration locks R package
    versions for long-term reproducibility

> **Tip**: You can adopt projr at any project stage—whether starting
> fresh or adding structure to existing work.

> **Note**: Git/GitHub prompts are guided; no prior Git knowledge
> needed.

## Installation

``` r
# Once on CRAN (future):
# install.packages("projr")

# For now, install from GitHub:
remotes::install_github("SATVILab/projr")
```

## Documentation

- **[Get
  started](https://satvilab.github.io/projr/articles/get-started.html)** -
  Quick setup guide
- **[How-to
  guides](https://satvilab.github.io/projr/articles/how-to-guides.html)** -
  Task-focused recipes  
- **[Concepts](https://satvilab.github.io/projr/articles/concepts.html)** -
  Understanding projr’s approach
- **[Design](https://satvilab.github.io/projr/articles/design.html)** -
  Design philosophy and architecture
- **[Reference](https://satvilab.github.io/projr/reference/index.html)** -
  Complete function documentation

## Getting help

If you encounter a bug or have a feature request, please [file an
issue](https://github.com/SATVILab/projr/issues).

## Licence

See [LICENSE](https://satvilab.github.io/projr/LICENSE) file for
details.
