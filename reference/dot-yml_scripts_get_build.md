# Get list of scripts to build for production builds

Scripts go directly under build.scripts (no sub-keys allowed) Format:
scripts: c("file1.qmd", "file2.qmd")

## Usage

``` r
.yml_scripts_get_build(profile)
```

## Arguments

- profile:

  Profile name

## Value

Vector of script paths to build or NULL
