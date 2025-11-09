# Build script-related functions

Convenience functions to add or remove scripts to run before or after
the build.

- .yml_script_add\`: Add a script to run before or after the build.

- .yml_script_rm\`: Remove scripts to run.

.yml_script_add_pre` and .yml_script_add_post` are wrappers around
.yml_script_add`that set the`stage`argument to`"pre"`or`"post"`, respectively. .yml_script_rm_all`
removes all scripts.

## Usage

``` r
projr_yml_script_add(
  path,
  title,
  stage,
  cue = NULL,
  overwrite = TRUE,
  profile = "default"
)

projr_yml_script_rm(title, path = NULL, profile = "default")

projr_yml_script_rm_all(profile = "default")

projr_yml_script_add_pre(
  path,
  title,
  cue = NULL,
  overwrite = TRUE,
  profile = "default"
)

projr_yml_script_add_post(
  path,
  title,
  cue = NULL,
  overwrite = TRUE,
  profile = "default"
)
```

## Arguments

- path:

  character vector. Path(s) to scripts, relative to project root (if not
  absolute).

- title:

  character. Title for set of scripts. Initial and trailing spaces are
  removed, and the middle spaces are converted to dashes. For example,
  `" a b "` is converted to `"a-b"`. \`

- stage:

  "pre" or "post". Whether to run the script before or after the build.

- cue:

  "build", "dev", "patch", "minor" or "major". Which minimum build level
  triggers the scripts. "build" and "dev" are equivalent, and always
  trigger the scripts.

- overwrite:

  logical. Whether to overwrite any script settings of the same title in
  the `projr` configuration file. If `FALSE` and there already exists a
  key under `build/script` with the name `title`, an error is thrown.
  Default is `TRUE`.

- profile:

  character. Profile to add the script to. If
  ``` "default"`` (the default), the script is added to the default profile, which is  ```\_projr.yml\`.

## Details

Within a stage (pre- or post-build), scripts are run in the order set in
`_projr.yml`. They are not run in the same environment as the build
process. The pre-build scripts are run immediately after bumping the
project version (if that is done) and immediately before committing the
present state of the code to Git. The post-build scripts are run
immediately after committing the present state of the code to Git, and
before distributing project artefacts to the remotes.
