# Build hook-related functions

Convenience functions to add or remove hooks to run before or after the
build.

- `projr_yml_hooks_add`: Add hook script(s) to run before or after the
  build.

- `projr_yml_hooks_rm_all`: Remove all hooks.

- `projr_yml_hooks_add_pre`: Add hook(s) to run before the build.

- `projr_yml_hooks_add_post`: Add hook(s) to run after the build.

## Usage

``` r
projr_yml_hooks_add(path, stage, overwrite = TRUE, profile = "default")

projr_yml_hooks_rm_all(profile = "default")

projr_yml_hooks_add_pre(path, overwrite = TRUE, profile = "default")

projr_yml_hooks_add_post(path, overwrite = TRUE, profile = "default")
```

## Arguments

- path:

  character vector. Path(s) to hook scripts, relative to project root
  (if not absolute).

- stage:

  "pre", "post", or "both". Whether to run the hook before the build
  ("pre"), after the build ("post"), or in both stages ("both"). Hooks
  with stage "pre" are stored under `build.hooks.pre`, hooks with stage
  "post" are stored under `build.hooks.post`, and hooks with stage
  "both" are stored under `build.hooks.both`.

- overwrite:

  logical. Whether to overwrite existing hooks or append to them.
  Default is `TRUE`.

- profile:

  character. Profile to add the hook to. If `"default"` (the default),
  the hook is added to the default profile, which is `_projr.yml`.

## Details

Within a stage (pre- or post-build), hooks are run in the order
specified in `_projr.yml`. They are not run in the same environment as
the build process. The pre-build hooks are run immediately after bumping
the project version (if that is done) and immediately before committing
the present state of the code to Git. The post-build hooks are run
immediately after committing the present state of the code to Git, and
before distributing project artifacts to the remotes.

Hooks are stored as simple character vectors in the YAML:

    build:
      hooks:
        pre: ["pre-hook.R"]
        post: ["post-hook.R"]
        both: ["both-hook.R"]
