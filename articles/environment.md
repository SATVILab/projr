# Environment Variables

Environment variables configure projr projects without hardcoding
values, useful for managing settings across environments and storing
auth tokens.

## Environment Files

projr reads `KEY=VALUE` files in a specific order. Variables already set
in your system environment are never overridden.

| File                     | Purpose                                       | Git-tracked? |
|--------------------------|-----------------------------------------------|:------------:|
| `_environment.local`     | Machine-specific overrides (highest priority) |      No      |
| `_environment-<profile>` | Profile-specific settings                     |     Yes      |
| `_environment`           | Global defaults (lowest priority)             |     Yes      |

## File Format

One variable per line. Names must match `[a-zA-Z][a-zA-Z0-9_]*`. Values
are taken as-is after `=` (leading/trailing whitespace trimmed). No
quotes needed — quotes become part of the value. Empty values (`VAR=`)
are skipped. Lines starting with `#` are comments; inline `#` comments
are also supported.

``` bash
# Build configuration
PROJR_OUTPUT_LEVEL=std
PROJR_LOG_DETAILED=TRUE  # inline comment

# URLs and paths work without quotes
API_URL=https://api.example.com?param=value
DATA_PATH=/path/to/data

# Secrets go in _environment.local
# GITHUB_PAT=your_token_here
```

Do not put spaces around `=`. `KEY = value` won’t parse correctly.

## Example Setup

`_environment`:

``` bash
PROJR_OUTPUT_LEVEL=std
PROJR_LOG_DETAILED=TRUE
PROJR_CLEAR_OUTPUT=pre
```

`_environment-dev`:

``` bash
PROJR_OUTPUT_LEVEL=debug
PROJR_CLEAR_OUTPUT=never
```

`_environment.local`:

``` bash
GITHUB_PAT=ghp_your_github_token_here
OSF_PAT=your_osf_token_here
```

## Loading Variables

``` r
projr_env_set()                                    # load all files
projr_env_set("_environment.local")                # load specific file
projr_env_set(c("_environment", "_environment-dev")) # load multiple
```

Without arguments, files are loaded in this order (first match wins):

1.  `_environment.local`
2.  `_environment-<QUARTO_PROFILE>` (if set)
3.  `_environment-<PROJR_PROFILE>` (if set)
4.  `_environment`

Non-existent files are silently skipped. `_environment.local` is
automatically added to `.gitignore`.

## Profiles

Set `PROJR_PROFILE` or `QUARTO_PROFILE` to load profile-specific files.

``` r
Sys.setenv(PROJR_PROFILE = "dev")
projr_env_set()  # loads _environment-dev

# Multiple profiles (comma or semicolon separated)
Sys.setenv(PROJR_PROFILE = "test,dev")
projr_env_set()  # loads _environment-test, then _environment-dev
```

`QUARTO_PROFILE` works the same way (comma-separated only) and takes
precedence over `PROJR_PROFILE` in the loading order.

Profile names `"default"` and `"local"` are reserved and filtered out
automatically.

## Key Variables

### Build Control

| Variable             | Values                 | Default                     | Notes            |
|----------------------|------------------------|-----------------------------|------------------|
| `PROJR_OUTPUT_LEVEL` | `none`, `std`, `debug` | `none` (dev) / `std` (prod) | Case-sensitive   |
| `PROJR_CLEAR_OUTPUT` | `pre`, `post`, `never` | `pre`                       | Case-sensitive   |
| `PROJR_LOG_DETAILED` | `TRUE`/`FALSE`         | `TRUE`                      | Case-insensitive |
| `PROJR_AUTO_INSTALL` | `TRUE`/`FALSE`         | Unset                       | Case-insensitive |

`PROJR_OUTPUT_LEVEL` controls console verbosity. `debug` shows detailed
messages including remote operations.

`PROJR_CLEAR_OUTPUT` controls when output directories are cleared: `pre`
clears before build, `post` after, `never` skips clearing.

`PROJR_LOG_DETAILED` controls detailed `.qmd` log files in
`cache/projr/log/`. Build history (`builds.md`) is always maintained.

`PROJR_AUTO_INSTALL` auto-installs missing R packages without prompting.
When unset, interactive sessions prompt the user; non-interactive
sessions error.

``` r
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
projr_build_dev()
```

Build function parameters (e.g., `output_level`, `clear_output`)
override these environment variables when explicitly provided.

### Profiles

| Variable         | Format                       | Notes                                 |
|------------------|------------------------------|---------------------------------------|
| `PROJR_PROFILE`  | Comma or semicolon separated | Earlier profiles take precedence      |
| `QUARTO_PROFILE` | Comma separated              | Takes precedence over `PROJR_PROFILE` |

### Authentication

| Variable       | Purpose                                          |
|----------------|--------------------------------------------------|
| `GITHUB_PAT`   | GitHub API operations (checked first)            |
| `GITHUB_TOKEN` | GitHub API fallback (checked after `GITHUB_PAT`) |
| `OSF_PAT`      | OSF remote destinations                          |

Falls back to `gitcreds` if neither GitHub variable is set. Store tokens
in `_environment.local` only — never commit them.

``` r
projr_instr_auth_github()  # setup instructions
projr_instr_auth_osf()     # setup instructions
```

## Precedence

From highest to lowest priority:

1.  Build function parameters (`output_level = "std"`)
2.  System/session environment (`Sys.setenv(...)`)
3.  `_environment.local`
4.  `_environment-<QUARTO_PROFILE>`
5.  `_environment-<PROJR_PROFILE>`
6.  `_environment`

Variables already set are never overridden by files:

``` r
Sys.setenv(MY_VAR = "original")
# _environment contains: MY_VAR=new
projr_env_set()
Sys.getenv("MY_VAR")  # "original"
```

## Required Variables

Create `_environment.required` to list variables that must be set.
During builds, projr warns (but continues) if any are missing.

``` bash
# _environment.required
GITHUB_PAT
DATABASE_URL
API_KEY
```

``` r
writeLines(c("GITHUB_PAT", "API_KEY"), "_environment.required")
writeLines(c("GITHUB_PAT=ghp_...", "API_KEY=..."), "_environment.local")
projr_build_dev()  # warns if required variables are missing
```

## Tips

- Store secrets in `_environment.local` — it’s auto-added to
  `.gitignore`.
- Use `_environment.required` to document mandatory variables for your
  team.
- `PROJR_OUTPUT_LEVEL` and `PROJR_CLEAR_OUTPUT` are case-sensitive
  (lowercase only). `PROJR_LOG_DETAILED` and `PROJR_AUTO_INSTALL` accept
  any case.

## See Also

- [`?projr_env_set`](https://satvilab.github.io/projr/reference/projr_env_set.md)
  — Load environment variables from files
- [`?projr_build_dev`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  — Development builds
- [`?projr_build`](https://satvilab.github.io/projr/reference/projr_build.md)
  — Production builds
- [`?projr_profile_create`](https://satvilab.github.io/projr/reference/projr_profile_create.md)
  — Create new profiles
