---
applyTo: "**/*.{yml,yaml}"
---

# YAML Configuration Guidelines for projr

## Purpose & Scope

Guidelines for working with YAML configuration files in projr, including `_projr.yml`, build scripts, hooks, and validation.

---

## YAML Configuration Structure

The package uses YAML configuration (`_projr.yml`) for project settings.

### Key Configuration Areas

- `directories` - Directory labels, paths, ignore settings
- `build.git` - Git commit, push, add-untracked settings
- `build.restrictions` - Build restrictions (e.g., branch restrictions)
- `build.scripts` - Scripts to build (overrides auto-detection)
- `build.hooks` - Pre/post build hooks
- `dev.scripts` - Development-specific build scripts
- `dev.hooks` - Development-specific hooks
- `metadata.version-format` - Version string format

---

## Build Scripts Configuration

### Purpose

Explicitly specify which documents/scripts to build instead of relying on automatic detection.

### YAML Structure

```yaml
# Production build scripts
build:
  scripts:
    - analysis.qmd
    - report.Rmd
    - data-processing.R

# Development build scripts (exclusive override)
dev:
  scripts:
    - quick-test.qmd
    - debug.R
```

### Key Points

- `build.scripts` only accepts plain character vectors (no sub-keys or named lists)
- `dev.scripts` provides exclusive control for dev builds (no fallback to `build.scripts`)
- Scripts specified in `_projr.yml` override `_quarto.yml` and `_bookdown.yml` configurations
- Priority order for dev builds: file param → dev.scripts → _quarto/_bookdown → auto-detect
- Priority order for production builds: file param → build.scripts → _quarto/_bookdown → auto-detect

---

## Build Hooks Configuration

### Purpose

Run custom scripts before (pre) or after (post) the build process.

### YAML Structure

```yaml
build:
  hooks:
    pre:
      - setup.R
      - download-data.R
    post:
      - cleanup.R
      - send-notifications.R
    both:
      - log-timestamp.R

dev:
  hooks:
    pre: dev-setup.R
    both: dev-logger.R
```

### Key Points

- Hooks are stored as simple character vectors (no titles, no "path" keys)
- Three stage keys: `pre` (before build), `post` (after build), `both` (both stages)
- `build.hooks` are always ignored in dev runs
- `dev.hooks` are always ignored in production runs
- Hooks run in the order specified in `_projr.yml`
- Hooks are NOT run in the same environment as the build process

### Hook Execution Timing

- **Pre-build hooks**: Run after version bump, before Git commit
- **Post-build hooks**: Run after Git commit, before distributing artifacts

---

## Build Restrictions Configuration

### Purpose

Control which branches can perform production builds and whether to check if the branch is behind its remote upstream. Allows restricting builds to specific branches (e.g., `main`, `release`) while allowing development builds on any branch.

### YAML Structure

```yaml
build:
  restrictions:
    branch: main       # Only allow builds on main branch
    not_behind: true   # Check if branch is behind remote (default: true)
    # OR
    branch:            # Allow builds on multiple branches
      - main
      - release
      - hotfix
    not_behind: false  # Disable upstream check
    # OR
    branch: true       # Allow builds on any branch (default, can be omitted)
```

### Key Points

- Restrictions only apply to production builds (`projr_build_*()` functions), NOT dev builds (`projr_build_dev()`)
- **branch**:
  - `branch: true` (default) - Allows builds on any branch
  - `branch: c("main", "dev")` - Only allows builds on specified branches
  - `branch: false` - Restricts builds on all branches (rarely useful)
- **not_behind**:
  - `not_behind: true` (default) - Build fails if branch is behind remote upstream
  - `not_behind: false` - Disables the check for being behind remote
- If not in a Git repository, restrictions are not enforced
- Error messages clearly indicate current branch and allowed branches

### Setting Restrictions

```r
# Allow builds only on main branch
projr_yml_restrictions_set(branch = "main")

# Allow builds on multiple branches
projr_yml_restrictions_set(branch = c("main", "dev", "release"))

# Remove restrictions (allow on any branch)
projr_yml_restrictions_set(branch = TRUE)

# Disable check for being behind remote
projr_yml_restrictions_set(not_behind = FALSE)

# Set both branch and not_behind restrictions
projr_yml_restrictions_set(branch = "main", not_behind = FALSE)
```

---

## File Existence Validation

Before any build starts, all scripts and hooks are validated:
- Build scripts from `build.scripts`
- Dev scripts from `dev.scripts`
- Build hooks from `build.hooks` (pre, post, both)
- Dev hooks from `dev.hooks` (pre, post, both)

Function: `.yml_scripts_hooks_check_exist(is_dev_build)` in `R/yml-check.R`

---

## YAML Validation

The `projr_yml_check()` function validates the entire `_projr.yml` configuration.

### Validated Sections

- **Directories** - Directory labels, paths, ignore settings
- **Build settings** - Git, dest, and label configurations
- **Build restrictions** - build.restrictions.branch is logical or character
- **Dev settings** - dev.scripts and dev.hooks keys
- **Metadata** - metadata.version-format
- **Scripts** - build.scripts and dev.scripts are character vectors
- **Hooks** - build.hooks and dev.hooks structure (pre/post/both stages)
- **Cite** - build.cite is either logical or list with valid keys

### Valid build.* Keys

"dev-output", "script", "hooks", "scripts", "git", "github", "package", "local", "osf", "cite", "restrictions"

### When Adding New YAML Options

1. Add validation logic to appropriate check function in `R/yml-check.R`
2. Add the check function to `projr_yml_check()` if it's a new top-level category
3. Write tests in `tests/testthat/test-yml-check.R` covering valid and invalid configurations

---

## Code Examples

```yaml
# Correct: Simple character vectors for scripts
build:
  scripts:
    - analysis.qmd
    - report.Rmd

# Correct: Hooks with different stages
build:
  hooks:
    pre:
      - setup.R
    post:
      - cleanup.R

# Incorrect: Don't add sub-keys under build.scripts
build:
  scripts:
    - path: analysis.qmd  # Wrong!
      title: "Analysis"  # Wrong!

# Incorrect: Don't create nested structures for hooks
build:
  hooks:
    pre:
      - path: setup.R  # Wrong!
        title: "Setup"  # Wrong!
```

---

## Common Pitfalls

- Don't add sub-keys under `build.scripts` - it only accepts plain vectors
- Don't create nested structures for hooks - they're simple character vectors
- Remember `dev.scripts` is exclusive (no fallback to `build.scripts`)
- Remember `build.hooks` are ignored in dev runs; use `dev.hooks` for dev-specific hooks
- File paths in scripts/hooks are relative to project root

## Working with YAML in R

- Configuration is read and managed through `yml-*.R` files
- Functions like `.yml_dir_get_label_*()` retrieve configuration values
- Use existing YAML helper functions rather than reading files directly
