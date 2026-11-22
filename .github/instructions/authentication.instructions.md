---
applyTo: "R/auth*.R"
---

# Authentication Guidelines for projr

## Purpose & Scope

Authentication checks for GitHub and OSF operations. All API calls must fail gracefully with helpful error messages when credentials are missing.

---

## Authentication Functions

### Core Functions (R/auth.R)

```r
.auth_get_github_pat()           # Retrieve GitHub PAT from environment/gitcreds
.auth_get_osf_pat()              # Retrieve OSF PAT from environment
.auth_check_github(context)      # Check and throw error if GitHub auth missing
.auth_check_osf(context)         # Check and throw error if OSF auth missing
```

### Exported Functions

```r
projr_instr_auth_github()        # Print GitHub authentication instructions
projr_instr_auth_osf()           # Print OSF authentication instructions
```

---

## Where Auth Checks Are Required

### GitHub Operations

All functions that call `gh::` or `gitcreds::` must have `.auth_check_github()`:

```r
# Before any gh:: calls
.auth_check_github("operation description")

# Examples of functions requiring auth check:

# - .git_clone() - When inferring username from gh::gh_whoami()
# - .init_github_impl() - Before creating GitHub repository
# - .remote_host_rm_github() - Before deleting GitHub repository
# - .gh_guess_repo() - When using gh::gh_tree_remote()
```

### OSF Operations

All OSF wrapper functions must have `.auth_check_osf()`:

```r
# Before any osfr:: calls
.auth_check_osf("operation description")

# Examples of functions requiring auth check:
# - .remote_create_osf() - Creating OSF nodes
# - .remote_get_osf() - Retrieving OSF nodes
# - .remote_host_rm_osf() - Deleting OSF nodes
# - .osf_upload(), .osf_download(), .osf_ls_files() - All OSF wrappers
```

---

## Authentication Check Pattern

```r
# Correct: GitHub operation with auth check
.my_github_function <- function(...) {
  .auth_check_github("operation description")
  # Now safe to call gh:: functions
  user <- gh::gh_whoami()$login
  # ... rest of implementation
}

# Correct: OSF operation with auth check
.my_osf_function <- function(...) {
  .auth_check_osf("operation description")
  # Now safe to call osfr:: functions
  node <- osfr::osf_retrieve_node(id)
  # ... rest of implementation
}

# Incorrect: Missing auth check before API call
.my_github_function <- function(...) {
  user <- gh::gh_whoami()$login  # Will fail silently without auth
}
```

---

## Environment Variables

### GitHub Authentication

**Token Search Priority** (implemented in `.auth_get_github_pat_find()`):

1. `GITHUB_PAT` environment variable - Checked first, highest priority
2. `gh::gh_token()` - Uses gh package to retrieve token if available (may check GITHUB_TOKEN, gitcreds, etc.)
3. `gitcreds::gitcreds_get()` - Fallback to gitcreds credential manager if gh package not available
4. `GITHUB_TOKEN` environment variable - Final fallback, lowest priority

This precedence ensures user-specific tokens (`GITHUB_PAT`) take priority over system tokens (`GITHUB_TOKEN`), which is important for CI/CD environments where `GITHUB_TOKEN` may have limited permissions.

The token should have appropriate scopes:
- `repo` - For repository operations
- `workflow` - For GitHub Actions
- Other scopes as needed

### OSF Authentication

- `OSF_PAT` - Required for OSF remote destinations
- No fallback mechanism
- Must be set explicitly for OSF operations

---

## Testing Authentication

Use fixture tokens and mocked credential lookups to cover both authenticated and unauthenticated paths. Ordinary testthat runs and R CMD check must work offline in a clean test home. Exercise real GitHub Releases in a dedicated fixture repository using its job's scoped token. [Proposed live release fixture](../live-release-fixture.md)

---

## Build-Time Auth Checks

During builds, `.build_check_auth_remote()` is called via `.build_env_check()` to verify required credentials are available for configured remote destinations.

This prevents build failures mid-process when credentials are missing.

---

## Error Messages

Provide clear, actionable error messages:

```r
# Good error message
stop(
  "GitHub authentication is required for ", context, ". ",
  "Set GITHUB_PAT environment variable or run projr_instr_auth_github() for instructions."
)

# Good error message
stop(
  "OSF authentication is required for ", context, ". ",
  "Set OSF_PAT environment variable or run projr_instr_auth_osf() for instructions."
)

# Bad error message
stop("Auth failed")  # Not helpful!
```

---

## Code Examples

```r
# Correct: Full auth check pattern
.init_github_impl <- function(repo_name, private = FALSE) {
  .auth_check_github("creating GitHub repository")

  # Now safe to use gh:: functions
  user <- gh::gh_whoami()$login
  repo <- gh::gh(
    "POST /user/repos",
    name = repo_name,
    private = private
  )

  return(repo)
}

# Correct: OSF operation with auth check
.remote_create_osf <- function(title) {
  .auth_check_osf("creating OSF node")

  # Now safe to use osfr:: functions
  node <- osfr::osf_create_project(title)
  return(node)
}

# Incorrect: No auth check
.init_github_impl <- function(repo_name) {
  gh::gh("POST /user/repos", name = repo_name)  # May fail silently
}
```

---

## Integration with Remote Destinations

When adding remote destinations that require authentication:

1. Add auth check at the start of the function
2. Provide clear context in the error message
3. Add build-time auth check to `.build_check_auth_remote()`
4. Document required environment variables
5. Add tests with `skip_if(!nzchar(Sys.getenv("TOKEN_NAME")))`

---

## Common Pitfalls

- Don't forget auth check when adding new GitHub/OSF operations
- Don't assume credentials are available - always check first
- Provide helpful error messages with instructions
- Remember to restore env vars in tests that modify them
- Use appropriate skip conditions in tests requiring credentials
