# Test maintenance change record — 2026-10-20

Scope: test safety and test value, plus existing devcontainer settings required
for checks. No scientific methods, new APIs, or package redesign. Starting
checkout: clean at 6bb054a53e41a006bb2be88eb7057aca2a8adda0.

## Safety and environment

- .devcontainer/devcontainer.json: use the existing r-packages feature for declared
  httr, styler, rmarkdown, knitr, bookdown, quarto, codemeta and cffr; disable auto-install and live
  GitHub tests. Rendering tools already exist in the image. Retain image/lock.
- setup.R: session-owned temporary HOME/Git config and explicit user R
  environment/profile paths, no prompts or non-file Git
  transports, blank credential env, scoped gh/gitcreds credential-discovery mocks.
  Restore state at teardown. Remove shared marker/abc/check-directory cleanup.
- helper-github.R, helper-remote.R, helper-setup.R: remove automatic GitHub repo
  lifecycle, shared markers and credential-asset download/loading; remove global
  Git edits and eager project setup. Temporary projects honor requested prefixes.
- test-git.R: replace credential-writing live push test with local bare Git;
  check both command-line Git and gert push the expected commit.
- test-auth.R: mock credential host derivation, including enterprise URLs.
- test-init-github.R: remove credential-asset setup; use current .init_github()
  instead of the removed .init_std_github() helper.
- test-build-pre.R, test-remote-github.R, test-github-httr-comprehensive.R: gate live
  tests before auth/network discovery; retain mocked HTTP tests.
- test-check.R, test-dest-send-prepare.R, test-renv.R: remove DNS prerequisites
  from local tests. Renv integration retains its explicit opt-in; lockfile JSON
  tests no longer initialize/activate renv.
- test-build-check-packages.R and test-misc.R: replace real missing-package
  installation attempts with scoped mocks, force the noninteractive branch,
  restore environment state, and assert zero/one exact installer calls.
- test-safety-offline.R: check explicit opt-in, fail-closed remote deletion,
  session-owned temporary directories and user R configuration location. Never remove unknown markers or repos.

Automatic live GitHub repo creation/deletion remains disabled even with opt-in.
A managed live fixture needs a separate decision. No live lane was run.

- test-cite.R: remove duplicate codemeta/CFF writer calls from the omnibus test.
  Dedicated tests now verify no calls for absent create=FALSE targets and exact
  writer arguments/project context for create=TRUE. Real CITATION/header/BibTeX
  tests remain; external metadata writers' serialization is their packages'
  responsibility. This removes observed CRAN/Bioconductor index lookups without
  fake-file assertions or production citation changes.

## Test value

- test-hash.R: compare MD5 to tools::md5sum; compare unnamed digests for content
  differences; exact classification lists; no project copies for pure tables.
- test-change.R: retain both-empty case; remove added/removed duplicates already
  covered in test-hash.R; no project fixture for empty-manifest constructor.
- test-path-coverage.R: assert exact excluded-directory and file preservation.
- test-path-validation.R: remove duplicate exact-empty exclusion assertions.
- test-path.R: replace 600 RNG assertions with two vector assertions over six
  deterministic seeds, restoring caller RNG state.
- test-build-post-comprehensive.R: create current-cache content and verify it
  survives cleanup, replacing a conditional tautology.
- test-integration-comprehensive.R: exact required hook logs; explicit restore
  success/failure and restored filename/version/hash equality. Each successful
  restore fixture deletes its own source files and asserts they are absent before
  restore, so a no-op returning TRUE cannot pass. No catch-all
  acceptance of restore errors. Finally blocks only clean test-owned state.

- test-scripts.R and test-dir-get-coverage.R: correct stale relative-path
  expectations to explicit absolute fixture paths, preserving explicit dev-script
  relative values and NULL fallback checks. The missing-script error matcher
  still requires the named missing script and cause while accepting its absolute
  path. Production path behavior is unchanged.

## Production fixes

One directly exposed minor fix in R/yml-scripts.R: development-build fallback
now returns .yml_scripts_get_build(profile) directly. That helper already resolves
configured paths and preserves NULL. Removing the duplicate .path_get() prevents
a missing script list from becoming the project directory. Existing test-scripts.R
checks the NULL fallback and resolved configured paths. No restore implementation
is changed. Larger exposed failures remain for the maintainer's decision.

## Remote validation

.github/workflows/offline-tests.yaml uses read-only contents permission and no
secret inputs; checkout does not persist credentials. Dependencies install before
tests. Test steps run as the runner user in a disconnected Linux network namespace
with env -i, temporary HOME and file-only Git transports. The script runs focused full tests
then LITE, recording counts and failures as artifacts. Existing
PAT-enabled workflows are not dispatched.

## Validation

Completed locally before the memory-pressure stop:
- Parsed 193 R files and valid devcontainer JSON.
- Focused safety/Git/auth/init run passed after fixture corrections, with five
  existing skips. Seven new safety expectations passed.
- Hash/change/path/cache focused assertions passed. Comprehensive workflows
  stopped before restore assertions on missing bookdown and a build-script
  fallback defect. Declared dependencies were then installed, and the one-line
  fallback fix was made; these workflows await remote revalidation.
- devtools::document() completed in a disposable copy. The full LITE run and
  dry styler::style_pkg(dry="on") were interrupted for memory relief; neither
  is claimed to have passed.
- git diff --check passed after changes.

At the user's memory-pressure request, all three task-owned containers were
verified stopped; the test container needed forced shutdown after its grace
period. Local WSL logs remain at /tmp/projr-test-{safety,value,lite,docstyle}-20261020.log.
No further local R/tests/builds are started.

The initial publication command was rejected before execution by automatic
approval review. The user subsequently explicitly approved publishing the scoped
branch/workflow, opening a draft PR and running remote checks. Remote results
will be recorded below. Existing PAT workflows are not dispatched. Tests use vanilla R in a disposable existing devcontainer
image, no host HOME mounts, empty inherited env and networking disabled. Declared
dependencies are installed separately from CRAN as an unprivileged container
user, without repository hooks.


### Intermediate remote results

Run 39423727644 at 66bc7ec6 completed LITE (92 files through test-yml.R):
1,748 tests, 5,471 passed expectations, 11 failed expectations, 1 error,
3 warnings, 113 skips. All failures/error were stale expectations in
test-scripts.R and test-dir-get-coverage.R and have been corrected in tests.
The focused phase was cancelled when that head was superseded; its restore
checks are not claimed to have completed. Latest-head validation remains pending.
The launcher now uses testthat::set_max_fails(Inf), the supported API, to keep
all failure details in reports.


Measured diagnostic test times (not a benchmark against the original branch):
older LITE cases totalled 1,011.1 seconds and older focused cases 358.2 seconds.
The slowest LITE families were test-remote-local.R (170.9s), test-build.R (137.5s),
test-restore.R (130.6s), and test-workflow-lite.R (112.3s).
Focused comprehensive integration cases took 310.9s and all four executed with
no skips/failures/errors at 7adc1e04. That run's other failures were solely the
subsequently corrected script assertions. Its three earlier LITE warnings were
external citation-writer registry lookups, now isolated with boundary mocks.

### Final offline boundary corrections

The two pure test-renv.R lockfile cases mock only renv::activate(); the
categorization case no longer initializes/activates a project first. Real
lockfile JSON reading and package classification remain under test.

Citation tests retain real CITATION/header/BibTeX generation but disable cffr
GitHub-topic and dependency enrichment within each relevant test. Setup loads
cffr under a narrowly scoped empty registry response because cffr 1.4.2 queries
available.packages() during namespace loading; the original utils and imported
function bindings are restored immediately afterward. Dedicated codemeta/CFF
writer-dispatch tests mock the external writers and assert their calls.
