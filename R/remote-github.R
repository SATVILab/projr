# GitHub Release API - Compatibility Layer
# ==========================================
# This file provides backwards-compatible function names that delegate to
# the actual implementations in remote-github-httr.R
#
# New naming convention:
# - .gh_release_*_httr() for direct httr implementations
# - .gh_release_*_pb() for piggyback implementations (in remote-github-piggyback.R)
# - Old .github_* names maintained here for backwards compatibility

# Backwards-compatible aliases (delegate to _httr implementations)
.github_api_base <- function(api_url = NULL) {
  .gh_api_base(api_url)
}

.github_release_exists <- function(repo, tag, api_url = NULL, token = NULL) {
  .gh_release_exists_httr(repo, tag, api_url, token)
}

.github_release_get <- function(repo, tag, api_url = NULL, token = NULL) {
  .gh_release_get_httr(repo, tag, api_url, token)
}

.github_asset_delete <- function(repo, asset_id, api_url = NULL, token = NULL) {
  .gh_release_asset_delete_httr(repo, asset_id, api_url, token)
}

.github_asset_upload <- function(repo, tag, file_path, asset_name = NULL,
                                  overwrite = TRUE, api_url = NULL, token = NULL) {
  .gh_release_asset_upload_httr(repo, tag, file_path, asset_name, overwrite, api_url, token)
}
