library(testthat)
devtools::load_all()
.test_set_select()
devtools::test()

renv::install('devtools', prompt = FALSE)
devtools::load_all()
projr_renv_update()



.projr_change_get_manifest_label <- function(manifest_pre,
                                             manifest_post,
                                             type_pre,
                                             remote_pre,
                                             type_post,
                                             remote_post,
                                             label) {
  # actual version available

  manifest_pre <- .projr_change_get_manifest_label_get_manifest_pre(
    version_pre, manifest_pre
  )
  manifest_post <- .projr_change_get_manifest_label_get_manifest_post(
    version_post, manifest_post
  )
  must_upload <- .projr_change_get_manifest_label_check_match(
    version_pre, version_post, manifest_post, manifest_pre, manifest_post
  )
  # now how to decide what to upload?
  # so, before we just compared.
  # before we compared before to after.
  # now, we could do that, but what if the latest change is
  # with a later version? Do we compare against that?
  # but what's the sense in that, if that isn't even online?
  # does it matter whether it's online or not?
  if (must_upload) {
    .projr_change_get_hash(hash_pre = manifest_pre, hash_post = manifest_post)
  } else {
    .projr_zero_list_manifest_get()
  }


}

.projr_change_get_manifest_label_get_manifest_post <- function(version_post,
                                                               manifest_post) {
  manifest_post[manifest_post[["version"]] == version_post, ]
}

.projr_change_get_manifest_label_get_manifest_pre <- function(version_pre,
                                                              manifest_pre) {
  # actual contents
  manifest_pre[manifest_pre[["version"]] == version_pre, ]
}
