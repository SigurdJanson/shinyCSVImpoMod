
test_that("... import module UI returns a valid tag list", {
  expect_silent(obs <- ModuleImportUI("TEST"))
  expect_s3_class(obs, c("shiny.tag.list", "list"))
})

# Currently passes when run separately but not as part of a package CHECK
# test_that("Run Demo", {
#   expect_silent(ImportCSVDemo())
# })
