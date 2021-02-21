



test_that("... import module UI returns a valid tag list", {
  obs <- ModuleImportUI("TEST")
  expect_s3_class(obs, c("shiny.tag.list", "list"))
})
