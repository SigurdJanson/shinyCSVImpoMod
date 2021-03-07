
test_that("runExample: Error", {
  # Obviously non-existing example
  expect_error(shiny.CSVImport::runExample("9999999"), "does not exist")

  # Invalid argument type (though an example "01_hello" exists this still won't work)
  expect_error(shiny.CSVImport::runExample(01), "does not exist")

  # Falsy but not NA
  expect_error(shiny.CSVImport::runExample(NULL), "Type of 'examples' argument is invalid")
  expect_error(shiny.CSVImport::runExample(""), "Type of 'examples' argument is invalid")

  # Ambiguous
  expect_error(shiny.CSVImport::runExample("0"), "Example '.*' does not exist")
})



test_that("runExample: show help", {
  expect_message(shiny.CSVImport::runExample(), "^Valid examples are")
})


# # Won't work - Success cases not tested at the moment
# test_that("runExample: Run App", {
#   skip_on_cran()
#
#   expect_silent(shiny.CSVImport::runExample("01"))
#   expect_silent(shiny.CSVImport::runExample("01_hello"))
# })

