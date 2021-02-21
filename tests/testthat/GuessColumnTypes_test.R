
# FOR DEBUGGING
# df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
#                header = TRUE, quote  = "", sep    = ";", stringsAsFactors = FALSE,
#                colClasses = "character")
# GuessColumnTypes(df)


test_that("", {
  df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
                header = TRUE, quote  = "", sep    = ";", stringsAsFactors = FALSE,
                colClasses = "character")

  expect_silent(
    obs <- GuessColumnTypes(df)
  )
  exp <- setNames(c("character", "character", "double", "date", "number"),
                  colnames(df))
  expect_identical(obs, exp)
})


test_that("Wrong data types", {
  expect_error(GuessColumnTypes(NULL))
  expect_error(GuessColumnTypes(NA))
  expect_error(GuessColumnTypes(table(1:6, 6:1)))
})
