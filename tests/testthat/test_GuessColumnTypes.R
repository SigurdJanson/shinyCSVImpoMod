
# FOR DEBUGGING
# df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
#                header = TRUE, quote  = "", sep    = ";", stringsAsFactors = FALSE,
#                colClasses = "character")
# GuessColumnTypes(df)

GetDataFromFile <- function() {
  read.csv(header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
           colClasses = "character",
           text = "Spalte A;Name;Alter;Datum;GermanFloatingPoint;Truth
A;Jan;46;24.01.1975;1.024,64;TRUE
B;Geert;49;19.04.1971;123,45;FALSE
C;Hagen;41;03.02.1980;1.000.000,99;true")
}


test_that("", {
  df <- GetDataFromFile()

  expect_silent(
    obs <- GuessColumnTypes(df)
  )
  exp <- setNames(c("character", "character", "double", "date", "number", "logical"),
                  colnames(df))
  expect_identical(obs, exp)
})


test_that("Wrong data types", {
  expect_error(GuessColumnTypes(NULL))
  expect_error(GuessColumnTypes(NA))
  expect_error(GuessColumnTypes(table(1:7, 7:1)))
})
