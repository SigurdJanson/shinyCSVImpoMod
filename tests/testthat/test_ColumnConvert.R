require(readr)

GetDataFromFile <- function() {
  read.csv(header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
           colClasses = "character",
           text = "Spalte A;Name;Alter;Datum;GermanFloatingPoint;Truth
A;Jan;46;24.01.1975;1.024,64;TRUE
B;Geert;49;19.04.1971;123,45;FALSE
C;Hagen;41;03.02.1980;1.000.000,99;true")
}


test_that("Success Case: numeric (double, integer)", {
  # SETUP
  VecA <- 1:3
  VecB <- seq(from = 1, to = 2, by = 0.5)
  df <- data.frame(A = as.character(VecA), B = as.character(VecB))

  expect_silent(df <- ColumnConvert(df, list("integer", "double")))
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  expect_identical(df$A, as.integer(VecA))
  expect_identical(df$B, VecB)
})


test_that("Success Case: find", {
  # SETUP
  df <- GetDataFromFile()
  NCol <- ncol(df)

  # FIND COLUMNS CONTAINING DIGITS (%d)
  CanBeFound <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  expect_silent( df <- ColumnConvert(df, as.list(rep("find", NCol)), as.list(rep(".", NCol))) )
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  for (i in 1:NCol) {
    if (!CanBeFound[i])
      expect_identical(df[[i]], rep(CanBeFound[i], nrow(df)), info = paste0("Column", i))
    else
      if (i == 4)
        expect_identical(df[[i]], rep(CanBeFound[i], nrow(df)), info = paste0("Column", i))
      else
        expect_identical(df[[i]], c(TRUE, FALSE, TRUE), info = paste0("Column", i))
  }
})



test_that("Success Case: regexfind", {
  # SETUP
  df <- GetDataFromFile()
  NCol <- ncol(df)

  # FIND COLUMNS CONTAINING DIGITS (%d)
  Expected <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  expect_silent( df <- ColumnConvert(df, as.list(rep("regexfind", NCol)), as.list(rep("\\d", NCol))) )
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  for (i in 1:NCol)
    expect_identical(df[[i]], rep(Expected[i], nrow(df)), info = paste0("Column", i))


  # SETUP
  df <- GetDataFromFile()
  NCol <- ncol(df)

  # FIND COLUMNS CONTAINING only DIGITS (^%d$)
  Expected <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  expect_silent( df <- ColumnConvert(df, as.list(rep("regexfind", NCol)), as.list(rep("^\\d*$", NCol))) )
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  for (i in 1:NCol)
    expect_identical(df[[i]], rep(Expected[i], nrow(df)), info = paste0("Column", i))

})


test_that("Success Case: factor", {
  # SETUP
  df <- GetDataFromFile()
  NCol <- ncol(df)

  # ONLY CHARACTER COLUMNS
  #---Expected <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  NLevels  <- c(3, 3, 3, 3, 3, 3)
  expect_silent( df <- ColumnConvert(df, as.list(rep("factor", NCol))) )
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  for (i in 1:NCol) {
    expect_s3_class(df[[i]], "factor")
    expect_length(levels(df[[i]]), NLevels[i])
  }
})


# test_that("Success Case: logical", {
#   # SETUP
#   df <- GetDataFromFile()
#   NCol <- ncol(df)
#
#   # ONLY CHARACTER COLUMNS
#   IsLogical <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
#   expect_silent( df <- ColumnConvert(df, as.list(rep("logical", NCol))) )
#   expect_type(df, "list")
#   expect_s3_class(df, "data.frame")
#   for (i in 1:NCol) {
#     if (IsLogical[i])
#       expect_identical(df[[i]], c(TRUE, FALSE, TRUE), info = paste0("Column", i))
#     else
#       expect_identical(df[[i]], rep(NA, nrow(df)), info = paste0("Column", i))
#   }
# })

test_that("Success Case: Mixed", {
  # SETUP
  df <- GetDataFromFile()
  NCol <- ncol(df)
  Converter <- list("character", "character", "integer", "date", "double", "logical")
  Formats   <- list(NA, NA, NA, "%d.%m.%Y", NA, NA)
  Locale    <- locale(date_names = "de",
                      date_format   = c("%d.%m.%Y"),
                      time_format   = c("%T"),
                      decimal_mark  = ",",
                      grouping_mark = ".",
                      encoding = "UTF-8",
                      asciify = FALSE)
  Expected <- c("character", "character", "integer", "Date", "double", "logical")

  # TEST
  expect_silent( df <- ColumnConvert(df, Converter, Formats) )
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")
  for (i in 1:NCol) {
    if (Expected[i] == "Date")
      expect_s3_class(df[[i]], Expected[i])
    else
      expect_type(df[[i]], Expected[i])
  }
})



test_that("Success Case: Skip Columns", {
  # SETUP
  df <- GetDataFromFile()
  Converter <- list("character", NULL, "integer", "date", "double", NULL)
  Formats   <- list(NA, NA, NA, "%d.%m.%Y", NA, NA)
  Locale    <- locale(date_names = "de",
                      date_format   = c("%d.%m.%Y"),
                      time_format   = c("%T"),
                      decimal_mark  = ",",
                      grouping_mark = ".",
                      encoding = "UTF-8",
                      asciify = FALSE)
  Expected <- c("character", "integer", "Date", "double")

  # TEST
  expect_silent( df <- ColumnConvert(df, Converter, Formats) )
  NCol <- ncol(df) # NCol has changed after operation
  expect_type(df, "list")
  expect_s3_class(df, "data.frame")

  expect_identical(ncol(df), length(unlist(Converter))) # `unlist` drops NULL values

  for (i in 1:NCol) {
    if (Expected[i] == "Date")
      expect_s3_class(df[[i]], Expected[i])
    else
      expect_type(df[[i]], Expected[i])
  }
})




test_that("Preconditions", {
  # SETUP
  VecA <- 1:3
  VecB <- seq(from = 1, to = 2, by = 0.5)
  df <- data.frame(A = as.character(VecA), B = as.character(VecB))

  expect_error(df <- ColumnConvert(1:3, list("integer", "double")))
  expect_error(df <- ColumnConvert(df, c("integer", "double")))
  expect_error(df <- ColumnConvert(df, list("integer", "double"), 1:3))
})
