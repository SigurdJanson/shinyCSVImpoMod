##
##
##

test_that("VerifyExpectedFormat: NULL returns Default", {
  # Arrange
  # Act
  observed <- VerifyExpectedFormat(NULL)
  # Assert
  expect_type(observed, "list")
  expect_equal(names(observed), c("LangCode", "Header", "ColSep",
                                  "ThousandsSep", "DecimalsSep",
                                  "DateFormat", "TimeFormat",
                                  "Quote", "StringsAsFactors"))
})



test_that("VerifyExpectedFormat: Unknown field yields error", {
  # Arrange
  Input <- list("en", TRUE, ";", ",", ".", "dd", "__EXTRA__", "mm:ss", "'", FALSE)
  names(Input) <- c("LangCode", "Header", "ColSep", "ThousandsSep",
                    "DecimalsSep", "DateFormat", "__EXTRA__", "TimeFormat",
                    "Quote", "StringsAsFactors")
  Input <- Input[sample.int(length(Input))]

  # Act
  # Assert
  expect_error( VerifyExpectedFormat(Input), "unknown fields" )
})


test_that("VerifyExpectedFormat: Missing field yields error", {
  # Arrange
  Input <- list("en", TRUE, ";", ",", ".", "dd", "mm:ss", "'", FALSE)
  names(Input) <- c("LangCode", "Header", "ColSep", "ThousandsSep",
                    "DecimalsSep", "DateFormat", "TimeFormat",
                    "Quote", "StringsAsFactors")
  Input <- Input[sample.int(length(Input))]
  Input[sample.int(length(Input), 1L)] <- NULL

  # Act
  # Assert
  expect_error( VerifyExpectedFormat(Input), "Missing field" )
})
