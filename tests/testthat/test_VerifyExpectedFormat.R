##
##
##

test_that("VerifyExpectedFormat: NULL returns Default", {
  # Arrange
  # Act
  observed <- VerifyExpectedFormat(NULL)
  # Assert
  expect_type(observed, "list")
  # expect_equal(names(observed), c("LangCode", "Header", "ColSep",
  #                                 "ThousandsSep", "DecimalsSep",
  #                                 "DateFormat", "TimeFormat",
  #                                 "Quote", "StringsAsFactors"))
  expect_identical(observed, .ExpectedDefault)
})



test_that("VerifyExpectedFormat: Unknown field yields error", {
  # Arrange
  Input <- list("en", TRUE, ";", ",", ".", "dd", "__EXTRA__", "mm:ss", "'", FALSE)
  names(Input) <- c("LangCode", "Header", "ColSep", "ThousandsSep",
                    "DecimalsSep", "DateFormat", "__EXTRA__", "TimeFormat",
                    "Quote", "StringsAsFactors")
  Input <- Input[sample.int(length(Input))]

  # Act
  observed <- VerifyExpectedFormat(Input)

  # Assert
  expect_length(observed, length(.ExpectedDefault))
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
  observed <- VerifyExpectedFormat(Input)

  # Assert
  expect_length(observed, length(Input) + 1L )
})
