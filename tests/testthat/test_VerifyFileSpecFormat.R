##
##
##

test_that("VerifyFileSpecFormat: NULL returns Default", {
  # Arrange
  # Act
  observed <- VerifyFileSpecFormat(NULL)
  # Assert
  expect_type(observed, "list")
  expect_identical(observed, .ExpectedDefault)
})


test_that("VerifyFileSpecFormat: NA returns Default", {
  # Arrange
  # Act
  observed <- VerifyFileSpecFormat(NA)
  # Assert
  expect_type(observed, "list")
  expect_identical(observed, .ExpectedDefault)
})


test_that("VerifyFileSpecFormat: !is.list returns error", {
  # Arrange
  # Act
  # Assert
  expect_error({
    observed <- VerifyFileSpecFormat(1:3)
  }, "^Expected must be a list")
})



test_that("VerifyFileSpecFormat: Unknown field yields error", {
  # Arrange
  Input <- list("en", TRUE, ";", ",", ".", "dd", "__EXTRA__", "mm:ss", "'", FALSE)
  names(Input) <- c("LangCode", "Header", "ColSep", "ThousandsSep",
                    "DecimalsSep", "DateFormat", "__EXTRA__", "TimeFormat",
                    "Quote", "StringsAsFactors")
  Input <- Input[sample.int(length(Input))]

  # Act
  observed <- VerifyFileSpecFormat(Input)

  # Assert
  expect_length(observed, length(.ExpectedDefault))
})



test_that("VerifyFileSpecFormat: Missing field yields error", {
  # Arrange
  Input <- list("en", TRUE, ";", ",", ".", "dd", "mm:ss", "'", FALSE)
  names(Input) <- c("LangCode", "Header", "ColSep", "ThousandsSep",
                    "DecimalsSep", "DateFormat", "TimeFormat",
                    "Quote", "StringsAsFactors")
  Input <- Input[sample.int(length(Input))]
  Input[sample.int(length(Input), 1L)] <- NULL

  # Act
  observed <- VerifyFileSpecFormat(Input)

  # Assert
  expect_length(observed, length(Input) + 1L )
})
