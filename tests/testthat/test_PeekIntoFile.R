

test_that("`GetSpec=default` returns `data frame`", {
  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "table.csv")

  # Act
  Result <- PeekIntoFile(TestFile)

  # Assert
  expect_s3_class(Result, "data.frame")
  expect_s3_class(Result, "tbl_df")
})


test_that("`GetSpec=FALSE` returns `data frame`", {
  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "table.csv")

  # Act
  Result <- PeekIntoFile(TestFile, GetSpec = FALSE)

  # Assert
  expect_type(Result, "list")
  expect_s3_class(Result, "data.frame")
  expect_s3_class(Result, "tbl_df")
})



test_that("`GetSpec=FALSE` returns `data frame`", {
  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "table.csv")

  # Act
  Result <- PeekIntoFile(TestFile, GetSpec = TRUE)

  # Assert
  expect_s3_class(Result, "col_spec")
})



test_that("Data with problems returns a data frame with problems listed", {
  ExpectedProblemCount <- 1L # There is 1 problem

  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "table_corrupt.csv")

  # Act
  expect_warning(
    Result <- PeekIntoFile(TestFile, GetSpec = TRUE)
  )

  # Assert
  expect_type(Result, "list")
  expect_s3_class(Result, "data.frame")
  expect_s3_class(Result, "tbl_df")
  expect_equal(ExpectedProblemCount, nrow(Result))
})



test_that("Non-truthy file name throws an error", {
  # Arrange
  TestFile <- list(NA, NULL, NaN, "")

  # Act
  # Assert
  for (f in TestFile) {
    expect_error({
      Result <- PeekIntoFile(f, GetSpec = TRUE)
    }, "^Invalid file name.*")
  }
})





## HasFileHeader() #####

test_that("Module test file: HasFileHeader==TRUE", {
  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "table.csv")

  Data1 <- vroom::vroom(TestFile, show_col_types = FALSE)
  Spec1 <- vroom::spec(Data1)

  # Act
  Result <- HasFileHeader(Spec1, TestFile)

  # Assert
  expect_true(Result)
})


test_that("mtcars: HasFileHeader==TRUE", {
  # Arrange
  TestFile <- vroom::vroom_example("mtcars.csv")

  Data1 <- vroom::vroom(TestFile, show_col_types = FALSE)
  Spec1 <- vroom::spec(Data1)

  # Act
  Result <- HasFileHeader(Spec1, TestFile)

  # Assert
  expect_true(Result)
})


test_that("Module test file: HasFileHeader==FALSE", {
  # Arrange
  TestFile <- system.file("extdata", package = "shiny.CSVImport")
  TestFile <- file.path(TestFile, "headerlesstable.csv")

  Data1 <- vroom::vroom(TestFile, show_col_types = FALSE)
  Spec1 <- vroom::spec(Data1)

  # Act
  Result <- HasFileHeader(Spec1, TestFile)

  # Assert
  expect_false(Result)
})
