

test_that("VerifyColSpecFormat: mtcars: col_spec: returns as is", {
  # Arrange
  data <- vroom::vroom(vroom::vroom_example("mtcars.csv"), show_col_types = FALSE)
  expected <- vroom::spec(data)
  # Act
  observed <- VerifyColSpecFormat(expected)
  # Assert
  expect_s3_class(observed, "col_spec")
  expect_length(observed$cols, ncol(data))
  expect_equal(names(observed$cols), colnames(data))
})



test_that("VerifyColSpecFormat: mtcars: tibble: returns `col_spec`", {
  # Arrange
  data <- vroom::vroom(vroom::vroom_example("mtcars.csv"), show_col_types = FALSE)
  expected <- vroom::spec(data)

  # Act
  observed <- VerifyColSpecFormat(data)

  # Assert
  expect_s3_class(observed, "col_spec")
  expect_length(observed$cols, ncol(data))
  expect_equal(names(observed$cols), colnames(data))
})


test_that("Valid list", {
  # Arrange
  tbldata <- vroom::vroom(vroom::vroom_example("mtcars.csv"), show_col_types = FALSE)
  expected <- vroom::spec(tbldata)

  data <- list(
    Name = c("model", colnames(mtcars)),
    NameInFile = c("model", colnames(mtcars)),
    Type = c("c", rep("d", ncol(mtcars))),
    Format = rep("", ncol(mtcars)+1)
  )

  # Act
  observed <- VerifyColSpecFormat(data)

  # Assert
  expect_s3_class(observed, "col_spec")
  expect_length(observed$cols, length(data$Name))
  expect_equal(names(observed$cols), data$Name)
})


test_that("Unknown class returns error", {
  # Arrange
  # Act
  # Assert
  expect_error(
    VerifyColSpecFormat(mtcars)
  )
})


