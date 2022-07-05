##
## COL_SPEC ###############
##

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


##
## TIBBLE ###############
##

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



##
## LIST ###############
## Name, NameInFile, Type, Format
##

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



test_that("List: ambiguous vector lengths yield error", {
  # Arrange
  data <- list(
    Name = c("model", colnames(mtcars)),
    NameInFile = c("model", colnames(mtcars)),
    Type = c("c", rep("d", ncol(mtcars))),
    Format = rep("", ncol(mtcars)-4)
  )

  # Act
  # Assert
  expect_error(
    VerifyColSpecFormat(data)
  )
})


test_that("List: missing element yields error", {
  for (i in c("Name", "NameInFile", "Type")) {
    # Arrange
    data <- list(
      Name = c("model", colnames(mtcars)),
      NameInFile = c("model", colnames(mtcars)),
      Type = c("c", rep("d", ncol(mtcars))),
      Format = rep("", ncol(mtcars)-4)
    )
    data[[i]] <- NULL

    # Act
    # Assert
    expect_error(
      VerifyColSpecFormat(data), "Wrong format"
    )

  }
})


test_that("List: missing 'format' will be handled by function", {
  # Arrange
  data <- list(
    Name = c("model", colnames(mtcars)),
    NameInFile = c("model", colnames(mtcars)),
    Type = c("c", rep("d", ncol(mtcars)))
  )

  # Act
  observed <- VerifyColSpecFormat(data)

  # Assert
  expect_s3_class(observed, "col_spec")
  expect_length(observed$cols, length(data$Name))
  expect_equal(names(observed$cols), data$Name)
})



##
## UNKNOWN FORMAT ########
##

test_that("Unknown class returns error", {
  # Arrange
  # Act
  # Assert
  expect_error(
    VerifyColSpecFormat(mtcars)
  )
})


##
## FALSY ########
##
test_that("FALSY but not required", {
  # Arrange
  # Act
  # Assert
  expect_error(
    VerifyColSpecFormat(NA, Required = FALSE)
  )
})

test_that("FALSY and REQUIRED: error", {
  # Arrange
  # Act
  # Assert
  expect_error(
    VerifyColSpecFormat(NA, Required = TRUE),
    "Column specification is required but missing or invalid"
  )
})
