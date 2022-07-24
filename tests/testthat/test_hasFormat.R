
# CHARACTER, ABBREVIATION ======


test_that("Character: list of character(1), abbreviations, faulty", {
  #
  # Act
  # Assert
  expect_silent(
    result <- hasFormat("t", "d", "i", "T") # all valid
  )
  expect_error(
    result <- hasFormat("t", "d", "8", "i", "T") # 8 is invalid
  )
})




test_that("Character: list of character(1), abbreviations", {
  #
  # Act
  result <- hasFormat("t", "d", "i", "T")

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})


test_that("Character: character vector, abbreviations", {
  #
  x <- c("t", "d", "i", "T") # time, double, integer, datetime

  # Act
  result <- hasFormat(x)

  # Assert
  expect_identical(
    result,
    list(TRUE, FALSE, FALSE, TRUE)
  )
})


test_that("Character: character vector, abbreviations", {
  #
  x1 <- c("i") # integer
  x2 <- c("t", "n") # time, numeric
  x3 <- c("D", "d", "l") # date, double, logical
  x4 <- c("d", "d", "I", "T") # double, double, big integer, datetime

  # Act
  result <- hasFormat(x1, x2, x3, x4)

  # Assert
  expect_identical(
    result,
    list(
      c(FALSE),
      c(TRUE, FALSE),
      c(TRUE, FALSE, FALSE),
      c(FALSE, FALSE, FALSE, TRUE)
    )
  )
})





# CHARACTER, COLLECTOR NAMES ======


test_that("Character: list of character(1), collector names", {
  #
  # Act
  result <- hasFormat("col_time", "col_double", "col_integer", "col_datetime")

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})



# CHARACTER, LONG NAMES ======


test_that("Character: list of character(1), long names", {
  #
  # Act
  result <- hasFormat("time", "double", "integer", "datetime")

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})

test_that("Character: long names duplicates", {
  #
  # Act
  result <- hasFormat("numeric", "double", "POSIXct", "datetime")

  # Assert
  expect_identical(
    result,
    list(c(FALSE), c(FALSE), c(TRUE), c(TRUE))
  )
})


test_that("Character: character vector, long names", {
  #
  x <- c("time", "double", "integer", "datetime")

  # Act
  result <- hasFormat.character(x)

  # Assert
  expect_identical(
    result,
    list(TRUE, FALSE, FALSE, TRUE)
  )
})


#
#
# test_that("Character: character vector, abbreviations", {
#   #
#   x1 <- c("i") # integer
#   x2 <- c("t", "n") # time, numeric
#   x3 <- c("D", "d", "l") # date, double, logical
#   x4 <- c("d", "d", "I", "T") # double, double, big integer, datetime
#
#   # Act
#   result <- hasFormat.character(x1, x2, x3, x4)
#
#   # Assert
#   expect_identical(
#     result,
#     list(
#       c(FALSE),
#       c(TRUE, FALSE),
#       c(TRUE, FALSE, FALSE),
#       c(FALSE, FALSE, FALSE, TRUE)
#     )
#   )
# })





# COLLECTOR ======

test_that("Collector: list of vector(1)", {
  #
  expect_s3_class(col_time(), "collector")
  # Act
  result <- hasFormat(col_time(), col_double(), col_integer(), col_datetime())

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})








# COLSPEC ======

test_that("Col_Spec: list of (1)", {
  #
  # Act
  result <- hasFormat.col_spec(
    cols(col_time()), cols(col_double()), cols(col_integer()), cols(col_datetime()))

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})



test_that("Col_Spec: list of (n)", {
  #
  x1 <- cols(col_integer())
  x2 <- cols(col_time(), col_number())
  x3 <- cols(col_date(), col_double(), col_logical())
  x4 <- cols(col_double(), col_double(), col_big_integer(), col_datetime())

  # Act
  result <- hasFormat.col_spec(x1, x2, x3, x4)

  # Assert
  expect_identical(
    result,
    list(
      c(FALSE),
      c(TRUE, FALSE),
      c(TRUE, FALSE, FALSE),
      c(FALSE, FALSE, FALSE, TRUE)
    )
  )
})





test_that("Function: list of functions", {
  #
  # Act
  result <- hasFormat(col_time, col_double, col_integer, col_datetime)

  # Assert
  expect_identical(
    result,
    list(c(TRUE), c(FALSE), c(FALSE), c(TRUE))
  )
})
