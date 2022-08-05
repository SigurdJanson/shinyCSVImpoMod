
test_that("All truthy", {
  for (i in -3:3) {
    x <- 1:i
    expect_identical(isTruthyInside(x), rep(TRUE, length(x)))
  }

  for (i in 1:3) {
    x <- LETTERS[1:i]
    expect_identical( isTruthyInside(x), rep(TRUE, length(x)) )
  }
})


test_that("Some falsy", {
  obs <- isTruthyInside(c(1, NA, 2))
  expect_identical(obs, c(TRUE, FALSE, TRUE))

  obs <- isTruthyInside(list(1, NA, NULL, integer(0), 2))
  expect_identical(obs, c(TRUE, FALSE, FALSE, FALSE, TRUE))
})


test_that("Mother of falsy", {
  obs <- isTruthyInside(NULL)
  expect_null(obs)

  obs <- isTruthyInside(NA)
  expect_null(obs)

  obs <- isTruthyInside(integer())
  expect_null(obs)
})



test_that("list(NA) yields FALSE", {
  # Act
  result <- isTruthyInside(list(NA))
  #> FALSE

  # Assert
  expect_identical(result, FALSE)
})



test_that("data.frame(NA) yields FALSE", {
  # Act
  result <- isTruthyInside(data.frame(NA))

  # Assert
  expect_identical(result, FALSE)
})


test_that("data.frame with any NA yields TRUE because it is not recursive", {
  # Act
  result <- isTruthyInside(
    data.frame(
      A = 1:20,
      B = LETTERS[1:20],
      c = c(1, 2, 3, NA, 5:20)
      )
    )

  # Assert
  expect_identical(result, c(TRUE, TRUE, TRUE))
})
