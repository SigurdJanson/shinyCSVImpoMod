
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
