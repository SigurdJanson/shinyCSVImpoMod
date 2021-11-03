
MakeList <- function(len = 10L) {
  # Numbers from
  Result <- as.list(1:len)
  # Letters from 1-len, i.e. A-...
  names(Result) <- LETTERS[sample.int(len)]

  return(Result)
}



test_that("Nothing to replace returns exact same list", {
  # Arrange
  Default <- MakeList(10L)
  L <- MakeList(10L)

  # Act
  Observed <- FixupList(L, Default)

  # Assert
  expect_identical(Observed, L)
})



test_that("Falsy value is replaced", {
  ListLength <- 10L
  Falsies <- list(integer(), NA, NULL)

  for (Falsy in Falsies) {
    # Arrange
    Default <- MakeList(ListLength)
    L <- MakeList(10L)
    FalsyPosition <- LETTERS[sample.int(ListLength, 1L)]
    L[FalsyPosition] <- list(Falsy)

    # Act
    Observed <- FixupList(L, Default)

    # Assert
    expect_false(anyNA(Observed))
    expect_identical(Observed[FalsyPosition], Default[FalsyPosition])
    expect_identical(Observed[names(Observed) != FalsyPosition], L[names(Observed) != FalsyPosition])
  }
})




test_that("Missing value is added", {
  ListLength <- 10L
  # Arrange
  Default <- MakeList(ListLength)
  L <- MakeList(10L)
  FalsyPosition <- LETTERS[sample.int(ListLength, 1L)]
  L <- L[names(L) != FalsyPosition]

  skip_if(FalsyPosition %in% names(L), message = "Test assumption wrong;")

  # Act
  Observed <- FixupList(L, Default)

  # Assert
  expect_false(anyNA(Observed))
  expect_length(Observed, ListLength)
  expect_identical(Observed[FalsyPosition], Default[FalsyPosition])
})




test_that("Argument x = NULL returns error", {
  # Arrange
  Default <- MakeList(10L)
  L <- MakeList(10L)

  # Act
  # Assert
  expect_error(FixupList(NULL, Default))
})
test_that("Argument Default = NULL returns error", {
  # Arrange
  Default <- MakeList(10L)
  L <- MakeList(10L)

  # Act
  # Assert
  expect_error(FixupList(L, NULL))
})
