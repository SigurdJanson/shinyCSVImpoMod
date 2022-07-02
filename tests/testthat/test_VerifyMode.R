
# The valid modes as global variable to be available for all tests
ValidModes <- c("AsIs", "Desired", "UserDefined")

# Mock a calling context similar to `ModuleImportServer`.
Test <- function(Mode = c("AsIs", "Desired", "UserDefined")) {
  VerifyMode(Mode)
}


test_that("VerifyMode: Extrapolates incomplete but valid arguments", {
  # Arrange
  TestModes <- c("A", "D", "U")
  Results <- c()
  # Act
  for (s in TestModes) {
    Results <- c(Results, Test(s))
  }
  # Assert
  expect_identical(Results, ValidModes)
})


test_that("VerifyMode: Throws error if argument can't be extrapolated", {
  # Arrange
  TestModes <- c("B", "E", "T")
  Results <- c()
  # Act
  # Assert
  for (s in TestModes) {
    expect_error(Results <- c(Results, Test(s)))
  }
})

