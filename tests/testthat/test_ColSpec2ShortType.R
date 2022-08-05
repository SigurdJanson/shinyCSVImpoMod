
test_that("German", {
  #
  path <- system.file("extdata", "table.csv", package = "shiny.CSVImport")
  cs <- vroom::vroom(path, delim=";", show_col_types=FALSE)

  #Act
  result <- ColSpec2ShortType(vroom::spec(cs))

  #Assert
  expect_identical(unname(result), unlist(strsplit("ccdccltc", "")))
})


test_that("MplsStops.csv", {
  #
  cs <- vroom::vroom("https://vincentarelbundock.github.io/Rdatasets/csv/carData/MplsStops.csv", show_col_types=FALSE)

  #Act
  result <- ColSpec2ShortType(vroom::spec(cs))

  #Assert
  expect_identical(unname(result), unlist(strsplit("dcTccccccccdddc", "")))
})
