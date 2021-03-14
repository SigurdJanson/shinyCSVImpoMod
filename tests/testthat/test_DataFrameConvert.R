GetDataFromFile <- function() {
  read.csv(header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
           colClasses = "character",
           text = "Spalte A;Name;Alter;Datum;GermanFloatingPoint;Truth
A;Jan;46;24.01.1975;1.024,64;TRUE
B;Geert;49;19.04.1971;123,45;FALSE
C;Hagen;41;03.02.1980;1.000.000,99;true")
}

.DefaultOptions <- list(
  LangCode = "en",
  Header = TRUE,
  ColSep = ";",
  ThousandsSep = ".",
  DecimalsSep = ",",
  DateFormat = "%d.%m.%Y",
  TimeFormat = "%H:%M:%S", # strptime() default
  Quote = "",
  StringsAsFactors = FALSE
)



# Complete data frame, new column names ------------------------------------------
test_that("Preview, Complete data frame, new column names", {
  # SETUP
  ColSpec <- list(
    Name       = as.list(paste0("Super", 1:6)),
    NameInFile = list("Spalte.A", "Name", "Alter", "Datum", "GermanFloatingPoint", "Truth"),
    Type       = list("character", "character", "integer", "date", "number", "logical"),
    Format     = list(NA, NA, NULL, NA, NA, vector())
  )
  obs <- expect_silent(
    DataFrameConvert(GetDataFromFile(), ColSpec, .DefaultOptions, Preview = TRUE)
  )

  # TEST
  expect_type(obs, "list") #  a data frame is a list
  expect_identical(ncol(obs), length(ColSpec$Name))
  expect_identical(colnames(obs), unlist(ColSpec$Name))
  expect_identical(sapply(obs, typeof),
                   setNames(rep("character", length(ColSpec$Name)), names(obs)))
})



test_that("No Preview, Complete data frame, new column names", {
  ColSpec <- list(
    Name       = as.list(paste0("Super", 1:6)),
    NameInFile = list("Spalte.A", "Name", "Alter", "Datum", "GermanFloatingPoint", "Truth"),
    Type       = list("character", "character", "integer", "date", "number", "logical"),
    Format     = list(NA, NA, NULL, NA, NA, vector())
  )

  # Date format given in: Options
  # SETUP
  Options <- .DefaultOptions

  # TEST
  obs <- expect_silent(
    DataFrameConvert(GetDataFromFile(), ColSpec, Options, Preview = FALSE)
  )
  expect_type(obs, "list") #  a data frame is a list
  expect_identical(ncol(obs), length(ColSpec$Name))
  expect_identical(colnames(obs), unlist(ColSpec$Name))
  expect_identical(sapply(obs, typeof),
                   setNames(c("character", "character", "integer", "double", "double", "logical"),
                            names(obs)))
  expect_identical(sapply(obs, class),
                   setNames(c("character", "character", "integer", "Date", "numeric", "logical"),
                            names(obs)))

  expect_identical(obs$Super1, LETTERS[1:3])
  expect_identical(obs$Super2, c("Jan", "Geert", "Hagen"))
  expect_identical(obs$Super3, c(46L, 49L, 41L))
  expect_identical(obs$Super4, c(as.Date("24.01.1975", "%d.%m.%Y"), as.Date("19.04.1971", "%d.%m.%Y"), as.Date("03.02.1980", "%d.%m.%Y")))
  expect_equal(obs$Super5, c(1024.64, 123.45, 1000000.99))
  expect_identical(obs$Super6, c(TRUE, FALSE, TRUE))


  # Date format given in: ColSpec
  # SETUP
  ColSpec$Format <- list(NA, NA, NULL, "%d.%m.%Y", NA, vector())
  Options <- .DefaultOptions
  Options$DateFormat <- "%Y/%m/%d"

  # TEST
  obs <- expect_silent(
    DataFrameConvert(GetDataFromFile(), ColSpec, .DefaultOptions, Preview = FALSE)
  )
  expect_type(obs, "list") #  a data frame is a list
  expect_identical(ncol(obs), length(ColSpec$Name))
  expect_identical(colnames(obs), unlist(ColSpec$Name))
  expect_identical(sapply(obs, typeof),
                   setNames(c("character", "character", "integer", "double", "double", "logical"),
                            names(obs)))
  expect_identical(sapply(obs, class),
                   setNames(c("character", "character", "integer", "Date", "numeric", "logical"),
                            names(obs)))
})




# Request PARTIAL data frame, change column names ------------------------------------------
test_that("Preview, Partial data frame, new column names", {
  # SETUP
  ColSpec <- list(
    Name       = as.list(paste0("Super", 2:6)),
    NameInFile = list("Name", "Alter", "Datum", "GermanFloatingPoint", "Truth"),
    Type       = list("character", "integer", "date", "number", "logical"),
    Format     = list(NA, NULL, NA, NA, vector())
  )

  # TEST
  obs <- expect_silent(
    DataFrameConvert(GetDataFromFile(), ColSpec, .DefaultOptions, Preview = FALSE)
  )
  expect_type(obs, "list") #  a data frame is a list
  expect_identical(ncol(obs), length(ColSpec$Name))
  expect_identical(colnames(obs), unlist(ColSpec$Name))
  expect_identical(sapply(obs, class),
                   setNames(c("character", "integer", "Date", "numeric", "logical"),
                            names(obs)))
})




# Request MORE variables than exist, change column names ------------------------------------------
test_that("Preview, redundant data frame, new column names", {
  # SETUP
  ColSpec <- list(
    Name       = as.list(c(paste0("Super", 1:3), "Extra", paste0("Super", 4:6))),
    NameInFile = list("Spalte.A", "Name", "Alter", "Extra", "Datum", "GermanFloatingPoint", "Truth"),
    Type       = list("character", "character", "integer", "time", "date", "number", "logical"),
    Format     = list(NA, NA, NULL, NA, NA, NA, vector())
  )
  ExpectedNames <- as.list(c(paste0("Super", 1:6)))

  # TEST
  obs <- expect_silent(
    DataFrameConvert(GetDataFromFile(), ColSpec, .DefaultOptions, Preview = FALSE)
  )
  expect_type(obs, "list") #  a data frame is a list
  expect_identical(ncol(obs), length(ExpectedNames))
  expect_identical(colnames(obs), unlist(ExpectedNames))
  expect_identical(sapply(obs, class),
                   setNames(c("character", "character", "integer", "Date", "numeric", "logical"),
                            names(obs)))
})

