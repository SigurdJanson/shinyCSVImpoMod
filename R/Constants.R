# CONSTANTS


# What is the max number of columns that can be imported?
.ImportMaxCol = 999L

# Supported import modes (set by the calling app)
.ImpModes <- c(Desired = "Desired", AsIs = "AsIs", User = "UserDefined")

# The expected names of the list containing the column specification
.ColumnSpecificationListNames <- c(Name = "Name",
                                   NameInFile = "NameInFile",
                                   Type = "Type",
                                   Format = "Format")


#
# SUPPORTED DATA TYPES =======
#

.ColumnDataTypes <- c(
  col_character = "c",
  col_factor    = "f",
  col_logical   = "l",
  col_double    = "d",
  col_integer   = "i",
  col_big_integer = "I",
  col_number    = "n",
  col_date      = "D",
  col_datetime  = "T",
  col_time      = "t"
)

# Column types supported by vroom
.ColumnTypes <- c(
  # SPECIAL COLUMN TYPES
  col_skip = "_", # "-"
  col_guess = "?",
  .ColumnDataTypes
)

.ColumnDataTypesLong <- c(
  character = "c",
  factor    = "f",
  logical   = "l",
  double    = "d",
  numeric   = "d",
  integer   = "i",
  big_integer = "I",
  number    = "n",
  date      = "D",
  datetime  = "T",
  POSIXct   = "T",
  time      = "t"
)

.ColumnTypesLong <- c(
  # SPECIAL COLUMN TYPES
  skip = "_",
  `NULL` = "_",
  guess = "guess",
  `NA` = "guess",
  .ColumnDataTypesLong
)

# DATA TYPE FORMATS

.DateFormats <- list(
  `yyyy-MM-dd` = "%Y-%m-%d",
  `dd-MM-yyyy` = "%d-%m-%Y",
  `dd.MM.yy` = "%d.%m.%y",
  `dd.MM.yyyy` = "%d.%m.%Y",
  `d.M.yyyy` = "%d.%M.%Y",
  `dd/MM/yy` = "%d/%m/%y",
  `dd/MM/yyyy` = "%d/%m/%Y",
  `d/M/yyyy` = "%d/%M/%Y")

.TimeFormats <-
  list(
    `HH:MM:SS` = "%H:%M:%S",
    `HH:MM` = "%R",
    `HH:MM:SS am/pm` = "%I:%M:%S %p",
    `HH:MM am/pm` = "%I:%M %p")


