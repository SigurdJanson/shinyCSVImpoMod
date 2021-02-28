library(shiny)
library(shiny.i18n)
library(readr)

#TODO
#- Handle StringAsFactors
#- Provide UI for users to modify column names and handle
#- Provide UI for users to modify column types and format and handle

.DefaultOptions <- list(
  Header = TRUE,
  ColSep = ";",
  ThousandsSep = ",",
  DecimalsSep = ".",
  DateFormat = "%Y-%m-%d", # strptime() default
  TimeFormat = "%H:%M:%S", # strptime() default
  Quote = "",
  StringsAsFactors = FALSE
)

#' @title The UI function of the CSV import module
#' @param Id Module namespace to be set by by caller
#'
#' @export
#' @import shiny
ModuleImportUI <- function(Id) {
  ns <- NS(Id)
  tagList(
    uiOutput(ns("uiFileInput")),
    uiOutput(ns("uiGlobalSettings")),
    uiOutput(ns("uiPreview"))
  )
}


#' @title The server function of the CSV import module
#' @param Id Module name space
#' @param ColSpec A list specifying the columns to import
#' @param Options is a list with the basic settings to load the CSV file (see details).
#' @details
#' `ColSpec` must be a named list with the vectors:
#' \describe{
#'   \item{Name}{A list of variable (i.e. column) names that shall replace the
#'   column heads in the file (character).}
#'   \item{NameInFile}{A list of column heads in the CSV file (character).}
#'   \item{Type}{The data types of each variable (character). If no value
#'   (i.e. falsy values) is given, it will be guessed.}
#'   \item{Format}{An additional format specification (character). That
#'   is supported by "datetime", "date", and "time". If none is given
#'   the `Options` are used. "find" and "regexfind" types demand a format.}
#' }
#' `Options` can have these fields:
#' \describe{
#'   \item{LangCode}{Language code (e.g. "de" or "en)}
#'   \item{Header}{Does the CSV file have a header? (`TRUE`/`FALSE`;
#'         see [utils::read.csv()] argument `header`)}
#'   \item{ColSep}{A character separating columns (
#'         see [utils::read.csv()] argument `sep`)}
#'   \item{ThousandsSep}{The character that separates thousands in numbers.}
#'   \item{DecimalsSep}{The character used decimal points in the file
#'         (see [utils::read.csv()] argument `dec`)}
#'   \item{DateFormat}{Format used for dates in the file
#'         (format specification by [base::strptime()]).}
#'   \item{TimeFormat}{Format used for temporal data in the file
#'         (format specification by [base::strptime()]).}
#'   \item{Quote}{Character to identify text
#'         see [utils::read.csv()] argument `quote`}
#'   \item{StringsAsFactors}{Shall strings be converted to factors (unless
#'         specified otherwise) (`TRUE`/`FALSE`;
#'         see [utils::read.csv()] argument `stringsAsFactors`).}
#' }
#' @return a data frame containing the uploaded CSV file
#' @export
#' @import shiny
#' @importFrom utils head read.csv
#' @importFrom readr default_locale locale
ModuleImportServer <- function(Id, ColSpec = NULL, Options = NULL) {
  moduleServer(
    Id,

    function(input, output, session) {
      ns <- NS(Id) # set up name space
      i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("data", "translation.json", package = "shiny.CSVImport"))
      i18n$set_translation_language("de")

      GlobalLocale  <- locale(date_names = ifelse(is.null(Options$LangCode), "en", Options$LangCode),
                              date_format = ifelse(is.null(Options$DateFormat), "%AD", Options$DateFormat),
                              time_format = ifelse(is.null(Options$TimeFormat), "%AT", Options$TimeFormat),
                              decimal_mark = ifelse(is.null(Options$DecimalsSep), ".", Options$DecimalsSep),
                              grouping_mark = ifelse(is.null(Options$ThousandsSep), ",", Options$ThousandsSep),
                              tz = "UTC", encoding = "UTF-8", asciify = FALSE)
      if (is.null(Options)) {
        Options <- .DefaultOptions
      }


      # UI -----------------
      output$uiFileInput <- renderUI({
        tagList(
          fluidRow(
            column(
              h3(i18n$t("Import")), width = 12L
            ),
            column(
              fileInput(ns("inpImportData"), i18n$t("lblDataImport"),
                        accept = c("text/csv", ".csv"),
                        buttonLabel = i18n$t("btnBrowse"), placeholder = i18n$t("lblFilePlaceholder"),
                        width = "100%"),
              width = 6L
            ))
        )
      })

      output$uiGlobalSettings <- renderUI({
        tagList(
          fluidRow(
            column(
              checkboxInput(ns("inpHeader"), i18n$t("lblHasHeader"), value = Options$Header),
              width = 6L
            )),
          fluidRow(
            column(
              textInput(ns("inpColSep"), i18n$t("lblColumnSeparator"), Options$ColSep),
              width = 3L
            ),
            column(
              textInput(ns("inpThousandsSep"), i18n$t("lblThousandsSep"), Options$ThousandsSep),
              width = 3L
            ),
            column(
              selectInput(ns("inpDecimalsSep"),  i18n$t("lblDecimalsSep"),
                          choices = list(`Comma (,)` = ",", `Dot (.)` = "."), selected = Options$DecimalsSep),
              width = 3L
            ),
            column(
              textInput(ns("inpDateFormat"),  i18n$t("lblDateFormat"), Options$DateFormat),
              width = 3L
            ),
            column(
              textInput(ns("inpTimeFormat"),  i18n$t("lblTimeFormat"), Options$TimeFormat),
              width = 3L
            ),
            column(
              selectInput(ns("inpQuote"), i18n$t("lblQuote"),
                          choices = c("None" = "", "Double quote" = "\"", "Single quote" = "'"),
                          selected = Options$Quote),
              width = 3L
            )
          )
        )
      })


      output$uiPreview <- renderUI({
        tagList(
          fluidRow(
            column(
              hr(),
              h3(i18n$t("Preview")),
              tableOutput(ns("outImportDataPreview")),
              width = 12L
            )
          )
        )
      })


      # SERVER -------------
      # The selected file, if any
      UserFile <- reactive({
        # If no file is selected, don't do anything
        req(input$inpImportData)

        input$inpImportData
      })

      #' @title The user's data, parsed into a data frame.
      #' @note The raw data frame has only columns of type `character` and must
      #' be converted later.
      #' @returns Either a data frame or an error message
      RawDataFrame <- reactive({
        req(UserFile())

        Result <- NULL
        tryCatch(
          Result <- read.csv(UserFile()$datapath,
                             header = input$inpHeader,
                             quote  = input$inpQuote,
                             sep    = input$inpColSep,
                             stringsAsFactors = FALSE,
                             colClasses = "character"),
          error = function(e) Result <<- e$message,
          warning = function(w) Result <<- w$message
        )
        return(Result)
      })


      # The data frame returned by the module to the calling app.
      # Does all the required data type conversions.
      DataFrame <- debounce(reactive({
        req(
          is.data.frame(RawDataFrame()),
          !identical(input$inpDecimalsSep, input$inpThousandsSep),
          input$inpThousandsSep
        )


        df <- RawDataFrame()

        if (!is.null(ColSpec$NameInFile) && length(ColSpec$NameInFile) > 0) {
          ColNames <- names(df)
          # get logical vector identifying relevant positions
          WantedNFound   <- intersect(ColSpec$NameInFile, ColNames)
          df <- df[, ColNames %in% WantedNFound]
          names(df) <- ColSpec$Name[match(WantedNFound, ColNames)]
        }

        # Get column types
        Locale <- locale(date_names = ifelse(is.null(Options$LangCode), "de", Options$LangCode),
                         date_format = ifelse(isTruthy(input$inpDateFormat), input$inpDateFormat, Options$DateFormat),
                         time_format = ifelse(isTruthy(input$inpTimeFormat), input$inpTimeFormat, Options$TimeFormat),
                         decimal_mark = ifelse(isTruthy(input$inpDecimalsSep), input$inpDecimalsSep, Options$DecimalsSep),
                         grouping_mark = ifelse(isTruthy(input$inpThousandsSep), input$inpThousandsSep, Options$ThousandsSep),
                         tz = "UTC", encoding = "UTF-8", asciify = FALSE)
        ColTypes <- GuessColumnTypes(df, Locale)
        if (isTruthy(ColSpec$Type)) { # pre-specified types take precedence over guessed type
          ColTypes <- replace(ColSpec$Type, !isTruthyInside(ColSpec$Type), ColTypes)
        }

        df <- ColumnConvert(df, as.list(ColTypes), ColSpec$Format, Locale)
        #TODO: Options$StringsAsFactors

        return(df)
      }), 2000L)



      #
      #
      output$outImportDataPreview <- renderTable({
        shiny::validate(
          need(RawDataFrame(), i18n$t("No data available for preview")),
          need(is.data.frame(RawDataFrame()), ifelse(is.character(RawDataFrame()), RawDataFrame(), i18n$t("CSV cannot be parsed"))),
          need(input$inpDecimalsSep != input$inpThousandsSep, i18n$t("Decimal and thousands separator cannot be equal")),
          need(isTruthy(input$inpThousandsSep), i18n$t("Thousands separator is not valid"))
        )
        df <- head(RawDataFrame())

        # Match names to `ColSpec$NameInFile`, drop all missings, and
        # ... replace with desired names
        if (!is.null(ColSpec$NameInFile) && length(ColSpec$NameInFile) > 0) {
          ColNames <- names(df)
          WantedNotFound <- setdiff(ColSpec$NameInFile, ColNames)
          if (length(WantedNotFound) > 0) showNotification(i18n$t("Some requested columns have not been found"))
          #-UnWanted       <- setdiff(ColNames, ColSpec$NameInFile) # TODO: NOT USED
          # get logical vector identifying relevant positions
          WantedNFound   <- intersect(ColSpec$NameInFile, ColNames)
          df <- df[, ColNames %in% WantedNFound]
          names(df) <- ColSpec$Name[match(WantedNFound, ColNames)]
        }

        # Get column types
        Locale <- locale(date_names = ifelse(is.null(Options$LangCode), "de", Options$LangCode),
                         date_format = ifelse(isTruthy(input$inpDateFormat), input$inpDateFormat, Options$DateFormat),
                         time_format = ifelse(isTruthy(input$inpTimeFormat), input$inpTimeFormat, Options$TimeFormat),
                         decimal_mark = ifelse(isTruthy(input$inpDecimalsSep), input$inpDecimalsSep, Options$DecimalsSep),
                         grouping_mark = ifelse(isTruthy(input$inpThousandsSep), input$inpThousandsSep, Options$ThousandsSep),
                         tz = "UTC", encoding = "UTF-8", asciify = FALSE)
        ColTypes <- GuessColumnTypes(df, Locale)
        if (isTruthy(ColSpec$Type)) { # pre-specified types take precedence over guessed type
          ColTypes <- replace(ColSpec$Type, !isTruthyInside(ColSpec$Type), ColTypes)
        }

        # Convert to test what works and re-convert to 'character'
        #TODO

        # Create preview data frame
        ColTypesStr <- i18n$t(sprintf("<%s>", ColTypes))
        df <- rbind(Types = ColTypesStr, df)
        return(df)
      })

      # Return the reactive that yields the data frame
      return(DataFrame)
    }
  )
}

