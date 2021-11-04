library(shiny)
library(shiny.i18n)
library(shinyjs)
library(vroom)
library(readr)



#' @title The UI function of the CSV import module
#' @param Id Module namespace to be set by by caller
#'
#' @export
#' @import shiny
#' @importFrom shinyjs useShinyjs
ModuleImportUI <- function(Id) {
  ns <- NS(Id)
  tagList(
    useShinyjs(),
    uiOutput(ns("uiFileInput")),
    uiOutput(ns("uiGlobalSettings")),
    uiOutput(ns("uiPreview"))
  )
}




#' @title The server function of the CSV import module
#' @param Id Module name space
#' @param ColSpec A list specifying the columns to import.
#' @param Expected A list describing the expected CSV format.
#' (see details).
#' @param Options Use `Options$UILang` for the language
#' in the user interface (allowed values: "en", "de").
#' @details
#' `ColSpec` must be a named list with the vectors:
#' \describe{
#'   \item{Name}{A list of variable (i.e. column) names
#'   that shall replace the column heads in the file (character).}
#'   \item{NameInFile}{A list of column heads in the CSV
#'   file (character).}
#'   \item{Type}{The data types of each variable (character).
#'   If no value (i.e. falsy values) is given, it will be
#'   guessed.}
#'   \item{Format}{An additional format specification (character).
#'   That is supported by "datetime", "date", and "time". If none
#'   is given the `Expected` options are used.}
#' }
#' `Expected` can have these fields:
#' \describe{
#'   \item{LangCode}{Language code (e.g. "de" or "en)}
#'   \item{Header}{Does the CSV file have a header? (`TRUE` (default)/`FALSE`;
#'         see [utils::read.csv()] argument `header`). If a column specification
#'         is available, `Header` will be coerced to `TRUE`.}
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
#'   \item{StringsAsFactors}{Convert all strings to factors (`TRUE`/`FALSE` is default;
#'         see [utils::read.csv()] argument `stringsAsFactors`).}
#' }
#' @return a data frame containing the uploaded CSV file
#' @export
#' @import shiny
#' @importFrom shinyjs disabled
#' @importFrom utils head read.csv
#' @importFrom readr default_locale locale
ModuleImportServer <- function(Id,
                               ColSpec = NULL, Expected = NULL, Options = NULL) {

  # COLUMNS SPECIFICATION
  tryCatch(
    ColSpec <- VerifyColSpecFormat(ColSpec),
    error = function(e) e,
    warning = function(w) w
  )

  # EXPECTED DEFAULTS (column separator, date/time and number formats)
  tryCatch(
    Expected <- VerifyExpectedFormat(Expected),
    error = function(e) e,
    warning = function(w) w
  )

  # MODULE OPTIONS
  tryCatch(
    Options <- VerifyNSetupOptions(Options),
    error = function(e) e,
    warning = function(w) w
  )

  # I18N
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("extdata", "translation.json",
                                        package = "shiny.CSVImport"))
  # setup translator (if language isn't available, default is en)
  if (Options$UILang %in% i18n$get_languages())
    i18n$set_translation_language(Options$UILang)
  else
    i18n$set_translation_language(.DefaultOptions$UILang)


  # SETUP MODULE SERVER
  moduleServer(Id,

    function(input, output, session) {
      #
      # SETUP -----------------
      ns <- NS(Id) # set up name space

      #
      # UI -----------------
      output$uiFileInput <- renderUI({
        tagList(
          fluidRow(
            column(
              h3(i18n$t("Import")), width = 12L
            ),
            column(
              fileInput(ns("inpImportData"), i18n$t("lblDataImport"),
                        accept = c("text/csv", ".csv", ".txt"),
                        buttonLabel = i18n$t("btnBrowse"),
                        placeholder = i18n$t("lblFilePlaceholder"),
                        width = "100%"),
              width = 6L
            ))
        )
      })


      output$uiGlobalSettings <- renderUI({
        # Setup selection choices
        ChoicesDecimalsSep <- list(",", ".")
        names(ChoicesDecimalsSep) <- c(paste(i18n$t("Comma"), "(,)"), paste(i18n$t("Period"), "(.)") )
        ChoicesQuote <- list("", "\"", "'")
        names(ChoicesQuote) <- i18n$t(c("None", "Double quote", "Single quote"))
        ChoicesDate <- list(`yyyy-MM-dd` = "%Y-%m-%d",
                            `dd-MM-yyyy` = "%d-%m-%Y",
                            `dd.MM.yy` = "%d.%m.%y",
                            `dd.MM.yyyy` = "%d.%m.%Y",
                            `d.M.yyyy` = "%d.%M.%Y",
                            `dd/MM/yy` = "%d/%m/%y",
                            `dd/MM/yyyy` = "%d/%m/%Y",
                            `d/M/yyyy` = "%d/%M/%Y")
        ChoicesTime <- list(list(`HH:MM:SS` = "%H:%M:%S",
                                 `HH:MM` = "%R"),
                            list(`HH:MM:SS am/pm` = "%I:%M:%S %p",
                                 `HH:MM am/pm` = "%I:%M %p"))
        names(ChoicesTime) <- paste(c(12, 24), i18n$t("hours"))

        tagList(
          fluidRow(
            column(
              if (!is.null(ColSpec))
                disabled(checkboxInput(ns("inpHeader"), i18n$t("lblHasHeader"),
                                       value = Expected$Header))
              else
                checkboxInput(ns("inpHeader"), i18n$t("lblHasHeader"),
                              value = Expected$Header),
              width = 6L
            )),
          fluidRow(
            column(
              textInput(ns("inpColSep"), i18n$t("lblColumnSeparator"),
                        Expected$ColSep),
              width = 3L
            ),
            column(
              textInput(ns("inpThousandsSep"), i18n$t("lblThousandsSep"),
                        Expected$ThousandsSep),
              width = 3L
            ),
            column(
              selectInput(ns("inpDecimalsSep"),  i18n$t("lblDecimalsSep"),
                          choices = ChoicesDecimalsSep,
                          selected = Expected$DecimalsSep),
              width = 3L
            ),
            column(
              tags$div(
                title=i18n$t("inpDateFormat.Tooltip"),
                selectizeInput(ns("inpDateFormat"), i18n$t("lblDateFormat"),
                               choices = ChoicesDate,
                               selected = Expected$DateFormat,
                               options = list(create = TRUE))
              ),
              width = 3L
            ),
            column(
              selectizeInput(ns("inpTimeFormat"), i18n$t("lblTimeFormat"),
                             choices = ChoicesTime,
                             selected = Expected$TimeFormat,
                             options = list(create = TRUE)),
              width = 3L
            ),
            column(
              selectizeInput(ns("inpQuote"), i18n$t("lblQuote"),
                          choices = ChoicesQuote,
                          selected = Expected$Quote,
                          options = list(allowEmptyOption = TRUE)),
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


    #
    # SERVER -------------
      LiveColSpec <- reactive({
        req(input$inpImportData)

        if (isTruthy(ColSpec))
          return(ColSpec)
        else {
          return(PeekIntoFile(input$inpImportData$datapath, GetSpec = TRUE))
        }
      })



      LiveOptions <- reactive({
        Result <- Expected
        for (inp in c("Header", "ColSep", "ThousandsSep", "DecimalsSep",
                    "DateFormat", "TimeFormat", "Quote")) {
          if (isTruthy(input[[paste0("inp", inp)]])) {
            Result[[inp]] <- input[[paste0("inp", inp)]]
          }
        }
        #Result[["StringsAsFactors"]] # Not used at the moment setting
        return(Result)
      })


      #' @title The user's data, parsed into a data frame.
      #' @note The raw data frame has only columns of type `character` and must
      #' be converted later.
      #' @returns Either a data frame or an error message
      RawDataFrame <- reactive({
        req(input$inpImportData)
        req(LiveOptions())
        req(length(LiveOptions()) > 0)

        Result <- NULL
        tryCatch(
          Result <- PeekIntoFile(input$inpImportData$datapath,
                                  col_names = LiveOptions()$Header,
                                  quote  = LiveOptions()$Quote,
                                  delim  = LiveOptions()$ColSep,
                                  col_types = paste0(rep("c", .ImportMaxCol))),
          error = function(e) Result <<- e$message,
          warning = function(w) Result <<- w$message
        )

        # check if all requested variables are there
        if (isTruthy(ColSpec$NameInFile) && length(ColSpec$NameInFile) > 0L) {
          ColNames <- names(Result)
          WantedNotFound <- setdiff(ColSpec$NameInFile, ColNames)
          # if (length(WantedNotFound) > 0)
          #   showNotification(i18n$t("Some requested columns have not been found"))
          #-UnWanted <- setdiff(ColNames, ColSpec$NameInFile) # CURRENTLY NOT USED
        }

        return(Result)
      })


      # The data frame returned by the module to the calling app.
      # Does all the required data type conversions.
      DataFrame <- debounce(reactive({
        req(
          is.data.frame(RawDataFrame()),
          input$inpColSep,
          !identical(input$inpDecimalsSep, input$inpThousandsSep),
          input$inpThousandsSep
        )

        tryCatch(
          df <- DataFrameConvert(RawDataFrame(), LiveColSpec(), LiveOptions(), Preview = FALSE),
          error = function(e) shiny::validate(need(NULL, i18n$t(e$message)))
        )
        return(df)
      }), 2000L)



      #
      # The preview inside the module
      output$outImportDataPreview <- renderTable({
        shiny::validate(
          need(RawDataFrame(),
               i18n$t("No data available for preview")),
          need(is.data.frame(RawDataFrame()),
               ifelse(is.character(RawDataFrame()), RawDataFrame(), i18n$t("CSV cannot be parsed"))),
          need(input$inpColSep,
               i18n$t("msgMissingColSep")),
          need(input$inpDecimalsSep != input$inpThousandsSep,
               i18n$t("Decimal and thousands separator cannot be equal")),
          need(input$inpThousandsSep,
               i18n$t("Thousands separator is not valid"))
        )

        tryCatch(
          df <- DataFrameConvert(RawDataFrame(), LiveColSpec(), LiveOptions(), Preview = TRUE),
          error = function(e) shiny::validate(need(NULL, i18n$t(e$message)))
        )
        df[1,] <- i18n$t(unlist(df[1,]))

        return(df)
      },
      sanitize.text.function = function(x) sapply(x, .HandleUTF8))

      # Return the reactive that yields the data frame
      return(DataFrame)
    }
  )
}

