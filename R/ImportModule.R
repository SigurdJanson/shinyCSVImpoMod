library(shiny)

.DefaultOptions <- list(
  Header = TRUE,
  ColSep = ";",
  ThousandsSep = ",",
  DecimalsSep = ".",
  DateFormat = "%Y-%m-%d",
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
#' @param Id Module namespace
#' @param Options is a list with the basic settings to load the CSV file (see details).
#' @details `Options` has these fields:
#' \describe{
#'   \item{Header}{Does the CSV file have a header? (`TRUE`/`FALSE`;
#'         see [utils::read.csv()] argument `header`)}
#'   \item{ColSep}{A character separating columns (
#'         see [utils::read.csv()] argument `sep`)}
#'   \item{ThousandsSep}{The character that separates thousands in numbers.}
#'   \item{DecimalsSep}{The character used decimal points in the file
#'         (see [utils::read.csv()] argument `dec`)}
#'   \item{DateFormat}{Format used for dates in the file
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
#' @importFrom methods setAs setClass
#' @importFrom utils head read.csv
ModuleImportServer <- function(Id, Options = NULL) {
  moduleServer(
    Id,

    function(input, output, session) {
      ColumnMapping <- list() # TODO: not used do far

      ns <- NS(Id)

      if (is.null(Options)) {
        Options <- .DefaultOptions
      }


      # UI -----------------
      output$uiFileInput <- renderUI({
        tagList(
          fluidRow(
            column(
              h3("Import"), width = 12L
            ),
            column(
              fileInput(ns("inpImportData"), "Neue Daten importieren", accept = c("text/csv", ".csv"),
                        buttonLabel = "Durchsuchen...", placeholder = "Keine Datei gewählt", width = "100%"),
              width = 6L
            ))
        )
      })

      output$uiGlobalSettings <- renderUI({
        tagList(
          fluidRow(
            column(
              checkboxInput(ns("inpHeader"), "Hat Überschriften", value = Options$Header),
              width = 6L
            )),
          fluidRow(
            column(
              textInput(ns("inpColSep"), "Spaltentrennzeichen", Options$ColSep),
              width = 3L
            ),
            column(
              textInput(ns("inpThousandsSep"), "Tausender-Trennzeichen", Options$ThousandsSep),
              width = 3L
            ),
            column(
              textInput(ns("inpDecimalsSep"),  "Dezimaltrennzeichen", Options$DecimalsSep),
              width = 3L
            ),
            column(
              textInput(ns("inpDateFormat"),  "Datumsformat", Options$DateFormat),
              width = 3L
            ),
            column(
              selectInput(ns("inpQuote"), "Texterkennungszeichen",
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
              h3("Vorschau"),
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
        validate(need(input$inpImportData, message = FALSE))
        input$inpImportData
      })

      # The user's data, parsed into a data frame
      DataFrame <- reactive({
        req(UserFile())

        setClass("date")
        setAs("character", "date", function(from) as.POSIXct(from, format = input$inpDateFormat) )

        read.csv(UserFile()$datapath,
                 header = input$inpHeader,
                 quote  = input$inpQuote,
                 sep    = input$inpColSep,
                 stringsAsFactors = Options$StringsAsFactors,
                 colClasses = c("character", "character", "numeric", "date")) #CHANGE
        # TODO: convert dates
        # TODO: "inpThousandsSep"
        # TODO: "inpDecimalsSep"
      })


      output$outImportDataPreview <- renderTable({
        need(DataFrame(), "No data has been loaded")
        return(head(DataFrame()))
      })

      # Return the reactive that yields the data frame
      return(DataFrame)
    }
  )
}

# ## MAIN APP == ## == ## =============
# ui <- fluidPage(
#   ModuleImportUI("ProjectDataFile"),
#   tableOutput("AppOutputTest")
# )
#
# server <- function(input, output, session) {
#   DataFile <- ModuleImportServer("ProjectDataFile")
#
#   output$AppOutputTest <- renderTable({
#     need(DataFile(), "Keine Daten vorhanden")
#     return(DataFile())
#   })
# }
#
# shinyApp(ui, server)
