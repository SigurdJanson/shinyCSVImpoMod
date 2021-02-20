library(shiny)


#' 
ModuleImportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        h3("Import"), width = 12L
      ),
      column(
        fileInput(ns("inpImportData"), "Neue Daten importieren", accept = c("text/csv", ".csv"),
                  buttonLabel = "Durchsuchen...", placeholder = "Keine Datei gewählt", width = "100%"),
        width = 6L
      )),
    fluidRow(
      column(
        checkboxInput(ns("inpHeader"), "Hat Überschriften", value = TRUE),
        width = 6L
      )),
    fluidRow(
      column(
        textInput(ns("inpColSeparator"), "Spaltentrennzeichen", ";"),
        width = 3L
      ),
      column(
        textInput(ns("inpThousandsSep"), "Tausender-Trennzeichen", "."),
        width = 3L
      ),
      column(
        textInput(ns("inpDecimalsSep"),  "Dezimaltrennzeichen", ","),
        width = 3L
      ),
      column(
        textInput(ns("inpDateFormat"),  "Datumsformat", "%d.%m.%Y"),
        width = 3L
      ),
      column(
        selectInput(ns("inpQuote"), "Texterkennungszeichen", c("None" = "", "Double quote" = "\"", "Single quote" = "'")),
        width = 3L
      ),
      column(
        hr(),
        h3("Vorschau"),
        tableOutput(ns("outImportDataPreview")),
        width = 12L
      )
    )
  )
}


ModuleImportServer <- function(id, stringsAsFactors = TRUE) {
  moduleServer(
    id,

    function(input, output, session) {
      ColumnMapping <- list()
      
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
                 sep    = input$inpColSeparator,
                 stringsAsFactors = stringsAsFactors,
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

## MAIN APP == ## == ## =============
ui <- fluidPage(
  ModuleImportUI("ProjectDataFile"),
  tableOutput("AppOutputTest")
)

server <- function(input, output, session) {
  DataFile <- ModuleImportServer("ProjectDataFile")
  
  output$AppOutputTest <- renderTable({
    need(DataFile(), "Keine Daten vorhanden")
    return(DataFile())
  })
}

shinyApp(ui, server)