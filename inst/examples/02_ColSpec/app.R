shinyApp(
  fluidPage(
    title = "CSV Import Sample App",
    wellPanel(
      ModuleImportUI("ProjectDataFile")
    ),
    h3("Example App Preview"),
    tableOutput("AppOutputTest")
  ),
  function(input,output,session){
    DataFile <- ModuleImportServer("ProjectDataFile",
                                   UiLng = "en",
                                   ColSpec = list(
                                     Name = list("Name", "Age", "Date", "Double", "T/F", "Time", "LETTER"),
                                     NameInFile = list(NA, NULL, "Datum", "GermanFloatingPoint", "Truth", "Time", "Column A"),
                                     Type = list("character", "integer", "date", "number", "logical", "time", "character"),
                                     Format = list(NA, NA, "%d.%m.%Y", NA, NA, "%H:%M", NA)
                                     )
                                   )

    output$AppOutputTest <- renderTable({
      need(DataFile(), "No data available")
      return(DataFile())
    })
  }
)
