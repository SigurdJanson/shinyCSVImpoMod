shinyApp(
  fluidPage(
    title = "CSV Import Sample App",
    ModuleImportUI("ProjectDataFile"),
    h3("Example App Preview"),
    tableOutput("AppOutputTest")
  ),
  function(input,output,session){
    DataFile <- ModuleImportServer("ProjectDataFile",
                                   UiLng = "de",
                                   ColSpec = list(
                                     Name = list("Name", "Age", "Date", "Double", "T/F", "Time", "eMoney", "LETTER"),
                                     NameInFile = list(NA, NULL, "Datum", "GermanFloatingPoint", "Truth", "Time", NA, "Column A"),
                                     Type = list("character", "integer", "date", "number", "logical", "time", "number", "character"),
                                     Format = list(NA, NA, "%d.%m.%Y", NA, NA, "%H:%M", NA, NA)
                                     )
                                   )

    output$AppOutputTest <- renderTable({
      need(DataFile(), "Keine Daten vorhanden")
      return(DataFile())
    })
  }
)
