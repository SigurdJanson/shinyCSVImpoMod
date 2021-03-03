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
                                     Name = list("LETTER", "Name", "Age", "Date", "Double", "T/F", "Time"),
                                     NameInFile = list("Column A", NA, NULL, "Datum", "GermanFloatingPoint", "Truth", "Time"),
                                     Type = list("character", "character", "integer", "date", "number", "logical", "time"),
                                     Format = list(NA, NA, NA, "%d.%m.%Y", NA, NA, "%H:%M")
                                     )
                                   )

    output$AppOutputTest <- renderTable({
      need(DataFile(), "Keine Daten vorhanden")
      return(DataFile())
    })
  }
)
