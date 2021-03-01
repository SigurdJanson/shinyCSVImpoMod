shinyApp(
  fluidPage(
    title = "CSV Import Sample App",
    ModuleImportUI("ProjectDataFile"),
    h3("Example App Preview"),
    tableOutput("AppOutputTest")
  ),
  function(input,output,session){
    DataFile <- ModuleImportServer("ProjectDataFile", UiLng = "de")

    output$AppOutputTest <- renderTable({
      need(DataFile(), "Keine Daten vorhanden")
      return(DataFile())
    })
  }
)
