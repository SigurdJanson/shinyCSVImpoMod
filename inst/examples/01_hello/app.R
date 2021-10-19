shinyApp(
  fluidPage(
    title = "CSV Import Sample App",
    ModuleImportUI("ProjectDataFile"),
    h3("Example App Preview"),
    tableOutput("AppOutputTest")
  ),
  function(input,output,session){
    DataFile <- ModuleImportServer("ProjectDataFile", UiLng = "en")

    output$AppOutputTest <- renderTable({
      need(DataFile(), "No data available")
      return(DataFile())
    })
  }
)
