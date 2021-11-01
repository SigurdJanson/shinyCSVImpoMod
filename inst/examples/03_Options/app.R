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
                                   Expected = list(
                                     Header = TRUE,
                                     ColSep = ";",
                                     ThousandsSep = ".",
                                     DecimalsSep = ",",
                                     DateFormat = "%d.%m.%Y", # strptime() default
                                     TimeFormat = "%k:%M", # strptime() default
                                     Quote = "",
                                     StringsAsFactors = FALSE
                                   )

    )

    output$AppOutputTest <- renderTable({
      need(DataFile(), "No data available")
      return(DataFile())
    })
  }
)
