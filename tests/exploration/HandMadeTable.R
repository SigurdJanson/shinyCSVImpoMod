library(shiny)

shinyApp(
  fluidPage(
    title = "CSV Import Sample App",
    h3("Exploration App"),
    uiOutput("AppOutputTest")
  ),
  function(input,output,session){
    print(getwd())

    DataFile <- vroom::vroom("../../inst/extdata/table.csv", delim=";", progress=FALSE)

    renderTableRow <- function(Row) {
      paste0("<tr>", paste0("<td>", Row, "</td>", collapse = ""), "</tr>")
    }


    #' renderRow-Functions
    #'
    #' ```
    #' <div class="form-group shiny-input-container">
    #'   <label class="control-label" id="ColName 2-label" for="ColName 2">...</label>
    #'   <input id="ColName 2" type="text" class="form-control" value="A"/>
    #' </div>
    #' ```
    #' In this case we 1. skip the label, 2. remove the class "form-group" because it
    #' just adds a bottom padding, 3.
    renderRowTextInput <- function(ColNames, Values=NULL) {
      .render <- function(.colname, .val)
        div(
          class="shiny-input-container-inline",
          tags$input(id=.colname,
                     type="text",
                     class="form-control",
                     value=.val,
                     placeholder="Enter new label") #TODO: user `Label` argument
        )

      Result <- lapply(paste("ColName", ColNames), .render, .val="A") # TODO: use `Values`
      Result <- lapply(Result, function(x) paste0("<td>", x, "</td>"))
      paste("<tr>", paste(Result, collapse = ""), "</tr>")
    }




    #' renderRow-Functions
    #'
    #' @param ColNames
    #' @param Values
    #'
    #' @return
    #' @export
    #'
    #' @examples
    renderRowCheckBox <- function(ColNames, Label=NULL, Values=NULL) {
      .render <- function(.colname, .label, .val)
        div(
          class="shiny-input-container-inline",
          div(class="checkbox",
              tags$label(
                tags$input(id=.colname, type="checkbox", value=.val),
                span(.label)
              )
          )
        )

      Result <- lapply(paste("ColName", ColNames), .render, .val=TRUE, .label=Label) # TODO: use `Values`
      Result <- lapply(Result, function(x) paste0("<td>", x, "</td>"))
      paste("<tr>", paste(Result, collapse = ""), "</tr>")
    }


    output$AppOutputTest <- renderUI({
      need(DataFile, "No data available")

      df <- head(DataFile)

      # Tbl <- HTML(
      #   "<div>",
      #   "<table class=\"table shiny-table table- spacing-s\">",
      #   "<thead>",
      #   paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>"),
      #   "</thead>",
      #   "<tbody>",
      #   paste0("<tr>", paste0("<td>", df[1,], "</td>", collapse = ""), "</tr>"),
      #   "</tbody>",
      #   "</table>",
      #   "</div>"
      # )
      Content <- HTML(base::apply(df, 1, renderTableRow))
      Types <- HTML(
        paste0("<tr>", paste0("<td>", "&lt;", rep("TYPE", ncol(df)), "&gt;", "</td>", collapse = ""), "</tr>"))
      ColNameEdit <- HTML(as.character(renderRowTextInput(colnames(df))))
      Include <- HTML(as.character(renderRowCheckBox(colnames(df), "Include")))

      Tbl <- withTags(
        div(
          tags$table(class="table shiny-table table- spacing-s",
            thead(
              HTML(paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>"))
            ),
            tbody(
              ColNameEdit
            ),
            tbody(
              Include
            ),
            tbody(
              Types
            ),
            tbody(
              Content
            )
          )
        )
      )

      return(Tbl)
      #return(DataFile)
    })
  }
)

