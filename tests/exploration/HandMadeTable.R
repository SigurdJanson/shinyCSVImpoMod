library(shiny)


# Constant to represent table settings
.Setting <- c(Visible = TRUE, Enabled = TRUE)




if_else <- function (condition, true, false, missing = NULL)
{
  if (!is.logical(condition)) {
    msg <- glue("`condition` must be a logical vector, not {friendly_type_of(condition)}.")
    abort(msg)
  }
  out <- true[rep(NA_integer_, length(condition))]
  out <- replace_with(out, condition, true, "`true`", "length of `condition`")
  out <- replace_with(out, !condition, false, "`false`", "length of `condition`")
  out <- replace_with(out, is.na(condition), missing, "`missing`",
                      "length of `condition`")
  out
}




shinyApp(


  fluidPage(
    title = "CSV Import Sample App",
    h3("Exploration App to Create a Specific Rendering for a Table"),
    fluidRow(
      column(3, "Edit", checkboxInput("chbEditVisible", "Visible"), checkboxInput("chbEditEnabled", "Enabled")),
      column(3, "Include", checkboxInput("chbIncludeVisible", "Visible"), checkboxInput("chbIncludeEnabled", "Enabled")),
      column(3, "Type", checkboxInput("chbTypeVisible", "Visible"), checkboxInput("chbTypeEnabled", "Enabled"))
    ), hr(),
    uiOutput("AppOutputTest")
  ),



  function(input,output,session){
    print(getwd())
    DataFile <- vroom::vroom("../../inst/extdata/table.csv", delim=";", show_col_types=FALSE)

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
    #' @param Enabled (not vectorised)
    #'
    #' @return
    #' @export
    #'
    #' @examples
    renderRowCheckBox <- function(ColNames, Label=NULL, Values=NULL, Enabled=TRUE) {
      .render <- function(.colname, .label, .val, .enable)
        div(
          class="shiny-input-container-inline",
          div(class="checkbox",
              tags$label(
                tags$input(id=.colname, type="checkbox", value=.val, disabled=!.enable),
                span(.label)
              )
          )
        )

      Result <- lapply(paste("ColName", ColNames), .render, .val=TRUE, .label=Label, .enable=Enabled) # TODO: use `Values`
      Result <- lapply(Result, function(x) paste0("<td>", x, "</td>"))
      paste("<tr>", paste(Result, collapse = ""), "</tr>")
    }



    renderDataPreview <- function(Data, NameEdit=.Setting, Types=.Setting, Include=.Setting) {
      df <- head(DataFile)

      if (NameEdit["Visible"]){
        Rendered <- renderRowTextInput(colnames(df)) # TODO: Enabled
        HtmlNameEdit <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlNameEdit <- HTML("")


      if (Include["Visible"]) {
        Rendered <- renderRowCheckBox(colnames(df), "Include", Enabled = Include["Enabled"])
        HtmlInclude <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlInclude <- HTML("")


      if (Types["Visible"]) {
        Rendered <- HTML(
          paste0("<tr>", paste0("<td>", "&lt;", rep("TYPE", ncol(df)), "&gt;", "</td>", collapse = ""), "</tr>"))

        #Rendered <- renderRowCheckBox(colnames(df), "Include", Enabled = Include["Enabled"])
        HtmlTypes <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlTypes <- HTML("")


      Content <- HTML(base::apply(df, 1, renderTableRow))

      Tbl <- withTags(
        div(
          tags$table(
            class="table shiny-table table- spacing-s",
            thead(
              HTML(paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>"))
            ),
            HtmlNameEdit,
            HtmlInclude,
            HtmlTypes,
            tbody(Content)
          )
        )
      )
      return(Tbl)
    }



    output$AppOutputTest <- renderUI({
      need(DataFile, "No data available")

      Result <- renderDataPreview(DataFile,
                                  NameEdit=c(Visible=input$chbEditVisible,
                                            Enabled=input$chbEditEnabled),
                                  Types=c(Visible=input$chbTypeVisible,
                                            Enabled=input$chbTypeEnabled),
                                  Include=c(Visible=input$chbIncludeVisible,
                                            Enabled=input$chbIncludeEnabled))

      return(Result)
    })
  }
)

