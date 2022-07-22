library(shiny)
library(shinyjs)

source("../../R/PreviewRenderHelper.R")

# Constant to represent table settings
.Setting <- c(Visible = TRUE, Enabled = TRUE)


replace_with <- function(x, i, val, name, reason = NULL, error_call = caller_env()) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason, error_call = error_call)
  check_type(val, x, name, error_call = error_call)
  check_class(val, x, name, error_call = error_call)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}

if_else <- function (condition, true, false, missing = NULL)
{
  if (!is.logical(condition)) {
    abort("`condition` must be a logical vector")
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



  function(input,output,session) {
    print(getwd())
    DataFile <- vroom::vroom("../../inst/extdata/table.csv", delim=";", show_col_types=FALSE)




    #' renderRow-Functions
    #'

    #' In this case we 1. skip the label, 2. remove the class "form-group" because it
    #' just adds a bottom padding, 3.
    renderRowTextInput <- function(ColNames, Label=NULL, Values=NULL, Enabled=TRUE) {
      Result <- mapply(FUN = .renderTextInput,
                       .colname=ColNames, .label=Label, .val=Values, .enable=Enabled,
                       SIMPLIFY = FALSE)
      .renderTableRow(Result)
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
      Result <- mapply(FUN = .renderCheckBox,
                       .colname=ColNames, .label=Label, .val=Values, .enable=Enabled,
                       SIMPLIFY = FALSE)
      .renderTableRow(Result)
    }



    #' renderDataPreview
    #'
    #'
    renderDataPreview <- function(Data, ColSpec,
                                  NameEdit=.Setting, Types=.Setting, Include=.Setting) {
      df <- head(DataFile)

      if (NameEdit["Visible"]){
        Rendered <- renderRowTextInput(colnames(df), paste("Edit new name", 1:ncol(df)), LETTERS[1:ncol(df)], Enabled = NameEdit["Enabled"])
        HtmlNameEdit <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlNameEdit <- HTML("")


      if (Include["Visible"]) {
        Rendered <- renderRowCheckBox(colnames(df), "Include", Values=TRUE, Enabled = Include["Enabled"])
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


      Content <- HTML(base::apply(df, 1, .renderTableRow))

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

#htmlEscape

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

