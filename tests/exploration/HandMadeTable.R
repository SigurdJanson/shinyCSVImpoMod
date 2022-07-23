library(shiny)
library(shinyjs)

source("../../R/PreviewRenderHelper.R")

# Constant to represent table settings
.Setting <- c(Visible = TRUE, Enabled = TRUE)


# replace_with <- function(x, i, val, name, reason = NULL, error_call = caller_env()) {
#   if (is.null(val)) {
#     return(x)
#   }
#
#   check_length(val, x, name, reason, error_call = error_call)
#   check_type(val, x, name, error_call = error_call)
#   check_class(val, x, name, error_call = error_call)
#
#   i[is.na(i)] <- FALSE
#
#   if (length(val) == 1L) {
#     x[i] <- val
#   } else {
#     x[i] <- val[i]
#   }
#
#   x
# }
#
# if_else <- function (condition, true, false, missing = NULL)
# {
#   if (!is.logical(condition)) {
#     abort("`condition` must be a logical vector")
#   }
#   out <- true[rep(NA_integer_, length(condition))]
#   out <- replace_with(out, condition, true, "`true`", "length of `condition`")
#   out <- replace_with(out, !condition, false, "`false`", "length of `condition`")
#   out <- replace_with(out, is.na(condition), missing, "`missing`",
#                       "length of `condition`")
#   out
# }

.datasets <- c("GermanFormat", "MPLs Stops", "mtcars", "iris", "ToothGrowth", "PlantGrowth", "USArrests")


shinyApp(


  fluidPage(
    title = "CSV Import Sample App",
    h3("Exploration App to Create a Specific Rendering for a Table"),
    fluidRow(
      column(3, selectInput("inpDataset", label = "Data Set", choices=.datasets)),
      column(3, numericInput("inpPreviewLength", label = "Preview Length", 5, min = 1, max=99))
    ),
    fluidRow(
      column(3, "Edit", checkboxInput("chbEditVisible", "Visible"), checkboxInput("chbEditEnabled", "Enabled")),
      column(3, "Include", checkboxInput("chbIncludeVisible", "Visible"), checkboxInput("chbIncludeEnabled", "Enabled")),
      column(3, "Type", checkboxInput("chbTypeVisible", "Visible"), checkboxInput("chbTypeEnabled", "Enabled"))
    ), hr(),
    uiOutput("AppOutputTest")
  ),



  function(input, output, session) {
    #-print(getwd())

    LiveColSpec <- reactiveVal() # not used, yet

    DataFile <- reactive({
      if (input$inpDataset == .datasets[1]) {
        result <- vroom::vroom("../../inst/extdata/table.csv", delim=";", show_col_types=FALSE)
        LiveColSpec(vroom::spec(result))
      }
      else if (input$inpDataset == .datasets[2]) {
        result <- vroom::vroom("https://vincentarelbundock.github.io/Rdatasets/csv/carData/MplsStops.csv", show_col_types=FALSE)
        LiveColSpec(vroom::spec(result))
      }
      else {
        result <- get(input$inpDataset)
        LiveColSpec(NULL)
      }
      return(result)
    })




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
    renderDataPreview <- function(Data, ColSpec, ViewLen,
                                  NameEdit=.Setting, Types=.Setting, Include=.Setting) {
      df <- head(Data, n = ifelse(missing(ViewLen), 5L, ViewLen))
      if (isTruthy(ColSpec))
        HeadNames <- names(ColSpec$cols)
      else
        HeadNames <- colnames(Data)

      if (NameEdit["Visible"]){
        Rendered <- renderRowTextInput(colnames(df), "Edit new name", HeadNames, Enabled = NameEdit["Enabled"])
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
      need(DataFile(), "No data available")

      Result <- renderDataPreview(DataFile(), ColSpec = LiveColSpec(), ViewLen = input$inpPreviewLength,
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

