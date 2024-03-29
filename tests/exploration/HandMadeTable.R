library(shiny)
library(shinyjs)

source("../../R/PreviewRenderHelper.R")
source("../../R/Constants.R")
source("../../R/DataTypes.R")
source("../../R/TruthyHelpers.R")

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
      column(3, selectInput("inpDataset", label = "Data Set", choices=.datasets, selectize = FALSE)),
      column(3, numericInput("inpPreviewLength", label = "Preview Length", 5, min = 1, max=99))
    ),
    fluidRow(
      column(3, "Edit", checkboxInput("chbEditVisible", "Visible"), checkboxInput("chbEditEnabled", "Enabled")),
      column(3, "Include", checkboxInput("chbIncludeVisible", "Visible"), checkboxInput("chbIncludeEnabled", "Enabled")),
      column(3, "Type", checkboxInput("chbTypeVisible", "Visible"), checkboxInput("chbTypeEnabled", "Enabled")),
      column(3, "Format", checkboxInput("chbFormatVisible", "Visible"), checkboxInput("chbFormatEnabled", "Enabled"))
    ), hr(),
    uiOutput("AppOutputTest")
  ),



  function(input, output, session) {
    #-print(getwd())

    LiveColSpec <- reactiveVal() # mimic the reactive value of the module

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
        colspec <- vroom::spec(vroom::vroom(vroom::vroom_format(result)))
        LiveColSpec(colspec)
      }
      return(result)
    })




    #' renderDataPreview
    #'
    #' Creates the HTML for a table preview.
    #'
    #' @param Data The data to look at
    #' @param ColSpec A column specification of class `col_spec`
    #' (see [vroom::cols()]).
    #' @param ViewLen The number of rows to display
    #' @param NameEdit The settings of column name input fields
    #' (default is `c(Visible = TRUE, Enabled = TRUE)`).
    #' @param Types The settings type drop downs
    #' (default is `c(Visible = TRUE, Enabled = TRUE)`).
    #' @param Format The settings for the format inputs
    #' (default is `c(Visible = TRUE, Enabled = TRUE)`).
    #' @param Include The settings for the include check boxes
    #' (default is `c(Visible = TRUE, Enabled = TRUE)`).
    renderDataPreview <- function(Data, ColSpec, ViewLen,
                                  NameEdit = .Setting,
                                  Types    = .Setting,
                                  Format   = .Setting,
                                  Include  = .Setting) {

      if (!isTruthy(ColSpec) || !isa(ColSpec, .ColSpecClass))
        stop("Preview needs a valid column specification")

      df <- head(Data, n = ifelse(missing(ViewLen), 5L, ViewLen))

      HeadNames <- names(ColSpec$cols)


      if (NameEdit["Visible"]){ # TODO: l10n
        Rendered <- renderRowTextInput(colnames(df), "Edit new name", HeadNames, Enabled = NameEdit["Enabled"])
        HtmlNameEdit <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlNameEdit <- HTML("")


      if (Include["Visible"]) { # TODO: l10n
        Rendered <- renderRowCheckBox(colnames(df), "Include", Values=TRUE, Enabled = Include["Enabled"])
        HtmlInclude <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlInclude <- HTML("")


      if (Types["Visible"]) {
        SelectedValues <- ColSpec2ShortType(ColSpec)
        FriendlyNames <- sapply(strsplit(names(.ColumnDataTypes), "_"), `[[`, 2) # TODO: l10n
        Rendered <- renderRowSelect(colnames(df),
                                    Label   = FriendlyNames,
                                    Values  = SelectedValues,
                                    Choices = .ColumnDataTypes,
                                    Enabled = Types["Enabled"])
        HtmlTypes <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlTypes <- HTML("")


      if (Format["Visible"]) {
        FriendlyNames <- "abc"
        Values <- sapply(ColSpec$cols, \(x) `[[`(x, "format"))
        Values[Values == ""] <- "Default"  #TODO: use FileSpec here as default
        Values[!isTruthyInside(Values)] <- "---"
        Enabled <- Format["Enabled"] & unlist(hasFormat(ColSpec))
        Rendered <- renderRowTextInput(colnames(df),
                                       Label   = FriendlyNames,
                                       Values  = Values,
                                       Enabled = Enabled)
        HtmlFormat <- tags$tbody(HTML(as.character(Rendered)))
      }
      else
        HtmlFormat <- HTML("")


      Content <- HTML(base::apply(df, 1, .renderTableRow))

      Tbl <- withTags(
        div(
          tags$table(
            class="table shiny-table table- spacing-s",
            thead(
              HTML(paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>"))
            ),
            HtmlNameEdit,
            HtmlTypes,
            HtmlFormat,
            HtmlInclude,
            tbody(Content)
          )
        )
      )
      return(Tbl)
    }



    output$AppOutputTest <- renderUI({
      need(DataFile(), "No data available")

      Result <- renderDataPreview(DataFile(), ColSpec = LiveColSpec(),
                                  ViewLen = input$inpPreviewLength,
                                  NameEdit=c(Visible=input$chbEditVisible,
                                             Enabled=input$chbEditEnabled),
                                  Types=c(Visible=input$chbTypeVisible,
                                          Enabled=input$chbTypeEnabled),
                                  Include=c(Visible=input$chbIncludeVisible,
                                            Enabled=input$chbIncludeEnabled),
                                  Format=c(Visible=input$chbFormatVisible,
                                            Enabled=input$chbFormatEnabled))

      return(Result)
    })
  }
)

