


# CSS classes
.ContainerClass <- "shiny-input-container-inline" # from shiny
.DisabledClass <-  "shinyjs-disabled" # from shinyjs

.EditSuffix <- "_name"
.InclSuffix <- "_incl"
.TypeSuffix <- "_type"



# renderRow...-Functions =========

#' renderRow...-Functions
#'
#' These functions render an **interactive row** of the preview table used
#' to import CSV files in the module.
#'
#' @note The package does NOT export these functions.
#'
#' @param ColNames The names of the columns are required to generate input-ids
#' for the widgets.
#' @param Label An optional label for the input widget in each column.
#' Not supported for all interactive row types.
#' @param Values Optional values to be set.
#' @param Enabled Is the row interactive or does it only show the current
#' state (not vectorised).
#'
#' @return A `list()` with a `shiny.tag` class that can be converted into an HTML
#' string via `as.character()` and saved to a file with [`save_html()`].
#' @seealso About Shiny HTML tag lists refer to [htmltools::builder].
#'
#' @name renderRowFunctions
NULL



#' @details
#' `renderRowCheckBox` creates a tag list of a table row with a check box in each
#' cell.
#'
#' @rdname renderRowFunctions
renderRowTextInput <- function(ColNames, Label=NULL, Values=NULL, Enabled=TRUE) {
  Result <- mapply(FUN = .renderTextInput,
                   .colname=ColNames, .label=Label, .val=Values, .enable=Enabled,
                   SIMPLIFY = FALSE)
  .renderTableRow(Result)
}




#' @details
#' `renderRowCheckBox` creates a tag list of a table row with a check box in each
#' cell.
#'
#' @rdname renderRowFunctions
renderRowCheckBox <- function(ColNames, Label=NULL, Values=NULL, Enabled=TRUE) {
  Result <- mapply(FUN = .renderCheckBox,
                   .colname=ColNames, .label=Label, .val=Values, .enable=Enabled,
                   SIMPLIFY = FALSE)
  .renderTableRow(Result)
}


#' @details
#' `renderRowSelect` creates a tag list of a table row with a select widget
#' in each cell.
#'
#' @rdname renderRowFunctions
renderRowSelect <- function(ColNames, Label=NULL, Values=NULL, Enabled=TRUE) {
  Result <- mapply(FUN = .renderSelect,
                   .colname=ColNames, .enable=Enabled,
                   MoreArgs = list(.label=Label, .val=Values),
                   SIMPLIFY = FALSE)
  .renderTableRow(Result)
}




#' .renderTableRow
#'
#' Render elements of a preview table
#'
#' @param Elements A list of cell contents. The created row will have
#' as many cells as `Elements` has entries.
#'
#' @returns A `list()` with a `shiny.tag` class.
#' @seealso This is a helper function for the [renderRowFunctions].
.renderTableRow <- function(Elements) {
  Result <- lapply(Elements, function(x) paste0("<td>", x, "</td>"))
  paste("<tr>", paste(Result, collapse = ""), "</tr>")
}





# Inline Widgets =========


#' Inline Widgets
#'
#' Each function creates a tag list of interactive widgets fitting into a table.
#'
#' To fit it into the table the code created by Shiny has been adapted (at the cost of
#' a dependency to the current Shiny version this package was created for).
#'
#' @name inlinewidgets
NULL


#' @details
#' `.renderTextInput` renders a single test input field to be used inline.
#' It generates a modified code of this:
#'
#' ```
#' <div class="form-group shiny-input-container">
#'   <label class="control-label" id="ColName 2-label" for="ColName 2">...</label>
#'   <input id="ColName 2" type="text" class="form-control" value="A"/>
#' </div>
#' ```
#' In this case we 1. skip the label, 2. remove the class "form-group" because it
#' just adds a bottom padding, 3.

#' @returns A `list()` with a `shiny.tag` class.
#' @seealso These are helper functions for the [renderRowFunctions].
#' @rdname inlinewidgets
.renderTextInput <- function(.colname, .label, .val, .enable) {
  result <-
    div(
      class=paste(.ContainerClass, ifelse(!.enable, .DisabledClass, "")),
      tags$input(id=paste0(.colname, .EditSuffix),
                 type="text",
                 class="form-control",
                 value=.val,
                 placeholder=.label)
    )
  if (!.enable) {
    result <- tagAppendAttributes(result, .cssSelector="input", disabled=NA)
  }

  return(result)
}




#' @details
#' `.renderCheckBox` renders a single check box to be used inline.
#' @rdname inlinewidgets
.renderCheckBox <- function(.colname, .label, .val, .enable) {
  result <-
    div(
      class=paste(.ContainerClass, ifelse(!.enable, .DisabledClass, "")),
      div(class="checkbox",
          tags$label(
            tags$input(id=paste0(.colname, .InclSuffix),
                       type="checkbox",
                       value=.val),
            span(.label)
          )
      )
    )
  if (!.enable)
    result <-tagAppendAttributes(result, .cssSelector="input", disabled=NA)

  return(result)
}


#' @details
#' `.renderSelect` renders a select input to be used inline.
#' ```
#' # The original Shiny code in 1.7.1 will be modified for inline use
#' <div class="form-group shiny-input-container">
#'   <label class="control-label" id="id-label" for="id">label</label>
#'   <div>
#'     <select id="id">
#'        <option value="A" selected>A</option>
#         <option value="B">B</option>
#         <option value="C">C</option>
#'      </select>
#'      <script type="application/json" data-for="id" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#'    </div>
#'  </div>
#' ```
#' @rdname inlinewidgets
.renderSelect <- function(.colname, .label, .val, .enable) {
  options <- mapply(tags$option,
                    .label,
                    value=.val,
                    SIMPLIFY = FALSE)
  result <-
    div(
      class=paste(.ContainerClass, ifelse(!.enable, .DisabledClass, "")),
      div(
        tags$select(
          id=paste0(.colname, .InclSuffix),
          options
        )
      )
    )
  if (!.enable)
    result <- tagAppendAttributes(result, .cssSelector="select", disabled=NA)

  return(result)
}

