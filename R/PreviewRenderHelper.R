


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


#' @param Choices List of values to select from.  If elements of the
#' list are named, then that name — rather than the value — is displayed
#' to the user (@seealso [shiny::selectInput]).
#' @details
#' `renderRowSelect` creates a tag list of a table row with a select widget
#' in each cell.
#'
#' @rdname renderRowFunctions
renderRowSelect <- function(ColNames, Label=NULL, Values=NULL, Choices=NULL, Enabled=TRUE) {
  Result <- mapply(FUN = .renderSelect,
                   .colname=ColNames, .enable=Enabled, .val = Values,
                   MoreArgs = list(.label=Label, .chc=Choices),
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
#' @param .colname The name of the column in which the widget is placed
#' @param .label A label identifying the expected input to the user
#' @param .val A value to set
#' @param .enable logical for enabled=TRUE or disabled=FALSE
#' @param .chc A list of choices (select widgets, only; see [`shiny::selectInput`]).
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
#' @importFrom htmltools css htmlEscape
.renderSelect <- function(.colname, .label, .val, .chc, .enable) {
  inputId <- .colname
  label <- .label
  multiple <- FALSE
  selectize <- FALSE
  width <- NULL
  size <- NULL

  selected <- restoreInput(id = inputId, default = .val)
  choices <- choicesWithNames(.chc)
  if (is.null(selected)) {
    if (!multiple)
      selected <- firstChoice(choices)
  }
  else selected <- as.character(selected)
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  selectTag <- tags$select(id = inputId, class = if (!selectize)
    "form-control", size = size, selectOptions(choices, selected, inputId, selectize))
  if (multiple)
    selectTag$attribs$multiple <- "multiple"
  res <- div(class = paste(.ContainerClass, ifelse(!.enable, .DisabledClass, "")),
             style = htmltools::css(width = validateCssUnit(width)),
             #shiny:::shinyInputLabel(inputId, label),
             div(selectTag))

  if (!.enable) # Added to original Shiny code
    res <- tagAppendAttributes(res, .cssSelector="select", disabled=NA)

  #if (!selectize) # always
    return(res)
  #selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices)) # never

  # .chc <- shiny:::choicesWithNames(.chc)
  # options <- mapply(tags$option,
  #                   names(.chc),
  #                   value=.chc,
  #                   SIMPLIFY = FALSE)
  # if (isTruthy(.val))
  #   htmltools::tagQuery(options)$find(paste0("option[value]=", .val))$addAttrs(selected=NA)
  #   #result <- tagAppendAttributes(result, .cssSelector=paste0("option[value]=", .val), selected=NA)
  #
  # # TODO: add selected boolean attribute to the .val argument
  # result <-
  #   div(
  #     class=paste(.ContainerClass, ifelse(!.enable, .DisabledClass, "")),
  #     div(
  #       tags$select(
  #         id=paste0(.colname, .InclSuffix),
  #         options
  #       )
  #     )
  #   )
  # return(result)
}


# HELPERS =====

# taken from shiny:::choicesWithNames
choicesWithNames <- function (choices) {
  if (hasGroups(choices)) {
    processGroupedChoices(choices)
  }
  else {
    processFlatChoices(choices)
  }
}

# taken from shiny:::processFlatChoices
processFlatChoices <- function (choices) {
  #choices <- setDefaultNames(asCharacter(choices))
  choices <- choices |>
    as.character() |>
    stats::setNames(names(choices)) |>
    setDefaultNames()

  as.list(choices)
}


# taken from shiny:::setDefaultNames && shiny:::asNamed
setDefaultNames <- function (x) {
  if (is.null(names(x))) {
    names(x) <- character(length(x))
  }
  emptyNames <- names(x) == "" #!isTruthy(names(x)) # changed line
  names(x)[emptyNames] <- as.character(x)[emptyNames]
  x
}

# taken from shiny:::hasGroups
hasGroups <- function (choices) {
  is.list(choices) && any(vapply(choices, isGroup, logical(1)))
}

# taken from shiny:::processGroupedChoices
processGroupedChoices <- function (choices)
{
  stopifnot(is.list(choices))
  choices <- asNamed(choices)
  choices <- mapply(function(name, choice) {
    choiceIsGroup <- isGroup(choice)
    if (choiceIsGroup && name == "") {
      stop("All sub-lists in \"choices\" must be named.")
    }
    else if (choiceIsGroup) {
      processFlatChoices(choice)
    }
    else {
      as.character(choice)
    }
  }, names(choices), choices, SIMPLIFY = FALSE)
  setDefaultNames(choices)
}


# taken from shiny:::selectOptions
selectOptions <- function (choices, selected = NULL, inputId, perfWarning = FALSE)
{
  if (length(choices) >= 1000) {
    warning("The select input \"", inputId, "\" contains a large number of ",
            "options; consider using server-side selectize for massively improved ",
            "performance. See the Details section of the ?selectizeInput help topic.",
            call. = FALSE)
  }
  html <- mapply(choices, names(choices), FUN = \(choice,label) {
    if (is.list(choice)) {
      sprintf("<optgroup label=\"%s\">\n%s\n</optgroup>",
              htmltools::htmlEscape(label, TRUE),
              selectOptions(choice, selected, inputId, perfWarning))
    }
    else {
      sprintf("<option value=\"%s\"%s>%s</option>",
              htmltools::htmlEscape(choice, TRUE),
              if (choice %in% selected)
                " selected"
              else
                "",
              htmltools::htmlEscape(label))
    }
  })
  HTML(paste(html, collapse = "\n"))
}
