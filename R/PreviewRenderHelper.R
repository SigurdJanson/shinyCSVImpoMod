


# CSS classes
.ContainerClass <- "shiny-input-container-inline" # from shiny
.DisabledClass <-  "shinyjs-disabled" # from shinyjs

.EditSuffix <- "_name"
.InclSuffix <- "_incl"
.TypeSuffix <- "_type"



#' Render elements of a preview table
#'
#' @param Elements
#'
#' @return
#'
#' @examples
.renderTableRow <- function(Elements) {
  Result <- lapply(Elements, function(x) paste0("<td>", x, "</td>"))
  paste("<tr>", paste(Result, collapse = ""), "</tr>")
}



#' ```
#' <div class="form-group shiny-input-container">
#'   <label class="control-label" id="ColName 2-label" for="ColName 2">...</label>
#'   <input id="ColName 2" type="text" class="form-control" value="A"/>
#' </div>
#' ```
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




#'
#'
#'
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



