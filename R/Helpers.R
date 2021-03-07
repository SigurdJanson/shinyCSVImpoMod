#' Convert characters to utf8 representation
#' @param x A string
#' @return All characters are replaced by unicode representations
.HandleUTF8 <- function(x){
  map <- function(x) {
    m <- utf8ToInt(x)
    #-if (is.na(m)) x <- enc2utf8(x)
    return(ifelse(is.na(m), x, sprintf("&#%d;", m)))
  }
  xs <- strsplit(as.character(x), "")[[1]]
  paste0(sapply(xs, map), collapse="")
}
