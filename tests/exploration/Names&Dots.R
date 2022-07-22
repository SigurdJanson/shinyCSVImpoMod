

as.character(list(A="A", "B", C=NULL))
#> [1] "A"    "B"    "NULL"

x <- list(A="A", "B", C=NULL)
names(x)


reprex::reprex({
  library(shiny)

ta <- function(...) ...names()

tb <- function(...) names(list(...))

tc <- function(...) names(rlang::dots_list(...))

ta(A=1, B="zwei", 3, D=NULL)
tb(A=1, B="zwei", 3, D=NULL)
tc(A=1, B="zwei", 3, D=NULL)


ua <- function(...) ...

ub <- function(...) list(...)

uc <- function(...) rlang::dots_list(...)

ua(A=1, B="zwei", 3, D=NULL)
ub(A=1, B="zwei", 3, D=NULL)
uc(A=1, B="zwei", 3, D=NULL)


sapply(ua(A=1, B="zwei", 3, D=NULL), function(x) paste0(names(x), "=", x))
lapply(ua(A=1, B="zwei", 3, D=NULL), function(x) paste0(names(x), "=", x))

mapply(function(n, x) paste(n, x, sep="="),
       ta(A=1, B="zwei", 3, D=NULL),
       ua(A=1, B="zwei", 3, D=NULL),
       SIMPLIFY = TRUE, USE.NAMES = TRUE)


.cpaste <- function(a, b, sep = "=")
  paste0(
    a, ifelse(isTruthy(a) && isTruthy(b), "=", ""), b
  )
mapply(function(n, x) .cpaste(n, x),
       ta(A=1, B="zwei", 3, D=NULL),
       ua(A=1, B="zwei", 3, D=NULL),
       SIMPLIFY = TRUE, USE.NAMES = TRUE)
})
