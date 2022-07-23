


test_that("", {
  x <- 2
  obs <- HtmlAttrStr(id="my id", A="A", "B", C=NULL, D=NA, E=Inf, F=NaN, NA, H=x, Y="Fa\"il", `Zor>ro`="Z")
  expect_identical(
    obs,
    c(r"{id="my id"}", r"{A="A"}", r"{B}", "C", "D", r"{H="2"}", r"{Y="Fa_il"}", r"{Zor_ro="Z"}")
  )
})
