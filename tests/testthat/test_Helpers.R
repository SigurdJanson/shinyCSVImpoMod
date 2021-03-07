test_that(".HandleUTF8", {
  expect_identical(.HandleUTF8("◨"), "&#9704;")
  expect_identical(.HandleUTF8("abc"), paste0("&#", 97:99, ";", collapse = ""))

  # This could give trouble on a German installation on Windows
  #-expect_identical(.HandleUTF8("€"), "€")
  expect_identical(.HandleUTF8("€"), "&#8364;") # strangely in 'testthat' it works
})
