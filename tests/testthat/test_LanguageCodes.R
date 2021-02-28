test_that("GetLanguageTag: Success", {
  expect_silent(obs <- GetLanguageTag(c("de-de", "de-CH", "EN-GB", "ar", "AF")))
  expect_identical(obs, c("de", "de", "en", "ar", "af"))
})

test_that("GetLanguageTag: Falsy input", {
  expect_error(obs <- GetLanguageTag(character()))
  expect_error(obs <- GetLanguageTag(NULL))
})





test_that("GetCountryTag: Success", {
  expect_silent(obs <- GetCountryTag(c("de-de", "de-CH", "EN-GB", "es")))
  expect_identical(obs, c("DE", "CH", "GB", NA))
})

test_that("GetCountryTag: Falsy input", {
  expect_error(obs <- GetCountryTag(character()))
  expect_error(obs <- GetCountryTag(NULL))
})





test_that("FormatLocaleCode: Success", {
  expect_silent(obs <- FormatLocaleCode(c("de-de", "de-CH", "EN-GB", "Es")))
  expect_identical(obs, c("de-DE", "de-CH", "en-GB", "es"))
})
test_that("FormatLocaleCode: Falsy input", {
  expect_error(obs <- FormatLocaleCode(character()))
  expect_error(obs <- FormatLocaleCode(NULL))
})

