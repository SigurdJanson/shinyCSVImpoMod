library(testthat)
library(readr)


test_that("Get Default", {
  expect_silent(obs <- FindLocale(""))
  # Check formalities
  expect_s3_class(obs, "locale")
  expect_named(obs, names(locale()))
  expect_named(obs$date_names, names(locale()$date_names)) # names of sub-list
  # Check content
  expect_identical(obs$decimal_mark, unname(Sys.localeconv()["mon_decimal_point"]))
  expect_identical(obs$grouping_mark, unname(Sys.localeconv()["mon_thousands_sep"]))
  expect_identical(obs$date_format, default_locale()$date_format)
  expect_identical(obs$time_format, default_locale()$time_format)
  expect_identical(obs$tz, default_locale()$tz)
  expect_identical(obs$encoding, default_locale()$encoding)
})


test_that("Find by country prefix only: de (when 'de-DE' & 'de-CH' exist)", {
  expect_silent(obs <- FindLocale("de"))
  expect_identical(obs, FindLocale("de-DE"))
})


test_that("Exact find: xx-XX", {
  expect_silent(obs <- FindLocale("de-CH"))
  expect_identical(obs$decimal_mark, ".")
  expect_identical(obs$grouping_mark, "'")
})


test_that("Find by country prefix only: ko (when only 'ko' exists)", {
  expect_silent(obs <- FindLocale("ko"))
  # Check content
  expect_identical(obs$decimal_mark,  ".")
  expect_identical(obs$grouping_mark, ",")
  expect_identical(obs$date_format, "%AD")
  expect_identical(obs$time_format, "%AT")
  expect_identical(obs$tz, "UTC")
  expect_identical(obs$encoding, "UTF-8")
})
