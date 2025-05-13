# tests/test_nih_reporter.R

library(testthat)
library(httr2)
library(dplyr)

test_that("get_publications_for_core_projects returns a tibble", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_publications_for_core_projects(core_project_numbers)
  expect_s3_class(object = result, class = "tbl_df")
})

test_that("get_publications_for_core_projects returns expected columns", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_publications_for_core_projects(core_project_numbers)
  expect_setequal(colnames(result), c("core_project_number", "found", "applid", "pmid", "pubmed_url"))
})

test_that("get_publications_for_core_projects handles missing publications", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_publications_for_core_projects(core_project_numbers)
  expect_equal(nrow(result), 3)
  expect_equal(result$found[1], FALSE)
  expect_equal(result$found[2], TRUE)
  expect_equal(result$found[3], TRUE)
})

test_that("get_publications_for_core_projects handles invalid input", {
  expect_error(get_publications_for_core_projects(123), "Input must be a character vector")
})
