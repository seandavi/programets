library(testthat)
library(programets) 

test_that("get_github_by_topic() errors if topics is empty", {
  expect_error(get_github_by_topic(character()), "At least one topic must be provided")
})

test_that("get_github_by_topic() returns a data frame", {
  skip_on_cran()
  skip_if_offline()

  result <- get_github_by_topic(c("rstats"), limit = 5)
  expect_s3_class(result, "data.frame")
})

test_that("Returned data frame contains expected columns", {
  skip_on_cran()
  skip_if_offline()

  result <- get_github_by_topic(c("bioinformatics"), limit = 3)
  expected_cols <- c(
    "name", "owner", "stars", "forks", "open_issues",
    "language", "license", "created_at", "pushed_at", "updated_at", "html_url"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_github_by_topic() returns no results for nonsense topic", {
  skip_on_cran()
  skip_if_offline()

  result <- get_github_by_topic(c("thisisaveryunlikelytopicname1234567890"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
