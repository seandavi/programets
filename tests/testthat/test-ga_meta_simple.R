test_that("ga_meta_simple returns expected tibble", {
  # Call the function
  res <- ga_meta_simple()

  # Check if the result is a tibble
  expect_s3_class(res, "tbl_df")

  # Check for expected columns
  expected_cols <- c(
    "apiName", "uiName", "description", "category",
    "deprecatedApiNames", "class", "type"
  )
  expect_true(all(expected_cols %in% names(res)))

  # Check if the number of records is greater than zero
  expect_gt(nrow(res), 0)
})