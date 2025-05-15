test_that("ga_dataframe returns expected tibble", {
  # Call the function

  ga_id <- 388188354

  json_file <- gargle::secret_decrypt_json(
    path = system.file(
      "secret",
      "ga4-acess-keyfile.json",
      package = "programets"
    ),
    key = "GARGLE_ENCRYPTION_KEY"
  )

  googleAnalyticsR::ga_auth(
    json_file = json_file
  )

  daily_user_country_data <- ga_dataframe(
    property_id = ga_id,
    dimensions = c("date", "newVsReturning", "country"), # Added dimensions
    metrics = c("activeUsers", "sessions"), # Example metrics
    start_date = Sys.Date() - 365,
    end_date = Sys.Date()
)

  # Check if the result is a tibble
  expect_s3_class(daily_user_country_data, "tbl_df")

  # Check if the number of records is greater than zero
  expect_gt(nrow(daily_user_country_data), 0)
})