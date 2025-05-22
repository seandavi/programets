# tests/test_nih_reporter.R

library(testthat)
library(httr2)
library(dplyr)

test_that("get_core_project_info returns a tibble", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_core_project_info(core_project_numbers)
  expect_s3_class(object = result, class = "tbl_df")
})

test_that("get_core_project_info returns expected columns", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_core_project_info(core_project_numbers)
  expect_setequal(
    colnames(result), 
    column_names <- c(
      "core_project_num", "found_publication", "appl_id", "subproject_id", "fiscal_year",
      "project_num", "project_serial_num", "organization", "award_type", "activity_code",
      "is_active", "project_num_split", "principal_investigators", "contact_pi_name",
      "program_officers", "agency_ic_admin", "agency_ic_fundings", "cong_dist",
      "spending_categories", "project_start_date", "project_end_date", "organization_type",
      "geo_lat_lon", "opportunity_number", "full_study_section", "award_notice_date",
      "is_new", "mechanism_code_dc", "terms", "pref_terms", "abstract_text", "project_title",
      "phr_text", "spending_categories_desc", "agency_code", "covid_response", "arra_funded",
      "budget_start", "budget_end", "cfda_code", "funding_mechanism", "direct_cost_amt",
      "indirect_cost_amt", "project_detail_url", "date_added", "pmid", "pubmed_url"
    )
  )
})

test_that("get_core_project_info handles missing publications", {
  core_project_numbers <- c("u54od036472", "99999999")
  result <- get_core_project_info(core_project_numbers)
  expect_equal(nrow(result), 9)
  expect_equal(result$found_publication[7], FALSE)
  expect_equal(result$found_publication[8], TRUE)
  expect_equal(result$found_publication[9], TRUE)
})

test_that("get_core_project_info handles invalid input", {
  expect_error(get_core_project_info(123), "Input must be a character vector")
})
