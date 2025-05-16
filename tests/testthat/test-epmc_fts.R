library(testthat)
library(dplyr)
# Assuming epmc_search is in the global environment or loaded from the package
# If epmc_search is not exported, you might need to use programets:::epmc_search

# Mocking httr2::req_perform and related functions might be necessary for robust testing
# without actual API calls. For this example, we'll do some tests that might hit the API
# and some that check input validation.

test_that("epmc_search handles invalid query input", {
  expect_error(epmc_search(query = 123), "`query` must be a non-empty character string.")
  expect_error(epmc_search(query = ""), "`query` must be a non-empty character string.")
  expect_error(epmc_search(query = c("query1", "query2")), "`query` must be a non-empty character string.")
})

test_that("epmc_search handles invalid page_limit input", {
  expect_error(epmc_search(query = "test", page_limit = "a"), "`page_limit` must be a positive number.")
  expect_error(epmc_search(query = "test", page_limit = 0), "`page_limit` must be a positive number.")
  expect_error(epmc_search(query = "test", page_limit = -1), "`page_limit` must be a positive number.")
})

test_that("epmc_search returns a tibble for a valid query that yields no results", {
  # This query is unlikely to return results, or we can mock the API response
  # For now, assume it might hit the API but expect a tibble
  # A very specific query unlikely to have results:
  no_results_query <- "asdfqwerzxcvasdfqwerzxcv unlikely query string"
  
  # Suppress informational messages during the test
  suppressMessages({
    results <- epmc_search(query = no_results_query, page_limit = 1)
  })
  expect_s3_class(results, "tbl_df")
  # Check if it returns the predefined empty structure
  expected_cols <- c("id", "source", "pmid", "pmcid", "doi", "title", "authorString", 
                     "journalTitle", "issue", "journalVolume", "pubYear", "journalIssn", 
                     "pageInfo", "pubType", "isOpenAccess", "inEPMC", "inPMC", 
                     "hasPDF", "hasBook", "citedByCount", "hasReferences", 
                     "hasTextMinedTerms", "hasDbCrossReferences", "hasLabsLinks", 
                     "hasTMAccessionNumbers", "firstPublicationDate")
  expect_equal(names(results), expected_cols)
  expect_equal(nrow(results), 0)
})

# The following tests will hit the actual API and are more like integration tests.
# They can be slow and dependent on network connectivity and API status.
# For CI/CD, these might be skipped or run less frequently.

# To properly test without hitting the API, one would use packages like `httptest` or `webmockr`
# to mock httr2 responses.

# Test with a known query that should return results (requires API call)
# This test is marked to be skipped on CRAN as it requires network access.
skip_on_cran()

test_that("epmc_search retrieves results for a known query", {
  # A common term that should yield results
  # Using a very small page_limit to minimize data transfer and time
  results <- epmc_search(query = "aspirin", page_limit = 1)
  expect_s3_class(results, "tbl_df")
  expect_gt(nrow(results), 0) # Expect some results for "aspirin"
  expect_true("pmid" %in% names(results)) # Check for a key column
})

test_that("epmc_search respects page_limit argument", {
  # Query for a common term, limit to 1 page (which means up to 1000 results by default pageSize)
  # Then query for a smaller number of results if possible, or check number of pages fetched (if observable)
  # This test is a bit tricky without mocking, as we don't know exact number of results per page
  # For now, we check that it doesn't error and returns a tibble.
  
  # We can check if the number of results is <= pageSize (1000 in the function)
  # when page_limit is 1.
  results_one_page <- epmc_search(query = "cancer", page_limit = 1)
  expect_s3_class(results_one_page, "tbl_df")
  if (nrow(results_one_page) > 0) { # Only check if results are returned
    expect_lte(nrow(results_one_page), 1000) # pageSize is 1000
  }

  # If we had a way to count API calls or pages fetched (e.g., via messages or a counter),
  # we could test page_limit more directly. The current `rlang::inform` messages
  # could be captured if needed, but that makes tests more complex.
})

test_that("epmc_search handles queries with special characters (if API supports)", {
  # Example: query with a hyphen or other characters. 
  # This depends on how EuropePMC handles them.
  # For now, just a basic check that it doesn't immediately error out.
  results <- epmc_search(query = "COVID-19", page_limit = 1)
  expect_s3_class(results, "tbl_df")
  # Further checks would depend on expected behavior for such queries.
})

# Example of how one might use httptest (conceptual)
# library(httptest)
# test_that("epmc_search parses mocked successful response correctly", {
#   with_mock_dir("fixtures/epmc_success", {
#     results <- epmc_search(query = "mocked_success", page_limit = 1)
#     expect_s3_class(results, "tbl_df")
#     expect_equal(nrow(results), 1) # Assuming mock returns 1 result
#     expect_equal(results$pmid[1], "123456")
#   })
# })
# 
# test_that("epmc_search handles mocked API error", {
#   with_mock_dir("fixtures/epmc_error", {
#     expect_error(epmc_search(query = "mocked_error", page_limit = 1), 
#                  "Europe PMC API request failed") # Or more specific error
#   })
# })
