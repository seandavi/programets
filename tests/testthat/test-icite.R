test_that("icite returns expected tibble for valid PMIDs", {
  pmids <- c(26001965, 25015380)
  icite_records <- icite(pmids)

  # Check if the result is a tibble
  expect_s3_class(icite_records, "tbl_df")

  # Check for expected columns
  expected_cols <- c(
    "pmid", "authors", "citation_count", "citations_per_year",
    "expected_citations_per_year", "field_citation_rate",
    "is_research_article", "journal", "nih_percentile",
    "relative_citation_ratio", "title", "year"
  )
  expect_true(all(expected_cols %in% names(icite_records)))

  # Check if the number of records matches the number of PMIDs
  expect_equal(nrow(icite_records), length(pmids))

  # Check if PMIDs in the result match the input PMIDs
  expect_true(all(pmids %in% icite_records$pmid))
})

test_that("icite handles more than 1000 PMIDs", {
  pmids <- 1:1001
  expect_error(icite(pmids), "maximum pubmed ids exceeded")
})
