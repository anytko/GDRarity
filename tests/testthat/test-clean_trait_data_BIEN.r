library(testthat)

test_that("clean_trait_data_BIEN cleans data correctly without author info", {
  df <- tibble::tibble(
    scrubbed_species_binomial = c("Species A", "Species A", "Species B"),
    trait_name = c("Leaf Area", "Leaf Area", "Seed Mass"),
    trait_value = c(10, 12, 5),
    unit = c("cm2", "cm2", "g"),
    method = c("measurement", "measurement", "measurement"),
    url_source = c("source1", "source1", "source2")
  )
  
  result <- clean_trait_data_BIEN(df)
  
  expect_true("species_name" %in% names(result))
  expect_true(all(c("Leaf Area", "Seed Mass") %in% colnames(result)))
  expect_equal(nrow(result), 2)
})

test_that("clean_trait_data_BIEN requires author columns when author_info = TRUE", {
  df <- tibble::tibble(
    scrubbed_species_binomial = "Species A",
    trait_name = "Leaf Area",
    trait_value = 10,
    unit = "cm2",
    method = "measurement",
    url_source = "source1",
    project_pi = "Dr. X",
    project_pi_contact = "x@example.com"
  )
  
  expect_no_error(clean_trait_data_BIEN(df, author_info = TRUE))
})
