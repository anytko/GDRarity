library(testthat)
library(rgl)

# Create mock data for testing
mock_data <- data.frame(
  species_name = c("Acer_campestre", "Acer_monspessulanum", "Acer_negundo", "Acer_opalus", "Acer_platanoides", "Acer_pseudoplatanus", "Acer_saccharinum"),
  fun_dist = runif(7, min = -2, max = 2),
  range_size = runif(7, min = -2, max = 2),
  evol_dist = runif(7, min = -2, max = 2),
  classifications = c("Common", "Common", "Endemic", "Indicator", "Environmentally Rare", "Common", "Adaptable Survivor")
)

# Test that the function throws errors for missing columns
test_that("plot_EER_status throws errors for missing columns", {
  expect_error(plot_EER_status(mock_data, "fun_dist", "evol_dist", "missing_range_size"),
               "Range size not found in the dataframe.")
  
  # Remove 'classifications' column
  no_classifications <- mock_data[, !colnames(mock_data) %in% "classifications"]
  expect_error(plot_EER_status(no_classifications, "fun_dist", "evol_dist", "range_size"),
               "Dataframe must have columns 'classifications', and 'species_name'.")
  
  # Remove 'species_name' column
  no_species_name <- mock_data[, !colnames(mock_data) %in% "species_name"]
  expect_error(plot_EER_status(no_species_name, "fun_dist", "evol_dist", "range_size"),
               "Dataframe must have columns 'classifications', and 'species_name'.")
  
  # Remove 'fun_dist' column
  no_fun_dist <- mock_data[, !colnames(mock_data) %in% "fun_dist"]
  expect_error(plot_EER_status(no_fun_dist, "fun_dist", "evol_dist", "range_size"),
               "Functional distinctiveness not found in the dataframe.")
  
  # Remove 'evol_dist' column
  no_evol_dist <- mock_data[, !colnames(mock_data) %in% "evol_dist"]
  expect_error(plot_EER_status(no_evol_dist, "fun_dist", "evol_dist", "range_size"),
               "Evolutionary distinctiveness not found in the dataframe.")
})


test_that("plot_EER_status generates the correct plot elements", {
  # Open a new 3D device
  open3d()
  
  # Generate the plot
  expect_silent({
    plot_EER_status(mock_data, "fun_dist", "range_size", "evol_dist")
  })
  
  # Retrieve shape IDs
  shape_ids <- as.numeric(ids3d(type = "shapes")$id)

    # Print debug information
    print("Shape IDs (numeric):")
    print(shape_ids)

    # Ensure shape_ids is a numeric vector
    expect_true(is.numeric(shape_ids))
    expect_true(length(shape_ids) > 0)
  
  # Check for the presence of points
  if (length(shape_ids) > 0) {
    # Retrieve vertices for the first shape
    points_attrib <- tryCatch({
      rgl.attrib(id = shape_ids[1], attrib = "vertices")
    }, error = function(e) {
      print(paste("Error retrieving attributes:", e$message))
      NULL
    })
    
    # Print debug information
    print("Points Attributes:")
    print(points_attrib)
    
    # Ensure points_attrib is not NULL
    expect_true(!is.null(points_attrib))
    
    # Check that the number of vertices matches the number of rows in mock_data
    if (!is.null(points_attrib)) {
      expect_true(nrow(points_attrib) == nrow(mock_data))
    }
  } else {
    fail("No shapes were found in the 3D plot.")
  }
  
  # Close the device
  close3d()
})
