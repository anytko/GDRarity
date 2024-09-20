library(testthat)
library(sf)
library(geojsonio)

# Create a temporary file with invalid content for testing
invalid_geojson_path <- tempfile(fileext = ".geojson")
writeLines("This is not a valid GeoJSON file", invalid_geojson_path)


# Define a function to create a valid temporary GeoJSON file for testing
create_temp_geojson <- function() {
  tmp_geojson <- tempfile(fileext = ".geojson")
  geojson_content <- '{
    "type": "FeatureCollection",
    "features": [
      {
        "type": "Feature",
        "geometry": {
          "type": "Polygon",
          "coordinates": [
            [
              [-10, 50],
              [10, 50],
              [10, 30],
              [-10, 30],
              [-10, 50]
            ]
          ]
        },
        "properties": {
          "continent": "TestContinent"
        }
      }
    ]
  }'
  writeLines(geojson_content, tmp_geojson)
  return(tmp_geojson)
}

test_that("get_continent_sf reads internal file correctly", {
  # Run the function without a URL to read the internal file
  result <- get_continent_sf()
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 8)
  expect_equal(result$continent[1], "Africa")  # Adjust according to your actual data
})

test_that("get_continent_sf can read a user-provided GeoJSON file", {
  # Create a temporary GeoJSON file for testing
  tmp_geojson <- create_temp_geojson()
  
  # Test the function with the temporary GeoJSON file
  result <- get_continent_sf(url = tmp_geojson)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_equal(result$continent[1], "TestContinent")
  
  # Clean up temporary file
  unlink(tmp_geojson)
})


