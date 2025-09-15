#' Get continent spatial data from a GeoJSON file
#'
#' This function reads a GeoJSON file from a given URL, extracts continent information, and returns an sf object with continent polygons.
#'
#' @param url Optional URL or file path to a GeoJSON file containing country or continent data. If `NULL` (default), the function uses the embedded Natural Earth continent boundaries from the package's `extdata` directory.
#' 
#' @return An `sf` object with:
#'   \itemize{
#'     \item `continent` — continent name.
#'     \item `geometry` — polygon geometry for that continent.
#'   }
#' 
#' @import geojsonio
#' @import sf
#' 
#' @examples
#' # Get continent data from a GeoJSON file
#' continent_sf <- get_continent_sf()
#' print(continent_sf)
#' @export
get_continent_sf <- function(url = NULL) {
  # Define the default path to the continent GeoJSON file in the package directory
  default_geojson_path <- system.file("extdata", "continents.geojson", package = "GeoFunPhy")
  
  # If no URL is provided, use the default GeoJSON file
  if (is.null(url)) {
    if (default_geojson_path == "") {
      stop("Default continent GeoJSON file not found. Ensure the file is in the 'extdata' directory of the package.")
    }
    # Read the default GeoJSON file from the package
    countries <- geojsonio::geojson_read(default_geojson_path, what = "sp")
  } else {
    # If a URL is provided, read the GeoJSON file from the URL
    countries <- geojsonio::geojson_read(url, what = "sp")
  }
  
  # Convert the data to sf
  countries_sf <- st_as_sf(countries)
  
  # Filter out countries with valid continent information
  continent_countries <- countries_sf[!is.na(countries_sf$continent), ]
  
  # Group by continent and combine geometries
  continent_polygons <- aggregate(continent_countries["geometry"], by=list(continent_countries$continent), FUN = function(x) st_union(x))
  
  # Create a new sf object with continent and geometry
  continent_sf <- st_sf(continent = continent_polygons$Group.1, geometry = continent_polygons$geometry)
  
  return(continent_sf)
}
