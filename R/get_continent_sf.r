#' Get continent spatial data from a GeoJSON file
#'
#' This function reads a GeoJSON file from a given URL, extracts continent information, and returns an sf object with continent polygons.
#'
#' @param url The URL of a GeoJSON file containing country or continent data
#' @return An \code{sf} object containing polygons representing continents. The object includes a column for the continent names and corresponding geometries.
#' @import geojsonio
#' @import sf
#' @export
#' @examples
#' # Get continent data from a GeoJSON file
#' continent_sf <- get_continent_sf(url = "https://d2ad6b4ur7yvpq.cloudfront.net/naturalearth-3.3.0/ne_50m_admin_0_countries.geojson")
#' print(continent_sf)
#' @export
#' 
get_continent_sf <- function(url) {
  # Read the geojson file
  countries <- geojsonio::geojson_read(url, what = "sp")
  
  # Convert to sf
  countries_sf <- st_as_sf(countries)
  
  # Filter out countries with valid continent information
  continent_countries <- countries_sf[!is.na(countries_sf$continent), ]
  
  # Group by continent and combine geometries
  continent_polygons <- aggregate(continent_countries["geometry"], by=list(continent_countries$continent), FUN = function(x) st_union(x))
  
  # Create a new sf object with continent and geometry
  continent_sf <- st_sf(continent = continent_polygons$Group.1, geometry = continent_polygons$geometry)
  
  return(continent_sf)
}

