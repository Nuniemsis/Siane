#' @import "rgdal"
#'@importFrom raster shift extent bind
#'@importFrom sp proj4string proj4string<-
#'@importFrom methods as
#'@title Loads a spanish map
#'
#' @description A function that returns a S4 object(map) of a Spain specific map

#' @examples
#' \dontrun{
#' obj <- register_siane("/home/ncarvalho/Downloads/") # Registering a sample of Siane
#' shp <- siane_map(obj, level = "Municipios", canarias = FALSE, peninsula = "close") 
#' plot(shp) # Plot the map}


#' @param obj : Path to the Siane maps.
#' @param level : Is a string that represents the administrative level.
#' @param year : The numeric year of the maps requested.
#' @param canarias : A boolean value(TRUE/FALSE) to take Canarias into account.
#' @param scale : It's the scale of the map.
#' @param peninsula : Parameter that specificates the layout of Canarias islands.

#' @return A S4 object specified by the user
#'
#'
#' @details - \code{obj} should be a string of the path to the maps. It should contain the subfolders \bold{SIANE_CARTO_BASE_S_3M} and \bold{SIANE_CARTO_BASE_S_6M5}. \cr
#' - \code{level} can take the values of "Provincias","Municipios" or "Comunidades". The default value \cr is \code{level <- "Provincias"} \cr
#' - \code{canarias} set to TRUE returns the Canarias map. \code{canarias} set to FALSE returns to the Peninsulae maps. \cr
#' - The \code{year} variable should be numeric. If the map is not available for that year, the loaded map will be the latest available map. \cr
#' The default value for the year is the latest year in which the maps are available. \cr
#' - The \code{scale} of the maps can be 1:3000000 or 1:6500000. The corresponding values for the \code{scale} variable are "3m" and "6m". \cr
#' The default value for the municipalities maps is \code{scale <- "3m"} . The default value for the other maps is \code{scale <- "6m"} \cr
#' - The \code{peninsula} variable can take the values "far", "close" and "none".
#' @export
#'
#'
#'

siane_map <- function(obj, canarias, year, level, scale, peninsula){

  if(missing(canarias)){
    canarias <- FALSE
  }

  if(missing(peninsula)){
    peninsula <- "close"
  }
  if(canarias == TRUE){

  if(peninsula == "far"){
  shp_peninsula <- siane_map_piece(obj = obj, year = year,
                          canarias = FALSE, level = level,
                          scale = scale) # Loading the municipality's map of Spain
  shp_canarias <- siane_map_piece(obj = obj, year = year,
                          canarias = TRUE , level = level,
                          scale = scale) # Canarias True

  shp_total <- bind(shp_peninsula, shp_canarias)
  return(shp_total)

  }
  if(peninsula == "close"){
    shp_peninsula <- siane_map_piece(obj = obj, canarias = FALSE, level = level, scale = scale) # Loading the municipality's map of Spain
    shp_canarias <- siane_map_piece(obj = obj, canarias = TRUE , level = level, scale = scale) # Canarias True

    shp_canarias_shifted <- shift(shp_canarias, x = 18, y = 8)
    shp_total <- bind(shp_peninsula, shp_canarias_shifted)


    rectangle <- as(extent(-0.5, 5, 9, 11.6), "SpatialPolygons")
    proj4string(rectangle) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

    shif_rect <- shift(rectangle, y = 26)
    shp_rect_total <- bind(shif_rect,shp_total)

    return(shp_rect_total)
  }
  if(peninsula == "none"){
    shp <- siane_map_piece(obj = obj, canarias = canarias, level = level, year = year, scale = scale) # Loading the municipality's map of Spain
    return(shp)
  }
  }
  else{

    # canarias = FALSE
    shp <- siane_map_piece(obj = obj, level = level, canarias = canarias, year = year, scale = scale) # Loading the municipality's map of Spain

    return(shp)

    }




  }
