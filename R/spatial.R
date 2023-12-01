################################################################################
# SPATIAL FUNCTIONS
################################################################################
# ==============================================================================
# Convert degrees to radians
# ==============================================================================
#' Convert from degrees to radians
#'
#' @param coordinate Longitude/Latitude (degrees).
#'
#' @rdname radians
#' @export
radians <- function( coordinate ) {
  coordinate * pi / 180
}
# ==============================================================================
# Convert radians to degrees
# ==============================================================================
#' Convert from degrees to radians
#'
#' @param coordinate Longitude/Latitude (radians).
#'
#' @rdname degrees
#' @export
degrees <- function( coordinate ) {
  ( coordinate * 180 / pi ) %% 360
}
# ==============================================================================
# Calculate distance between two points
# ==============================================================================
#' Calculate distance between two points
#'
#' @param Longitude Longitude (degrees).
#' @param Latitude Latitude (degrees).
#'
#' @rdname line.distance
#' @export
line.distance <- function( Longitude ,
                           Latitude ) {
  # ----------------------------------------------------------------------------
  # Convert to radians
  # ----------------------------------------------------------------------------
  Longitude_radians <- radians( Longitude )
  Latitude_radians <- radians( Latitude )
  n <- length( Longitude_radians )
  # ----------------------------------------------------------------------------
  # Calculate distances (radians)
  # ----------------------------------------------------------------------------
  dLongitude <- diff( Longitude_radians )
  dLatitude <- diff( Latitude_radians )
  # ----------------------------------------------------------------------------
  # Haversine distance
  # ----------------------------------------------------------------------------
  h <- sin( dLatitude / 2 ) ^ 2 + cos( Latitude_radians[ 1 : ( n - 1 ) ] ) *
    cos( Latitude_radians[ 2 : n ] ) * sin( dLongitude / 2 ) ^ 2
  hav <- 2 * atan2( sqrt( h ) , sqrt( 1 - h ) )
  # ----------------------------------------------------------------------------
  # Convert to meters
  # ----------------------------------------------------------------------------
  distance <- 6371e3 * hav
  # ----------------------------------------------------------------------------
  # Return output
  # ----------------------------------------------------------------------------
  return( distance )
}
# ==============================================================================
# Calculate along-track distance for each transect
# ==============================================================================
#' Calculate along-track/transect distance for each transect
#'
#' @param Interval Interval (along-transect interval number).
#' @param Longitude Longitude (degrees).
#' @param Latitude Latitude (degrees).
#'
#' @rdname transect.distance
#' @importFrom dplyr pull arrange
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
transect.distance <- function( Interval ,
                               Longitude ,
                               Latitude ) {
  # ----------------------------------------------------------------------------
  # Identify gaps in transect intervals
  # ----------------------------------------------------------------------------
  tibble( Interval , Longitude , Latitude ) %>%
    arrange( Longitude , Latitude ) %>%
    pull( Interval ) -> Interval
  Interval_run <- abs( diff( Interval ) )
  Effort_index <- c( 0 ,
                     which( Interval_run > 1 ) ,
                     length( Interval ) )
  # ----------------------------------------------------------------------------
  # Calculate along-track/transect effort distance
  # ----------------------------------------------------------------------------
  if ( length( Effort_index ) > 2 ) {
    effort_interval <- lapply( seq_along( Effort_index )[ - length( Effort_index ) ] ,
                               function( i ) {
                                 Longitude_short <- Longitude[ ( Effort_index[ i ] + 1 ) : Effort_index[ i + 1 ] ]
                                 Latitude_short <- Latitude[ ( Effort_index[ i ] + 1 ) : Effort_index[ i + 1 ] ]
                                 distance <- line.distance( Longitude_short ,
                                                            Latitude_short )
                                 interval_sum <- c( 0 ,
                                                    distance )
                               } )
    interval_length <- cumsum( unlist( effort_interval ) )
  } else {
    interval_length <- c( 0 ,
                          cumsum( line.distance( Longitude ,
                                                 Latitude ) ) )
  }
  # ----------------------------------------------------------------------------
  # Return transect interval distance
  # ----------------------------------------------------------------------------
  return( interval_length )
}
# ==============================================================================
# Remove bad coordinate values
# ==============================================================================
#' Remove bad coordinate values
#'
#' @param data Export tibble.
#'
#' @rdname coordinate.clean
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
coordinate.clean <- function( data ) {
  # ----------------------------------------------------------------------------
  # Reduce the tibble (remove bad coordinates)
  # ----------------------------------------------------------------------------
  data %>%
    filter( ( ! is.na( Longitude ) &
                abs( Longitude ) != 999 ) &
              ( ! is.na( Latitude ) &
                  abs( Latitude ) != 999 ) ) %>%
    return
}
# ==============================================================================
# Wrapper function that calculates the along-transect distance
# ==============================================================================
#' Wrapper function that calculates the along-transect distance
#'
#' @param data Export tibble.
#'
#' @rdname survey.distance
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @export
survey.distance <- function( data ) {
  # ----------------------------------------------------------------------------
  # Group by each survey, line, and frequency -- then calculate distance
  # ----------------------------------------------------------------------------
  data %>%
    coordinate.clean( ) %>%
    group_by( Survey , Frequency , Line ) %>%
    mutate( Distance = transect.distance( Interval , Longitude , Latitude ) ) %>%
    ungroup( ) %>%
    return
}
