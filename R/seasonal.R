################################################################################
# SEASONAL FUNCTIONS
################################################################################
# ==============================================================================
# Define seasonal interval based on dates
# ==============================================================================
#' Define seasonal interval based on dates
#'
#' @param data Export tibble.
#' @param month_pad Number of months to pad along the x-axis.
#' @param season_definition Season definition (meteorological vs astronomical).
#'
#' @rdname seasonal_interval
#' @importFrom lubridate month year years leap_year ymd interval
#' @importFrom dplyr mutate case_when slice if_else
#' @export
seasonal_interval <- function( data ,
                               month_pad = 2 ,
                               season_definition = "meteorological" ) {
  # ----------------------------------------------------------------------------
  # Define date and year ranges
  # ----------------------------------------------------------------------------
  # Date range
  date_range <- range( data$Date )
  # Year range
  year_range <- range( year( data$Date ) )
  years <- seq( from = year_range[ 1 ] - 1 ,
                to = year_range[ 2 ] + 1 ,
                by = 1 )
  # ----------------------------------------------------------------------------
  # Season tibble
  # ----------------------------------------------------------------------------
  tibble( Season = rep( x = c( "Winter" ,
                               "Spring" ,
                               "Summer" ,
                               "Fall" ) ,
                        times = length( years ) ) ,
          Year = rep( x = years ,
                      each = 4 ) ) -> seasons
  # ----------------------------------------------------------------------------
  # Define seasons based on season definition
  # ----------------------------------------------------------------------------
  if ( season_definition == "meteorological" ) {
    seasons %>%
      mutate(
        Date_start = case_when(
          Season == "Winter" ~ ymd( paste0( Year - 1 ,
                                            "1201" ) ) ,
          Season == "Spring" ~ ymd( paste0( Year ,
                                            "0301" ) ) ,
          Season == "Summer" ~ ymd( paste0( Year ,
                                            "0601" ) ) ,
          Season == "Fall" ~ ymd( paste0( Year ,
                                          "0901" ) )
        ) ,
        Date_end = case_when(
          Season == "Winter" & leap_year( Year ) ~ ymd( paste0( Year ,
                                                                "0229" ) ) ,
          Season == "Winter" & ! leap_year( Year ) ~ ymd( paste0( Year ,
                                                                  "0228" ) ) ,
          Season == "Spring" ~ ymd( paste0( Year ,
                                            "0531" ) ) ,
          Season == "Summer" ~ ymd( paste0( Year ,
                                            "0831" ) ) ,
          Season == "Fall" ~ ymd( paste0( Year ,
                                          "1130" ) )
        )
      ) -> seasons_tibble
  } else if ( season_definition == "astronomical" ) {
    seasons %>%
      mutate(
        Date_start = case_when(
          Season == "Winter" ~ ymd( paste0( Year - 1 ,
                                            "1221" ) ) ,
          Season == "Spring" ~ ymd( paste0( Year ,
                                            "0321" ) ) ,
          Season == "Summer" ~ ymd( paste0( Year ,
                                            "0621" ) ) ,
          Season == "Fall" ~ ymd( paste0( Year ,
                                          "0921" ) )
        ) ,
        Date_end = case_when(
          Season == "Winter" ~ ymd( paste0( Year ,
                                            "0320" ) ) ,
          Season == "Spring" ~ ymd( paste0( Year ,
                                            "0620" ) ) ,
          Season == "Summer" ~ ymd( paste0( Year ,
                                            "0920" ) ) ,
          Season == "Fall" ~ ymd( paste0( Year ,
                                          "1220" ) )
        )
      ) -> seasons_tibble
  }
  # ----------------------------------------------------------------------------
  # Define season interval
  # ----------------------------------------------------------------------------
  seasons_tibble %>%
    mutate( Date_interval = interval( Date_start , Date_end ) ) -> season_interval
  padded_months <- date_range + months( c( - month_pad ,
                                           month_pad ) )
  season_interval %>%
    slice( which( ( padded_months[ 1 ] %within% season_interval$Date_interval ) == T ) :
             which( ( padded_months[ 2 ] %within% season_interval$Date_interval ) == T ) ) %>%
    mutate( N = 1 : n( ) ,
            Date_start = if_else( N == 1 ,
                                  padded_months[ 1 ] ,
                                  Date_start ) ,
            Date_end = if_else( N == max( N ) ,
                                padded_months[ 2 ] ,
                                Date_end ) ) -> season_interval
  # ----------------------------------------------------------------------------
  # Return
  # ----------------------------------------------------------------------------
  return( season_interval )
}
