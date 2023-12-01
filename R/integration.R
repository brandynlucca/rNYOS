################################################################################
# VERTICAL INTEGRATION
################################################################################
# ==============================================================================
# Delineate depth limits in dataset
# ==============================================================================
#' Delineate depth limits in dataset
#'
#' @param data Export tibble.
#'
#' @rdname depth.limits
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
depth.limits <- function( data ) {
  # ----------------------------------------------------------------------------
  # Depth filter
  # ----------------------------------------------------------------------------
  data %>%
    filter( ( Frequency %in% c( "38kHz" , "70kHz" , "120kHz" ) &
                Layer_depth_max <= 300 ) |
              ( Frequency == "200kHz" &
                  Layer_depth_max <= 200 ) ) %>%
    return
}
# ==============================================================================
# Vertically integrate backscatter to calculate NASC
# ==============================================================================
#' Vertically integrate backscatter to calculate NASC
#'
#' @param data Export tibble.
#'
#' @rdname integrate.nasc
#' @importFrom dplyr group_by mutate ungroup reframe nest_by select
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
integrate.nasc <- function( data ) {
  # ----------------------------------------------------------------------------
  # Depth filter
  # ----------------------------------------------------------------------------
  data %>%
    depth.limits( ) %>%
    mutate( Date = as.Date( Datetime ) ) %>%
    # --------------------------------------------------------------------------
    # Vertically integrate
    # --------------------------------------------------------------------------
    group_by( Survey , Line , Frequency , Bandwidth ,
              Date , Interval , Longitude , Latitude ) %>%
    reframe( NASC = sum( NASC ) ) %>%
    nest_by( Survey ) %>%
    group_by( Survey ) %>%
    mutate( Date_mean = map( data ,
                            function( .x ) mean( .x$Date ) ) ) %>%
    unnest( c( data , Date_mean ) ) %>%
    mutate( Date = Date_mean ) %>%
    select( - Date_mean ) %>%
    return
}
