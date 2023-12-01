################################################################################
# STATISTICS
################################################################################
# ==============================================================================
# Calculate the stratified mean (based on transect length)
# ==============================================================================
#' Calculate the stratified mean (based on transect length)
#'
#' @param data Export tibble.
#' @param ci_pct Confidence interval percentile. (Default = 0.95)
#' @rdname stratified.mean
#' @importFrom dplyr nest_by ungroup mutate select
#' @importFrom purrr map_dbl
#' @importFrom magrittr %>%
#' @export
statified.mean <- function( data ,
                            ci_pct = 0.95 ) {
  # ----------------------------------------------------------------------------
  # Calculate confidence interval percentile
  # ----------------------------------------------------------------------------
  pct <- 1 - ( ( 1 - ci_pct ) / 2 )
  # ----------------------------------------------------------------------------
  # Stratified mean calculation
  # ----------------------------------------------------------------------------
  # Stratum 1
  data %>%
    nest_by( Survey , Date , Line , Bandwidth , Frequency ) %>%
    ungroup( ) %>%
    mutate( Transect_length = map_dbl( data ,
                                       ~ max( .x$Distance ) ) ,
            NASC_line_mean = map_dbl( data ,
                                      ~ mean( .x$NASC ) ) ) %>%
    group_by( Survey , Line , Frequency ) %>%
    mutate( Survey_line_transect_mean = mean( Transect_length ) ,
            N = map_dbl( data ,
                         ~ length( .x$NASC ) ) ) %>%
    ungroup( ) %>%
    mutate( Stratum_1_weight = Transect_length / Survey_line_transect_mean ,
            Stratum_1_mean = Stratum_1_weight * NASC_line_mean ,
            Stratum_1_deviation = ( NASC_line_mean - Stratum_1_mean ) ^ 2 ,
            Stratum_1_MSE = Stratum_1_weight ^ 2 * Stratum_1_deviation ) %>%
    select( - Bandwidth , - data ) %>%
    group_by( Date , Survey , Line , Frequency ) %>%
    reframe( Transect_effort = sum( Transect_length ) ,
             Transect_mean_NASC = mean( Stratum_1_mean ) ,
             Transect_variance_NASC = sum( Stratum_1_MSE ) / ( n( ) * ( n( ) - 1 ) ) ,
             N = sum( N ) ) %>%
    # Stratum 2
    group_by( Date , Survey , Frequency ) %>%
    mutate( Survey_transect_mean = mean( Transect_effort ) ) %>%
    ungroup( ) %>%
    mutate( Stratum_2_weight = Transect_effort / Survey_transect_mean ,
            Stratum_2_mean = Transect_mean_NASC * Stratum_2_weight ,
            Stratum_2_deviation = ( Transect_mean_NASC - Stratum_2_mean ) ^ 2 ,
            Stratum_2_MSE = Stratum_2_weight ^ 2 * Stratum_2_deviation ) %>%
    group_by( Date , Survey , Frequency ) %>%
    reframe( Survey_effort = sum( Transect_effort ) ,
             Survey_mean_NASC = mean( Stratum_2_mean ) ,
             Survey_variance_NASC = sum( Stratum_2_MSE ) / ( n( ) * ( n( ) - 1 ) ) ,
             N = n( ) ) %>%
    mutate( Survey_CI = qnorm( ci_pct ) * sqrt( Survey_variance_NASC ) / sqrt( N ) ) %>%
    # Stratum 3
    group_by( Frequency ) %>%
    mutate( Transect_frequency = mean( Survey_effort ) ,
            Frequency_mean_NASC = mean( Survey_mean_NASC ) ) %>%
    ungroup( ) %>%
    mutate( Stratum_3_weight = Survey_effort / Transect_frequency ,
            Stratum_3_mean = Survey_mean_NASC * Stratum_3_weight ,
            Stratum_3_deviation = ( Survey_mean_NASC - Stratum_3_mean ) ^ 2 ,
            Stratum_3_MSE = Stratum_3_weight ^ 2 * Stratum_3_deviation ) %>%
    group_by( Frequency ) %>%
    mutate( Variance_frequency = sum( Stratum_3_MSE ) / ( n( ) * ( n( ) - 1 ) ) ,
            Frequency_mean_NASC = mean( Stratum_3_mean ) ,
            N_frequency = n( ) ) %>%
    ungroup( ) %>%
    mutate( Frequency_CI = qnorm( ci_pct ) * sqrt( Variance_frequency ) / sqrt( N_frequency ) ) %>%
    select( Survey , Date , Frequency , Survey_mean_NASC , Survey_CI ,
            Frequency_mean_NASC , Frequency_CI ) %>%
    return
}
