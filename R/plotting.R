################################################################################
# PRIMARY AND UTILITY PLOTTING FUNCTIONS
################################################################################
# ==============================================================================
# Color palettes
# ==============================================================================
#' Seasonal color palette
#'
#' @param starting_season The first season that will be plotted (left-to-right)
#' along x-axis of plot.
#' @rdname stratified_palette
#' @export
stratified_palette <- function( starting_season ) {
  # ----------------------------------------------------------------------------
  # Default color order
  season_colors <- c( "#77bc65" , # spring
                      "#ffff6d" , # summer
                      "#ff972f" , # fall
                      "#729fcf" ) # winter
  # Default season order
  season_order <- c( "Spring" ,
                     "Winter" ,
                     "Summer" ,
                     "Fall" )
  # ----------------------------------------------------------------------------
  # Reorder seasons based on the starting season input
  season_index <- which( season_order == starting_season )
  # ----------------------------------------------------------------------------
  # Redefine color order
  reorder_colors <- c( season_colors[ season_index : 4 ] ,
                       season_colors[ 1 : ( season_index - 1 ) ] )[ 1 : 4 ]
  # ----------------------------------------------------------------------------
  # Generate colors
  return( reorder_colors )
}
# ==============================================================================
# Stratified mean NASC plot
# ==============================================================================
#' Stratified mean NASC plot
#'
#' @param data Export tibble.
#' @param frequency Numeric frequencies (kHz) that will be plotted.
#' @param month_pad Number of months to pad along the x-axis.
#' @param season_definition Season definition (meteorological vs astronomical).
#' @param point.size Plot point size.
#' @param line.width Plot line width.
#' @param text.main.size Main text size.
#' @param text.axis.size Axis text size.
#' @param text.legend.size Legend text size.
#' @param text.label.size Label text size.
#' @param background_alpha Season color transparency.
#' @param common_axis Fixed versus facetted scales.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by reframe mutate left_join ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom ggnewscale new_scale_color new_scale_fill
#' @importFrom ggh4x facetted_pos_scales
#' @import ggplot2
#' @rdname plot.nasc
#' @export
plot.nasc <- function( data ,
                       frequency = c( 38 , 70 , 120 , 200 ) ,
                       month_pad = 2 ,
                       season_definition = "meteorological" ,
                       point.size = 3.25 ,
                       line.width = 2.00 ,
                       text.main.size = 15.25 ,
                       text.axis.size = 14.25 ,
                       text.legend.size = 14.50 ,
                       text.label.size = 5.5 ,
                       background_alpha = 0.40 ,
                       common_axis = F ) {
  # ----------------------------------------------------------------------------
  # Initial data processing
  # ----------------------------------------------------------------------------
  data %>%
    filter( Frequency %in% paste0( frequency , "kHz" ) ) %>%
    mutate( Frequency = factor( Frequency ,
                                levels = c( "38kHz" ,
                                            "70kHz" ,
                                            "120kHz" ,
                                            "200kHz" ) ) ) -> input_data
  # ----------------------------------------------------------------------------
  # Seasonal background coloration
  # ----------------------------------------------------------------------------
  # Calculate seasonal interval
  seasons <- suppressWarnings( seasonal_interval( data ,
                                                  month_pad = month_pad ,
                                                  season_definition = season_definition ) %>%
                                 mutate( Season = fct_inorder( Season ) ) )
  # Colors
  season_colors <- stratified_palette( seasons$Season[ 1 ] )
  # ----------------------------------------------------------------------------
  # Axis limits and breaks
  # ----------------------------------------------------------------------------
  # y-axis breaks
  lapply(
    1 : nrow( frequency_limits ) ,
    FUN = function( x ) {
      paste( pretty( x = seq( from = 0 ,
                              to = frequency_limits$Max[ x ] ,
                              by = 100 ) ,
                     n = 4 ,
                     min.n = 4 ) ,
             collapse = "," )
    }
  ) -> frequency_breaks
  # y-axis limits
  input_data %>%
    mutate( Survey_CI = if_else( is.na( Survey_CI ) ,
                                 0 ,
                                 Survey_CI ) ) %>%
    group_by( Frequency ) %>%
    reframe( Max = max( Survey_mean_NASC + Survey_CI ) * 1.10 ) -> frequency_limits
  # combine to create parseable text for the y-axis
  lapply(
    1 : nrow( frequency_limits ) ,
    FUN = function( x ) {
      eval(
        parse(
          text = paste0(
            "scale_y_continuous( limits = c( - 10 , " ,
            round( frequency_limits$Max[ x ] ) , " ) , " ,
            "breaks = c( " ,
            frequency_breaks[ x ][[ 1 ]] , " ) , " ,
            "labels = c( " ,
            frequency_breaks[ x ][[ 1 ]] , " ) ) "
          )
        )
      )
    }
  ) -> facetted_y_axes
  # ----------------------------------------------------------------------------
  # Axis limits and breaks
  # ----------------------------------------------------------------------------
  # Plot
  input_data %>%
    ggplot( ) +
    geom_rect(
      mapping = aes( xmin = Date_start ,
                     xmax = Date_end + days( 1 ) ,
                     ymin = - Inf ,
                     ymax = Inf ,
                     fill = Season ) ,
      data = season_interval %>%
        mutate( Season = fct_inorder( Season ) ) ,
      alpha = background_alpha
    ) +
    scale_x_date( ) +
    scale_fill_manual(
      name = NULL ,
      values = season_colors ,
      guide = guide_legend(
        nrow = 2 ,
        order = 1
      )
    ) +
    new_scale_fill( ) +
    geom_hline(
      mapping = aes( yintercept = Mean ,
                     color = "A" ) ,
      data = . %>%
        group_by( Frequency ) %>%
        reframe( Mean = mean( Frequency_mean_NASC ) ,
                 LCI = mean( Frequency_mean_NASC ) - mean( Frequency_CI ) ,
                 UCI = mean( Frequency_mean_NASC ) + mean( Frequency_CI ) )
    ) +
    geom_ribbon(
      mapping = aes( x = Date_start ,
                     ymin = LCI ,
                     ymax = UCI ,
                     color = "A" ) ,
      data = season_interval %>%
        reframe( Date_start = rep( x = season_interval$Date_start ,
                                   times = 4 ) ,
                 Date_end = rep( x = season_interval$Date_end ,
                                 times = 4 ) ,
                 Frequency = rep( x = paste0( frequency , "kHz" ) ,
                                  each = nrow( season_interval ) ) ) %>%
        left_join( data %>%
                     filter( Frequency %in% paste0( frequency , "kHz" ) ) %>%
                     group_by( Frequency ) %>%
                     reframe( Mean = mean( Frequency_mean_NASC ) ,
                              LCI = mean( Frequency_mean_NASC ) - mean( Frequency_CI ) ,
                              UCI = mean( Frequency_mean_NASC ) + mean( Frequency_CI ) ) ,
                   by = "Frequency" ) %>%
        group_by( Frequency ) %>%
        mutate( N = 1 : n( ) ,
                Date_start = if_else( N == 1 ,
                                      Date_start - months( month_pad * 2 ) ,
                                      if_else( N == max( N ) ,
                                               Date_start + months( month_pad * 2 ) ,
                                               Date_start ) ) ) %>%
        ungroup( ) %>%
        mutate( Frequency = factor( Frequency ,
                                    levels = c( "38kHz" ,
                                                "70kHz" ,
                                                "120kHz" ,
                                                "200kHz" ) ) ) ,
      fill = NA ,
      linetype = 2
    ) +
    scale_color_manual(
      name = "Frequency-specific" ,
      labels = expression( "Mean"%+-%"CI") ,
      values = "gray50" ,
      guide = guide_legend(
        title.position = "top" ,
        order = 3
      )
    ) +
    new_scale_color( ) +
    geom_linerange(
      mapping = aes( x = Date ,
                     ymin = Survey_mean_NASC - Survey_CI ,
                     ymax = Survey_mean_NASC + Survey_CI ,
                     color = "B" ) ,
      linewidth = line.width
    ) +
    geom_point(
      mapping = aes( x = Date ,
                     y = Survey_mean_NASC ,
                     color = "B" ) ,
      size = point.size ,
      shape = 1 ,
      stroke = 1.25
    ) +
    scale_color_manual(
      name = "Survey-specific" ,
      labels = expression( "Mean"%+-%"CI") ,
      values = "black" ,
      guide = guide_legend(
        title.position = "top" ,
        order = 2
      )
    ) +
    coord_cartesian(
      xlim = c( min( seasons$Date_start ) ,
                max( seasons$Date_end ) ) ,
      expand = 0
    ) +
    facet_wrap( ~ Frequency ,
                scales = ifelse( common_axis ,
                                 "fixed" ,
                                 "free_y" ) ) +
    facetted_pos_scales(
      y = facetted_y_axes
    ) +
    geom_label(
      mapping = aes( x = x ,
                     y = Inf ,
                     label = lab ) ,
      data = tibble( Frequency = factor( paste0( frequency , "kHz" ) ,
                                         levels = c( "38kHz" ,
                                                     "70kHz" ,
                                                     "120kHz" ,
                                                     "200kHz" ) ) ) %>%
        mutate( x = min( seasons$Date_start ) ,
                lab = gsub( pattern = "kHz" ,
                            replacement = " kHz" ,
                            x = Frequency ) ) ,
      size = text.label.size ,
      hjust = 0 ,
      vjust = 1 ,
      fontface = "bold" ) +
    theme_bw( ) +
    theme(
      text = element_text( size = text.main.size ,
                           color = 'black' ) ,
      axis.text = element_text( size = text.axis.size ,
                                color = 'black' ) ,
      legend.text = element_text( size = text.legend.size ,
                                  color = 'black' ) ,
      panel.grid = element_blank( ) ,
      strip.background = element_blank( ) ,
      strip.text = element_blank( ) ,
      legend.position = "top" ,
      legend.direction = "horizontal" ,
      legend.justification = "left"
    ) +
    labs( x = NULL ,
          y = expression( italic('NASC')~('m'^2~'nmi'^-2) ) ) -> nasc_plot
  # ----------------------------------------------------------------------------
  # Return
  # ----------------------------------------------------------------------------
  suppressWarnings( print( nasc_plot ) )
}
