################################################################################
# DIAGNOSTICS
################################################################################
# ==============================================================================
# Alert generation function
# ==============================================================================
#' Alert message generation function
#'
#' @param text Generated text output.
#' @param .envir Environment to evaluate the glue expressions in.
alert_generate <- function( text ,
                            .envir = parent.frame( ) ) {
  # ----------------------------------------------------------------------------
  # Create alert object
  # ----------------------------------------------------------------------------
  # CLI alert status message function
  cli::cli_alert( text ,
                  wrap = T ,
                  .envir = .envir )
  # Produce CLI container
  cli::cli_div( theme = list(
    .alert = list( `margin-left` = 2 ,
                   before = "" ) )
  )
  # ----------------------------------------------------------------------------
  # Generate message and print out in console
  lapply( text[ - 1 ] ,
          cli::cli_alert ,
          wrap = T ,
          .envir = .envir )
  cli::cli_end( )
}
# ==============================================================================
# Check for any issues with data export
# ==============================================================================
#' Read in backscatter export file
#'
#' @param data Export tibble.
#'
#' @importFrom cli cli_alert_danger cli_alert_success symbol
#' @rdname sanity
#' @export
sanity <- function( data ) {
  # ----------------------------------------------------------------------------
  # Check for issues with major variables
  # ----------------------------------------------------------------------------
  # Longitude
  if ( any( is.na( data$Longitude ) ) ) {
    flag_length <- length( which( is.na( data$Longitude ) ) )
    longitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                            "\n" ,
                            "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = NA!')" )
  } else if ( any( data$Longitude == -999 ) ) {
    flag_length <- length( which( data$Longitude == -999 ) )
    longitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                            "\n" ,
                            "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = -999!')" )
  } else if ( any( data$Longitude == 999 ) ) {
    flag_length <- paste0( length( which( data$Longitude == 999 ) ) )
    longitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                            "\n" ,
                            "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = 999!')" )
  } else {
    longitude_flag <- "cli::cli_alert_success('No issues with Longitude detected.')"
  }
  # ----------------------------------------------------------------------------
  # Latitude
  if ( any( is.na( data$Latitude ) ) ) {
    flag_length <- length( which( is.na( data$Latitude ) ) )
    latitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                           "\n" ,
                           "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = NA!')" )
  } else if ( any( data$Latitude == -999 ) ) {
    flag_length <- length( which( data$Latitude == -999 ) )
    latitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                           "\n" ,
                           "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = -999!')" )
  } else if ( any( data$Latitude == 999 ) ) {
    flag_length <- paste0( length( which( data$Latitude == 999 ) ) )
    latitude_flag <- list( "cli::cli_alert_danger('Issues with Latitude detected.')" ,
                           "\n" ,
                           "alert_generate('{cli::symbol$fancy_question_mark} {.field {flag_length}} values = 999!')" )
  } else {
    latitude_flag <- "cli::cli_alert_success('No issues with Latitude detected.')"
  }
  # ----------------------------------------------------------------------------
  # Datetime
  datetime_flag <- ifelse( any( is.na( data$Datetime ) ) ,
                           "cli::cli_alert_danger('Issues with Datetime detected.')" ,
                           "cli::cli_alert_success('No issues with Datetime detected.')" )
  # Check for Sv_mean values ---------------------------------------------------
  if ( any( is.na( data$Sv_mean ) ) | any( data$Sv_mean > 0 ) ) {
    sv_mean_flag <- list( "cli::cli_alert_danger('Issues with Sv_mean detected.')" )
  } else if ( any( data$Sv_mean > -20 ) ) {
    sv_mean_flag <- list( "alert_generate('{cli::symbol$warning} Possible issues with Sv_mean warrants further attention.')" ,
                          "\n" ,
                          "alert_generate('{cli::symbol$fancy_question_mark} Sv_mean values greater than -20 dB detected!')" )
  } else {
    sv_mean_flag <- "cli::cli_alert_success('No issues with Sv_mean detected.')"
  }
  # ----------------------------------------------------------------------------
  # Depth
  depth_flag <- ifelse( any( is.na( data$Layer_depth_max ) |
                               is.na( data$Layer_depth_min ) ) ,
                        "cli::cli_alert_danger('Issues with Depth Layers detected.')" ,
                        "cli::cli_alert_success('No issues with Depth Layers detected.')" )
  # Check for NASC values ------------------------------------------------------
  if ( any( is.na( data$NASC ) ) | any( data$NASC < 0 ) ) {
    nasc_flag <- list( "cli::cli_alert_danger('Issues with NASC detected.')" )
  } else if ( any( data$NASC > 1e6 ) ) {
    nasc_flag <- list( "alert_generate('{cli::symbol$warning} Possible issues with NASC warrants further attention.')" ,
                       "\n" ,
                       "alert_generate('{cli::symbol$fancy_question_mark} NASC values greater than 1e6 m^2 nmi^-2 detected!')" )
  } else {
    nasc_flag <- "cli::cli_alert_success('No issues with NASC detected.')"
  }
  # ----------------------------------------------------------------------------
  # Print / evaluate text output
  # ----------------------------------------------------------------------------
  # Print sanity check
  eval(
    parse( text = c( longitude_flag , '\n' ,
                     latitude_flag , '\n' ,
                     datetime_flag , '\n' ,
                     depth_flag , '\n' ,
                     sv_mean_flag , '\n' ,
                     nasc_flag ) )
  )
}
# ==============================================================================
# Summarize data tibble
# ==============================================================================
#' Read in backscatter export file
#'
#' @param data Export tibble.
#'
#' @importFrom cli cli_alert_danger cli_alert_success symbol
#' @rdname peek
#' @export
peek <- function( data ) {
  # ----------------------------------------------------------------------------
  # Summarize variables and tibble metadata
  # ----------------------------------------------------------------------------
  # Summarize number of surveys
  len_surveys <- length( unique( data$Survey ) )
  # Summarize number of frequencies
  len_frequencies <- length( unique( data$Frequency ) )
  name_frequencies <- paste0( paste0( gsub( pattern = "kHz" ,
                                            replacement = "" ,
                                            x = unique( data$Frequency ) )[ order( as.numeric( gsub( pattern = "kHz" ,
                                                                                                              replacement = "" ,
                                                                                                              x = unique( data$Frequency ) ) ) ) ] ,
                                      collapse = '/' ) ,
                              " kHz" )
  # Summarize depth range
  depth_min <- min( data$Layer_depth_min )
  depth_max <- max( data$Layer_depth_max )
  # Summarize Sv_mean range
  Sv_mean_min <- round( min( data$Sv_mean[ data$Sv_mean != - 999 ] ) )
  Sv_mean_max <- round( max( data$Sv_mean ) )
  # Summarize NASC range
  NASC_min <- round( min( data$NASC ) )
  NASC_max <- round( max( data$NASC ) )
  # Summarize date range
  len_days <- length( unique( as.Date( data$Datetime ) ) )
  dateMin <- min( as.Date( data$Datetime ) )
  dateMax <- max( as.Date( data$Datetime ) )
  # ----------------------------------------------------------------------------
  # Print out object summary into console
  # ----------------------------------------------------------------------------
  # Text print
  cat(
    ' Number of surveys:' , paste0( len_surveys ) , '\n' ,
    '    Date range:' , paste0( dateMin , " to " , dateMax ) , '\n' ,
    '    Survey days:' , paste0( len_days ) , 'days\n' ,
    ' Depth range:' , paste0( '[' , depth_min , ', ' , depth_max , ']' ) , 'm\n' ,
    ' Number of frequencies:' , paste0( len_frequencies ) , '\n' ,
    '    Unique frequencies:' , paste0( name_frequencies ) , '\n' ,
    ' Sv range:' , paste0( '[' , Sv_mean_min , ', ' , Sv_mean_max , ']' ) , 'dB\n' ,
    ' NASC range:' , paste0( '[' , NASC_min , ', ' , NASC_max , ']' ) , 'm^2 nmi^-2'
  )
}
