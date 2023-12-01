################################################################################
# PRIMARY READING FUNCTIONS
################################################################################
# ==============================================================================
# Primary export file reading function
# ==============================================================================
#' Read in backscatter export file
#'
#' @param file Export file (.csv).
#' @param write_file Write file. (Default: `F`)
#' @param save_directory File path where processed files will be saved.
#'
#' @importFrom stringr str_extract str_detect str_split
#' @importFrom readr read_csv cols
#' @importFrom dplyr mutate group_by ungroup rename arrange filter reframe select between
#' @importFrom tidyr unnest
#' @importFrom purrr as_vector
#' @importFrom lubridate ymd_hms
#' @importFrom magrittr %>%
#' @rdname read.export
#' @export
read.export <- function( file ,
                         write_file = F ,
                         save_directory ) {
  # ----------------------------------------------------------------------------
  # Parse metadata from filename
  # ----------------------------------------------------------------------------
  # Survey name
  survey_name <- gsub( "NYOS" ,
                       "" ,
                       str_extract( file ,
                                    pattern = "([0-9]+)+" ) )
  # ----------------------------------------------------------------------------
  # Frequency name
  frequency_name <- gsub( "khz" ,
                          "kHz" ,
                          str_extract( tolower( file ) ,
                                       pattern = "([0-9]+)*khz" ) )
  # ----------------------------------------------------------------------------
  # Line name
  line_name <- str_extract( file ,
                            pattern = "Line+([0-9]+)" )
  # ----------------------------------------------------------------------------
  # Bandwidth name
  bandwidth_name <- str_extract( tolower( file ) ,
                                 pattern = "([a-z]+)*band" )
  # ----------------------------------------------------------------------------
  # Read in file
  # ----------------------------------------------------------------------------
  # Data import
  data <- read_csv( file ,
                    col_types = cols( ) ,
                    progress = F )
  # ----------------------------------------------------------------------------
  # Remove missing Interval labels
  data %>%
    filter( ! is.na( Interval ) ) -> data
  # ----------------------------------------------------------------------------
  # Parse file column names
  # ----------------------------------------------------------------------------
  # Column names
  column_names <- colnames( data )
  # ----------------------------------------------------------------------------
  # Column name specification
  # ----------------------------------------------------------------------------
  # Longitude
  longitude_detect <- str_detect( column_names ,
                                  pattern = "Lon" )
  # Replace
  if ( sum( longitude_detect ) > 1 ) {
    data %>%
      group_by( Interval ) %>%
      mutate( Longitude = median( c( Lon_S , Lon_E ) ) ) %>%
      ungroup( ) -> data
  } else if ( sum( longitude_detect ) == 1 ) {
    if ( ! ( "Longitude" %in% column_names ) ) {
      data %>%
        rename( "Longitude" = "Lon_M" ) -> data
    }
  } else {
    stop( paste0( "Invalid Longitude -- At least requires 'Lon_M' or 'Lon_S' and 'Lon_E'" ) )
  }
  # ----------------------------------------------------------------------------
  # Latitude
  latitude_detect <- str_detect( column_names ,
                                 pattern = "Lat" )
  # Replace
  if ( sum( latitude_detect ) > 1 ) {
    data %>%
      group_by( Interval ) %>%
      mutate( Latitude = median( c( Lat_S , Lat_E ) ) ) %>%
      ungroup( ) -> data
  } else if ( sum( latitude_detect ) == 1 ) {
    if ( ! ( "Latitude" %in% column_names ) ) {
      data %>%
        rename( "Latitude" = "Lat_M" ) -> data
    }
  } else if ( ! ( "Latitude" %in% column_names ) ) {
    stop( paste0( "Invalid Latitude -- At least requires 'Lat_M' or 'Lat_S' and 'Lat_E'" ) )
  }
  # ----------------------------------------------------------------------------
  # Date
  date_detect <- str_detect( column_names ,
                             pattern = "Date" )
  # Replace
  if ( sum( date_detect ) > 1 ) {
    data %>%
      nest_by( Interval , Date_S , Date_E ) %>%
      group_by( Interval ) %>%
      mutate( Date = format( median( strptime( c( Date_S , Date_E ) ,
                                               "%Y%m%d" ) ) ,
                             "%Y%m%d" ) ) %>%
      ungroup( ) %>%
      unnest( c( data ) ) -> data
  } else if ( sum( date_detect ) == 1 ) {
    if ( ! ( "Datetime" %in% column_names ) ) {
      data %>%
        mutate( Date_M = ymd( Date_M ) ) %>%
        rename( "Date" = "Date_M" ) -> data
    }
  } else {
    stop( paste0( "Invalid Date -- At least requires 'Date_M' or 'Date_S' and 'Date_E'" ) )
  }
  # ----------------------------------------------------------------------------
  # Time
  time_detect <- str_detect( column_names ,
                             pattern = "Time" )
  # #Replace
  if ( sum( time_detect ) > 1 ) {
    data %>%
      nest_by( Interval , Time_S , Time_E ) %>%
      group_by( Interval ) %>%
      mutate( Time = format( median( strptime( c( Time_S , Time_E ) ,
                                               "%H:%M:%S" ) ) ,
                             "%H:%M:%S" ) ) %>%
      ungroup( ) %>%
      unnest( c( data ) ) -> data
  } else if ( sum( time_detect ) == 1 ) {
    data %>%
      rename( "Time" = "Time_M" ) -> data
  } else {
    if ( ! ( "Datetime" %in% column_names ) ) {
      stop( paste0( "Invalid Time -- At least requires 'Time_M' or 'Time_S' and 'Time_E'" ) )
    }
  }
  # ----------------------------------------------------------------------------
  # Standard deviation
  standard_deviation_detect <- str_detect( column_names ,
                                           pattern = "Standard_deviation" )
  # Replace
  if ( sum( standard_deviation_detect ) < 1 )  {
    data %>%
      mutate( sv_StDev = 0 ) -> data
  } else {
    data %>%
      rename( sv_StDev = Standard_deviation ) -> data
  }
  # ----------------------------------------------------------------------------
  # Layer
  layer_depth_detect <- str_detect( column_names ,
                                    pattern = "Layer_depth_" )
  # Replace
  if ( sum( layer_depth_detect ) < 1 ) {
    if ( sum( thickness_detect ) == 1 ) {
      layer_thickness <- max( data$Thickness_mean )
      data %>%
        mutate( Layer_depth_min = layer_thickness * ( Layer - 1 ) ,
                Layer_depth_max = layer_thickness * ( Layer ) ) -> data
    } else if ( sum( depth_detect ) == 1 ) {
      data %>%
        group_by( Interval ) %>%
        arrange( Layer ) %>%
        ungroup( ) %>%
        group_by( Interval ) %>%
        arrange( Layer ) %>%
        filter( between( Layer ,
                         min( Layer ) + 2 ,
                         max( Layer ) - 2 ) ) %>%
        reframe( Median_difference = diff( Depth_mean ) ) %>%
        reframe( Thickness = median( Median_difference ) ) %>%
        as_vector( ) -> layer_thickness

      data %>%
        mutate( Layer_depth_min = layer_thickness * ( Layer - 1 ) ,
                Layer_depth_max = layer_thickness * ( Layer ) ) -> data
    } else {
      stop( paste0( "Invalid depth interval -- 'Layer_depth_min'/'Layer_depth_max missing' -- At least requires 'Depth_mean' or 'Thickness_mean' " ) )
    }
  }
  # ----------------------------------------------------------------------------
  # Further refine dataframe
  # ----------------------------------------------------------------------------
  # Preen
  if ( ! ( "Datetime" %in% column_names ) ) {
    data %>%
      mutate( Survey = paste0( "NYOS" , survey_name ) ,
              Line = line_name ,
              Frequency = frequency_name ,
              Bandwidth = bandwidth_name ,
              Datetime = ymd_hms( paste( Date , Time ) ) ) %>%
      select( Survey , Line , Frequency , Bandwidth ,
              Interval , Layer ,
              Longitude , Latitude , Datetime ,
              Layer_depth_min , Layer_depth_max ,
              Sv_mean , sv_StDev , NASC ) %>%
      arrange( Interval , Layer ) -> output
  } else {
    data -> output
  }
  depth_detect <- str_detect( column_names ,
                              pattern = "Depth_mean" )
  # ----------------------------------------------------------------------------
  # Save file ?
  # ----------------------------------------------------------------------------
  if ( write_file ) {
    if ( is.na( save_directory ) | missing( save_directory ) ) {
      # Create save directory
      save_directory <- paste0( str_extract( file ,
                                             ".+/" ) ,
                                "exports_processed/" )
      # Evaluate if directory already exists
      if ( ! dir.exists( save_directory ) ) {
        # Create directory if missing
        dir.create( save_directory )
      }
    }
    # Write .csv file
    filename <- str_split( file , "/" )[[ 1 ]][ lengths( str_split( file , "/" ) ) ]
    gsub( pattern = "csv" ,
          replacement = "processed.csv" ,
          x = filename ) -> new_filename
    write.csv( output ,
               paste0( save_directory ,
                       new_filename ) ,
               row.names = F )
  }
  # ----------------------------------------------------------------------------
  # Return
  # ----------------------------------------------------------------------------
  return( output )
}
# ==============================================================================
# Batch reading function
# ==============================================================================
#' Batch read in backscatter export files.
#'
#' @param directory File path where files are saved.
#'
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @importFrom cli pb_bar pb_percent
#' @rdname batch.read
#' @export
batch.read <- function( directory ) {
  # ----------------------------------------------------------------------------
  # Map out, distribute, and concatenate files
  # ----------------------------------------------------------------------------
  # List files
  list.files( directory ,
              pattern = '.csv' ,
              full.names = T ) %>%
    # Map and concatenate files
    map_dfr( . ,
             ~ read.export( .x ,
                            write_file = F ) ,
             # Progress bar
             .progress = list(
               type = 'iterator' ,
               format = "Reading export files {cli::pb_bar} {cli::pb_percent}" ,
               clear = F
             ) ) %>%
    return
}
# ==============================================================================
# Batch processing/conversion function
# ==============================================================================
#' Batch read in backscatter export files.
#'
#' @param directory File path where files are saved.
#' @param return_object Return object (tibble). (Default: `T`)
#' @param write_file Write file. (Default: `F`)
#' @param save_directory File path where processed files will be saved.
#'
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @importFrom cli pb_bar pb_percent
#' @rdname batch.process
#' @export
batch.process <- function( directory ,
                           return_object = T ,
                           write_file = F ,
                           save_directory ) {
  # ----------------------------------------------------------------------------
  # Define progress bar text output
  # ----------------------------------------------------------------------------
  # Progress bar syntax
  syntax <- ifelse( write_file ,
                    "Reading and re-writing export files {cli::pb_bar} {cli::pb_percent}" ,
                    "Reading export files {cli::pb_bar} {cli::pb_percent}" )
  # ----------------------------------------------------------------------------
  # Map out, distribute, and concatenate files
  # ----------------------------------------------------------------------------
  save_directory <- ifelse( missing( save_directory ) ,
                            NA ,
                            save_directory )
  # List files
  list.files( directory ,
              pattern = '.csv' ,
              full.names = T ) %>%
    # Map and concatenate files
    map_dfr( . ,
             ~ read.export( .x ,
                            write_file = write_file ,
                            save_directory = save_directory ) ,
             # Progress bar
             .progress = list(
               type = 'iterator' ,
               format = syntax ,
               clear = F
             ) ) -> output
  # ----------------------------------------------------------------------------
  # Return object, if defined
  # ----------------------------------------------------------------------------
  if ( return_object ) {
    return( output )
  }
}
