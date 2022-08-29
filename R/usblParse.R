############################
#USBL net tow data parser
#18 DEC 2020 BML
############################
############################
#Select directory with all USBL data files
#This comprises .PSONLDD file extensions
#This script parses all files in the directory line-by-line,
#so make sure directoy only contains USBL files
file_dir <- "H:/AR049.2020/net_tow.data/usbl_sensor.data/raw/"
#Write directory: where to save a *.csv file
file_out <- "H:/AR049.2020/net_tow.data/usbl_sensor.data/proc/AR049_usbl_IKMT_depth.csv"
#Output will contain the following fields:
#Datetime_UTC: Datetime (UTC)
#Latitude: Latitude (decimal degrees N)
#Longitude: Longitude (decimal degrees E)
#Depth: USBL range/depth (m)
#F2, F3, and F5 are placeholders for unspecified datafields -- to be updated
#ID: ID of USBL deployment -- should be all 'IKMT 2009'
############################
#DO NOT EDIT BELOW THIS LINE
############################
#This will require the stringi and lubridate packages
library(stringi)
library(lubridate)
#Parse out files
file_list <- list.files(file_dir, full.names=T, pattern="*.PSONLDD")
##########################
#Concatenate all of the files
#Files should be .PSONLDD file extensions
#These files all use comma delimiters for data fields,
#but space delimiters for the metadata fields
#First we build the parsing function
usblParse <- function(file){
  #Read files line-by-line
  data_stream <- stringi::stri_read_lines(file)
  #Break up by delimiters
  date_delim <- strsplit(data_stream, ",")
  #Now pull apart the data strings from within each block of data
  data_parser <- function(x){
    #Field length check
    l <- length(x)
    datetime <- strsplit(x[1], " ")[[1]][c(2,3)]
    data <- data.frame(Datetime=lubridate::ymd_hms(paste(datetime[1],datetime[2])),
                       Latitude=as.numeric(x[5]), Longitude=as.numeric(x[6]), 
                       Depth=as.numeric(x[7]), F2=as.numeric(x[8]), 
                       F3=as.numeric(x[9]), 
                       F5=as.numeric(x[11]), ID=x[3])
    return(data)
  }
  #Iterate through all files
  data_parsed <- lapply(date_delim, data_parser)
  data_df <- do.call(rbind.data.frame, data_parsed)
  data_df <- data_df[data_df$ID == "IKMT 2009",]
  #Output
  return(data_df)
  print(paste(file))
}
#Now we parse all files; this will print everything as a single dataframe for IKMT values
usbl_data <- do.call(rbind.data.frame, lapply(file_list, usblParse))
#Write file
write.csv(usbl_data, file_out)
############################
