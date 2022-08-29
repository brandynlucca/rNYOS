#Function for generating *.evl files for TV80 and Star-ODDI TD sensor data
evlWrite <- function(file, file.out=NULL, type, tz="GMT", ref_lat=40){
  require(lubridate)
  require(stringr)
  filestr <- unlist(str_split(file,c("/")))
  
  if(tolower(type) == "tv"){
    file_check <- grepl("\\.csv$", file)
    
    if(isFALSE(file_check)){
      stop("Simrad sensor data exported from the TV80 software is expected to be a *.csv. Check to make sure file input is correct, or 
           change `type` argument from type='df' to type='td'.")
    }else{
      df <- read.csv(file, header=T, sep=";")
      colnames(df) <- c("Date","Time","Battery","Pitchstb","Height","Depth","Roll",
                          "Spread","Pitchprt","Temp","Pitch","Rollprt","Depthprt","Tempstb")
      dtstr <- unlist(str_split(filestr[length(filestr)],"_"))
      timeref <-  length(unlist(str_split(dtstr[2],"")))
      dateref <- unlist(str_split(dtstr[1],""))
      
      for(i in 1:nrow(df)){
        tstr <- unlist(str_split(df$Time[i],""))
        if(length(tstr) < timeref){
          DT <- paste(df$Date[i]," ", as.numeric(tstr[1])+12,":",tstr[2],tstr[3],":",tstr[4],tstr[5], sep="")
          DT <- ymd_hms(DT, tz=tz)
          
          if(tz != "GMT" | tz != "UTC"){
            DT <- with_tz(DT, tz="GMT")
          }
          
          df$Date[i] <- strftime(DT, format=paste(dateref[1],dateref[2],"%y%m%d",sep=""))
          df$Time[i] <- strftime(DT, format="%H%M%S0000")
        }
      }
    }
  }else if(tolower(type) == "td"){
    file_check <- grepl("\\.xlsx$", file)
    
    if(isFALSE(file_check)){
      file_check <- grep("\\.csv$", file)
      format <- "csv"
    }else{
      format <- "xlsx"
    }
    
    if(isFALSE(file_check)){
      stop("Star-ODDI sensor data exported from the SeaStar software is expected to be a *.xlsx. Check to make sure file input is correct,
      or change `type` argument from type='td' to type='tv'.")
    }else{
      require(oce)
      if(format == "xlsx"){
        require(openxlsx)
        df <- read.xlsx(file, sheet="DAT", startRow=1)
        colnames(df) <- c("Datetime","Temperature","Pressure")
        df$Datetime <- as.POSIXct(df$Datetime * (60*60*24), origin="1899-12-30", tz=tz)
        
        if(!xor(tz != "UTC", tz != "GMT")){
          df$Datetime <- with_tz(df$Datetime, tz="GMT")
          message(paste("Converting datetime time zone from ", tz, "to GMT/UTC."))
        }
      }else{
        df <- read.csv(file, header=T)
        df$Datetime <- as.POSIXct(df$Datetime, origin="1899-12-30", tz=tz)
        
        if(!xor(tz != "UTC", tz != "GMT")){
          df$Datetime <- with_tz(df$Datetime, tz="GMT")
          message(paste("Converting datetime time zone from ", tz, "to GMT/UTC."))
        }
      }
    }
      
      #Calculate depths 
      df$Depth <- swDepth(df$Pressure*10, ref_lat)
      df$Date <- strftime(df$Datetime, format=paste(year(df$Datetime),"%m%d",sep=""), tz=tz)
      df$Time <- strftime(df$Datetime, format="%H%M%S0000", tz=tz)
    }
    
  df_out <- data.frame(dates=df$Date, 
                       times=df$Time, 
                       dum1=rep("",nrow(df)), 
                       depth=round(df$Depth,1), 
                       dum2=rep(3,nrow(df)))
  
  if(length(file.out) == 0){
    dest <- paste(filestr[1:(length(filestr)-1)], collapse="/")
    if(type == "tv"){
      ext <- unlist(str_split(filestr[length(filestr)],"[.]"))[1]
    }else if(type == "td"){
      str_par <- unlist(str_split(filestr[length(filestr)],"[.]"))
      ext <- paste(str_par[1:(length(str_par)-2)],collapse="_")
    }
    file.out <- paste(dest,"/",ext,"_",type,"_proc.evl",sep="")
  }
  
  if(isFALSE(grepl("\\.evl$", file.out))){
    stop("File output extension must be *.evl. Unable to write *.evl file to selected output directory.")
  }else{
    message("The *.evl file was successfully initialized.")
    message(paste0("Directory: ", dest))
    message(paste0("Filename: ", ext))
    message("File now being reformatted to the correct datatable format for Echoview.")
  }
  
  fileConn <- file(file.out, open="wt")
  header <- paste("EVBD 3 10.0.293.38183 \n",nrow(df_out),sep="")
  writeLines(header, fileConn)
  write.table(df_out, file=fileConn, append=T, row.names=F, col.names=F, quote=F)
  close(fileConn)
  message(paste0(file.out, " successfully written and saved!"))
}
