library(stringi)
library(tidyverse)
library(oce)

profile <- read.csv("C:/Users/Brandyn/OneDrive - Stony Brook University/AEON.ROMS.JOB_July2021.csv")
pressure_atm <- abs(profile$Depth) / 10.06 #approximate
pressure_dbar <- pressure_atm*1.01325*10 #approximate
sound_speed <- round(oce::swSoundSpeed(salinity=profile$Salinity,
                                       temperature=profile$Temperature,
                                       pressure=pressure_dbar,
                                       longitude=profile$Longitude[1],
                                       latitude=profile$Latitude[1]), 1)
depth <- abs(round(profile$Depth, 0))
depth_str <- paste0("   CTDDepthProfile = ", paste0(c(rev(depth)), collapse=" "), " # (meters)")
ss_str <- paste0("   SoundSpeedProfile = ", paste0(c(rev(sound_speed)), collapse=" "), " # (meters per second)")


ecs <- stringi::stri_read_lines("C:/Users/Brandyn/Downloads/testing.ecs")
ssp_index <- which(str_detect(ecs, "SoundSpeedProfile"))
ecs_new <- c(ecs[1:(ssp_index[1]-1)], depth_str, ss_str, ecs[(ssp_index[1]+1):length(ecs)])
date <- lubridate::ymd(as.POSIXct(profile$Date[1], 
                                  format="%Y-%m-%d %H:%M:%S"))
date_str <- gsub("-",".", date)
file_str <- paste("AEON", 
                  date_str, 
                  profile$Station[1], 
                  "Sound_speed_profile_EK80_calibration",
                  "1m_vertical_averaged",
                  "ecs",
                  sep=".")
dir_str <- "C:/Users/Brandyn/OneDrive - Stony Brook University/"
save_file <- paste0(dir_str, file_str)  

write.table(noquote(ecs_new), 
            file=save_file,
            row.names=F, 
            col.names=F,
            quote=F)