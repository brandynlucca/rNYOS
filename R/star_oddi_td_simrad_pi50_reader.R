#Non-Java .xlsx reader 
library(openxlsx)

#CSIRO seawater algorithms; based on TEOS-10
library(oce)
library(viridis)

#
# filestring <- "C:/Users/ales/Downloads/NYOS2002.T01.FR.2020.02.09.1903.td.xlsx"
# filestring <- "C:\\Users\\Brandyn\\Downloads\\D109\\NY2207.T02.2022.07.29.0536.data\\NY2207.T02.2022.07.29.0536.td.xlsx"
filestring <- "C:\\Users\\Brandyn\\Downloads\\D109\\NY2207.T03.2022.07.29.2351.data\\NY2207.T03.2022.07.29.2351.td.xlsx"
foot <- openxlsx::read.xlsx(filestring, sheet="DAT")
colnames(foot) <- c("Datetime","Temperature","Pressure")

# filestring <- "C:/Users/ales/Downloads/NYOS2002.T01.HR.2020.02.09.1903.td.xlsx"
# filestring <- "C:\\Users\\Brandyn\\Downloads\\D108\\NY2207.T02.2022.07.29.0536.data\\NY2207.T02.2022.07.29.0536.td.xlsx"
filestring <- "C:\\Users\\Brandyn\\Downloads\\D108\\NY2207.T03.2022.07.29.2351.data\\NY2207.T03.2022.07.29.2351.td.xlsx"

head <- openxlsx::read.xlsx(filestring, sheet="DAT")
colnames(head) <- c("Datetime","Temperature","Pressure")

#
foot$Depth <- swDepth(foot$Pressure*10,40.11403)
head$Depth <- swDepth(head$Pressure*10,40.11403)

#
foot$Datetime <- as.POSIXct(foot$Datetime * (60*60*24), origin="1899-12-30",tz="GMT")
head$Datetime <- as.POSIXct(head$Datetime * (60*60*24), origin="1899-12-30",tz="GMT")


#
ggplot() +
  geom_point(data=foot, aes(x=Datetime,y=Depth,colour=Temperature), size=2) +
  geom_point(data=head, aes(x=Datetime,y=Depth,colour=Temperature), size=2) +
  scale_color_viridis(breaks=c(10,15,20,25,30),
                      # limits=c(10,30),
                      option="plasma",
                      oob=scales::squish) + 
  theme_bw() +
  theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
        panel.grid=element_blank(),plot.title=element_text(hjust=0.5)) +
  labs(x="Datetime (GMT)",
       y="Depth (m)",
       colour=expression(paste("Temperature (",degree,"C)",sep="")),
       title=paste(" (Max depth = ",round(max(foot$Depth),0),"m)",sep="")) +
  scale_y_reverse()
  
#TV80
require(lubridate)
require(stringr)
# tv80 <- read.csv("C:/Users/Brandyn/Downloads/20200209_185958_measurements.csv", sep=";")
# tv80 <- read.csv("C:/Users/Brandyn/Downloads/Simrad.TV80/20220729_014549_measurements.csv", sep=";")
tv80 <- read.csv("C:/Users/Brandyn/Downloads/Simrad.TV80/20220729_195459_measurements.csv", sep=";")
colnames(tv80) <- c("Date", "Time", "HRHeight", "Temperature", "HRBattery",
                    "HRDepth", "STBTemperature", "STBBattery", "STBPitch",
                    "STBRoll", "SpreadDoor", "PRTPitch", "PRTRoll", "PRTDepth")

tv80$Date <- ymd(tv80$Date)
tv80$Datetime <- NA
offset <- -12
pm <- T
for(i in 1:nrow(tv80)){
  filestr <- unlist(str_split(tv80$Time[i],""))
  hours <- as.numeric(filestr[1])
  
  if(pm == TRUE){
    hours <- hours + 12
  }
  datestr <- tv80$Date[i]
  timestr <- as.POSIXct(paste0(datestr, " ", hours,":",filestr[2],filestr[3],":",filestr[4],filestr[5]),
                        format="%Y-%m-%d %H:%M:%S",
                        tz="America/New_York")
  timestr <- format(timestr,
                    format="%Y-%m-%d %H:%M:%S",
                    tz="GMT")
  tv80$Datetime[i] <- timestr
}



# tv80$Datetime <- paste(tv80$Date,tv80$Time,sep=" ")
# tv80$Datetime <- as.POSIXct(tv80$Datetime, tz="GMT") + (24*60*60)
# tv80$Datetime <- as.POSIXct(tv80$Datetime, tz="GMT")

tv80$Datetime <- as.POSIXct(tv80$Datetime,
                            format="%Y-%m-%d %H:%M:%S",
                            tz="GMT")
mrg <- merge(foot, head, by="Datetime")
mrg$Diff <- mrg$Depth.x - mrg$Depth.y
max(mrg$Diff)
#
ggplot() +
  geom_point(data=foot, aes(x=Datetime,y=Depth,fill=Temperature), size=2, pch=21) +
  geom_point(data=head, aes(x=Datetime,y=Depth,fill=Temperature), size=2, pch=21) + 
  geom_path(data=tv80, aes(x=Datetime,y=HRDepth, color="A"), size=2) +
  scale_fill_viridis(breaks=c(10,15,20),option="plasma") + 
  scale_x_datetime(limits=c(as.POSIXct("2022-07-29 05:38:42", tz="GMT",
                                       format="%Y-%m-%d %H:%M:%S"),
                            as.POSIXct("2022-07-29 06:20:42", tz="GMT",
                                       format="%Y-%m-%d %H:%M:%S"))) +
  # scale_x_datetime(limits=c(as.POSIXct("2022-07-29 23:51:00", tz="GMT",
  #                                      format="%Y-%m-%d %H:%M:%S"),
  #                           as.POSIXct("2022-07-30 00:30:42", tz="GMT",
  #                                      format="%Y-%m-%d %H:%M:%S")),
  #                  labels=scales::date_format("%Y-%m-%d %H:%M:%S"),
  #                  breaks=scales::date_breaks("15 mins")) +
  geom_text(aes(x=min(foot$Datetime),
                y=Inf,
                label=paste0("Max depth = ", round(max(foot$Depth), 0), "m")),
            hjust=0.5,
            vjust=-3,
            size=5.5) +
  geom_text(aes(x=min(foot$Datetime),
                y=Inf,
                label=paste0("Max net vertical spread = ", round(max(mrg$Diff), 0), "m")),
            hjust=0.5,
            vjust=-1,
            size=5.5) +
  coord_cartesian(expand=0,
                  ylim=c(max(foot$Depth)*1.1, 0)) +
  scale_color_manual(name=NULL,
                     values="red",
                     labels="TV80 headrope") +

  theme_bw() +
  theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
        panel.grid=element_blank(),plot.title=element_text(hjust=0.5)) +
  labs(x="Datetime (GMT)",
       y="Depth (m)",
       colour=expression(paste("Temperature (",degree,"C)",sep="")))

ggplot() +
  geom_line(data=foot,
            aes(x=Datetime,
                y=Depth,
                color="B"),
            size=1.5) +
  geom_line(data=head,
            aes(x=Datetime,
                y=Depth,
                color="A"),
            size=1.5) +
  geom_line(data=tv80,
            aes(x=Datetime,
                y=HRDepth,
                color="C"),
            size=1.5) + 
  geom_line(data=tv80,
            aes(x=Datetime,
                y=PRTDepth,
                color="D"),
            size=1.5) +
  coord_cartesian(expand=0,
                  ylim=c(max(foot$Depth)*1.1, 0)) +
  scale_x_datetime(labels=scales::date_format("%Y-%m-%d %H:%M:%S"),
                   breaks=scales::date_breaks("15 mins"),
                   limits=c(
                     # as.POSIXct("2022-07-29 05:42:42", tz="GMT",
                     #            format="%Y-%m-%d %H:%M:%S"),
                     # as.POSIXct("2022-07-29 06:17:42", tz="GMT",
                     #            format="%Y-%m-%d %H:%M:%S")
                     as.POSIXct("2022-07-30 00:05:00", tz="GMT",
                                format="%Y-%m-%d %H:%M:%S"),
                     as.POSIXct("2022-07-30 00:30:42", tz="GMT",
                                format="%Y-%m-%d %H:%M:%S")
                                                                     
                   )) +
  theme_bw() +
  theme(text=element_text(size=16), 
        axis.text=element_text(size=14, colour="black"),
        panel.grid=element_blank(),
        legend.text=element_text(hjust=0,
                                 size=15)) +
  geom_text(aes(x=x,
                y=y,
                label=lab),
            data=tibble::tibble(
              # x=rep(mean(foot$Datetime) - 35*60, 4),
              x=rep(median(foot$Datetime)-20*60, 4),
              y=c(2, 3, 4, 5),
              lab=c("T03",
                    paste0("Max footrope depth = ", round(max(foot$Depth), 1), " m"),
                    paste0("Max net vertical spread = ", round(max(mrg$Diff), 1), " m"),
                    paste0("Max door spread = ", round(max(tv80$SpreadDoor, na.rm=T), 1), " m"))),
            size=5.5, 
            hjust=0) +
  scale_color_manual(
    name=NULL,
    labels=c(
      expression(Headrope[TD]),
      expression(Footroope[TD]),
      expression(Headrope[TV80]),
      expression(Portside~door[TV80])
    ),
    values=c(
      "#d7191c",
      "#fdae61",
      "#2c7bb6",
      "#abd9e9"
    )) +
  labs(x="Datetime (GMT)",
       y="Depth (m)")
    
    



    
    
  