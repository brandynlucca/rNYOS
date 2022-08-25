#TD Sensor reader v. 3.5
#Rambling code - Brandyn Lucca - 30 September 2019
#To do: eliminate *.xlsx files by converting to *.csv ahead of time;
#or simply build xlsx2csv converting function? Seems... reundant...
#*.xlsx is the second-worst file format behind *.docx >:(
#To do: push commits to echogram; overlay tows with acoustic data
#To do: catenary calculator for x-y-z position behind vessel

#Non-Java .xlsx reader
library(openxlsx)

#CSIRO seawater algorithms; based on TEOS-10
library(oce)

#ggplot for plotting later
library(ggplot2)
library(viridis)
library(gridExtra)

#Read in metadata
# metadata <- read.csv("G:/EN626.2018/td_sensor/td.deployment.metadata.csv")
# metadata <- read.csv("E:/AR049.td.deployment.metadata.csv")
metadata <- read.csv("I:/nyos2207.td.deployment.metadata.csv")
metadata$DecDeg <- metadata$Latitude.deg + metadata$Latitude.min/60
metadata$Datetime_S <- as.POSIXct(paste(metadata$Date_S," ",metadata$Time_S,sep=""), format="%Y-%m-%d %H:%M:%S",tz="GMT")
metadata$Datetime_E <- as.POSIXct(paste(metadata$Date_E," ",metadata$Time_E,sep=""), format="%Y-%m-%d %H:%M:%S",tz="GMT")

#Parse data files; will loop if necessary
filestring <- file.choose()
filestr <- strsplit(filestring,c("[.]"))
filestr <- strsplit(filestr[[1]][2],"_")

td.df <- openxlsx::read.xlsx(filestring, sheet="DAT")
colnames(td.df) <- c("Datetime","Temperature","Pressure")

#Convert from serial datetime (Excel default) to Y-m-d H:M:S
td.df$Datetime <- as.POSIXct(td.df$Datetime * (60*60*24), origin="1899-12-30",tz="GMT")

#Begin the loop(s)!
#Script will generate a *.csv file of the profiles, as well as a *.jpeg of the profile: depth vs. time with temperature colored
#Metadata file eliminates non-tow measurements and allows for more accurate depth estimates using swDepth(...)
if(length(filestr[[1]]) > 1){
  for(i in 1:length(filestr[[1]])){

    met.idx <- which(metadata$Haul == filestr[[1]][i])
    td.df$Depth <- swDepth(td.df$Pressure*10,metadata$DecDeg[met.idx])
    td.sub <- td.df[td.df$Datetime >= metadata$Datetime_S[met.idx] &
                      td.df$Datetime <= metadata$Datetime_E[met.idx],]

    write.csv(td.sub,paste("E:/AR049.",filestr[[1]][i],".",metadata$Date_S[met.idx],
                           ".td_proc.csv",sep=""),row.names=FALSE)

    p1 <- ggplot(data=td.sub, aes(x=Datetime,y=Depth)) + geom_point(aes(colour=Temperature),size=2) +
      scale_color_viridis(breaks=c(5,10,15,20,25,30),limits=c(2,30),option="plasma") + theme_bw() +
      scale_y_reverse(expand=c(0,0),limits=c(1.1*max(td.sub$Depth,na.rm=T),1)) +
      theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
            panel.grid=element_blank(),plot.title=element_text(hjust=0.5)) +
      labs(x="Datetime (GMT)",
           y="Depth (m)",
           colour=expression(paste("Temperature (",degree,"C)",sep="")),
           title=paste(filestr[[1]][i]," (Max depth = ",round(max(td.sub$Depth),0),"m)",sep=""))

    p2 <- ggplot(data=td.sub, aes(x=Temperature,y=Depth,colour=Datetime)) + geom_point(size=2) +
      geom_path(size=3) +
      scale_y_reverse(limits=c(max(td.sub$Depth)*1.1,2),expand=c(0,0)) +
      scale_x_continuous(limits=c(0.975*min(td.sub$Temperature[td.sub$Depth > 1.5],na.rm=T),
                                  1.025*max(td.sub$Temperature[td.sub$Depth > 1.5],na.rm=T))) +
      theme_bw() +
      theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
            panel.grid=element_blank()) +
      labs(x=expression(paste("Temperature (",degree,"C)",sep="")),
           y="Depth (m)")

    g1 <- grid.arrange(p1,p2)

    ggsave(paste("E:/AR049.",filestr[[1]][i],".",metadata$Date_S[met.idx],".td_proc.jpeg",sep=""),p1)
  }
}else{
  met.idx <- which(metadata$Haul == filestr[[1]][1])
  td.df$Depth <- swDepth(td.df$Pressure*10,metadata$DecDeg[met.idx])
  td.sub <- td.df[td.df$Datetime >= metadata$Datetime_S[met.idx] &
                    td.df$Datetime <= metadata$Datetime_E[met.idx],]

  # write.csv(td.sub,paste("E:/AR049.",filestr[[1]][1],".",metadata$Date_S[met.idx],
  #                        ".td_proc.csv",sep=""), row.names=FALSE)

  p1 <- ggplot(data=td.sub, aes(x=Datetime,y=Depth)) + geom_point(aes(colour=Temperature),size=2) +
    scale_color_viridis(breaks=c(5,10,15,20,25,30),limits=c(4,30),option="plasma") + theme_bw() +
    scale_y_reverse(expand=c(0,0),limits=c(1.1*max(td.sub$Depth,na.rm=T),1)) +
    theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
          panel.grid=element_blank(),plot.title=element_text(hjust=0.5)) +
    labs(x="Datetime (GMT)",
         y="Depth (m)",
         colour=expression(paste("Temperature (",degree,"C)",sep="")),
         title=paste(filestr[[1]][1]," (Max depth = ",round(max(td.sub$Depth,na.rm=T),0),"m)",sep=""))

  p2 <- ggplot(data=td.sub, aes(x=Temperature,y=Depth,colour=Datetime)) + geom_point(size=2) +
    geom_path(size=3) +
    scale_y_reverse(limits=c(max(td.df$Depth)*1.1,2),expand=c(0,0)) +
    scale_x_continuous(limits=c(0.975*min(td.sub$Temperature[td.sub$Depth > 1.5]),
                                1.025*max(td.sub$Temperature[td.sub$Depth > 1.5]))) +
    theme_bw() +
    theme(text=element_text(size=16), axis.text=element_text(size=14, colour="black"),
          panel.grid=element_blank()) +
    labs(x=expression(paste("Temperature (",degree,"C)",sep="")),
         y="Depth (m)")

  g1 <- grid.arrange(p1,p2)

  ggsave(paste("E:/AR049.",filestr[[1]][1],".",metadata$Date_S[met.idx],".td_proc.jpeg",sep=""),p1)
}

