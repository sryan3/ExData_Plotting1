plot4 <- function() {
  require(data.table)
  library(datasets)
  findRows<-fread("data/household_power_consumption.txt", header = TRUE, select = 1)
  all<-(which(findRows$Date %in% c("1/2/2007", "2/2/2007")) )
  skipLines<- min(all)
  keepRows<- length(all)+1
  headers = names(findRows)
  rm(findRows)
  DT <- read.table("data/household_power_consumption.txt",sep=";",header=FALSE, as.is=TRUE,skip = skipLines , nrows = keepRows,na.strings=c('?'),colClasses=c("character","character",rep("numeric",7)))
  DT$timestamp <- strptime(paste(DT$V1, DT$V2), "%d/%m/%Y %H:%M:%S")

#  hist(DT$V3,col = "red",breaks=20,xlim=c(0,6),ylim=c(0, 1200),main="Global Active Power", xlab="Global Active Power (kilowatts)")
  par(mfcol=c(2,2))
  with(DT, {
    plot(DT$timestamp,DT$V3,xaxt="n",type="l",ylab="Global Active Power",xlab="")
    axis.POSIXct(1,at=c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),labels=(format(c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),"%a")))
    #energy sub metering
    plot(DT$timestamp,DT$V7,xaxt="n",type="l",ylab="Energy sub metering",xlab="")
    lines(DT$timestamp,DT$V8, type="l", col="red")
    lines(DT$timestamp,DT$V9, type="l", col="blue")
    axis.POSIXct(1,at=c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),labels=(format(c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),"%a")))
    legend("topright",pch=20,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    #voltage
    plot(DT$timestamp,DT$V5,xaxt="n",type="l",ylab="Voltage",xlab="datetime")
    axis.POSIXct(1,at=c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),labels=(format(c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),"%a")))
    #global reactive power
    plot(DT$timestamp,DT$V4,xaxt="n",type="l",ylab="Global_reactive_power",xlab="datetime")
    axis.POSIXct(1,at=c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),labels=(format(c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),"%a")))
    axis(side=2,at=seq(0,0.5, 0.1), labels=seq(0,0.5, 0.1))
  })
}