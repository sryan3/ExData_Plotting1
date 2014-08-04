plot2 <- function() {
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
  plot(DT$timestamp,DT$V3,xaxt="n",type="l",ylab="Global Active Power (kilowatts)",xlab="")
  axis.POSIXct(1,at=c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),labels=(format(c(DT$timestamp[1],DT$timestamp[1441],DT$timestamp[2881]),"%a")))
}