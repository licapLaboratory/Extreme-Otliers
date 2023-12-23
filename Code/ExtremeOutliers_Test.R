#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--- DETECTION OF EXTREME OUTLIERS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#--- IDENTIFICATION OF EXTREME OUTLIERS

path <- '/licapLaboratory/'
setwd(path)

#--- LOADING LIBRARIES AND PACKAGES

loadLibrary <- function(){
  source(paste0(path,"/Code/ExtremeOutliers.R"))
  packs <- c("readr")
  oldPar_Margins <<- par()$mar
  par(mar=c(4.1,4.1,3.1,1.1))
  invisible(suppressMessages(suppressWarnings(lapply(packs, library, character.only=TRUE))))
}
loadLibrary()

#--- SIMULATIONS

# TDB1

dfExtreme <- data.frame(read_delim(file=paste0(path, '/TemporalDataBase/', 'dfTDB1.csv', sep=""), delim=';', col_names=TRUE, quote="", locale=locale(asciify=TRUE), trim_ws=TRUE))

gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.95, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.98, showInfo=TRUE)

# TDB2

dfExtreme <- data.frame(read_delim(file=paste0(path, '/TemporalDataBase/', 'dfTDB2.csv', sep=""), delim=';', col_names=TRUE, quote="", locale=locale(asciify=TRUE), trim_ws=TRUE))

gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.95, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.98, showInfo=TRUE)

# TDB3

dfExtreme <- data.frame(read_delim(file=paste0(path, '/TemporalDataBase/', 'dfTDB3.csv', sep=""), delim=';', col_names=TRUE, quote="", locale=locale(asciify=TRUE), trim_ws=TRUE))

gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.95, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.98, showInfo=TRUE)

# TDB4

dfExtreme <- data.frame(read_delim(file=paste0(path, '/TemporalDataBase/', 'dfTDB4.csv', sep=""), delim=';', col_names=TRUE, quote="", locale=locale(asciify=TRUE), trim_ws=TRUE))

gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.95, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.98, showInfo=TRUE)

# TDB5

dfExtreme <- data.frame(read_delim(file=paste0(path, '/TemporalDataBase/', 'dfTDB5.csv', sep=""), delim=';', col_names=TRUE, quote="", locale=locale(asciify=TRUE), trim_ws=TRUE))

gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.95, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.98, showInfo=TRUE)
gev <- GEVGumbel(df=dfExtreme, windowRange=c(1:1000), T_Percent=0.99, showInfo=TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------