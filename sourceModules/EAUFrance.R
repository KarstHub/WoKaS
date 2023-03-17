# R code for downloading spring (karst) discharge 
# observations from BANQUE HYDRO online data portal, France
# Author: Tunde Olarinoye
# Institute: University of Freiburg, Germany
# Email: tunde.olarinoye@hydmod.uni-freiburg.de

Sys.setenv(language = "en")

# required packages
library(XML)
library(httr)
library(rgdal)

# set directory
basePath ='./'
sourceModule <- paste0(basePath, 'sourceModules/')
source(paste0(sourceModule,'fileIO.R'))

##==========================================================================
##                              SECTION 1:                                ==  
##  DOWNLOAD KARST SPRINGS DISCHARGE FROM FRANCE BANQUE HYDRO DATABASE    ==  
##==========================================================================

# springs gauge stations
wokasRDS <- readRDS(paste0(sourceModule,"/station_info.rds"))
stationInfo <- subset(wokasRDS, ISO == "FR" & Source_type == "O" )

##### Download the dataset from Github repository

url <-"https://raw.githubusercontent.com/ayolawale/Karst_Project/main/EAU%20FRance%20Data-20230309T170036Z-001.zip" 
download.file(url, dest="./dataset_EAU.zip", mode="wb" ) 


##==========================================================
##                        SECTION 2:                      ==  
##  Unzip downloaded file, re-format and homogenisation   ==  
##==========================================================

unzip("dataset_EAU.zip", exdir = "./WoKaS_Dynamic_Datasets/")
fileNames <- unzip("dataset_EAU.zip", exdir = "./WoKaS_Dynamic_Datasets/")
filename <- paste0(basePath,fileNames)

# for every extracted csv file
for(i in 1:length(fileNames)){
  tmpFileCsv <- fileNames[i]
  con <- file(tmpFileCsv,'r')
  # read csv file
  skip_no <- grep("Date",readLines(con))
  
  if(length(skip_no) == 0){

    # delete directory
    unlink(paste0(outfolder,unlist(strsplit(unzippedList[i], "/"))[3]), recursive = T)
    next
  }
  table_data <- read.table(fileNames[i], sep = ",", dec = ".", skip = skip_no-1, header=T, col.names = c("date","discharge","Validit?","Qualification","Methode","Continuit?"))
  
  # format date column
  table_data$date <- gsub("/", ".", table_data$date)
  table_data$date <- gsub("T00:00:00.000Z", "", table_data$date)
  # select date and discharge columns
  springData <- table_data[,c("date","discharge")]
  
  # create metadata list
  stripName <- tail(unlist(strsplit(fileNames[i], "/")),1)[1]
  eauID <- unlist(strsplit(stripName,"_"))[1]
  eauID <- substring(eauID, first = 1, last = 8)
  wokasMeta <- subset(stationInfo, Local_database_ID == eauID)
  
  metaData <- list(id = as.character(eauID),
                   newID = wokasMeta$Location.Identifier,
                   name = wokasMeta$Name,
                   source = "Banque Hydro France",
                   sourceUrl = "http://www.hydro.eaufrance.fr/indexd.php?connect=1",
                   LAT = as.numeric(wokasMeta$Latitude),
                   LON = as.numeric(wokasMeta$Longitude),
                   unit = "m^3/s")
  
  fileIO.writeSpringData(springData, metaData)
  
  
}
# delete directory
unlink("./WoKaS_Dynamic_Datasets/EAU FRance Data", recursive = T)
unlink("./dataset_EAU.zip")

# --end
