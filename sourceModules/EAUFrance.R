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
library(jsonlite)
# set directory
basePath ='./'
sourceModule <- paste0(basePath, 'sourceModules/')
source(paste0(sourceModule,'fileIO.R'))

##==========================================================================
##                              SECTION 1:                                ==  
##  DOWNLOAD KARST SPRINGS DISCHARGE FROM FRANCE BANQUE HYDRO DATABASE    ==  
##==========================================================================
# web link for login to Baque Hydro database
content_type <- "text/html; charset=iso-8859-1"
offset = 0
limit = 100000
orderByColumn = -1
orderAsc = "true"


# springs gauge stations
wokasRDS <- readRDS(paste0(sourceModule,"/station_info.rds"))
stationInfo <- subset(wokasRDS, ISO == "FR" & Source_type == "O" )

# web link to search guage station(s)
Base_Url <- "https://www.hydro.eaufrance.fr/stationhydro/ajax/"
searchUrl <- "http://www.hydro.eaufrance.fr/selection.php?consulte=rechercher"

# web link to search and select guage station(s) 


# create folder to download datasets
outfolder <- paste0(basePath,"tmp_EAU")
dir.create(outfolder)

# for each guage station
for(i in 1:nrow(stationInfo)){
  stationIds <- subset(stationInfo, select=c(Name, Local_database_ID, Start_Date, End_Date))
  # select station id
  id <- as.character(stationIds$Local_database_ID[i])
  
  # search for guage station
  searchUrl <- paste0(Base_Url,id, "/series")
  selectUrl <- GET(searchUrl)
  
  # date query for requested dataset(s)
  Start_Date <- format(stationIds$Start_Date[i], "%d/%m/%Y")
  End_Date <- format(Sys.Date(), "%d/%m/%Y")
  
  # export url query
  export_query <- sprintf("hydro_series[startAt]=%s&hydro_series[endAt]=%s&hydro_series[variableType]=daily_variable&hydro_series[dailyVariable]=QmnJ&hydro_series_step=1&hydro_series[statusData]=pre_validated_and_validated",Start_Date,End_Date)
  
  # web link to export observation dataset(s)
  exportUrl <- paste(searchUrl, export_query, sep="?")
  r <- GET(exportUrl, query=list(offset=offset, limit=limit,orderByColumn=orderByColumn, orderAsc="true"))
  
  getData <- rawToChar(r$content)
  json_getContent <- fromJSON(getData)
  json_data <- json_getContent[["series"]]["data"]
  
  Data <- data.frame(json_data)
  
  if (length(Data) <=2){
    next
  }
  if (length(Data) >=2) {  
  df <- data.frame(cbind(Data[,2], Data[,1]))
  colnames(df) <- c("date", "discharge(l/s)")
  df$date <- gsub("T00:00:00Z","",df$date)
    
  file_name <- sprintf("/%s", id)
  con <- file(paste0(outfolder,file_name,".csv"),'w') 
  write.csv2(df, file=con, sep=',', dec='.', row.names=FALSE, quote=FALSE)
  close(con)
   
    }
  } 

##==========================================================
##                        SECTION 2:                      ==  
##  Unzip downloaded file, re-format and homogenisation   ==  
##==========================================================
# list all ".zip" files
filenames <- list.files(outfolder, pattern = ".csv")

# path to all ".zip" files
filePath <- paste0(outfolder,"/",filenames)

# for every extracted csv file
for(i in 1:length(filePath)){
  tmpFileCsv <- filePath[i]
  con <- file(tmpFileCsv,'r')
  # read csv file
  skip_no <- grep("date",readLines(con))
  
  if(length(skip_no) == 0){
    
    # delete directory
    unlink(paste0(outfolder,unlist(strsplit(filePath[i], "/"))[3]), recursive = T)
    next
  }
  table_data <- read.table(filePath[i], sep = ";", dec = ".", skip = skip_no-1, header=T, col.names = c("date","discharge"))
  
  # format date column
  table_data$date <- gsub("/", ".", table_data$date)
  table_data$date <- gsub("T00:00:00.000Z", "", table_data$date)
  table_data$discharge <- table_data$discharge*0.001
  # select date and discharge columns
  springData <- table_data[,c("date","discharge")]
  
  # create metadata list
  stripName <- tail(unlist(strsplit(filePath[i], "/")),1)[1]
  eauID <- substring(stripName, first = 1, last = 10)
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
unlink("./tmp_EAU")

# --end