# R code for downloading spring (karst) discharge observations from 
# Landesanstalt für Umwelt Baden-Wuerttemberg online data portal of USGS, Germany
# Author: Tunde Olarinoye
# Institute: University of Freiburg, Germany
# Email: tunde.olarinoye@hydmod.uni-freiburg.de

Sys.setenv(language="en")

# require packages
library(XML)
library(rgdal)
library(httr)
library(readxl)
library(jsonlite)

# set directory
basePath ='./'
sourceModule <- paste0(basePath, 'sourceModules/')
source(paste0(sourceModule,'fileIO.r'))
wokasRDS <- readRDS(paste0(sourceModule, "station_info.rds"))

# create folder to download datasets
outfolder <- paste0(basePath,"tmp_UDO/")
dir.create(outfolder)

##==========================================================
##                        SECTION 1                       ==                             
##    Load the UDO LUBW web link and download datasets    ==
##==========================================================
host = "https://udo.lubw.baden-wuerttemberg.de/"
offset = 0
limit = 20000
orderByColumn = -1
orderAsc = "true"
content_type = "application/x-www-form-urlencoded; charset=UTF-8"

# sta_id <- c("0601/515-7","0001/415-1","0600/554-9","0600/665-7",
#             "0014/714-5","0003/567-0","0003/863-3","0024/762-4") # 0026/762-5

# permaLink <- "https://udo.lubw.baden-wuerttemberg.de/public/q/jL5YC" # old permaLink
permaLinks <- c("public/q/1NjU9H4DepQ9c7JOg84UPV","public/q/63NyjDRoqtxLbPja1L9x31",
               "public/q/75cROriyYeWNj87ky7Q1Mh","public/q/4V9g6oP0b1R5vJdMHX1YUD",
               "public/q/3j9rxJT9kY50sthuM305OD","public/q/5VXqpCwq3roBTNSqLT1gXr",
               "public/q/7Eq6miCcuP0vFRU3RZ9AN2", "public/q/6c1i6JmwfCmWn6bC6vwQPO")

for(i in 1:length(permaLinks)){
  permaLink <- paste0(host, permaLinks[i])

  dataPage <- GET(permaLink)
  
  if(dataPage$status_code != 200){
    print("Connection server might be busy!\nRetrying connection to server...")
    RETRY(GET, permaLink)
  }
  
  getLink <- gsub("refdb%24ind1", "refdb$ind1",
                  gsub("meros%3Ameros", "meros:meros", dataPage[["url"]]), )
  
  jsID <- dataPage[["cookies"]][["value"]]
  
  getData <- GET(getLink, query=list(offset=offset, limit=limit,orderByColumn=orderByColumn,
                                     orderAsc="true"),set_cookies(JSESSIONID=jsID))
  
  getCont <- rawToChar(getData$content)
  json_getCont <- fromJSON(getCont)
  json_df <- json_getCont[[3]]["values"]
  
  df <- data.frame(matrix(ncol=10))
  colnames(df) <- c("id","name","lon_GK","lat_GK","gemeinde","komponente","Date","Q","unit","zustandige")
  
  for(i in 1:nrow(json_df)){
    
    id = json_df[i,][[1]][[2]]
    name = json_df[i,][[1]][3]
    lon_GK = json_df[i,][[1]][4]
    lat_GK = json_df[i,][[1]][5]
    gemeinde = json_df[i,][[1]][7]
    komponente = json_df[i,][[1]][9]
    Date = json_df[i,][[1]][11]
    Date = gsub("Z","",gsub("T"," ",Date))
    Q = json_df[i,][[1]][13]
    unit = json_df[i,][[1]][15]
    zustandige = json_df[i,][[1]][24]
    
    df[i,] = c(id,name,lon_GK,lat_GK,gemeinde,komponente,Date,Q,unit,zustandige)
  
  }
  
  filename <- gsub("/","",unique(df["id"])) 
  con <- file(paste0(outfolder,filename,".txt"),'w')
  write.table(df, file = con, sep='\t', dec='.', row.names=FALSE, quote=FALSE)
  close(con)
}

##==========================================================================
##                                SECTION 2                               ==                             
##    Refine datasets: Reproject coordinates, subset and Homogenization   ==
##==========================================================================

# read downloaded text files
txtFiles <- list.files(outfolder, ".txt")
for(i in 1:length(txtFiles)){
  
  tableData <- read.csv(paste0(outfolder,txtFiles[i]), header=T, sep="\t")
                        #col_names=c("id","name","lon_GK","lat_GK","gemeinde","komponente","Date","Q","unit","zustandige"))
  
  # reformat date
  tableData$date <- as.Date(tableData$Date, "%Y-%m-%d %H:%M:%S")
  tableData$date <- format.Date(tableData$date, "%d.%m.%Y %H:%M:%S")
  
  # convert discharge to m^3/s
  tableData$discharge <- as.numeric(tableData$Q)/1000
  
  # add projection information (actually this convert the data.frame to a variable of class "SpatialPoints")
  GK <- data.frame(tableData$lon_GK, tableData$lat_GK)
  names(GK) <- c("lon_GK", "lat_GK")
  coordinates(GK) <- c("lon_GK", "lat_GK")
  
  # defining WGS 84/UTM Zone 32
  proj4string(GK) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs ")
  
  # transform coordinates to  epsg:4326 WGS84
  WGS84 <- as.data.frame(spTransform(GK, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
  names(WGS84) <- c("lon_wgs84","lat_wgs84")
  
  # combine data frames
  tableData <- cbind(tableData,WGS84)
  
  # get station ids of datasets downloaded
  ids <- unique(tableData$id)
  
  # for each id 
  for(i in 1:length(ids)){
    # subset dataset
    springFill <- subset(tableData, id==ids[i])
    springData <- subset(tableData, id==ids[i], select = c("date","discharge"))
    
    # get spring's WoKaS id and spring name from stationInfo
    #wokasMeta <- subset(stationInfo, id == ids[i])
    wokasMeta <- subset(wokasRDS, Local_database_ID==ids[i])
    
    # write file to csv
    metaData <- list(id = wokasMeta$id,
                     newID = wokasMeta$Location.Identifier,
                     name = wokasMeta$Name,
                     source = "Landesanstalt für Umwelt, Baden-Württemberg",
                     sourceUrl = "http://udo.lubw.baden-wuerttemberg.de/public/",
                     LAT = as.numeric(springFill[1,14]),
                     LON = as.numeric(springFill[1,13]),
                     unit = "m^3/s")
    fileIO.writeSpringData(springData, metaData)
  }
}

# delete outfolder tmp dir
unlink(outfolder, recursive = T)

