# required packages
library("XML")
library("rgdal")
library("httr")
library("pdftools")
library("googledrive")
Sys.setenv(LANGUAGE="en")

basePath ='./'
sourceModule <- paste0(basePath, 'sourceModules/')
source(paste0(sourceModule,'fileIO.R'))

# create folder to download datasets
outfolder <- paste0(basePath,"tmp_GKD")
dir.create(outfolder)

##============================================
##                  SECTION 1               ==
##    Download spring discharge datasets    ==
##============================================
# load table data from gwk website
baseUrl <- "https://www.gkd.bayern.de/de/grundwasser/quellschuettung/tabellen"

# springs gauge stations
wokasRDS <- readRDS(paste0(sourceModule,"/station_info.rds"))
stationInfo <- subset(wokasRDS, ISO == "DE" & Research_group == "GKD" )

##### Download the dataset from Github repository
url <-"https://raw.githubusercontent.com/ayolawale/Karst_Project/main/GKD%20Bavaria%20Data-20230309T141844Z-001.zip" 
download.file(url, dest="./dataset_GKD.zip", mode="wb" ) 
unzip("dataset_GKD.zip", exdir = "./WoKaS_Dynamic_Datasets/")
fileNames <- unzip("dataset_GKD.zip", exdir = "./WoKaS_Dynamic_Datasets/")
	
##==========================================================
##                      SECTION 2:                        ==  
##  Unzip downloaded file, re-format and homogenisation   ==  
##==========================================================
# 	# get the names of files within the archive
# fileNames <- unzip(tmpFileArchive, list=TRUE)
	
# filter theses names and only continue with file names ending with 'csv'
fileNames <- fileNames[grep('csv$',fileNames)]

# extract id, start and end for each file 
fileInfos <- NULL
for(f in fileNames){
	f_last <- gsub('.*/','',f)
	x <- unlist(strsplit(f_last, '_'))
	if(length(x)==3) next
	id = x[1]
	if(length(x)==4){
	  start = as.Date(x[2], "%d.%m.%Y")
		end = as.Date(x[3], "%d.%m.%Y")
	}else{
		start = as.Date(NA)
		end = as.Date(x[4], "%d.%m.%Y")
	}
	fileInfos <- rbind(fileInfos, data.frame(id=id, start=start, end=end, filename=f))
}
# order files by end date
fileInfos <- fileInfos[order(fileInfos$end),] 

# loop over all files and combine their data
springData <- NULL
for( j in 1:nrow(fileInfos)){
	tmpFileCsv <- fileInfos$filename[j]
	con <- file(tmpFileCsv,'r')
	line <- '-'
	while(line !='') line <- readLines(con,n=1)
		
	dat <- try(read.table(con, sep=';', header=TRUE, dec=','),silent=TRUE)
	
	close(con)
	if (class(dat) == "try-error"){
	  # remove the temporary csv file
	  file.remove(tmpFileCsv) 
	  next
	}
	# extract coordinates from file
	if (class(dat) != "try-error"){
		  
	  coord_skip <- grep('Ostwert',readLines(tmpFileCsv))
		  
	  if(coord_skip==0){
	    WGS84 <- data.frame(lon_GK=NA,lat_GK=NA)
		  
	    }else{
	    coord <- read.csv(tmpFileCsv, header=F, sep=';', skip=coord_skip-1, nrows=1, col.names=c('long_name','lon','lat_name','lat','projection'), colClasses=c('character','character','character'))
		    
	    # get coordinate in EPSG 31468 projection
	    lon <- as.numeric(gsub('[^0-9.]','',coord$lon)); lat <- as.numeric(gsub('[^0-9.]','',coord$lat))
	    GK <- data.frame(lon_GK=as.numeric(lon),lat_GK=as.numeric(lat))
		    
	    # add projection information (actually this converst the data.frame to a variable of class "SpatialPoints")
	    coordinates(GK) <- c("lon_GK", "lat_GK")
	    proj4string(GK) <- CRS("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs") # Defining Gauss-Krueger zone 4
		    
	    # transform coordinates from Gauss-Kruger Zone 4 to WGS84
	    WGS84 <- spTransform(GK, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  	  }
		  
		}
		
	names(dat) <- c('date','discharge','qualityState')
	dat$date <- format(as.Date(dat$date),'%d.%m.%Y')
		
	# convert discharge unit to m^3/s
	dat$discharge <- dat$discharge*0.001
		
	springData <- rbind(springData, dat)
		
  }
	# file.remove(tmpFileArchive) # remove the temporary zip archive csv file

for(i in 1:nrow(stationInfo)){		
	cat(" -> write file")
  selectedId <- stationInfo$Local_database_ID[i]
	metaData <- list(id = selectedId,
	                 newID = stationInfo$Location.Identifier[i],
	                 name = stationInfo$Name[i],
	                 source = "GKD_Bavaria",
	                 sourceUrl = "https://www.gkd.bayern.de/de/grundwasser/quellschuettung//tabellen",
	                 LAT=WGS84$lat_GK,
	                 LON=WGS84$lon_GK,
	                 unit='m^3/s')

	fileIO.writeSpringData(springData[,c('date','discharge')], metaData)
	
	cat(" -> done :)\n")
}

# delete dir
unlink(outfolder, recursive = T)
# delete directory
unlink("./WoKaS_Dynamic_Datasets/GKD Bavaria Data/", recursive = T)
unlink("./dataset_GKD.zip")




