# Author: Tunde Olarinoye
# Institute: University of Freiburg, Germany
# Email: tunde.olarinoye@hydmod.uni-freiburg.de

Sys.setenv(language = "en")

# set dir
basePath = "./"
sourceModule <- paste0(basePath, 'sourceModules/')
source(paste0(sourceModule,'fileIO.R'))

# creates output folder
dataOut <- paste0(basePath,'WoKaS_Dynamic_Datasets')
dir.create(dataOut)

packageToInstall <- function(package, repos=NULL){
  if(is.null(repos)) repos='https://cran.r-project.org/'
  if(package %in% rownames(installed.packages()) == FALSE) install.packages(package, repos)
  else cat(sprintf('-> %s already installed\n', package))
}

# install required packages
packageToInstall('yaml')
packageToInstall('XML')
packageToInstall('httr')
packageToInstall('pdftools')
packageToInstall('rgdal')
packageToInstall('tidyverse')
packageToInstall('jsonlite')
packageToInstall('curl')
if("RHTMLForms" %in% rownames(installed.packages()) == FALSE) {install.packages(paste0(sourceModule,"RHTMLForms_0.6-0.tar.gz"), repos = NULL, type = "source")}


# download datasets
# to download all online dataset, run fileIO.runDowload()
# to download dataset from specific country, run fileIO.runDownload("country's ISO-2 code")

fileIO.runDownload(Germany)
