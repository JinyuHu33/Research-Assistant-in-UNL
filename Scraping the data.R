############################################################################
#load required packages 
############################################################################
require(tidyverse)
require(dplyer)
require(rvest)
require(lubridate)
require(foreach)
require(plyr)
require(data.table)
require(readr)
memory.limit(size=60000)

setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Data/Lrp_2014_2022")
############################################################################
#extract based on years from 2014 to 2022 and transfer to CSV
############################################################################
year <- c("2014")#,"2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
for(i in 1 : length(year)){
  url <- paste0('https://ftp.rma.usda.gov/pub/references/adm_livestock/', year[i],'/')
  
  href <- read_html(url)
  
  href_names <- as.list(html_attr(html_nodes(href, "a"), "href"))
  
  #delete useless urls that do not contain "lrp"
  href_names <- href_names[grepl("Lrp",href_names)]
  #extract dates from href_names
  href_names <- sub(".*Daily_","",href_names)
  href_year <- sub(".zip.*","", href_names)
  
  #create an empty dataframe to save the 9 new data
  new_data = data.frame()
  for (j in 1:length(href_year)){
    url <- paste0('https://ftp.rma.usda.gov/pub/references/adm_livestock/',year[i],'/',year[i],'_ADMLivestockLrp_Daily_', href_year[j],
                  '.zip')
    #save download zip files to certain path
    destfile <- paste0('C://Users//stigg//OneDrive//Desktop//Job with elliott//Scraping the data',href_year[j],'.zip')
    download.file(url, destfile)
    #some zip files has more than one txt file, read multiple file and bind them together
    diretory = unzip(paste0('C:/Users/stigg/OneDrive/Desktop/Job with elliott/Scraping the data', href_year[j],'.zip'))
    dataset = data.frame()
    for (k in 1:length(diretory)){
      data <- rbind.fill(dataset, read.delim(diretory[k], sep="|"))
      new_data <- rbind.fill(new_data, data)
    }
  
  }
  new_data <- fwrite(new_data, paste0(year[i], '_ADMLivestockLrp_scraping.csv'))
  
  }

############################################################################
#extraT data from Archive and transfer to CSV
############################################################################
Archive <- c("A00620_LrpActualEndingValue",
             "A00630_LrpRate")

for(i in 1: length(Archive)){
  url <- paste0('https://ftp.rma.usda.gov/pub/references/adm_livestock/Archive/', Archive[i],'.zip')
  
  
  destfile <- paste0('C://Users//stigg//OneDrive//Desktop//job with elliott//Scraping the data',Archive[i],'.zip')
  
  download.file(url, destfile)
  
  path = unzip(paste0('C:/Users/stigg/OneDrive/Desktop/job with elliott/Scraping the data', Archive[i],'.zip'))
  
  result <- read.delim(path, sep="|")  
  fwrite( result, paste0(Archive[i], '_Archive_scraping.csv'))
  
}




