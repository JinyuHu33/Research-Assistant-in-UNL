############################################################################
#load required packages 
############################################################################
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(scales)
library(fitdistrplus)
library(lubridate)
library(foreach)
library(data.table)
library(gtools)
library(plyr)

memory.limit(size = 60000)

############################################################################
#load the data #test 
############################################################################

#################
#Run data from Lrp_2014_2022
#################

setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Data/Lrp_2014_2022")
temp <- list.files(pattern = '*.csv')
data_2014.2022 <- lapply(temp, read.csv)

#################
#Run data from Archive 
#################

setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Data/lrp_archive")
temp <- list.files(pattern = '*.csv')
data_2002.2013 <- lapply(temp, read.csv)

#################
#rbind 11 files 
#################

data_2002.2022 <- do.call("rbind.fill", list(data_2014.2022[[1]], data_2014.2022[[2]], data_2014.2022[[3]],
                            data_2014.2022[[4]],data_2014.2022[[5]],data_2014.2022[[6]],
                            data_2014.2022[[7]],data_2014.2022[[8]],data_2014.2022[[9]],
                            data_2002.2013[[1]], data_2002.2013[[2]]))



############################################################################
#Clean the Data
############################################################################


#################
#Record.Type.Code,
#Record.Category.Code,
#Commodity.Code
#Insurance.Plan.Code 
#################

result_2002.2022 <- data_2002.2022 %>% dplyr::mutate(Record.Type.Code = as.factor(Record.Type.Code),
                                                     Record.Category.Code = as.factor(Record.Category.Code),
                                                     Commodity.Code = {dplyr::case_when(Insurance.Plan.Code == 81 & Commodity.Code == "801" ~ "feeder.cattle",
                                                                                        Insurance.Plan.Code == 81 & Commodity.Code == "802" ~ "fed.cattle",
                                                                                        Insurance.Plan.Code == 81 & Commodity.Code == "804" ~ "lamb",
                                                                                        Insurance.Plan.Code == 81 & Commodity.Code == "815" ~ "swine",
                                                                                        Insurance.Plan.Code == 82 & Commodity.Code == "803" ~ "cattle",
                                                                                        Insurance.Plan.Code == 82 & Commodity.Code == "847" ~ "dairy.cattle",
                                                                                        Insurance.Plan.Code == 82 & Commodity.Code == "815" ~ "swine",
                                                                                        Insurance.Plan.Code == 83 & Commodity.Code == "830" ~ "milk",
                                                                                        TRUE ~ "NA")},
                                                     Insurance.Plan.Code = {case_when(Insurance.Plan.Code == 81 ~ "lrp",     
                                                                                      TRUE ~ "NA")}) %>%




#################
#State.Code
#County.Code
#################



 dplyr::mutate(State.Code = {case_when(
  State.Code == 01 ~ "Alabama",
  State.Code == 02 ~ "Alaska",
  State.Code == 04 ~ "Arizona",
  State.Code == 05 ~ "Arkansas",
  State.Code == 06 ~ "California",
  State.Code == 08 ~ "Colorado",
  State.Code == 09 ~ "Connecticut",
  State.Code == 10 ~ "Delaware",
  State.Code == 12 ~ "Florida",
  State.Code == 13 ~ "Georgia",
  State.Code == 15 ~ "Hawaii",
  State.Code == 16 ~ "Idaho",
  State.Code == 17 ~ "Illinois",
  State.Code == 18 ~ "Indiana",
  State.Code == 19 ~ "Iowa",
  State.Code == 20 ~ "Kansas",
  State.Code == 21 ~ "Kentucky",
  State.Code == 22 ~ "Louisiana",
  State.Code == 23 ~ "Maine",
  State.Code == 24 ~ "Maryland",
  State.Code == 25 ~ "Massachusetts",
  State.Code == 26 ~ "Michigan",
  State.Code == 27 ~ "Minnesota",
  State.Code == 28 ~ "Mississippi",
  State.Code == 29 ~ "Missouri",
  State.Code == 30 ~ "Montana",
  State.Code == 31 ~ "Nebraska",
  State.Code == 32 ~ "Nevada",
  State.Code == 33 ~ "New Hampshire",
  State.Code == 34 ~ "New Jersey",
  State.Code == 35 ~ "New Mexico",
  State.Code == 36 ~ "New York",
  State.Code == 37 ~ "North Carolina",
  State.Code == 38 ~ "North Dakota",
  State.Code == 39 ~ "Ohio",
  State.Code == 40 ~ "Oklahoma",
  State.Code == 41 ~ "Oregon",
  State.Code == 42 ~ "Pennsylvania",
  State.Code == 44 ~ "Rhode Island",
  State.Code == 45 ~ "South Carolina",
  State.Code == 46 ~ "South Dakota",
  State.Code == 47 ~ "Tennessee",
  State.Code == 48 ~ "Texas",
  State.Code == 49 ~ "Utah",
  State.Code == 50 ~ "Vermont",
  State.Code == 51 ~ "Virginia",
  State.Code == 53 ~ "Washington",
  State.Code == 54 ~ "West Virginia",
  State.Code == 55 ~ "Wisconsin",
  State.Code == 56 ~ "Wyoming",
  TRUE ~ "NA")},
  County.Code = as.factor(County.Code)
)%>%



#################
#Type.Code
#practice code
#Price.Adjustment.Factor 
#################




 dplyr::mutate(Type.Code = {case_when(
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 809 ~ "steer.wt.1",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 810 ~ "steer.wt.2",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 811 ~ "heifer.wt.1",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 812 ~ "heifer.wt.2",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 813 ~ "brahman.wt.1",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 814 ~ "brahman.wt.2",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 815 ~ "dairy.wt.1",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 816 ~ "dairy.wt.2",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 817 ~ "unborn.steer.heifer",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 818 ~ "unborn.brahman",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == 819 ~ "unborn.dairy",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "fed.cattle" & Type.Code == 820 ~ "steers.heifers",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "lamb"  & Type.Code == 997 ~ "no.type.specified",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "swine" & Type.Code == 821 ~ "unborn.swine",
  Insurance.Plan.Code == "lrp" & Commodity.Code == "swine" & Type.Code == 997 ~ "no.type.specified",
  TRUE ~ "NA")},
  Practice.Code = as.factor(Practice.Code)
)%>%

 dplyr::mutate(Price.Adjustment.Factor = {case_when(
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "steer.wt.1" ~ 110,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "steer.wt.2" ~ 100,
  ##heifer.wt.1 
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "heifer.wt.1" ~ 100,
  ##heifer.wt.2
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "heifer.wt.2" ~ 90,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "unborn.steer.heifer" ~ 105,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "brahman.wt.1" ~ 100,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "brahman.wt.2" ~ 90,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "dairy.wt.1" ~ 50,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "dairy.wt.1" ~ 50,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "unborn.brahman" ~ 100,
  Insurance.Plan.Code == "lrp" & Commodity.Code == "feeder.cattle" & Type.Code == "unborn.dairy" ~ 50,
  TRUE ~ 100)})%>%






#################
#column that contains Date
#################

 dplyr::mutate(Sales.Effective.Date = as.Date(as.character(Sales.Effective.Date), format = "%Y%m%d"),
                                                           End.Date = as.Date(as.character(End.Date), format = "%Y%m%d"),
                                                           Released.Date = as.Date(as.character(Released.Date), format = "%Y%m%d"),
                                                           Deleted.Date = as.Date(as.character(Deleted.Date), format = "%Y%m%d"),
                                                           Filing.Date = as.Date(as.character(Filing.Date), format = "%Y%m%d")
) %>% dplyr::mutate(
  end.date.yr = year(End.Date),
  end.date.m = month(End.Date),
  contract.ym.lrp = paste0(end.date.yr,'-',end.date.m)
  
)%>%                    





#################
#subsidy level 
#################


 dplyr::mutate(end.date.yr.index = case_when(end.date.yr == 2014 |
                                                                                           end.date.yr == 2015 |
                                                                                           end.date.yr == 2016 |
                                                                                           end.date.yr == 2017 |
                                                                                           end.date.yr == 2018 ~ "2014.2018",#subsidy level 2014 to 2018
                                                                                         
                                                                                         end.date.yr == 2019 |
                                                                                           end.date.yr == 2020 ~ "2019.2020", #subsidy level 2019 to 2020
                                                                                         
                                                                                         end.date.yr == 2021 ~ "2021", #subsidy level 2021
                                                                                         
                                                                                         end.date.yr == 2022 ~ "2022", #subsidy level 2022
                                                                                         
                                                                                         
                                                                                         end.date.yr == 2002 |
                                                                                           end.date.yr == 2003 |
                                                                                           end.date.yr == 2004 |
                                                                                           end.date.yr == 2005 |
                                                                                           end.date.yr == 2006 |
                                                                                           end.date.yr == 2007 |
                                                                                           end.date.yr == 2008 |
                                                                                           end.date.yr == 2009 |
                                                                                           end.date.yr == 2010 |
                                                                                           end.date.yr == 2011 |
                                                                                           end.date.yr == 2012 |
                                                                                           end.date.yr == 2013  ~ "2002.2013"  #subsidy level 2002 to 2013
                                                                                         
))%>%



 dplyr::mutate(subsidy.level.2014.2018 = case_when(end.date.yr.index == "2014.2018" | Livestock.Coverage.Level.Percent >= 0.95 & Livestock.Coverage.Level.Percent <= 1.00 ~ 0.20,
                                                                                               end.date.yr.index == "2014.2018" | Livestock.Coverage.Level.Percent >= 0.90 & Livestock.Coverage.Level.Percent <  0.95 ~ 0.25,
                                                                                               end.date.yr.index == "2014.2018" | Livestock.Coverage.Level.Percent >= 0.80 & Livestock.Coverage.Level.Percent <  0.90 ~ 0.30,
                                                                                               end.date.yr.index == "2014.2018" |Livestock.Coverage.Level.Percent <  0.80 ~ 0.35,
                                                                                               TRUE ~ NA_real_),
                                                           subsidy.level.2019.2020 = case_when(end.date.yr.index == "2019.2020" | Livestock.Coverage.Level.Percent >= 0.95 & Livestock.Coverage.Level.Percent <= 1.00 ~ 0.25,
                                                                                               end.date.yr.index == "2019.2020" | Livestock.Coverage.Level.Percent >= 0.90 & Livestock.Coverage.Level.Percent <  0.95 ~ 0.30,
                                                                                               end.date.yr.index == "2019.2020" | Livestock.Coverage.Level.Percent >= 0.80 & Livestock.Coverage.Level.Percent <  0.90 ~ 0.35,
                                                                                               end.date.yr.index == "2019.2020" | Livestock.Coverage.Level.Percent <  0.80 ~ 0.45,
                                                                                               TRUE ~ NA_real_),
                                                           
                                                           subsidy.level.2021 = case_when(end.date.yr.index == "2021" | Livestock.Coverage.Level.Percent >= 0.95 & Livestock.Coverage.Level.Percent <= 1.00 ~ 0.35,
                                                                                          end.date.yr.index == "2021" | Livestock.Coverage.Level.Percent >= 0.90 & Livestock.Coverage.Level.Percent <  0.95 ~ 0.40,
                                                                                          end.date.yr.index == "2021" | Livestock.Coverage.Level.Percent >= 0.80 & Livestock.Coverage.Level.Percent <  0.90 ~ 0.45,
                                                                                          end.date.yr.index == "2021" | Livestock.Coverage.Level.Percent <  0.80 ~ 0.50,
                                                                                          TRUE ~ NA_real_),
                                                           
                                                           subsidy.level.2022 = case_when(end.date.yr.index == "2022" | Livestock.Coverage.Level.Percent >= 0.95 & Livestock.Coverage.Level.Percent <= 1.00 ~ 0.35,
                                                                                          end.date.yr.index == "2022" | Livestock.Coverage.Level.Percent >= 0.90 & Livestock.Coverage.Level.Percent <  0.95 ~ 0.40,
                                                                                          end.date.yr.index == "2022" | Livestock.Coverage.Level.Percent >= 0.80 & Livestock.Coverage.Level.Percent <  0.90 ~ 0.45,
                                                                                          end.date.yr.index == "2022" | Livestock.Coverage.Level.Percent <  0.80 ~ 0.50,
                                                                                          TRUE ~ NA_real_),
                                                           
                                                           subsidy.level.Archive.2002.2013 = case_when(end.date.yr.index == "2002.2013"| Livestock.Coverage.Level.Percent >= 0.95 & Livestock.Coverage.Level.Percent <= 1.00 ~ 0.13,
                                                                                                       end.date.yr.index == "2002.2013"  | Livestock.Coverage.Level.Percent >= 0.90 & Livestock.Coverage.Level.Percent <  0.95 ~ 0.13,
                                                                                                       end.date.yr.index == "2002.2013" | Livestock.Coverage.Level.Percent >= 0.80 & Livestock.Coverage.Level.Percent <  0.90 ~ 0.13,
                                                                                                       end.date.yr.index == "2002.2013" | Livestock.Coverage.Level.Percent <  0.80 ~ 0.13,
                                                                                                       TRUE ~ NA_real_)
                                                           
                                                           
)

############################################################################
#Save the Data
############################################################################
saveRDS(result_2002.2022, "result_2002.2022_07_07.RData")
























