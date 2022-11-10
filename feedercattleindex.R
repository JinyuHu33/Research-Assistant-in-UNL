############################################################################
#Load required packages
############################################################################
library(readr)
library(lubridate)
library(dplyr)
library(readxl)
library(tidyterra)
memory.limit(size = 80000)
############################################################################
#Read results 
############################################################################

#################
#Load data after formating
#################
setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")
result_2002.2022_clean <- readRDS("result_2002.2022_07_07_clean.RData")

#################
#Load fedder cattle index data
#################
setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Data/cash_prices")
FeederCattleIndex <- read_excel("./FeederCattleIndex.xls",sheet = 2) %>%
  slice(-c(1,2,3))%>%
  `colnames<-`(c("End.Date", "index.headsold", "index.weightsold", "index.price", "", "newindex.headsold", "newindex.weightsold", "newindex.price")) %>%
  dplyr::select(., End.Date:index.price)





############################################################################
#Clean the feeder cattle index
############################################################################

#change the date to format, '%Y-%m-%d
FeederCattleIndex$End.Date <-  format(as.Date(FeederCattleIndex$End.Date,"%m/%d/%Y"), '%Y-%m-%d')
FeederCattleIndex$End.Date <- as.Date(FeederCattleIndex$End.Date)

############################################################################
#Merge Datasets 
############################################################################

##join the feddercattleindex data frame with result_2014.2018 and apply price adjustment factor
Feeder.cattle.index.join_test <- result_2002.2022_clean %>% left_join(.,FeederCattleIndex,by = 'End.Date')%>% drop_na(index.price)%>% mutate(index.price = as.numeric(index.price))
Feeder.cattle.index.join_test_1 <- Feeder.cattle.index.join_test %>%
  mutate(index.price = ifelse(Type.Code == "steer.wt.2", index.price*1, 
                                      ifelse(Type.Code == "steer.wt.1", index.price*1.1,
                                             ifelse(Type.Code == "heifer.wt.1", index.price*1,
                                                    ifelse(Type.Code == "heifer.wt.1", index.price*0.9,
                                                           ifelse(Type.Code == "brahman.wt.1",index.price*1,
                                                                  ifelse(Type.Code == "brahman.wt.2", index.price*0.9,
                                                                         ifelse(Type.Code == "dairy.wt.1"|Type.Code == "dairy.wt.1",index.price*0.5,
                                                                         index.price
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
         )



saveRDS(Feeder.cattle.index.join_test_1,"Feeder_cattle_index_join.RData")

