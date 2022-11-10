############################################################################
#required package
############################################################################
install.packages("str2str")
library(readr)
library(dplyr)
library(tidyverse)
library(str2str)
############################################################################
#load the data
############################################################################
setwd("...")


direct_cattle_price <- read.csv(".../Datamart-Export_LM_CT150-Detail_20220613_083310.csv")

# first step , filter the data to STEER AND LIVE FOB, 35 - 65% Choice

############################################################################
#Clean the direct_cattle_data --- find the time difference
############################################################################
#filter out data to "STEER", "LIVE FOB", and "65 - 80% Choice"
direct_cattle_price <- direct_cattle_price %>% filter(Class.Description == "STEER",
                                                      Selling.Basis == "LIVE FOB",
                                                      Grade.Description == "65 - 80% Choice") 
 
#change report date to end date and also change the format 
direct_cattle_price$End.Date <-  format(as.Date(direct_cattle_price$Report.Date,"%m/%d/%Y"), '%Y-%m-%d')
direct_cattle_price$Report.Date <- NULL


#find whether all End.Dates in direct_cattle_price are span in 7 days
time_difference <- rep(1:length(direct_cattle_price$End.Date))
for( i in 1: length(direct_cattle_price$End.Date)){
  time_difference[i] <- difftime(direct_cattle_price$End.Date[i],direct_cattle_price$End.Date[i+1], units = "days")
}
direct_cattle_price$time_difference <- time_difference

#change days to integer 7 that dates near 7days 
direct_cattle_price_time_difference <- direct_cattle_price %>% dplyr::mutate(time_difference = ifelse(time_difference == "6" | 
                                                                                          time_difference == "6.95833333333333"|
                                                                                       
                                                                                          time_difference == "7"| 
                                                                                          time_difference == "7.04166666666667" |
                                                                                          time_difference == "5 "|
                                                                                          time_difference == "8"|
                                                                                          time_difference == "9"
                                                                                          ,1,0))%>%
  dplyr::filter(time_difference == 0)#find out that data from 2001-04-09 to  2022-06-06, 2013-09-30 to 2013_10-28, 2015-05-26 and 2015-05-25 have different gap of 
                                     #time difference



############################################################################
#Clean the direct_cattle_data ---duplicate data for each End.Date to 7 days 
############################################################################

#drop the time difference column 
direct_cattle_price$time_difference <- NULL
#generate ordered date sequence from 2001-04-09 to 2022-06-06
direct_cattle_price <- direct_cattle_price[rep(seq_len(nrow(direct_cattle_price)), each = 7), ]
#direct_cattle_price_test <- direct_cattle_price_test[-c(1,2,3,4,5,6),]
direct_cattle_price <- direct_cattle_price %>% arrange(End.Date)
#add index date
direct_cattle_price$index_date <- seq(as.Date("2001-04-09"), as.Date("2022-05-29"), "day") 


#because 2013-09-30 jump to 2013_10-28 therefore, we need to split data here 
direct_cattle_price_test_1 <- direct_cattle_price[1:4564,]



direct_cattle_price_test_2 <- direct_cattle_price[4565:7721,]
direct_cattle_price_test_2$index_date <- NULL 
#there are both 2015-05-26 and 2015-05-25 in the data set 
direct_cattle_price_test_2 <- direct_cattle_price_test_2 %>% filter(End.Date != "2015-05-26")

direct_cattle_price_test_2$index_date <- seq(as.Date("2013-10-28"), as.Date("2022-06-12
                                                                            "), "day") 

direct_cattle_price_test_2 <- direct_cattle_price_test_2[1:3144,]

direct_cattle_price <- rbind(direct_cattle_price_test_1 , direct_cattle_price_test_2)


#change column name
direct_cattle_price <- direct_cattle_price %>% select(-End.Date)%>%
  mutate(End.Date = index_date)%>%
  select(-index_date)






#load the  data


setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")
result_2002.2022_test <- readRDS("./result_2002.2022_test.RData")



direct_cattle_price <- direct_cattle_price %>% left_join(., result_2002.2022_test, by = "End.Date")
#save the data 
saveRDS(direct_cattle_price,"direct_cattle_price.RData")

















 
