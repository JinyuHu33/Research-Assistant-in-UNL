############################################################################
#required package
############################################################################
library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(str2str)
############################################################################
#load the data lrp data merged with feeder cattle index and clean a bit
############################################################################
setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")
join_data_2002.2022 <- readRDS("./Feeder_cattle_index_join.RData")


setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")
GDPDEF <- read.csv("./GDPDEF_2007.2016.csv")




#filter years to july 2007 - august 2016
join_data_2002.2022_1 <- join_data_2002.2022 %>% mutate(contract.ym.lrp = paste0(end.date.yr,'-',
                                                                                 end.date.m))%>%
  filter(contract.ym.lrp == '2007-07'|
           contract.ym.lrp == '2007-08'|
           contract.ym.lrp == '2007-09'|
           contract.ym.lrp == '2007-10'|
           contract.ym.lrp == '2007-11'|
           contract.ym.lrp == '2007-12'|
           end.date.yr == "2008"|
           end.date.yr == "2009"|
           end.date.yr == "2010"|
           end.date.yr == "2011"|
           end.date.yr == "2012"|
           end.date.yr == "2013"|
           end.date.yr == "2014"|
           end.date.yr == "2015"|
           contract.ym.lrp == '2016-01'|
           contract.ym.lrp == '2016-02'|
           contract.ym.lrp == '2016-03'|
           contract.ym.lrp == '2016-04'|
           contract.ym.lrp == '2016-05'|
           contract.ym.lrp == '2016-06'|
           contract.ym.lrp == '2016-07'|
           contract.ym.lrp == '2016-08')%>%
  mutate(index.headsold = as.numeric(index.headsold),
         index.weightsold = as.numeric(index.weightsold),
         index.price = as.numeric(index.price))%>%
  select(Sales.Effective.Date, 
         Endorsement.Length.Count,
         Expected.Ending.Value.Amount,
         Coverage.Price,
         Livestock.Coverage.Level.Percent,
         Cost.Per.Cwt.Amount, 
         End.Date,
         Actual.Ending.Value.Amount, 
         End.Date,
         end.date.m,
         end.date.yr,
         index.headsold,
         index.weightsold,
         index.price,
         Type.Code,
         State.Code)%>%
  filter(Endorsement.Length.Count <= 21&                #double make sure the length is smaller than 52
           Livestock.Coverage.Level.Percent >= 0.85)%>% #exclude the data that coverage level smaller than 0.85
  filter(Type.Code == "steer.wt.2"|
           Type.Code == "brahman.wt.2" |
           Type.Code == "dairy.wt.2" |
           Type.Code == "heifer.wt.2") %>%                 #Type.Code == "brahman.wt.2" |
                                                        #Type.Code == "dairy.wt.2" |
                                                        #Type.Code == "heifer.wt.2")
  filter(State.Code == "Tennessee")%>%
  mutate(coverage.level.quantiles = case_when(Livestock.Coverage.Level.Percent >= 0.8500 & Livestock.Coverage.Level.Percent <= 0.9055 ~ "1",
                                              Livestock.Coverage.Level.Percent >= 0.9056 & Livestock.Coverage.Level.Percent <= 0.9333 ~ "2",
                                              Livestock.Coverage.Level.Percent >= 0.9334 & Livestock.Coverage.Level.Percent <= 0.9563 ~ "3",
                                              Livestock.Coverage.Level.Percent >= 0.9564 & Livestock.Coverage.Level.Percent <= 0.9767 ~ "4",
                                              Livestock.Coverage.Level.Percent >= 0.9768 & Livestock.Coverage.Level.Percent <= 1.00 ~ "5")
        )%>%
 #add end date year and month   
  mutate(end.date.m.yr = paste0(end.date.yr,'-',end.date.m))



###############################
#calculate the indeminity price, if price greater than zero, then record, if smaller than zero, then change to 0 
###############################
join_data_2007.2016 <- join_data_2002.2022_1 %>% mutate(Indemnity.Price = Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount - index.price,
                                                           )
join_data_2007.2016$Indemnity.Price[join_data_2007.2016$Indemnity.Price <=0 ] <- 0                 

###############################                                                  
#calculate the Net Pirce by the formula
###############################
join_data_2007.2016 <- join_data_2007.2016 %>% mutate(Net.Price = index.price + Indemnity.Price - Cost.Per.Cwt.Amount) #why we do not consider subsidy level in here like first paper?

###############################
#change the Net Price by running adjusted annual gross domestic product implicit price deflator
###############################
join_data_2007.2016 <- join_data_2007.2016 %>% left_join(.,GDPDEF, by = "end.date.m.yr")%>%
  mutate(Net.Price = Net.Price * GDPDEF_rate)

###############################
#create a new variable Net.vs.Actual, if Net > Actual, then Net.vs.Actual ~ 1, if Net < Actual, then Net.vs.Actual ~ 0 
###############################
join_data_2007.2016 <- join_data_2007.2016 %>% mutate(Net.vs.Actual = case_when(Net.Price - index.price > 0 ~ "1",      
                                                                                Net.Price - index.price < 0 ~ "0") )    



#Table 1 & 2
join_data_2007.2016_average_NP <- join_data_2007.2016 %>% 
  group_by(end.date.m , Endorsement.Length.Count, coverage.level.quantiles)%>%
  summarise(Average.Net.Price = mean(Net.Price))
#transfer the column Average.Net.Price to vector 
Average_NP <- as.vector(join_data_2007.2016_average_NP$Average.Net.Price)
mat_Average_NP <- matrix(Average_NP, ncol = 15, nrow = 12, byrow = TRUE)
colnames(mat_Average_NP) <- c("13/1","13/2","13/3","13/4","13/5",
                              "17/1","17/2","17/3","17/4","17/5",
                              "21/1","21/2","21/3","21/4","21/5")
rownames(mat_Average_NP) <- c("January","February","March","April","May","June",
                              "July","August","September","October","November","December")


mat_Average_NP <- as.data.frame(mat_Average_NP)
split <- floor(nrow(mat_Average_NP))*0.50
mat_Average_NP_Jan.June <- mat_Average_NP %>%slice(1:split) ####################################first table 
mat_Average_NP_July.Decm <-  mat_Average_NP %>%slice(split+1 :n()) ####################################second table 


#Table 3 & 4
join_data_2007.2016_average_IP <- join_data_2007.2016 %>% 
  group_by(end.date.m , Endorsement.Length.Count, coverage.level.quantiles)%>%
  summarise(Average.IP = mean(index.price))
#transfer the column Average.IP to vector 
Average_IP <- as.vector(join_data_2007.2016_average_IP$Average.IP)
mat_Average_IP <- matrix(Average_IP, ncol = 15, nrow = 12, byrow = TRUE)
colnames(mat_Average_IP) <- c("13/1","13/2","13/3","13/4","13/5",
                              "17/1","17/2","17/3","17/4","17/5",
                              "21/1","21/2","21/3","21/4","21/5")
rownames(mat_Average_IP) <- c("January","February","March","April","May","June",
                              "July","August","September","October","November","December")

mat_Average_IP <- as.data.frame(mat_Average_IP)
split <- floor(nrow(mat_Average_IP))*0.50
mat_Average_IP_Jan.June <- mat_Average_IP %>%slice(1:split) ####################################third table 
mat_Average_IP_July.Decm <-  mat_Average_IP %>%slice(split+1 :n()) ####################################forth table 










############################################################################
#estimate --- Probit model 
#model_1 model2 model 3
############################################################################
join_data_2007.2016 <- join_data_2007.2016 %>% select(end.date.m, Endorsement.Length.Count, coverage.level.quantiles,
                                                            Net.vs.Actual)%>%
  mutate(combination = case_when(Endorsement.Length.Count == 13 & coverage.level.quantiles == 1 ~  "13/1", 
                                 Endorsement.Length.Count == 13 & coverage.level.quantiles == 2 ~ "13/2",
                                 Endorsement.Length.Count == 13 & coverage.level.quantiles == 3 ~ "13/3",
                                 Endorsement.Length.Count == 13 & coverage.level.quantiles == 4 ~ "13/4",
                                 Endorsement.Length.Count == 13 & coverage.level.quantiles == 5 ~ "13/5",
                                 
                                 Endorsement.Length.Count == 17 & coverage.level.quantiles == 1 ~ "17/1",
                                 Endorsement.Length.Count == 17 & coverage.level.quantiles == 2 ~ "17/2",
                                 Endorsement.Length.Count == 17 & coverage.level.quantiles == 3 ~ "17/3",
                                 Endorsement.Length.Count == 17 & coverage.level.quantiles == 4 ~ "17/4",
                                 Endorsement.Length.Count == 17 & coverage.level.quantiles == 5 ~ "17/5",
                                 
                                 Endorsement.Length.Count == 21 & coverage.level.quantiles == 1 ~ "21/1",
                                 Endorsement.Length.Count == 21 & coverage.level.quantiles == 2 ~ "21/2",
                                 Endorsement.Length.Count == 21 & coverage.level.quantiles == 3 ~ "21/3",
                                 Endorsement.Length.Count == 21 & coverage.level.quantiles == 4 ~ "21/4",
                                 Endorsement.Length.Count == 21 & coverage.level.quantiles == 5 ~ "21/5"))%>%
  group_by(end.date.m)%>%
  select(-Endorsement.Length.Count,-coverage.level.quantiles)


#get data frame for each month
join_data_2007.2016_model <- list()
end.date.m <- c("1","2","3","4","5","6","7","8","9","10","11","12")
myfun <- function(x){
  join_data_2007.2016_model[[x]]<- subset(join_data_2007.2016, join_data_2007.2016$end.date.m == x)}
join_data_2007.2016_model <- lapply(end.date.m,myfun)
#run each month in the probit model 
myprobit <- list()
myfun_2 <- function(i){
  myprobit <- glm(as.factor(Net.vs.Actual) ~ combination,
                       data = i, family = "binomial")
  }
myprobit <- lapply(join_data_2007.2016_model,myfun_2)










############################################################################
#figure
############################################################################


############################################################################
#table
############################################################################

#Table 5 & 6
t <- cbind(myprobit[[1]]$coefficients,myprobit[[2]]$coefficients,myprobit[[3]]$coefficients,myprobit[[4]]$coefficients,myprobit[[5]]$coefficients
            ,myprobit[[6]]$coefficients,myprobit[[7]]$coefficients,myprobit[[8]]$coefficients,myprobit[[9]]$coefficients,myprobit[[10]]$coefficients,
            myprobit[[11]]$coefficients,myprobit[[12]]$coefficients)
t <- as.data.frame(t)
#change column names
table_5_6 <- t %>%  rename(January  = "V1",  February = "V2", March = "V3", April = "V4",
                   May  = "V5", June = "V6", July = "V7", August = "V8",
                   September  = "V9", October = "V10", November = "V11", December = "V12"
                   )
#change row names
rownames(table_5_6) <- gsub("combination", "",rownames(t))
#split data to two 
table_5 <- t[,1:6]
table_6 <- t[,7:12]
#transfer data frame to table 
setDT(table_5,  keep.rownames=TRUE)  ####################################fifth table 
#split data to two 
setDT(table_6,  keep.rownames=TRUE) ###################################sixth table 




#Table 7 & 8
##############################
#calculate the predicted probability for each month
##############################
my_fun_3 <- function(myprobit,join_data_2007.2016_model){
  predicted_probability <- predict(myprobit,  join_data_2007.2016_model , type = "response")
}
predicted_probability <- mapply(my_fun_3, myprobit, join_data_2007.2016_model)

table_7 <- list()
for(i in 1: length(join_data_2007.2016_model)){
  join_data_2007.2016_model[[i]]$predicted.probability <- predicted_probability[[i]]
  p <- join_data_2007.2016_model[[i]] %>%
    group_by(combination)%>%
    summarize(predicted_probability = mean(predicted.probability))
  table_7[[i]] = p
}


##############################
#add coverage_level_length to the exsisting data 
##############################
table_7 <- do.call(cbind, table_7)
Coverage_Length_Level <- table_7 $combination
table_7 <- table_7[-c(1,3,5,7,9,11,13,15,17,19,21,23)]
colnames(table_7) <- c("January","February","March","April","May","June",
                       "July","August","September","October","November","December")
table_7 <- add_column(table_7,Coverage_Length_Level, .before = "January")

##############################
#transfer data frame to table 
##############################
setDT(table_7,  keep.rownames=TRUE)






