############################################################################
#required package
############################################################################
library(readr)
library(dplyr)
library(tidyverse)
library(str2str)
library(data.table)
library(stats)
library(ROCR)
library(ggplot2)

memory.limit(size = 60000)

############################################################################
#load the data lrp data merged with feeder cattle index and clean a bit
############################################################################
setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")

join_data_2002.2022 <- readRDS("./Feeder_cattle_index_join.RData")


#filter years to 2014 - 2018                                
join_data_2014.2018 <- join_data_2002.2022 %>% filter(end.date.yr == "2014"|
                                                      end.date.yr == "2015"|
                                                      end.date.yr == "2016"|
                                                      end.date.yr == "2017"|
                                                   end.date.yr == "2018")%>%
  mutate(index.headsold = as.numeric(index.headsold),
         index.weightsold = as.numeric(index.weightsold),
         index.price = as.numeric(index.price))%>%
  #drop irrelavent columns 
  select(-subsidy.level.2019.2020,
         -subsidy.level.2021,
         -subsidy.level.2022,
         -subsidy.level.Archive.2002.2013)%>%
  filter(Endorsement.Length.Count < 30  &                   #exclude endorsement length which over 30 hours  
           Livestock.Coverage.Level.Percent > 0.85)%>%                    #exclude coverage level which smaller 0.85
           filter(Type.Code == "steer.wt.2"|                          #focus on feeder cattles from 600 - 900 pound(inclusive or exclusive???)
           Type.Code == "heifer.wt.2"|
           Type.Code == "brahman.wt.2"|
           Type.Code == "dairy.wt.2")%>%
  select(Sales.Effective.Date, Livestock.Coverage.Level.Percent,
         Endorsement.Length.Count, Expected.Ending.Value.Amount,
         Coverage.Price, Cost.Per.Cwt.Amount, End.Date, index.price, end.date.m)   ########why is not actual ending amount but index.price???
  

#Note!!!!!!!: please be careful on circumstances when you run the data, if you run pre-2019, do not run 2020, vice versa.

###############################
#Net price and Indemnity.Price for LRP under pre-2019
###############################
join_data_2014.2018_1 <- join_data_2014.2018 %>% mutate(
  Indemnity.Price = (Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount) - index.price,
  Net.Price = (index.price + Indemnity.Price) - Cost.Per.Cwt.Amount*(1-0.13)      #according to the first replicate paper
                                                                                  #(page 1 / paragraph 3): "Previously the subsidy for LRP was 13% of the premium"
)


###############################
#Net price and Indemnity.Price for LRP under 2020
###############################
join_data_2014.2018_1 <- join_data_2014.2018 %>%mutate(
  subsidy.rate = case_when(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95 ~ 0.35,
                           Livestock.Coverage.Level.Percent <= 0.9499  & Livestock.Coverage.Level.Percent >= 0.90 ~ 0.40,
                           Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ~ 0.45
  ),
  Indemnity.Price = (Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount) - index.price,
  Net.Price = (index.price + Indemnity.Price) - Cost.Per.Cwt.Amount*(1- subsidy.rate)      #according to the first replicate paper
                                                                                           #(page 1 / paragraph 3): "Previously the subsidy for LRP was 
                                                                                           #13% of the premium; but starting in 2019, 
                                                                                           #subsidy rates increased and in 2020 further increases 
                                                                                           #were made. The 2020 subsidy rates are based on the coverage level."
)
###############################
#create a new variable Net.vs.Actual
###############################
join_data_2014.2018_1 <- join_data_2014.2018_1 %>% mutate(
  Net.vs.Actual =  case_when(Net.Price > index.price ~ "1",
                             Net.Price < index.price ~ "0")
)



  
  
 




############################################################################
#estimate --- logit model 
#model_1 model2 model 3
############################################################################

###############################
#seperate coverage length to 4 different level
###############################
join_data_2014.2018_pre_model <- join_data_2014.2018_1 %>% select(end.date.m, Endorsement.Length.Count,
                                                              Livestock.Coverage.Level.Percent, Net.vs.Actual)%>%
  group_by(end.date.m,  Endorsement.Length.Count, Livestock.Coverage.Level.Percent)%>%
  mutate(Week_13 = ifelse(Endorsement.Length.Count == 13,1,0),
         Week_17 = ifelse(Endorsement.Length.Count == 17,1,0),
         Week_21 = ifelse(Endorsement.Length.Count == 21,1,0),
         Week_26 = ifelse(Endorsement.Length.Count == 26,1,0)
          )


###############################
#extract data from join_data_2014.2018 for each month
###############################
join_data_2014.2018_model <- list()
end.date.m <- c("1","2","3","4","5","6","7","8","9","10","11","12")
myfun_1 <- function(x){
  join_data_2014.2018_model[[x]]<- subset(join_data_2014.2018_pre_model, 
                                          join_data_2014.2018_pre_model$end.date.m == x)}
join_data_2014.2018_model <- lapply(end.date.m,myfun_1)


###############################
#run Probit model in each month 
###############################
mylogit <- list()
myfun_2 <- function(i){
mylogit  <- glm(as.factor(Net.vs.Actual) ~  Livestock.Coverage.Level.Percent + Week_13 +
                  Week_17 + Week_21 + Week_26 + Livestock.Coverage.Level.Percent* Week_13 + 
                  Livestock.Coverage.Level.Percent* Week_17 +
                  Livestock.Coverage.Level.Percent* Week_21 + 
                  Livestock.Coverage.Level.Percent* Week_26,
              data = i, family = "binomial")

}
mylogit  <- lapply(join_data_2014.2018_model,myfun_2)







############################################################################
#table
############################################################################
#table------------------1

join_data_2014.2018_table_1 <- join_data_2014.2018_1 %>% select(end.date.m,
                                                                Net.vs.Actual,
                                                                Net.Price, index.price)%>%
  dplyr::group_by(end.date.m)%>%
  dplyr::summarise(Observation = n(),
            Average = table(Net.vs.Actual)[2]/n(),
            Standard.Deviation = sd(Net.vs.Actual, na.rm=TRUE),
            Minimum = 0,
            Maximum = 1)

join_data_2014.2018_table_1_2 <- join_data_2014.2018_1 %>% select(end.date.m,
                                                                  Net.Price, index.price)%>%
  dplyr::group_by(end.date.m)%>%
  dplyr::summarise(Observation = n(),
                   Average = mean(Net.Price - index.price,na.rm=TRUE),
                   Standard.Deviation = sd(Net.Price - index.price, na.rm=TRUE),
                   Minimum = min(Net.Price - index.price, na.rm=TRUE),
                   Maximum = max(Net.Price - index.price, na.rm=TRUE),
                   )

table_1 <- rbind(join_data_2014.2018_table_1, join_data_2014.2018_table_1_2)                  

table_1 <- table_1 %>%
  mutate(end.date.m = case_when(end.date.m == '1' ~ 'January',
                                end.date.m == '2' ~ 'February',
                                end.date.m == '3' ~ 'March',
                                end.date.m == '4' ~ 'April',
                                end.date.m == '5' ~ 'May',
                                end.date.m == '6' ~ 'June',
                                end.date.m == '7' ~ 'July',
                                end.date.m == '8' ~ 'August',
                                end.date.m == '9' ~ 'September',
                                end.date.m == '10' ~ 'October',
                                end.date.m == '11' ~ 'November',
                                end.date.m == '12' ~ 'December'))
setDT(table_1,  keep.rownames=TRUE) 


                                                                                                      
#table------------------2
join_data_2014.2018_table_2 <- join_data_2014.2018_1 %>% select(Cost.Per.Cwt.Amount, end.date.m)%>%
  dplyr::group_by(end.date.m)%>%
  dplyr::summarise(Observation = n(),
                   Average = mean(Cost.Per.Cwt.Amount, na.rm=TRUE),
                   Standard.Deviation = sd(Cost.Per.Cwt.Amount, na.rm=TRUE),
                   Minimum = min(Cost.Per.Cwt.Amount, na.rm = TRUE),
                   Maximum = max(Cost.Per.Cwt.Amount, na.rm = TRUE))

join_data_2014.2018_table_2_2 <- join_data_2014.2018_1 %>% select(Indemnity.Price, end.date.m)%>%
  dplyr::group_by(end.date.m)%>%
  dplyr::summarise(Observation = n(),
                   Average = mean(Indemnity.Price, na.rm=TRUE),
                   Standard.Deviation = sd(Indemnity.Price, na.rm=TRUE),
                   Minimum = min(Indemnity.Price, na.rm = TRUE),
                   Maximum = max(Indemnity.Price, na.rm = TRUE))

table_2 <- rbind(join_data_2014.2018_table_1, join_data_2014.2018_table_2_2)                  

table_2 <- table_2 %>%
  mutate(end.date.m = case_when(end.date.m == '1' ~ 'January',
                                end.date.m == '2' ~ 'February',
                                end.date.m == '3' ~ 'March',
                                end.date.m == '4' ~ 'April',
                                end.date.m == '5' ~ 'May',
                                end.date.m == '6' ~ 'June',
                                end.date.m == '7' ~ 'July',
                                end.date.m == '8' ~ 'August',
                                end.date.m == '9' ~ 'September',
                                end.date.m == '10' ~ 'October',
                                end.date.m == '11' ~ 'November',
                                end.date.m == '12' ~ 'December'))
setDT(table_2,  keep.rownames=TRUE) 
                                                                                


#table------------------3&4



#######################################
#create a dataframe consist by coefficients
#######################################
t <- cbind(mylogit[[1]]$coefficients,mylogit[[2]]$coefficients,
           mylogit[[3]]$coefficients,mylogit[[4]]$coefficients,
           mylogit[[5]]$coefficients,mylogit[[6]]$coefficients,
           mylogit[[7]]$coefficients,mylogit[[8]]$coefficients,
           mylogit[[9]]$coefficients,mylogit[[10]]$coefficients,
           mylogit[[11]]$coefficients,mylogit[[12]]$coefficients)
t <- as.data.frame(t)
#change column names
t <- t %>%  rename(January  = "V1",  February = "V2", March = "V3", April = "V4",
                   May  = "V5", June = "V6", July = "V7", August = "V8",
                   September  = "V9", October = "V10", November = "V11", December = "V12"
)


###############################
#add observation into dataframe
###############################
t[nrow(t) + 1,] <- table_1$Observation



###############################
#add r-square into dataframe                                 
#calculate McFadden's R-squared for each model
###############################
r_square <- vector()
for(i in 1: length(mylogit)){
  r_square[i] <- with(summary(mylogit[[i]]), 1 - deviance/null.deviance)
}
t[nrow(t) + 1,] <- r_square

                                                              

###############################
#add correctly predicted to dataframe
#run the auc test to check the model performance 
###############################
my_fun_3 <- function(mylogit,join_data_2014.2018_model){
  predicted_probability <- predict(mylogit ,  join_data_2014.2018_model , type = "response")
  join_data_2014.2018_model$predicted_probability <- predicted_probability
  hjy_predi <- prediction(join_data_2014.2018_model$predicted_probability, join_data_2014.2018_model$Net.vs.Actual)
  hjy_perf <- performance(hjy_predi, "auc")
  cat('the auc score is' , hjy_perf@y.values[[1]], "\n")
}
Correctly_Predicted <- mapply(my_fun_3, mylogit, join_data_2014.2018_model)

9###############################
#change row names
###############################
rownames(t) = c("Intercept", "Coverage Level", "13-week", 
                "17-week", "21-week", "26-week",
                "13-week x Coverage Level","17-week x Coverage Level",
                "21-week x Coverage Level", "26-week x Coverage Level",
                "Observation", "R-squared")#, "Correctly Predicted ")




###############################
#split data to two 
table_3 <- t[,1:6]
table_4 <- t[,7:12]
###############################


###############################
#transfer data frame to table 
setDT(table_3,  keep.rownames=TRUE)  ####################################fifth table 
#split data to two 
setDT(table_4,  keep.rownames=TRUE) ###################################sixth table 
###############################


#table------------------5&6

#######################################
#create a dataframe consist by coefficients
#######################################
t <- cbind(mylogit[[1]]$coefficients,mylogit[[2]]$coefficients,
           mylogit[[3]]$coefficients,mylogit[[4]]$coefficients,
           mylogit[[5]]$coefficients,mylogit[[6]]$coefficients,
           mylogit[[7]]$coefficients,mylogit[[8]]$coefficients,
           mylogit[[9]]$coefficients,mylogit[[10]]$coefficients,
           mylogit[[11]]$coefficients,mylogit[[12]]$coefficients)
t <- as.data.frame(t)
#change column names
t <- t %>%  rename(January  = "V1",  February = "V2", March = "V3", April = "V4",
                   May  = "V5", June = "V6", July = "V7", August = "V8",
                   September  = "V9", October = "V10", November = "V11", December = "V12"
)


###############################
#add observation into dataframe
###############################
t[nrow(t) + 1,] <- table_1$Observation


###############################
#add r-square into dataframe                                 
#calculate McFadden's R-squared for each model
###############################
r_square <- vector()
for(i in 1: length(mylogit)){
  r_square[i] <- with(summary(mylogit[[i]]), 1 - deviance/null.deviance)
}
t[nrow(t) + 1,] <- r_square



###############################
#add correctly predicted to dataframe
#run the auc test to check the model performance 
###############################
my_fun_3 <- function(mylogit,join_data_2014.2018_model){
  predicted_probability <- predict(mylogit ,  join_data_2014.2018_model , type = "response")
  join_data_2014.2018_model$predicted_probability <- predicted_probability
  hjy_predi <- prediction(join_data_2014.2018_model$predicted_probability, join_data_2014.2018_model$Net.vs.Actual)
  hjy_perf <- performance(hjy_predi, "auc")
  cat('the auc score is' , hjy_perf@y.values[[1]], "\n")
}
Correctly_Predicted <- mapply(my_fun_3, mylogit, join_data_2014.2018_model)

9###############################
#change row names
###############################
rownames(t) = c("Intercept", "Coverage Level", "13-week", 
                "17-week", "21-week", "26-week",
                "13-week x Coverage Level","17-week x Coverage Level",
                "21-week x Coverage Level", "26-week x Coverage Level",
                "Observation", "R-squared")#, "Correctly Predicted ")


###############################
#split data to two 
table_5 <- t[,1:6]
table_6 <- t[,7:12]
###############################


###############################
#transfer data frame to table 
setDT(table_5,  keep.rownames=TRUE)  ####################################fifth table 
#split data to two 
setDT(table_6,  keep.rownames=TRUE) ###################################sixth table 
###############################




#table------------------7


###############################
#Pre-2019 Subsidy Structure
###############################

table_7_pre_2019.subsidy.structure <- join_data_2014.2018 %>% select(end.date.m,Endorsement.Length.Count,
                                                                     Cost.Per.Cwt.Amount, Livestock.Coverage.Level.Percent)%>%
  mutate(subsidy.level = case_when(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95 ~ "85% - 89.99%",
                             Livestock.Coverage.Level.Percent <= 0.9499 & Livestock.Coverage.Level.Percent >= 0.90 ~ "90% - 94.99%",
                             Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ~ "95% - 100%"), #seperate subsidy rate to 3 groups
         premium.cost = Cost.Per.Cwt.Amount*(1-0.13))%>%  #calculate premium cost under pre-2019 circumstance
           group_by(subsidy.level,Endorsement.Length.Count)%>%
  summarize( subsidy.structure.pre.2019 = mean(premium.cost))




###############################
#2020 Subsidy Structure
###############################
table_7_2020.subsidy.structure <- join_data_2014.2018 %>% select(end.date.m,Endorsement.Length.Count,
                                                                     Cost.Per.Cwt.Amount, Livestock.Coverage.Level.Percent)%>%
  mutate(subsidy.rate = case_when(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95 ~ 0.35,
                                  Livestock.Coverage.Level.Percent <= 0.9499 & Livestock.Coverage.Level.Percent >= 0.90 ~ 0.40,
                                  Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ~ 0.45), #seperate subsidy rate to 3 groups
         premium.cost = Cost.Per.Cwt.Amount*(1-subsidy.rate), #calculate premium cost under pre-2019 circumstance
         subsidy.level = case_when(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95 ~ "85% - 89.99%",
                                  Livestock.Coverage.Level.Percent <= 0.9499 & Livestock.Coverage.Level.Percent >= 0.90 ~ "90% - 94.99%",
                                  Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ~ "95% - 100%"))%>%                                  
  group_by(subsidy.level,Endorsement.Length.Count)%>%
  summarize( subsidy.structure.2020 = mean(premium.cost))


###############################
#cbind two dataframes 
###############################
table_7_pre_2019.subsidy.structure$subsidy.structure.2020 <- table_7_2020.subsidy.structure$subsidy.structure.2020
table_7 <- table_7_pre_2019.subsidy.structure %>% mutate(change = subsidy.structure.pre.2019 - subsidy.structure.2020)

###############################
#transfer data frame to table 
###############################
setDT(table_7,  keep.rownames=TRUE) ###################################seventh table 
    




############################################################################
#figure
############################################################################

#figure ------------------1
###############################
#create dataframe to calculate probability for each coverage length 
###############################
figure_1 <- join_data_2014.2018_1 %>% select(Endorsement.Length.Count, Livestock.Coverage.Level.Percent,
                                             Net.vs.Actual) %>% 
  mutate(subsidy.level = case_when(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95 ~ "85% - 89.99%",
                            Livestock.Coverage.Level.Percent <= 0.9499 & Livestock.Coverage.Level.Percent >= 0.90 ~ "90% - 94.99%",
                            Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ~ "95% - 100%"))%>%
  group_by(Endorsement.Length.Count, subsidy.level)%>%
  summarize(Probability  = table(Net.vs.Actual)[2]/(table(Net.vs.Actual)[1] + table(Net.vs.Actual)[2]))%>%
  group_by(Endorsement.Length.Count)

###############################
#generate the dodge graph 
###############################
figure_1 %>% 
  ggplot(aes(x = as.character(Endorsement.Length.Count), y = Probability, fill =subsidy.level )) +
  geom_col(position = "dodge") 




#figure ------------------2
###############################
#create dataframe contains predicted probability 
###############################
my_fun_3 <- function(mylogit,join_data_2014.2018_model){
  predicted_probability <- predict(mylogit ,  join_data_2014.2018_model , type = "response")
}
predicted_probability <- mapply(my_fun_3, mylogit, join_data_2014.2018_model)


###############################
#print the figure for each month 
###############################
figure_2 <- list()
for(i in 1: length(join_data_2014.2018_model)){
    join_data_2014.2018_model[[i]]$predicted.probability <- predicted_probability[[i]]
    p = ggplot(data = join_data_2014.2018_model[[i]] ,aes(x = Livestock.Coverage.Level.Percent, y = predicted.probability, colour = as.character(Endorsement.Length.Count) ))+
    geom_line()
    figure_2[[i]] = p
    Sys.sleep(2)
    
}





