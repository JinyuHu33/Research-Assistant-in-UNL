############################################################################
#required package
############################################################################

library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(str2str)
library(ggplot2)
library(patchwork) 
library(hrbrthemes)
library(lubridate)
library(anytime)
library(clock)
memory.limit(size = 60000)

############################################################################
#load the data lrp data merged with feeder cattle index and clean a bit
############################################################################

setwd("C:/Users/edennis8/Dropbox/Nebraska/students/hu_jinyu/Out_Data")

join_data_2002.2022 <- readRDS("./Feeder_cattle_index_join.RData")

#filter years to 2014 - 2020                                
join_data_2014.2020 <- join_data_2002.2022 %>% filter(Commodity.Year >= 2014 & Commodity.Year <= 2020)%>%

  filter(Endorsement.Length.Count <= 30 )%>%                #exclude endorsement length which over 30 hours  
           filter(Livestock.Coverage.Level.Percent >= 0.85)%>%                  #exclude coverage level which smaller 0.85
  filter(Type.Code == "steer.wt.2"|                     #focus on feeder cattles from 600 - 900 pound(inclusive or exclusive???) only heifers and steers
         Type.Code == "heifer.wt.2"|
           Type.Code == "brahman.wt.2" |
           Type.Code == "dairy.wt.2"
           
          )%>%
  select(Sales.Effective.Date, Livestock.Coverage.Level.Percent,
         Endorsement.Length.Count, Expected.Ending.Value.Amount,
         Coverage.Price, Cost.Per.Cwt.Amount, End.Date, index.price, end.date.m, end.date.yr )   ########why is not actual ending amount but index.price???



###############################
#Net price and Indemnity.Price under both pre-2019 and 2020
###############################

join_data_2014.2020_1 <- join_data_2014.2020 %>% mutate(index.price = as.numeric(index.price),
                                                        Indemnity.Price = ifelse(Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount - index.price > 0,
                                                                                 Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount - index.price,0),  #calculate Indeminity Price
                                                        Net.Price = ifelse(end.date.yr == "2014"|
                                                                             end.date.yr == "2014"|
                                                                             end.date.yr == "2015"|
                                                                             end.date.yr == "2016"|
                                                                             end.date.yr == "2017"|
                                                                             end.date.yr == "2018",
                                                                           
                                                                           index.price + Indemnity.Price - Cost.Per.Cwt.Amount*(1- 0.13),
                                                                           
                                                                           ifelse(Livestock.Coverage.Level.Percent <= 1 & Livestock.Coverage.Level.Percent >= 0.95, 
                                                                                  index.price + Indemnity.Price - Cost.Per.Cwt.Amount*(1- 0.35), 
                                                                                  ifelse( Livestock.Coverage.Level.Percent <= 0.9499  & Livestock.Coverage.Level.Percent >= 0.90, 
                                                                                          index.price + Indemnity.Price - Cost.Per.Cwt.Amount*(1- 0.40),
                                                                                          ifelse(Livestock.Coverage.Level.Percent <= 0.8999 & Livestock.Coverage.Level.Percent >= 0.85 ,
                                                                                                 index.price + Indemnity.Price - Cost.Per.Cwt.Amount*(1- 0.45),0)
                                                                                  )
                                                                           )
                                                        ),
                                                        contract.ym.lrp = paste0(end.date.yr,'-',end.date.m)
)                                    #calculate the New Price accoring to it's year 




############################################################################
#figure
############################################################################
#figure ------------------1

#####################
#build the dataframe that is consisted by month
#####################

join_data_2014.2020_figure_1 <- join_data_2014.2020_1 %>% select(Indemnity.Price, Expected.Ending.Value.Amount, index.price, contract.ym.lrp
)%>% 
  group_by(contract.ym.lrp)%>%
  summarize(Indemnity.Payment = mean(Indemnity.Price),
            Ending.Price = mean(index.price),
            Expected.Ending.Price = mean( Expected.Ending.Value.Amount)
  )
join_data_2014.2020_figure_1 <- join_data_2014.2020_figure_1 %>%  mutate(contract.ym.lrp = anydate(contract.ym.lrp))




#####################
#plot the figure
#####################
q <-  ggplot(join_data_2014.2020_figure_1 , aes(x = contract.ym.lrp,stat="identity" )) +
  geom_line(aes(y=Ending.Price), stat="identity",color="blue",size=1) +
  geom_line(aes(y = Expected.Ending.Price), stat="identity",color="red",size=1)+
  geom_bar(aes(y = Indemnity.Payment), stat = "identity", colour = "azure3")+
  scale_y_continuous(
    name =  "price",
    sec.axis = sec_axis(~./10, name = "Indemnity"))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = '4 months') +
  theme_ipsum() 

q + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Figure 1. Average Monthly Expected Price, Actual Ending Price, and LRP Indemnity Payment")
#add label beside 



#figure ------------------2
#####################
#build the dataframe that is consisted by month and year
#####################
join_data_2014.2020_2 <- join_data_2014.2020 %>% mutate(index.price = as.numeric(index.price))%>%
                                                          mutate(
  Year = ifelse(end.date.yr == 2014 |
                  end.date.yr == 2015 |
                  end.date.yr == 2016 |
                  end.date.yr == 2017 |
                  end.date.yr == 2018,"2014-2018 Average Probability of Payment",
                ifelse(end.date.yr == 2019,"2019 Average Probability of Payment", "2020 Average Probability of Payment" ))) %>%
  group_by(end.date.m)%>%
  mutate(Indemnity.Received = ifelse(Livestock.Coverage.Level.Percent * Expected.Ending.Value.Amount - index.price > 0,
                                     1,0)) %>% group_by(Year,end.date.m) %>%
  summarize(Probability = table(Indemnity.Received)[1]/(table(Indemnity.Received)[1]+table(Indemnity.Received[2])))%>%
  mutate(end.date.m.index = case_when(end.date.m == 1 ~ "January",
                                      end.date.m == 2 ~ "February",
                                      end.date.m == 3 ~ "March",
                                      end.date.m == 4 ~ "April",
                                      end.date.m == 5 ~ "May",
                                      end.date.m == 6 ~ "June",
                                      end.date.m == 7 ~ "July",
                                      end.date.m == 8 ~ "August",
                                      end.date.m == 9 ~ "September",
                                      end.date.m == 10 ~ "October",
                                      end.date.m == 11 ~ "November",
                                      end.date.m == 12 ~ "December"))%>% 
  mutate(end.date.m.index = factor(end.date.m.index, levels=month.name),
         Probability = as.numeric(Probability),
         Year = as.factor(Year)) #as.factor for month to make order to the plot x-axis




#####################
#plot the figure
#####################

q <-  ggplot(join_data_2014.2020_2 , aes(x =end.date.m.index , y = Probability, group = Year, color = Year )) +
  geom_line(size = 1)+
  ggtitle("Figure 2. Average Monthly Probability of Receiving an Indemnity") +
  labs(x = "Month", y = "Probability") +
  theme_ipsum() 
q + theme(axis.text.x=element_text(angle=70, hjust=1)) 



#figure ------------------3

#####################
#build the dataframe that is consisted by month and year
#####################
join_data_2014.2020_3 <- join_data_2014.2020_1 %>% select(end.date.m, end.date.yr,Indemnity.Price) %>%
  mutate(Year = ifelse(end.date.yr == 2014 |
                         end.date.yr == 2015 |
                         end.date.yr == 2016 |
                         end.date.yr == 2017 |
                         end.date.yr == 2018,"2014-2018",
                       ifelse(end.date.yr == 2019,"2019", "2020" )))%>% group_by(Year,end.date.m )%>%
  summarize(Indemnity.Payment = mean(Indemnity.Price))%>%
  mutate(end.date.m.index = case_when(end.date.m == 1 ~ "January",
                                      end.date.m == 2 ~ "February",
                                      end.date.m == 3 ~ "March",
                                      end.date.m == 4 ~ "April",
                                      end.date.m == 5 ~ "May",
                                      end.date.m == 6 ~ "June",
                                      end.date.m == 7 ~ "July",
                                      end.date.m == 8 ~ "August",
                                      end.date.m == 9 ~ "September",
                                      end.date.m == 10 ~ "October",
                                      end.date.m == 11 ~ "November",
                                      end.date.m == 12 ~ "December",
  ))%>% 
  mutate(end.date.m.index = factor(end.date.m.index, levels=month.name))

#####################
#plot the figure
#####################
q <-  ggplot(join_data_2014.2020_3 , aes(x = end.date.m.index , y = Indemnity.Payment, fill = Year)) +
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Figure 3. Average Monthly LRP Indemnity Payments") +
  labs(x = "Month", y = "$/cwt")+
  theme_ipsum() 

q + theme(axis.text.x=element_text(angle=70, hjust=1)) 



#figure ------------------4


#####################
#build the dataframe that is consisted by month and year
#####################
join_data_2014.2020_4 <- join_data_2014.2020_1 %>% select(end.date.m, end.date.yr,Indemnity.Price, Cost.Per.Cwt.Amount) %>%
  mutate(Year = ifelse(end.date.yr == 2014 |
                         end.date.yr == 2015 |
                         end.date.yr == 2016 |
                         end.date.yr == 2017 |
                         end.date.yr == 2018,"2014-2018",
                       ifelse(end.date.yr == 2019,"2019", "2020" )))%>% group_by(Year,end.date.m )%>%
  summarize(Average.Monthly.Difference = mean(Indemnity.Price - Cost.Per.Cwt.Amount))%>%
  mutate(end.date.m.index = case_when(end.date.m == 1 ~ "January",
                                      end.date.m == 2 ~ "February",
                                      end.date.m == 3 ~ "March",
                                      end.date.m == 4 ~ "April",
                                      end.date.m == 5 ~ "May",
                                      end.date.m == 6 ~ "June",
                                      end.date.m == 7 ~ "July",
                                      end.date.m == 8 ~ "August",
                                      end.date.m == 9 ~ "September",
                                      end.date.m == 10 ~ "October",
                                      end.date.m == 11 ~ "November",
                                      end.date.m == 12 ~ "December",
  ))%>% 
  mutate(end.date.m.index = factor(end.date.m.index, levels=month.name))

#####################
#plot the figure
#####################
q <-  ggplot(join_data_2014.2020_4 , aes(x = end.date.m.index , y = Average.Monthly.Difference, fill = Year)) +
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Figure 4. Average Monthly Difference in Indemnity and Producer Premiums") +
  labs(x = "Month", y = "$/cwt") +
  theme_ipsum() 
q + theme(axis.text.x=element_text(angle=70, hjust=1)) 








