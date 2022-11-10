#  Research Assistant job in UNL Agriculture Economic Department

All the scripts in this folder are tasks that I completed in the summer 2022 research assistant job. The supervisor is Dr.Elliott Dennis. 

1.Scraping the data: 


Scraped 60,000,000+ LRP data during years 2014 - 2023 from https://ftp.rma.usda.gov/pub/references/adm_livestock/, and it is mainly composed of LRP data and Archive data.

2.Formating the data:


Cleaned the raw data from scraping, and applied feature engineering to generate new features. Bound 12 large datasets together, and prepared for future modeling.

3.cme_feeder 


Cleaned the data which contains CME price for feeder cattle.

4.cme_live


Cleaned the data which contains CME price for live cattle.

5.direct_cattle_price


Cleaned the direct cattle dataset. 
Calculated based on the average price of the week. However, the raw data contains missing dates, it cannot be directly calculated as group by “week”. This script used the method of calculating the difference of dates to address missing dates, and filled in the gaps to accurately calculate the weekly average direct cattle price from 2001-04-09 to 2022-06-06.

6.feedercattleindex


Calculated the index price for feeder cattle by its Type.Code according to the marketing sclae:

                                                                                                          Example:
                                                                                                          Type.Code == "steer.wt.2", index.price*1
                                                                                                          Type.Code == "steer.wt.1", index.price*1.1
                                                                                                          Type.Code == "heifer.wt.2", index.price*1
                                                                                                          Type.Code == "heifer.wt.1", index.price*0.9
                                                                                                          ...
7.Subsidy rate change


Identified changes in subsidy rates for LRP for Feeder Cattle. LRP data from 2014 to 2018 were leveraged to estimate monthly logit models to determine the likelihood of outcomes under the previous rate and the new structure; the result shows the new subsidy structure appears to be helpful for producers by lowering the cost of LRP.

8.Probability analysis modeling


Conducted probability analysis of indemnity pay-outs from feeder cattle LRP insurance. Estimated a monthly probit model to determine the coverage level and length to ensure the least financial loss for the producers; found that in the four marketing months a producer could purchase an LRP contract that would result in lower costs more than 50% of time. 

9. LRP during COVID19


Responsible for LRP Payment for Feeder Cattle analysis during COVID-19, compared descriptive statistics before and after COVID-19, and LRP Indemnity Payment was visualized for further comparison.


                                                            

