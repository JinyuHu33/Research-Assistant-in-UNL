############################################################################
#load required packages 
############################################################################
library(readxl)
library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(scales)
library(fitdistrplus)
############################################################################
#load data
############################################################################

setwd("...")

##options - put
FC.Put.Options.2003.10 <- readxl::read_xlsx("./Data/cme_FC/FC Put Options 2003-10.xlsx", sheet = 1, skip = 2)
FC.Put.Options.2011.15 <- readxl::read_xlsx("./Data/cme_FC/FC Put Options 2011-15.xlsx", sheet = 1, skip = 2)
FC.Put.Options.2016.20 <- readxl::read_xlsx("./Data/cme_FC/FC Put Options 2016-20.xlsx", sheet = 1, skip = 2)
## futures - feeder
FC.Futures.2003.20 <- readxl::read_xlsx("./Data/cme_FC/Feeders_OHLC_Vol_&_OI 2003-20.xlsx", sheet = 1, skip = 2)


############################################################################
##combine, format
############################################################################
cme.fc.put <- rbind(FC.Put.Options.2003.10, FC.Put.Options.2011.15, FC.Put.Options.2016.20) %>% 
  as.data.frame() %>%
  dplyr::rename(., clear.date = `Clear Date`, 
                product.long.desc.options = `Product Long Desc`, option.indicator = `Put Call Indicator`, contract.ym = `Contract Month Year (YYYYMM)`,
                strike.price = `Strike Price`, settle.price = `Settle Price`, volume.options = `Total Volume - CME Group`, implied.volatility = `Implied Volatility`) %>%
  dplyr::mutate(., clear.date = {clear.date %>% as.Date(., format = "%y-%m-%d", origin = "1970-01-01")},
                contract.ym = {contract.ym %>% as.character()}, strike.price = {strike.price/10})

cme.fc.futures <- FC.Futures.2003.20 %>%
  dplyr::rename(., clear.date = `Clear Date`, 
                product.long.desc.futures = `Product Long Desc`, contract.ym = `Contract Month Year (YYYYMM)`,
                close.price = `Settle Price`, open.price = `RTH Open Start Price`, high.price = `RTH High Price`, low.price = `RTH Low Price`,
                open.interest = `Open Interest`, volume.futures = `Total Volume - CME Group`) %>%
  dplyr::mutate(., clear.date = {clear.date %>% as.Date(., format = "%y-%m-%d", origin = "1970-01-01")},
                contract.ym = {contract.ym %>% as.character()})

#check data to make sure the formating is correct
cme.fc.put %>% str
cme.fc.futures %>% str

#Combine Futures and Options Data
cme.fc <- dplyr::left_join(cme.fc.put, cme.fc.futures, by = c("clear.date", "contract.ym")) %>%
  dplyr::mutate(., commodity.main = "feeder.cattle") %>%
  dplyr::select(., commodity.main, contract.ym, clear.date, option.indicator, strike.price, settle.price, close.price)

saveRDS(cme.fc,  file =  "cme.fc.RData")
