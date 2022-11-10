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

# setwd("...")

##options - put
LC.Put.Options.2003.09 <- readxl::read_xlsx("./Data/cme_LC/LC Put Options 2003-09.xlsx", sheet = 1, skip = 2)
LC.Put.Options.2010.14 <- readxl::read_xlsx("./Data/cme_LC/LC Put Options 2010-14.xlsx", sheet = 1, skip = 2)
LC.Put.Options.2015.20 <- readxl::read_xlsx("./Data/cme_LC/LC Put Options 2015-20.xlsx", sheet = 1, skip = 2)
## futures - fed/live cattle 
LC.Futures.2003.20 <- readxl::read_xlsx("./Data/cme_LC/LC_OHLC_Vol_&_OI 2003-20.xlsx", sheet = 1, skip = 2)


############################################################################
##combine, format
############################################################################
cme.lc.put <- rbind(LC.Put.Options.2003.09, LC.Put.Options.2010.14, LC.Put.Options.2015.20) %>% 
  as.data.frame() %>%
  dplyr::rename(., clear.date = `Clear Date`, 
                product.long.desc.options = `Product Long Desc`, option.indicator = `Put Call Indicator`, contract.ym = `Contract Month Year (YYYYMM)`,
                strike.price = `Strike Price`, settle.price = `Settle Price`, volume.options = `Total Volume - CME Group`, implied.volatility = `Implied Volatility`) %>%
  dplyr::mutate(., clear.date = {clear.date %>% as.Date(., format = "%y-%m-%d", origin = "1970-01-01")},
                contract.ym = {contract.ym %>% as.character()}, strike.price = {strike.price/10})

cme.lc.futures <- LC.Futures.2003.20 %>%
  dplyr::rename(., clear.date = `Clear Date`, 
                product.long.desc.futures = `Product Long Desc`, contract.ym = `Contract Month Year (YYYYMM)`,
                close.price = `Settle Price`, open.price = `RTH Open Start Price`, high.price = `RTH High Price`, low.price = `RTH Low Price`,
                open.interest = `Open Interest`, volume.futures = `Total Volume - CME Group`) %>%
  dplyr::mutate(., clear.date = {clear.date %>% as.Date(., format = "%y-%m-%d", origin = "1970-01-01")},
                contract.ym = {contract.ym %>% as.character()})

#check data to make sure the formating is correct
cme.lc.put %>% str
cme.lc.futures %>% str

#Combine Futures and Options Data
cme.lc <- dplyr::left_join(cme.lc.put, cme.lc.futures, by = c("clear.date", "contract.ym")) %>%
  dplyr::mutate(., commodity = "feeder.cattle") %>%
  dplyr::select(., commodity, contract.ym, clear.date, option.indicator, strike.price, settle.price, close.price)

saveRDS(cme.lc,"./Out_Data/cme_lc.RData")
