# r2kMetrics.r
# exploratory plots of R2k index

library(ggplot2)
library(dplyr)
library(data.table)

setwd("/Users/egoodwin/Documents/active/code/R/r2kMetrics/")

## read in data file
r2k.df = read.csv("r2k_screener.csv", header=TRUE, stringsAsFactors=FALSE)

## convert column names
names(r2k.df) = c("Symbol", "Name", "Sector", "Industry", "Market.Cap", 
                  "Ent.Val", "PEratio", "EV.EBITDA", "PSratio", "PBratio")

## data conversion
r2k.df$Market.Cap = as.numeric(r2k.df$Market.Cap)
r2k.df$Ent.Val = as.numeric(r2k.df$Ent.Val)
r2k.df$PEratio = as.numeric(r2k.df$PEratio)
r2k.df$EV.EBITDA = as.numeric(r2k.df$EV.EBITDA)
r2k.df$PSratio = as.numeric(r2k.df$PSratio)
r2k.df$PBratio = as.numeric(r2k.df$PBratio)

## set NA to 0
r2k.df[is.na(r2k.df)] = 0
r2k.df$TotMktCap = 0

## get total mkt cap and ent val for each industry
r2kTotMktCap = r2k.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
r2kTotEntVal = r2k.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))

iterator= r2kTotMktCap$Sector
for(i in iterator) {
  tmc = r2kTotMktCap[r2kTotMktCap$Sector == i,]$mktcaptot[1]
  r2k.df[r2k.df$Sector == i,]$TotMktCap = tmc 
}
## add total market cap and ent values as columns by Sector
q = c(0.34, 0.50, 0.66)
r2k.df
r2ksumm = r2k.df %>% 
  group_by (Sector) %>% 
  summarise_each(funs(median), 
                 EV.EBITDA*Market.Cap/TotMktCap, 
                 PEratio*Market.Cap/TotMktCap,
                 PSratio*Market.Cap/TotMktCap,
                 PBratio*Market.Cap/TotMktCap)

