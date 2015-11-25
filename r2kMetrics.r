# r2kMetrics.r
# exploratory plots of R2k index

library(ggplot2)
library(dplyr)
library(data.table)

setwd("/Users/egoodwin/Documents/active/code/R/r2kMetrics/")

## read in data file
r2k.df = read.csv("r2k_screener2.csv", header=TRUE, stringsAsFactors=FALSE)

## convert column names
names(r2k.df) = c("Symbol", "Name", "Sector", "Industry", "Market.Cap", 
                  "Ent.Val", "PEratio", "EV.EBITDA", "PSratio", "PBratio",
                  "PEmed", "EV2EBITmed", "PSmed","PBmed", "ROEmed", "ROAmed")

## data conversion
r2k.df$Market.Cap = as.numeric(r2k.df$Market.Cap)
r2k.df$Ent.Val = as.numeric(r2k.df$Ent.Val)
r2k.df$PEratio = as.numeric(r2k.df$PEratio)
r2k.df$EV.EBITDA = as.numeric(r2k.df$EV.EBITDA)
r2k.df$PSratio = as.numeric(r2k.df$PSratio)
r2k.df$PBratio = as.numeric(r2k.df$PBratio)
r2k.df$PEmed = as.numeric(r2k.df$PEmed)
r2k.df$EV2EBITmed = as.numeric(r2k.df$EV2EBITmed)
r2k.df$PSmed = as.numeric(r2k.df$PSmed)
r2k.df$PBmed = as.numeric(r2k.df$PBmed)
r2k.df$ROEmed = as.numeric(r2k.df$ROEmed)
r2k.df$ROAmed = as.numeric(r2k.df$ROAmed)

## set NA to 0
r2k.df[is.na(r2k.df)] = 0
r2k.df$TotMktCap = 0
r2k.df$TotEntVal = 0

## get total mkt cap and ent val for each industry
r2kTotMktCap = r2k.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
r2kTotEntVal = r2k.df %>% group_by(Sector) %>% summarise(entvaltot = sum(Ent.Val, na.rm=TRUE))

iterator= r2kTotMktCap$Sector
for(i in iterator) {
  tmc = r2kTotMktCap[r2kTotMktCap$Sector == i,]$mktcaptot[1]
  r2k.df[r2k.df$Sector == i,]$TotMktCap = as.numeric(tmc)
  
  tev = r2kTotEntVal[r2kTotEntVal$Sector == i,]$entvaltot[1]
  r2k.df[r2k.df$Sector == i,]$TotEntVal = as.numeric(tev)
}

## set up weighted value columns
r2k.df$wEV2EBIT = r2k.df$EV.EBITDA * r2k.df$Market.Cap / r2k.df$TotMktCap
r2k.df$wPE = r2k.df$PEratio * r2k.df$Market.Cap / r2k.df$TotMktCap
r2k.df$wPS = r2k.df$PSratio * r2k.df$Market.Cap / r2k.df$TotMktCap
r2k.df$wPB = r2k.df$PBratio * r2k.df$Market.Cap / r2k.df$TotMktCap

## get median weighted valuation metrics by industry
r2ksumm = r2k.df %>% 
    group_by(Sector) %>% 
    summarise_each(funs(median), wEV2EBIT, wPE, wPS, wPB) 
 
r2ksumm