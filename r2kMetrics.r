## r2kMetrics.r
## summary statistics for the Russell 2000 index
## based on Ycharts screen ccR2kmetrics
##

library(ggplot2)
library(dplyr)
library(data.table)

# setwd("/Users/egoodwin/Documents/active/code/R/r2kMetrics/")
setwd("/Users/egoodwin/Documents/code/r2kMetrics/")

## read in data file
r2k.df = read.csv("r2k_screener2.csv", header=TRUE, stringsAsFactors=FALSE)

## convert column names
names(r2k.df) = c("Symbol", "Name", "Sector", "Industry", "Market.Cap",
                  "Ent.Val", "PEratio", "EV.EBITDA", "PSratio", "PBratio",
                  "PEmed", "EV.EBITDAmed", "PSmed","PBmed", "ROEmed", "ROAmed")

## data conversion
r2k.df$Market.Cap = as.numeric(r2k.df$Market.Cap)
r2k.df$Ent.Val = as.numeric(r2k.df$Ent.Val)
r2k.df$PEratio = as.numeric(r2k.df$PEratio)
r2k.df$EV.EBITDA = as.numeric(r2k.df$EV.EBITDA)
r2k.df$PSratio = as.numeric(r2k.df$PSratio)
r2k.df$PBratio = as.numeric(r2k.df$PBratio)
r2k.df$PEmed = as.numeric(r2k.df$PEmed)
r2k.df$EV.EBITDAmed = as.numeric(r2k.df$EV.EBITDAmed)
r2k.df$PSmed = as.numeric(r2k.df$PSmed)
r2k.df$PBmed = as.numeric(r2k.df$PBmed)
r2k.df$ROEmed = as.numeric(r2k.df$ROEmed)
r2k.df$ROAmed = as.numeric(r2k.df$ROAmed)

## set up P/S Comparisons
PSComp.df = data.frame("Symbol" = r2k.df$Symbol,
                       "Name" = r2k.df$Name, 
                       "Sector" = r2k.df$Sector, 
                       "PSratio" = r2k.df$PSratio,
                       "PSmed" = r2k.df$PSmed,
                       "Market.Cap" = r2k.df$Market.Cap,
                       "Ent.Val" = r2k.df$Ent.Val)

PSComp.df = PSComp.df[complete.cases(PSComp.df),]
PSComp.df$TotMktCap = 0
PSComp.df$TotEntVal = 0

## get total mkt cap and ent val for each industry
PSCompTotMktCap = PSComp.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
PSCompTotEntVal = PSComp.df %>% group_by(Sector) %>% summarise(entvaltot = sum(Ent.Val, na.rm=TRUE))

iterator= PSCompTotMktCap$Sector
for(i in iterator) {
  tmc = PSCompTotMktCap[PSCompTotMktCap$Sector == i,]$mktcaptot[1]
  PSComp.df[PSComp.df$Sector == i,]$TotMktCap = as.numeric(tmc)
  
  tev = PSCompTotEntVal[PSCompTotEntVal$Sector == i,]$entvaltot[1]
  PSComp.df[PSComp.df$Sector == i,]$TotEntVal = as.numeric(tev)
}

## set up weighted value columns
PSComp.df$wPS = PSComp.df$PSratio * PSComp.df$Market.Cap / PSComp.df$TotMktCap
PSComp.df$wPS5 = PSComp.df$PSmed * PSComp.df$Market.Cap / PSComp.df$TotMktCap

## get median weighted valuation metrics by industry
PSCompsumm = PSComp.df %>%
  group_by(Sector) %>%
  summarise_each(funs(sum), wPS, wPS5)

head(PSCompsumm)

## set up P/E comps
PEComp.df = data.frame("Symbol" = r2k.df$Symbol,
                       "Name" = r2k.df$Name, 
                       "Sector" = r2k.df$Sector, 
                       "PEratio" = r2k.df$PEratio,
                       "PEmed" = r2k.df$PEmed,
                       "Market.Cap" = r2k.df$Market.Cap,
                       "Ent.Val" = r2k.df$Ent.Val)

PEComp.df = PEComp.df[complete.cases(PEComp.df),]
PEComp.df$TotMktCap = 0
PEComp.df$TotEntVal = 0

## get total mkt cap and ent val for each industry
PECompTotMktCap = PEComp.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
PECompTotEntVal = PEComp.df %>% group_by(Sector) %>% summarise(entvaltot = sum(Ent.Val, na.rm=TRUE))

iterator= PECompTotMktCap$Sector
for(i in iterator) {
  tmc = PECompTotMktCap[PECompTotMktCap$Sector == i,]$mktcaptot[1]
  PEComp.df[PEComp.df$Sector == i,]$TotMktCap = as.numeric(tmc)
  
  tev = PECompTotEntVal[PECompTotEntVal$Sector == i,]$entvaltot[1]
  PEComp.df[PEComp.df$Sector == i,]$TotEntVal = as.numeric(tev)
}

## set up weighted value columns
PEComp.df$wPE = PEComp.df$PEratio * PEComp.df$Market.Cap / PEComp.df$TotMktCap
PEComp.df$wPE5 = PEComp.df$PEmed * PEComp.df$Market.Cap / PEComp.df$TotMktCap

## get median weighted valuation metrics by industry
PECompsumm = PEComp.df %>%
  group_by(Sector) %>%
  summarise_each(funs(sum), wPE, wPE5)

head(PECompsumm)

## set up P/B comps
PBComp.df = data.frame("Symbol" = r2k.df$Symbol,
                       "Name" = r2k.df$Name, 
                       "Sector" = r2k.df$Sector, 
                       "PBratio" = r2k.df$PBratio,
                       "PBmed" = r2k.df$PBmed,
                       "Market.Cap" = r2k.df$Market.Cap,
                       "Ent.Val" = r2k.df$Ent.Val)

PBComp.df = PBComp.df[complete.cases(PBComp.df),]
PBComp.df$TotMktCap = 0
PBComp.df$TotEntVal = 0

## get total mkt cap and ent val for each industry
PBCompTotMktCap = PBComp.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
PBCompTotEntVal = PBComp.df %>% group_by(Sector) %>% summarise(entvaltot = sum(Ent.Val, na.rm=TRUE))

iterator= PBCompTotMktCap$Sector
for(i in iterator) {
  tmc = PBCompTotMktCap[PBCompTotMktCap$Sector == i,]$mktcaptot[1]
  PBComp.df[PBComp.df$Sector == i,]$TotMktCap = as.numeric(tmc)
  
  tev = PBCompTotEntVal[PBCompTotEntVal$Sector == i,]$entvaltot[1]
  PBComp.df[PBComp.df$Sector == i,]$TotEntVal = as.numeric(tev)
}

## set up weighted value columns
PBComp.df$wPB = PBComp.df$PBratio * PBComp.df$Market.Cap / PBComp.df$TotMktCap
PBComp.df$wPB5 = PBComp.df$PBmed * PBComp.df$Market.Cap / PBComp.df$TotMktCap

## get median weighted valuation metrics by industry
PBCompsumm = PBComp.df %>%
  group_by(Sector) %>%
  summarise_each(funs(sum), wPB, wPB5)

head(PBCompsumm)

## set up EV/EBITDA compares
EV.EBITDAComp.df = data.frame("Symbol" = r2k.df$Symbol,
                       "Name" = r2k.df$Name, 
                       "Sector" = r2k.df$Sector, 
                       "EV.EBITDA" = r2k.df$EV.EBITDA,
                       "EV.EBITDAmed" = r2k.df$EV.EBITDAmed,
                       "Market.Cap" = r2k.df$Market.Cap,
                       "Ent.Val" = r2k.df$Ent.Val)

EV.EBITDAComp.df = EV.EBITDAComp.df[complete.cases(EV.EBITDAComp.df),]
EV.EBITDAComp.df$TotMktCap = 0
EV.EBITDAComp.df$TotEntVal = 0

## get total mkt cap and ent val for each industry
EV.EBITDACompTotMktCap = EV.EBITDAComp.df %>% group_by(Sector) %>% summarise(mktcaptot = sum(Market.Cap, na.rm=TRUE))
EV.EBITDACompTotEntVal = EV.EBITDAComp.df %>% group_by(Sector) %>% summarise(entvaltot = sum(Ent.Val, na.rm=TRUE))

iterator = EV.EBITDACompTotMktCap$Sector
for(i in iterator) {
  tmc = EV.EBITDACompTotMktCap[EV.EBITDACompTotMktCap$Sector == i,]$mktcaptot[1]
  EV.EBITDAComp.df[EV.EBITDAComp.df$Sector == i,]$TotMktCap = as.numeric(tmc)
  
  tev = EV.EBITDACompTotEntVal[EV.EBITDACompTotEntVal$Sector == i,]$entvaltot[1]
  EV.EBITDAComp.df[EV.EBITDAComp.df$Sector == i,]$TotEntVal = as.numeric(tev)
}

## set up weighted value columns
EV.EBITDAComp.df$wEV.EBITDA = EV.EBITDAComp.df$EV.EBITDA * 
  EV.EBITDAComp.df$Market.Cap / EV.EBITDAComp.df$TotMktCap

EV.EBITDAComp.df$wEV.EBITDA5 = EV.EBITDAComp.df$EV.EBITDAmed * 
  EV.EBITDAComp.df$Market.Cap / EV.EBITDAComp.df$TotMktCap

## get median weighted valuation metrics by industry
EV.EBITDACompsumm = EV.EBITDAComp.df %>%
  group_by(Sector) %>%
  summarise_each(funs(sum), wEV.EBITDA, wEV.EBITDA5)

head(EV.EBITDACompsumm)

r2ksumm = merge(merge(merge(PSCompsumm, PECompsumm, by="Sector"), 
                      PBCompsumm, by = "Sector"), EV.EBITDACompsumm, by="Sector")
r2ksumm

## boxplots of distributions
# p = ggplot(subset(r2k.df, PEratio<=30), aes(Sector, PEratio)) + geom_boxplot()
# p
