# r2kMetrics.r
# exploratory plots of R2k index

library(ggplot2)
library(dplyr)
library(data.table)

setwd("/Users/egoodwin/Documents/active/code/R/r2kMetrics/")
r2k.df = read.csv("r2k_screener.csv")
names(r2k.df) = c("Symbol", "Name", "Sector", "Industry", "Market.Cap", 
                  "Ent.Val", "PEratio", "EV.EBITDA", "PSratio", "PBratio")

dt.r2k = data.table(r2k.df)
setkey(dt.r2k, Industry, PEratio)
ddply(dt.r2k, "Industry", summarise, pe = mean(PEratio), evebitda = mean(EV.EBITDA))

summarise(dt.r2k, mean(PEratio))
