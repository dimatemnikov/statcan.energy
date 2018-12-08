library(dplyr)
library(tidyverse)
library(extrafont)
font_import("Futura.ttc")


#Download latest csv table from StatCan, unzip, load, rename, move raw to new directory.
dir.create("Supply and demand of primary and secondary energy in terajoules, annual")
setwd("Supply and demand of primary and secondary energy in terajoules, annual")
zipfile <- "25100029.zip"
download.file("https://www150.statcan.gc.ca/n1/en/tbl/csv/25100029-eng.zip?st=1kd8812u",zipfile)
unzip(zipfile)
raw <- read.csv("25100029.csv")
file.remove(zipfile)
dir.create("Raw Data")
file.rename("25100029.csv", "Raw Data/rawdata.csv")
file.rename("25100029_MetaData.csv", "Raw Data/metadata.csv")


#head(raw)
#unique(raw$GEO)
#unique(raw$Supply.and.demand.characteristics)

#take energy demand for Canada, all years
CA_demand <- raw %>% filter(Supply.and.demand.characteristics == "Energy use, final demand",
                                   GEO == "Canada")

#take only total primary and secondary demand
CA_total_demand <- CA_demand %>% filter(Fuel.type == "Total primary and secondary energy")

#Line plot of Canada total prim and sec energy demand
p <- ggplot(CA_total_demand, aes(REF_DATE, VALUE/1000000)) +
  geom_line() +
  geom_point() +
  ggtitle("Canada Total Primary and Secondary Energy Demand") +
  xlab("\nYear") +
  ylab("Millions of Terajoules\n") +
  labs(caption = "Source: Statistics Canada.  Table  25-10-0029-01   Supply and demand of primary and secondary energy in terajoules, annual") +
  expand_limits(x = 1995, y = 0) +
  theme_minimal() +
  theme(text = element_text(family = "KeepCalm-Medium"))+
  theme(plot.caption=element_text(size=5)) 

p

unique(raw$Fuel.type)
#take demand for Canada, select fuels, all years
CA_fuel_demand <- raw %>% filter(GEO == "Canada",
                                 Fuel.type %in% c("Total coal",
                                                  "Crude oil",
                                                  "Natural gas",
                                                  "Primary electricity, hydro and nuclear",
                                                  "Total refined petroleum products",
                                                  "Secondary electricity, thermal",
                                                  "Non-energy products"))

#Line plot of Canada total energy demand
q <- ggplot(CA_fuel_demand, aes(REF_DATE, VALUE/1000000,colour=Fuel.type)) +
  geom_line(aes(group = Fuel.type)) +
  xlab("Year") +
  ylab("Millions of Terajoules") +
  labs(caption = "Source: Statistics Canada.  Table  25-10-0029-01   Supply and demand of primary and secondary energy in terajoules, annual") +
  expand_limits(x = 1995, y = 0) +
  theme_minimal() +
  theme(text = element_text(family = "KeepCalm-Medium"))+
  theme(plot.caption=element_text(size=5)) 


q

