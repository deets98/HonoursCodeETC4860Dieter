


# deleting everything
rm(list = ls())
# set working directory
# required packages
library(data.table)
library(dplyr)
library(bit64)
library(reshape2)
library(lubridate)
library(stringr)
library(rgdal)
library(maptools)
library(raster)
library(sp)
library(sf)
library(lwgeom)
library(readxl)
library(tools)
library(tidyr)
'%!in%' = Negate('%in%')
#grateful::cite_packages(out.format = "docx", out.dir = "C:/Users/Dieter/Downloads", pkgs = "Session")


yearquarter <- sort(apply(expand.grid( paste0( 2000:2021, "Q" ), 1:4 ), 1, paste, collapse="" ))
yearquarter <- c(yearquarter,"2022Q1")



set.seed(28804783)
FileSource <- "F:/Honours/Data/Mortgage Data/OutputsV3/dataloanlevel"
#yq <- "2000Q1"
InDataF <- NA
InDataNF <- NA

for (yq in yearquarter){
  print(yq)
  newdata <- fread(paste0(FileSource, yq, ".csv"))
  ForeclosedLoans <- newdata %>% filter(del_for == 1)
  ForeclosedLoans <- ForeclosedLoans$loan_id %>% unique()
  
  samplednewdataF <- newdata %>% filter(del_for == 1) %>%  group_by(loan_id) %>% slice_sample(n=1) %>% ungroup()
  samplednewdataNF <- newdata %>% filter(loan_id %!in% ForeclosedLoans) %>% group_by(loan_id) %>% slice_sample(n=1) %>% ungroup()
  if(yq == "200Q1"){
    InDataF <- samplednewdataF
    InDataNF <- samplednewdataNF
    
  } else {
    InDataF <- rbind(InDataF,samplednewdataF)
    InDataNF <- rbind(InDataNF,samplednewdataNF)
  }
  gc()
  rm(newdata)
  rm(samplednewdata)
}



InDataF <- InDataF[2:nrow(InDataF),]
InDataNF <- InDataNF[2:nrow(InDataNF),]

SampledData <- rbind(InDataNF, InDataF)
SampledData %>% fwrite("F:/Honours/Data/Mortgage Data/Outputs V2 FM/SampledAllFLoans.csv")
