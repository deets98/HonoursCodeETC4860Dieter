####################################################################
# create mortgage and economic data ################################
####################################################################


####################################################################
# 0. setup #########################################################
####################################################################
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
library(grateful)
library(tidyr)
#grateful::cite_packages(out.format = "docx", out.dir = "C:/Users/Dieter/Downloads", pkgs = "Session")
'%!in%' = Negate('%in%')
####################################################################


####################################################################
# 1. organise the fannie mae data ##################################
####################################################################

####################################################################
# 1.1. setup #######################################################
####################################################################
# deleting everything
#rm(list = ls())
#setwd( "D:/Fannie Mae Data" )
# required packages
library(data.table)
library(dplyr)
library(bit64)
# set threads
setDTthreads(12)
# defining the Loan Performance table headers
lppub_column_names <- c("POOL_ID", "LOAN_ID", "ACT_PERIOD", "CHANNEL", "SELLER", "SERVICER",
                        "MASTER_SERVICER", "ORIG_RATE", "CURR_RATE", "ORIG_UPB", "ISSUANCE_UPB",
                        "CURRENT_UPB", "ORIG_TERM", "ORIG_DATE", "FIRST_PAY", "LOAN_AGE",
                        "REM_MONTHS", "ADJ_REM_MONTHS", "MATR_DT", "OLTV", "OCLTV",
                        "NUM_BO", "DTI", "CSCORE_B", "CSCORE_C", "FIRST_FLAG", "PURPOSE",
                        "PROP", "NO_UNITS", "OCC_STAT", "STATE", "MSA", "ZIP", "MI_PCT",
                        "PRODUCT", "PPMT_FLG", "IO", "FIRST_PAY_IO", "MNTHS_TO_AMTZ_IO",
                        "DLQ_STATUS", "PMT_HISTORY", "MOD_FLAG", "MI_CANCEL_FLAG", "Zero_Bal_Code",
                        "ZB_DTE", "LAST_UPB", "RPRCH_DTE", "CURR_SCHD_PRNCPL", "TOT_SCHD_PRNCPL",
                        "UNSCHD_PRNCPL_CURR", "LAST_PAID_INSTALLMENT_DATE", "FORECLOSURE_DATE",
                        "DISPOSITION_DATE", "FORECLOSURE_COSTS", "PROPERTY_PRESERVATION_AND_REPAIR_COSTS",
                        "ASSET_RECOVERY_COSTS", "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS",
                        "ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY", "NET_SALES_PROCEEDS",
                        "CREDIT_ENHANCEMENT_PROCEEDS", "REPURCHASES_MAKE_WHOLE_PROCEEDS",
                        "OTHER_FORECLOSURE_PROCEEDS", "NON_INTEREST_BEARING_UPB", "PRINCIPAL_FORGIVENESS_AMOUNT",
                        "ORIGINAL_LIST_START_DATE", "ORIGINAL_LIST_PRICE", "CURRENT_LIST_START_DATE",
                        "CURRENT_LIST_PRICE", "ISSUE_SCOREB", "ISSUE_SCOREC", "CURR_SCOREB",
                        "CURR_SCOREC", "MI_TYPE", "SERV_IND", "CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT",
                        "CUMULATIVE_MODIFICATION_LOSS_AMOUNT", "CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS",
                        "CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS", "HOMEREADY_PROGRAM_INDICATOR",
                        "FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT", "RELOCATION_MORTGAGE_INDICATOR",
                        "ZERO_BALANCE_CODE_CHANGE_DATE", "LOAN_HOLDBACK_INDICATOR", "LOAN_HOLDBACK_EFFECTIVE_DATE",
                        "DELINQUENT_ACCRUED_INTEREST", "PROPERTY_INSPECTION_WAIVER_INDICATOR",
                        "HIGH_BALANCE_LOAN_INDICATOR", "ARM_5_YR_INDICATOR", "ARM_PRODUCT_TYPE",
                        "MONTHS_UNTIL_FIRST_PAYMENT_RESET", "MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET",
                        "INTEREST_RATE_CHANGE_DATE", "PAYMENT_CHANGE_DATE", "ARM_INDEX",
                        "ARM_CAP_STRUCTURE", "INITIAL_INTEREST_RATE_CAP", "PERIODIC_INTEREST_RATE_CAP",
                        "LIFETIME_INTEREST_RATE_CAP", "MARGIN", "BALLOON_INDICATOR",
                        "PLAN_NUMBER", "FORBEARANCE_INDICATOR", "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR",
                        "DEAL_NAME", "RE_PROCS_FLAG", "ADR_TYPE", "ADR_COUNT", "ADR_UPB")
lppub_column_classes <- c("character", "character", "character", "character", "character", "character",
                          "character", "numeric", "numeric", "numeric", "numeric",
                          "numeric", "numeric", "character", "character", "numeric", "numeric",
                          "numeric", "character", "numeric", "numeric", "character", "numeric",
                          "numeric", "numeric", "character", "character", "character",
                          "numeric", "character", "character", "character", "character",
                          "numeric", "character", "character", "character", "character",
                          "numeric", "character", "character", "character", "character",
                          "character", "character", "numeric", "character", "numeric",
                          "numeric", "numeric", "character", "character", "character",
                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric", "numeric", "numeric", "character",
                          "numeric", "character", "numeric", "numeric", "numeric", "numeric",
                          "numeric", "numeric", "character", "numeric", "numeric", "numeric",
                          "numeric", "character", "numeric", "character", "numeric", "character",
                          "numeric", "numeric", "character", "character", "numeric", "numeric",
                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric", "numeric", "numeric", "character",
                          "character", "character", "character", "character",
                          "character", "numeric", "numeric")
# combinations of year and quarter for which we have data
yearquarter       	<- sort( apply( expand.grid( paste0( 2000:2021, "Q" ), 1:4 ), 1, paste, collapse="" ) )
yearquarter  		<- c(yearquarter,"2022Q1")

# column numbers we want to keep (see "lppub_column_names" above)
#cols_selected     	<- c(2,3,9,10,12,13,14,16,17,20,22,23,24,27,28,30,31,32,33,35,37,40,52);
#last used cols_selected     	<- c(2,3,9,10,12,13,14,16,17,20,22,23,24,27,28,30,31,32,33,35,37,40,44,52);
cols_selected     	<- c(2,3,9,10,12,13,14,16,17,20,22,23,24,26,27,28,30,31,32,33,35,37,40,44,52);
#cols_selected     	<- c(2,3,9,10,12,13,14,16,17,20,22,23,24,27,28,30,31,32,33,34,35,37,40,52);


####################################################################

####################################################################
# 1.2. extracting data for quarterly vintages ######################
####################################################################
# looping

#Add first time home buyer, property type, occ status
for ( i in 1:NROW(yearquarter) ) {
  
  # year and quarter
  yq <- yearquarter[i]
  #yq <- "2000Q1"
  print(yq)
  
  
  #Unzip and Read Data
  ##Location of Zip Folder
  ziplocation <- "F:/Honours/Data/Mortgage Data/Performance_All.zip"
  filename <- paste(yq,".csv",sep="")
  data <- as.data.frame(fread(unzip(ziplocation, filename), sep = "|", select=cols_selected, colClasses = lppub_column_classes))
  
  
  #Add Column Names
  colnames(data) <- lppub_column_names[cols_selected]
  
  #Create Date Variables
  data <- data %>% mutate(year = as.numeric(substr(ACT_PERIOD, nchar(ACT_PERIOD)-3,nchar(ACT_PERIOD))),
                          month = as.numeric(substr(ACT_PERIOD, 1, nchar(ACT_PERIOD)-4)),
                          quarter = ceiling(month/3))
  
  
  #Create Default Indicators and Other Variables
  
  gc()
  data <- data %>% rename("oltv" = OLTV,
                          "cscore" =  CSCORE_B,
                          "state" = STATE,
                          "zip3" = ZIP,
                          "balance" = CURRENT_UPB,
                          "n_borrowers" = NUM_BO,
                          "dti" = DTI,
                          "purpose" = PURPOSE,
                          "prop" = PROP,
                          "occ_status" = OCC_STAT,
                          "loan_id" = LOAN_ID)
  
  
  
  
  data <- data %>% mutate(DLQ_STATUS_N = fifelse(DLQ_STATUS == "XX", 999, as.numeric(DLQ_STATUS)))
  data <- data %>% mutate(del_30p = fifelse(DLQ_STATUS_N >= 1 & DLQ_STATUS_N < 999 & Zero_Bal_Code == "", 1, 0))
  data <- data %>% mutate(del_60p = fifelse(DLQ_STATUS_N >= 2 & DLQ_STATUS_N < 999 & Zero_Bal_Code == "", 1, 0))
  data <- data %>% mutate(del_90p = fifelse(DLQ_STATUS_N >= 3 & DLQ_STATUS_N < 999 & Zero_Bal_Code == "", 1, 0))
  data <- data %>% mutate(del_180p = fifelse(DLQ_STATUS_N >= 6 & DLQ_STATUS_N < 999 & Zero_Bal_Code == "", 1, 0))
  data <- data %>% mutate(del_for = fifelse(Zero_Bal_Code == "02" | Zero_Bal_Code == "03" | Zero_Bal_Code == "09" | Zero_Bal_Code == "15" | (DLQ_STATUS >= 6 & DLQ_STATUS < 999), 1, 0))
  
  gc()
  # variables missing when foreclosed
  
  data <- data %>% arrange(loan_id,year,quarter,month)
  data <- data %>% mutate(fifelse(del_for == 1 & balance == 0, lag(balance,1), balance))
  
  # data <- data %>% mutate(CURR_RATE = fifelse(del_for == 1, lag(CURR_RATE,1), CURR_RATE),
  #                 balance = fifelse(del_for == 1, lag(balance,1), balance),
  #                 LOAN_AGE = fifelse(del_for == 1, lag(LOAN_AGE,1), LOAN_AGE),
  #                 REM_MONTHS = fifelse(del_for == 1, lag(REM_MONTHS,1), REM_MONTHS),
  #                 CURR_RATE = fifelse(del_for == 1, lag(CURR_RATE,1), CURR_RATE))
  
  
  gc()
  
  #data <- data %>% mutate(balance = fifelse(del_for == 1 & (balance == 0|is.na(balance)), balance_lag, balance)) sus this?
  #gc()
  
  
  data <- data %>% mutate(oyear = as.numeric(substr(yq,1,4)),
                          oquarter = as.numeric(substr(yq,6,6)))
  
  # Save Loan Level Data
  do_loan_level <- TRUE
  if(do_loan_level){
    
    # aggregating by zip code, state and quarter
    gc()
    data 		<- data %>%
      group_by(loan_id, state, zip3, year, quarter, oyear, oquarter, cscore, oltv, n_borrowers, dti, purpose, prop, occ_status) %>%
      summarise(balance = mean(balance, na.rm=TRUE ), 
                del_30p = max(del_30p, na.rm=TRUE ),
                del_60p = max(del_60p, na.rm=TRUE),
                del_90p = max(del_90p, na.rm=TRUE ), 
                del_180p = max(del_180p, na.rm=TRUE ), 
                del_for = max(del_for, na.rm=TRUE ))
    
    gc()
    # saving all data as csv
    localwrite <- "F:/Honours/Data/Mortgage Data/OutputsV3/"
    
    fwrite(data, paste0(localwrite, "dataloanlevel", yq, ".csv" ), row.names = FALSE, na="" );
    
    #Removing and Cleaning
    #Identifying Large Datasets
    rm(data)
    gc()
  }
  #Removing and Cleaning
  # rm(data)
  # gc()
  
  #Remove From Local
  fileR <- paste0("C:/Users/Dieter/Documents/",yq,".csv")
  if(file.exists(fileR)){
    file.remove(fileR)
  }
}



####################################################################
# 2. fema disaster declarations by county ##########################
####################################################################
# reading in data
data_county_fema <- as.data.frame(read.csv("F:/Honours/Data/Natural Disaster/DisasterDeclarationsSummaries.csv"))
# tidying up
names(data_county_fema)	<- tolower(names(data_county_fema))

data_county_fema <- data_county_fema %>% rename("county" = declaredcountyarea) %>% mutate(county = gsub("\\(", "", county)) %>% mutate(county = gsub("\\)", "", county))
data_county_fema <- data_county_fema %>% mutate(year = year(incidentbegindate),
                                                quarter = quarter(incidentbegindate))
data_county_fema <- data_county_fema %>% select(state, county, year, quarter, incidenttype) %>% filter(year >= 2000)


# creating dummy variables for different incident types
#Moved dam break and tsunami should be in flood?
storm_winter <- c("Severe Ice Storm","Freezing","Winter Storm","Snowstorm")
storm_summer <- c("Severe Storm","Tropical Storm","Coastal Storm","Hurricane","Typhoon","Tornado","Severe Storm(s)")
fire <- c("Fire")
flood <- c("Flood", "Dam/Levee Break","Tsunami")
biological <- c("Biological")
other <- c("Drought","Terrorist","Earthquake","Other","Chemical","Mud/Landslide","Toxic Substances","Volcanic Eruption")


data_county_fema <- data_county_fema %>% mutate(d_storm_winter = ifelse(incidenttype %in% storm_winter, 1, 0),
                                                d_storm_summer = ifelse(incidenttype %in% storm_summer, 1, 0),
                                                d_fire = ifelse(incidenttype %in% fire, 1, 0),
                                                d_flood = ifelse(incidenttype %in% flood, 1, 0),
                                                d_biological = ifelse(incidenttype %in% biological, 1, 0),
                                                d_other = ifelse(incidenttype %in% other, 1, 0))


data_county_fema %>% write.csv("F:/Honours/Data/Natural Disaster/data_county_fema.csv", row.names=FALSE, na="")
####################################################################


####################################################################
# 3. creating employment/unemployment data #########################
####################################################################

# reading in data

data_emp0 <- as.data.frame(fread("F:/Honours/Data/Employment Data/la.data.64.County.txt"))
data_series <- as.data.frame( fread("F:/Honours/Data/Employment Data/la.series.txt"))
# merging
data_emp <- data_emp0 %>% left_join(data_series, by = "series_id")
# keeping only "unemployment rate" and "employment"
data_emp <- data_emp %>% filter(measure_code %in% c(3,5))


# creating "measure", "county" and "state"
data_emp <- data_emp %>% mutate(measure = ifelse(grepl("Unemployment Rate:", series_title, fixed = TRUE), "Unemployment Rate", "Employment"))
data_emp <- data_emp %>% rename("series_title_0" = series_title) %>% mutate(series_title_0 = gsub("Unemployment Rate: ", "", series_title_0),
                                                                            series_title_0 = gsub("Employment: ", "", series_title_0),
                                                                            series_title_0 = gsub(" \\(U\\)", "", series_title_0))

data_emp <- data_emp %>% mutate(county = substring(series_title_0, 1, nchar(series_title_0)-4),
                                state = substring(series_title_0, nchar(series_title_0)-1, 1000))


data_emp <- data_emp %>% mutate(month = as.numeric(gsub("M", "", period)),
                                quarter = ceiling(month/3)) %>% filter(month <= 12)

data_emp <- data_emp %>% mutate(value = as.numeric(value))

# collapsing to quarterly
data_emp_q <- data_emp %>% group_by(measure, state, county, year, quarter) %>% 
  summarise(value_q = mean(value, na.rm = TRUE))



#View(data_emp_q)
# creating "employment" and "unemployment rate" series

data_employment <- data_emp_q %>% filter(measure == "Employment") %>% rename("e_emp" = value_q) %>% select(-measure)
data_unemployment <- data_emp_q %>% filter(measure != "Employment") %>% rename("e_ur" = value_q) %>% select(-measure)

data_employment <- data_employment %>% mutate(ID = paste(state,county,year,quarter, sep = "-"))
data_unemployment <- data_unemployment %>% mutate(ID = paste(state,county,year,quarter, sep = "-"))

data_lab_q <- data_employment %>% full_join(data_unemployment, by = "ID")

data_lab_q <- data_lab_q %>% rename("measure" = measure.x,
                                    "state" = state.x,
                                    "county" = county.x,
                                    "year" = year.x,
                                    "quarter" = quarter.x) %>% select(measure, state, county, year, quarter, e_emp, e_ur)

data_lab_q <- data_lab_q %>% 
  arrange(state,county,year,quarter, .by_group=TRUE)  %>%
  group_by(state,county) %>%
  mutate(e_emp_1 = lag(e_emp, n=1, default=NA), 
         e_ur_1 = lag(e_ur, n=1, default=NA))


data_lab_q <- data_lab_q %>% mutate(e_emp_dlog = log(e_emp/e_emp_1),
                                    e_ur_d = e_ur - e_ur_1) %>% select(-e_emp_1, -e_ur_1)


# taking lags
data_lab_q 		<- data_lab_q %>% 
  arrange(state,county,year,quarter, .by_group=TRUE)  %>%
  group_by(state,county) %>%
  mutate( 
    e_emp_dlog_0 = lag( e_emp_dlog, n=0, default=NA ), 
    e_emp_dlog_1 = lag( e_emp_dlog, n=1, default=NA ), 
    e_emp_dlog_2 = lag( e_emp_dlog, n=2, default=NA ), 
    e_emp_dlog_3 = lag( e_emp_dlog, n=3, default=NA ), 
    e_emp_dlog_4 = lag( e_emp_dlog, n=4, default=NA ), 
    e_emp_dlog_5 = lag( e_emp_dlog, n=5, default=NA ), 
    e_emp_dlog_6 = lag( e_emp_dlog, n=6, default=NA ), 
    e_emp_dlog_7 = lag( e_emp_dlog, n=7, default=NA ), 
    e_emp_dlog_8 = lag( e_emp_dlog, n=8, default=NA ),
    e_emp_dlog_9 = lag( e_emp_dlog, n=9, default=NA ),
    e_emp_dlog_10 = lag( e_emp_dlog, n=10, default=NA ),
    e_emp_dlog_11 = lag( e_emp_dlog, n=11, default=NA ),
    e_emp_dlog_12 = lag( e_emp_dlog, n=12, default=NA ),
    e_ur_0 = lag( e_ur, n=0, default=NA ), 
    e_ur_1 = lag( e_ur, n=1, default=NA ), 
    e_ur_2 = lag( e_ur, n=2, default=NA ), 
    e_ur_3 = lag( e_ur, n=3, default=NA ), 
    e_ur_4 = lag( e_ur, n=4, default=NA ), 
    e_ur_5 = lag( e_ur, n=5, default=NA ), 
    e_ur_6 = lag( e_ur, n=6, default=NA ), 
    e_ur_7 = lag( e_ur, n=7, default=NA ), 
    e_ur_8 = lag( e_ur, n=8, default=NA ), 
    e_ur_d_0 = lag( e_ur_d, n=0, default=NA ), 
    e_ur_d_1 = lag( e_ur_d, n=1, default=NA ), 
    e_ur_d_2 = lag( e_ur_d, n=2, default=NA ), 
    e_ur_d_3 = lag( e_ur_d, n=3, default=NA ), 
    e_ur_d_4 = lag( e_ur_d, n=4, default=NA ), 
    e_ur_d_5 = lag( e_ur_d, n=5, default=NA ), 
    e_ur_d_6 = lag( e_ur_d, n=6, default=NA ), 
    e_ur_d_7 = lag( e_ur_d, n=7, default=NA ), 
    e_ur_d_8 = lag( e_ur_d, n=8, default=NA ), 
    e_ur_d_9 = lag( e_ur_d, n=9, default=NA ), 
    e_ur_d_10 = lag( e_ur_d, n=10, default=NA ), 
    e_ur_d_11 = lag( e_ur_d, n=11, default=NA ), 
    e_ur_d_12 = lag( e_ur_d, n=12, default=NA ) 
  )

data_lab_q <- data_lab_q %>% select(-e_emp_dlog, e_emp, e_ur, e_ur_d)

#View(dat_lab_q)
# saving
#View(dat_lab_q)
data_lab_q %>% write.csv("F:/Honours/Data/Employment Data/data_county_lab.csv", row.names=FALSE, na="" )
####################################################################


####################################################################
# 4. creating house price data #####################################
####################################################################
# reading in data
data_hpi0 <- as.data.frame(fread("F:/Honours/Data/House Price Data/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"))

data_hpi1 <- data_hpi0 %>% select(-c("RegionID","SizeRank","RegionType","State","Metro","StateCodeFIPS","MunicipalCodeFIPS"))
# converting to long data
data_hpi 		<- melt(data_hpi1, id=c("RegionName","StateName"))



# creating dates
data_hpi <- data_hpi %>% mutate(date = as.character(variable),
                                Date2 = dmy(date),
                                year = year(Date2),
                                month = month(Date2),
                                quarter = ceiling(month/3)) %>% select(-Date2)


data_hpi <- data_hpi %>% rename("county" = RegionName,
                                "state" = StateName)
# collapsing to quarterly
data_hpi_q <- data_hpi %>% 
  group_by(state, county, year, quarter) %>%
  summarise(e_hpi = mean(value, na.rm = TRUE))
# percentage changes in prices
data_hpi_q <- data_hpi_q %>% 
  arrange(state, county, year, quarter, .by_group=TRUE)  %>%
  group_by(state, county) %>%
  mutate(e_hpi_1 = lag(e_hpi, n = 1, default = NA))

data_hpi_q <- data_hpi_q %>% mutate(e_hpi_dlog = log(e_hpi/e_hpi_1)) %>% 
  select(-e_hpi_1)



# taking lags
data_hpi_q <- data_hpi_q %>% 
  arrange(state, county, year, quarter, .by_group=TRUE) %>%
  group_by(state, county) %>%
  mutate(e_hpi_dlog_0 = lag(e_hpi_dlog, n=0, default=NA ), 
         e_hpi_dlog_1 = lag(e_hpi_dlog, n=1, default=NA), 
         e_hpi_dlog_2 = lag(e_hpi_dlog, n=2, default=NA), 
         e_hpi_dlog_3 = lag(e_hpi_dlog, n=3, default=NA), 
         e_hpi_dlog_4 = lag(e_hpi_dlog, n=4, default=NA), 
         e_hpi_dlog_5 = lag(e_hpi_dlog, n=5, default=NA), 
         e_hpi_dlog_6 = lag(e_hpi_dlog, n=6, default=NA), 
         e_hpi_dlog_7 = lag(e_hpi_dlog, n=7, default=NA), 
         e_hpi_dlog_8 = lag(e_hpi_dlog, n=8, default=NA),
         e_hpi_dlog_9 = lag(e_hpi_dlog, n=9, default=NA),
         e_hpi_dlog_10 = lag(e_hpi_dlog, n=10, default=NA),
         e_hpi_dlog_11 = lag(e_hpi_dlog, n=11, default=NA),
         e_hpi_dlog_12 = lag(e_hpi_dlog, n=12, default=NA)
         ) %>% select(-e_hpi_dlog)

# saving
#View(dat_hpi_q)
data_hpi_q %>% write.csv("F:/Honours/Data/House Price Data/data_county_hpi.csv", row.names=FALSE, na="")
####################################################################


####################################################################
# 5. population data by county by year #############################
####################################################################
# reading in data

dat_county_pop <- as.data.frame(read_excel( "F:/Honours/Data/County Population Data - Census Bureau/pop_county.xlsx", sheet="summary"))
dat_state_names	<- as.data.frame(read_excel( "F:/Honours/Data/County Population Data - Census Bureau/data_state_names.xlsx"))
dat_county_pop 		<- dat_county_pop %>% left_join(dat_state_names, by="state_name") %>% select(-state_name)

# reshaping to long 
dat_county_pop_year		<- melt(dat_county_pop, id=c("area","county","state")) %>% rename("year" = variable, "pop" = value) %>% select(-c(area)) %>% mutate(year = as.numeric(as.character(year)))
# creating variables

# saving
#View(dat_county_pop_year)
dat_county_pop_year %>% write.csv("F:/Honours/Data/County Population Data - Census Bureau/data_county_pop_year.csv", row.names=FALSE, na="" )
# creating origination pop dataset ASK ABOUT THIS?
# saving

#View(dat_county_opop_year)
#write.csv( dat_county_opop_oyear, "data_county_opop_oyear.csv", row.names=FALSE, na="" )
####################################################################


####################################################################
# 6. finding zip3-county correspondence using shapefiles ###########
####################################################################
# settings
sf_use_s2(FALSE); #this uses a flat earth model not a curvilinear one. The latter generates errors
# reading in data on zip-county correspondence
sf_zip	   		<- st_read( dsn=paste("F:/Honours/Data/Shape Files/cb_2020_us_zcta520_500k",sep=""), layer="cb_2020_us_zcta520_500k")
sf_county	   		<- st_read( dsn=paste("F:/Honours/Data/Shape Files/cb_2020_us_county_500k",sep=""), layer="cb_2020_us_county_500k")
#plot(sf_zip)
# aggregating the zip codes into zip3
sf_zip <- sf_zip %>% mutate(zip3 = substr(ZCTA5CE20, 1, 3))
sf_zip3 <- sf_zip %>% group_by(zip3) %>% summarize(geometry = st_union(geometry))

#plot(sf_zip3); 
#head(sf_zip3)
# calculating area of counties
sf_county	<- mutate(sf_county, county_area = st_area(sf_county))
# finding proportion of county in each zip3
#plot(sf_zip3[1,])

for ( i in 1:NROW(sf_zip3) ) {
  # intersecting zip3 and counties
  #i 			<- 1;
  ind_intersects 	<- st_intersects( sf_zip3[i,], sf_county ); 
  sf_int_i 		<- st_intersection( sf_zip3[i,], sf_county[unlist(ind_intersects),] );  
  sf_int_i		<- sf_int_i %>% 
    mutate( intersect_area = st_area(.) ) %>%
    st_drop_geometry()
  sf_int_i_slim 	<- sf_int_i[,c("zip3","COUNTYNS","intersect_area")] 
  # Merge by county name
  nc 			<- merge( sf_int_i_slim, sf_county, by=c("COUNTYNS"), all.x=TRUE )
  # Calculate coverage
  nc 			<- nc %>% 
    mutate( ratio=as.numeric(intersect_area/county_area) ) 
  # appending to the data frame
  if ( i==1 ) {
    dat_zip3_county_ratio 	<- nc;
  } else {
    dat_zip3_county_ratio 	<- rbind( dat_zip3_county_ratio, nc );
  }
  # printing progress
  flush.console()
  print(i);
}


# slimming down the data
#dat_zip3_county_ratio 		<- data_zip3_county_ratio;
dat_zip3_county_ratio$geometry <- NULL;
dat_zip3_county_ratio$state 	<- dat_zip3_county_ratio$STUSPS;
dat_zip3_county_ratio$county 	<- dat_zip3_county_ratio$NAMELSAD;
vars_keep 				<- c("zip3","state","county","intersect_area","county_area","ratio")
dat_zip3_county_ratio		<- dat_zip3_county_ratio[,vars_keep];
# saving
#View(dat_zip3_county_ratio)
write.csv( dat_zip3_county_ratio, "F:/Honours/Data/Shape Files/data_zip3_county_ratio.csv", row.names=FALSE, na="" )
####################################################################




# note: (1) just because we don't have any fema data doesn't mean that we should drop the county...
# ...it may just be that there were no incidents there over the period examined.
# 	(2) it seems like hpi data is missing quite often. why? 



####################################################################
# 7. combining all the data ########################################
####################################################################

####################################################################
# 7.0. tidying up counties #########################################
####################################################################
# whether to do
yesdo 	<- 1;
if ( yesdo==1 ) {
  data_county_lab <- fread("F:/Honours/Data/Employment Data/data_county_lab.csv")
  data_county_hpi <- fread("F:/Honours/Data/House Price Data/data_county_hpi.csv")
  data_county_fema <- fread("F:/Honours/Data/Natural Disaster/data_county_fema.csv")
  data_county_pop_year <- fread("F:/Honours/Data/County Population Data - Census Bureau/data_county_pop_year.csv")
  
  # finding county-state values for different datasets
  u_county_state_lab	<- cbind( unique( data_county_lab[,c("county","state")] ), "lab" )
  colnames(u_county_state_lab) <- c("county","state","type_lab")
  u_county_state_hpi 	<- cbind( unique( data_county_hpi[,c("county","state")] ), "hpi" )
  colnames(u_county_state_hpi) <- c("county","state","type_hpi")
  u_county_state_fema 	<- cbind( unique( data_county_fema[,c("county","state")] ), "fema" )
  colnames(u_county_state_fema) <- c("county","state","type_fema")
  u_county_state_pop 	<- cbind( unique( data_county_pop_year[,c("county","state")] ), "pop" )
  colnames(u_county_state_pop) <- c("county","state","type_pop")
  # merging
  u_all 	<- merge( u_county_state_lab, u_county_state_hpi, all=TRUE );
  u_all		<- merge( u_all, u_county_state_fema, all=TRUE );
  u_all		<- merge( u_all, u_county_state_pop, all=TRUE );
  u_all 	<- u_all[order(u_all$county,u_all$state),];
  #View(u_all)
}
# function to tidy up the county values
#series 	<- u_all$county;
function_tidy_county <- function( series ) {
  # setting up
  new_series 			<- series;
  # changing county to title case ("Alexandria city" to "Alexandria City")
  new_series 			<- toTitleCase( new_series );
  # fema data does not use city so remove from all instances
  new_series 			<- str_replace( new_series, "City", "" )
  # hpi data uses "Saint" but other data uses "St."
  new_series 			<- str_replace( new_series, "St.", "Saint" )
  # "Nantucket County/Town" to "Nantucket County" and "Denver County/City" to "Denver County"
  new_series 			<- str_replace( new_series, "County/Town", "County" )
  new_series 			<- str_replace( new_series, "County/City", "County" )
  # taking out unusual accents
  #string="Elizalde-González";
  new_series 			<- iconv( new_series, to='ASCII//TRANSLIT')
  # replacing anything in brackets 
  #new_series = "Ha(in (p)MSA)";
  #new_series 		<- str_replace( new_series, "\\s*\\([^\\)]+\\)", "" )
  first_bracket 		<- unlist(lapply(gregexpr(pattern="\\(", new_series), min))
  last_bracket 		<- unlist(lapply(gregexpr(pattern="\\)", new_series), max))
  term_in_brackets 		<- substr( new_series, first_bracket, last_bracket );
  term_in_brackets 		<- str_replace_all( term_in_brackets, "\\(", "\\\\(" )
  term_in_brackets 		<- str_replace_all( term_in_brackets, "\\)", "\\\\)" )
  #term_in_brackets
  new_series 			<- ifelse( nchar(term_in_brackets)>0, str_replace( new_series, term_in_brackets, "" ), new_series );
  # trimming
  new_series 			<- str_trim(new_series);
  # outputting
  return( new_series );
}
####################################################################

####################################################################
# 7.1. county data set #############################################
####################################################################
# reading in data for the county dataset
data_county_lab <- as.data.frame(fread("F:/Honours/Data/Employment Data/data_county_lab.csv"))
data_county_hpi <- as.data.frame(fread("F:/Honours/Data/House Price Data/data_county_hpi.csv"))
data_county_fema <- as.data.frame(fread("F:/Honours/Data/Natural Disaster/data_county_fema.csv"))
data_county_pop_year 		<- as.data.frame(fread("F:/Honours/Data/County Population Data - Census Bureau/data_county_pop_year.csv"))
# tidying up the county field based on the function above
data_county_lab$county 		<- function_tidy_county( data_county_lab$county );
data_county_hpi$county 		<- function_tidy_county( data_county_hpi$county );
data_county_fema$county 	<- function_tidy_county( data_county_fema$county );
data_county_pop_year$county 	<- function_tidy_county( data_county_pop_year$county );

data_county_lab <- data_county_lab %>% unique()
data_county_hpi <- data_county_hpi %>% unique()
data_county_fema <- data_county_fema %>% unique()
data_county_pop_year <- data_county_pop_year %>% unique()

#Fix Duplicates in FEMA
data_county_fema <- data_county_fema %>% group_by(state, county, year, quarter) %>% summarise(d_storm_winter = max(d_storm_winter),
                                                                                              d_storm_summer = max(d_storm_summer),
                                                                                              d_fire = max(d_fire),
                                                                                              d_flood = max(d_flood),
                                                                                              d_biological = max(d_biological),
                                                                                              d_other = max(d_other))

# creating county dataset
# creating template
u_county_state		<- unique( rbind( data_county_lab[,c("county","state")], data_county_hpi[,c("county","state")], data_county_fema[,c("county","state")], data_county_pop_year[,c("county","state")] ) );
template 			<- merge( u_county_state, merge( 2000:2022, 1:4, all=TRUE ), all=TRUE )
colnames(template) 	<- c("county","state","year","quarter");
NROW(template)
#View(template)
# merging to template
data_county 		<- template;
data_county 		<- merge( data_county, data_county_pop_year, by=c("county","state","year"), all.x=TRUE );
data_county 		<- merge( data_county, data_county_lab, by=c("county","state","year","quarter"), all.x=TRUE );
data_county 		<- merge( data_county, data_county_hpi, by=c("county","state","year","quarter"), all.x=TRUE );
data_county 		<- merge( data_county, data_county_fema, by=c("county","state","year","quarter"), all.x=TRUE );
# inserting zeros where no fema disaster occurred
d_vars 			<- grep( "d_", names(data_county), value=TRUE )
for ( i in 1:NROW(d_vars) ) {
  data_county[,d_vars[i]] <- ifelse( !is.na(data_county[,d_vars[i]]), data_county[,d_vars[i]], 0 );
}
# taking lags of fema indicators
data_county		<- data_county %>% 
  arrange(state,county,year,quarter, .by_group=TRUE)  %>%
  group_by(state,county) %>%
  mutate( 
    d_storm_winter_0 = lag( d_storm_winter, n=0, default=NA ), 
    d_storm_winter_1 = lag( d_storm_winter, n=1, default=NA ), 
    d_storm_winter_2 = lag( d_storm_winter, n=2, default=NA ), 
    d_storm_winter_3 = lag( d_storm_winter, n=3, default=NA ), 
    d_storm_winter_4 = lag( d_storm_winter, n=4, default=NA ), 
    d_storm_winter_5 = lag( d_storm_winter, n=5, default=NA ), 
    d_storm_winter_6 = lag( d_storm_winter, n=6, default=NA ), 
    d_storm_winter_7 = lag( d_storm_winter, n=7, default=NA ), 
    d_storm_winter_8 = lag( d_storm_winter, n=8, default=NA ), 
    d_storm_winter_9 = lag( d_storm_winter, n=9, default=NA ), 
    d_storm_winter_10 = lag( d_storm_winter, n=10, default=NA ), 
    d_storm_winter_11 = lag( d_storm_winter, n=11, default=NA ), 
    d_storm_winter_12 = lag( d_storm_winter, n=12, default=NA ),
    d_storm_winter_p1 = lead( d_storm_winter, n=1, default=NA ),
    d_storm_winter_p2 = lead( d_storm_winter, n=2, default=NA ),
    d_storm_winter_p3 = lead( d_storm_winter, n=3, default=NA ),
    d_storm_winter_p4 = lead( d_storm_winter, n=4, default=NA ),
    #
    d_storm_summer_0 = lag( d_storm_summer, n=0, default=NA ), 
    d_storm_summer_1 = lag( d_storm_summer, n=1, default=NA ), 
    d_storm_summer_2 = lag( d_storm_summer, n=2, default=NA ), 
    d_storm_summer_3 = lag( d_storm_summer, n=3, default=NA ), 
    d_storm_summer_4 = lag( d_storm_summer, n=4, default=NA ), 
    d_storm_summer_5 = lag( d_storm_summer, n=5, default=NA ), 
    d_storm_summer_6 = lag( d_storm_summer, n=6, default=NA ), 
    d_storm_summer_7 = lag( d_storm_summer, n=7, default=NA ), 
    d_storm_summer_8 = lag( d_storm_summer, n=8, default=NA ), 
    d_storm_summer_9 = lag( d_storm_summer, n=9, default=NA ), 
    d_storm_summer_10 = lag( d_storm_summer, n=10, default=NA ), 
    d_storm_summer_11 = lag( d_storm_summer, n=11, default=NA ), 
    d_storm_summer_12 = lag( d_storm_summer, n=12, default=NA ),
    d_storm_summer_p1 = lead( d_storm_summer, n=1, default=NA ), 
    d_storm_summer_p2 = lead( d_storm_summer, n=2, default=NA ), 
    d_storm_summer_p3 = lead( d_storm_summer, n=3, default=NA ), 
    d_storm_summer_p4 = lead( d_storm_summer, n=4, default=NA ), 
    #
    d_fire_0 = lag( d_fire, n=0, default=NA ), 
    d_fire_1 = lag( d_fire, n=1, default=NA ), 
    d_fire_2 = lag( d_fire, n=2, default=NA ), 
    d_fire_3 = lag( d_fire, n=3, default=NA ), 
    d_fire_4 = lag( d_fire, n=4, default=NA ), 
    d_fire_5 = lag( d_fire, n=5, default=NA ), 
    d_fire_6 = lag( d_fire, n=6, default=NA ), 
    d_fire_7 = lag( d_fire, n=7, default=NA ), 
    d_fire_8 = lag( d_fire, n=8, default=NA ), 
    d_fire_9 = lag( d_fire, n=9, default=NA ), 
    d_fire_10 = lag( d_fire, n=10, default=NA ), 
    d_fire_11 = lag( d_fire, n=11, default=NA ), 
    d_fire_12 = lag( d_fire, n=12, default=NA ),
    d_fire_p1 = lead( d_fire, n=1, default=NA ), 
    d_fire_p2 = lead( d_fire, n=2, default=NA ), 
    d_fire_p3 = lead( d_fire, n=3, default=NA ), 
    d_fire_p4 = lead( d_fire, n=4, default=NA ), 
    
    #
    d_flood_0 = lag( d_flood, n=0, default=NA ), 
    d_flood_1 = lag( d_flood, n=1, default=NA ), 
    d_flood_2 = lag( d_flood, n=2, default=NA ), 
    d_flood_3 = lag( d_flood, n=3, default=NA ), 
    d_flood_4 = lag( d_flood, n=4, default=NA ), 
    d_flood_5 = lag( d_flood, n=5, default=NA ), 
    d_flood_6 = lag( d_flood, n=6, default=NA ), 
    d_flood_7 = lag( d_flood, n=7, default=NA ), 
    d_flood_8 = lag( d_flood, n=8, default=NA ), 
    d_flood_9 = lag( d_flood, n=9, default=NA ), 
    d_flood_10 = lag( d_flood, n=10, default=NA ), 
    d_flood_11 = lag( d_flood, n=11, default=NA ), 
    d_flood_12 = lag( d_flood, n=12, default=NA ),
    d_flood_p1 = lead( d_flood, n=1, default=NA ),
    d_flood_p2 = lead( d_flood, n=2, default=NA ),
    d_flood_p3 = lead( d_flood, n=3, default=NA ),
    d_flood_p4 = lead( d_flood, n=4, default=NA ),
    #
    d_biological_0 = lag( d_biological, n=0, default=NA ), 
    d_biological_1 = lag( d_biological, n=1, default=NA ), 
    d_biological_2 = lag( d_biological, n=2, default=NA ), 
    d_biological_3 = lag( d_biological, n=3, default=NA ), 
    d_biological_4 = lag( d_biological, n=4, default=NA ), 
    d_biological_5 = lag( d_biological, n=5, default=NA ), 
    d_biological_6 = lag( d_biological, n=6, default=NA ), 
    d_biological_7 = lag( d_biological, n=7, default=NA ), 
    d_biological_8 = lag( d_biological, n=8, default=NA ), 
    d_biological_9 = lag( d_biological, n=9, default=NA ), 
    d_biological_10 = lag( d_biological, n=10, default=NA ), 
    d_biological_11 = lag( d_biological, n=11, default=NA ), 
    d_biological_12 = lag( d_biological, n=12, default=NA ), 
    #
    d_other_0 = lag( d_other, n=0, default=NA ), 
    d_other_1 = lag( d_other, n=1, default=NA ), 
    d_other_2 = lag( d_other, n=2, default=NA ), 
    d_other_3 = lag( d_other, n=3, default=NA ), 
    d_other_4 = lag( d_other, n=4, default=NA ), 
    d_other_5 = lag( d_other, n=5, default=NA ), 
    d_other_6 = lag( d_other, n=6, default=NA ), 
    d_other_7 = lag( d_other, n=7, default=NA ), 
    d_other_8 = lag( d_other, n=8, default=NA ), 
    d_other_9 = lag( d_other, n=9, default=NA ), 
    d_other_10 = lag( d_other, n=10, default=NA ), 
    d_other_11 = lag( d_other, n=11, default=NA ), 
    d_other_12 = lag( d_other, n=12, default=NA ), 
  )
data_county$d_storm_winter 	<- NULL;
data_county$d_storm_summer	<- NULL;
data_county$d_fire 		<- NULL;
data_county$d_flood	 	<- NULL;
data_county$d_biological 	<- NULL;
data_county$d_other 		<- NULL;
# saving
#View(data_county)
write.csv( data_county, "F:/Honours/Data/data_county.csv", row.names=FALSE, na="" )
####################################################################

####################################################################
# 7.2. zip3 data set ###############################################
####################################################################
# reading in data for the zip3 dataset
#data_del_zip3_qvintage 	<- as.data.frame( fread( "data_del_zip3_qvintage.csv" ) );
data_del_zip3_yvintage 		<- as.data.frame( fread( "F:/Honours/Data/Mortgage Data/Outputs/data_del_zip3_yvintage.csv" ) );
data_zip3_county_ratio 		<- as.data.frame( fread( "F:/Honours/Data/Shape Files/data_zip3_county_ratio.csv" ) );
data_county 			<- as.data.frame( fread( "F:/Honours/Data/data_county.csv" ) );
#data_county_opop_oyear 		<- as.data.frame( fread( "data_county_opop_oyear.csv" ) );
# tidying up the county field based on the function above
data_zip3_county_ratio$county <- function_tidy_county( data_zip3_county_ratio$county );
#data_county_opop_oyear$county <- function_tidy_county( data_county_opop_oyear$county );
# creating zip3 economic data by averaging county economic data
# creating template
year_quarter 		<- merge( 2000:2022, 1:4, all=TRUE );
colnames(year_quarter) 	<- c("year","quarter");
template 			<- merge( data_zip3_county_ratio, year_quarter, all=TRUE )
#colnames(template) 	<- c(colnames(data_zip3_county_ratio),"year","quarter");
NROW(template)
# merging to template (note merging opop by oyear and will use this as the weight)
data_zip3_county 		<- template;
data_zip3_county 		<- merge( data_zip3_county, data_county, by=c("county","state","year","quarter"), all.x=TRUE );
# weighted average of counties to generate zip3 data
data_zip3_county$pop_ratio 	<- data_zip3_county$pop * data_zip3_county$ratio;
data_zip3 				<- data_zip3_county %>%
  group_by(zip3,state,year,quarter) %>%
  summarise( 
    mean_e_ur_0 = weighted.mean( e_ur_0, pop_ratio, na.rm=TRUE ),
    mean_e_ur_d_0 = weighted.mean( e_ur_d_0, pop_ratio, na.rm=TRUE ),
    mean_e_hpi_dlog_0 = weighted.mean( e_hpi_dlog_0, pop_ratio, na.rm=TRUE ),
    mean_e_emp_dlog_0 = weighted.mean( e_emp_dlog_0, pop_ratio, na.rm=TRUE ),
    mean_d_storm_winter_0 = weighted.mean( d_storm_winter_0, pop_ratio, na.rm=TRUE ),
    mean_d_storm_summer_0 = weighted.mean( d_storm_summer_0, pop_ratio, na.rm=TRUE ),
    mean_d_fire_0 = weighted.mean( d_fire_0, pop_ratio, na.rm=TRUE ),
    mean_d_flood_0 = weighted.mean( d_flood_0, pop_ratio, na.rm=TRUE ),
    mean_d_biological_0 = weighted.mean( d_biological_0, pop_ratio, na.rm=TRUE ),
    mean_d_other_0 = weighted.mean( d_other_0, pop_ratio, na.rm=TRUE ),
  )
# taking lags 
data_zip3 		<- data_zip3 %>% 
  arrange(state,zip3,year,quarter, .by_group=TRUE)  %>%
  group_by(state,zip3) %>%
  mutate( 
    e_ur_0 = lag( mean_e_ur_0, n=0, default=NA ), 
    e_ur_1 = lag( mean_e_ur_0, n=1, default=NA ), 
    e_ur_2 = lag( mean_e_ur_0, n=2, default=NA ), 
    e_ur_3 = lag( mean_e_ur_0, n=3, default=NA ), 
    e_ur_4 = lag( mean_e_ur_0, n=4, default=NA ), 
    e_ur_5 = lag( mean_e_ur_0, n=5, default=NA ), 
    e_ur_6 = lag( mean_e_ur_0, n=6, default=NA ), 
    e_ur_7 = lag( mean_e_ur_0, n=7, default=NA ), 
    e_ur_8 = lag( mean_e_ur_0, n=8, default=NA ), 
    e_ur_9 = lag( mean_e_ur_0, n=9, default=NA ), 
    e_ur_10 = lag( mean_e_ur_0, n=10, default=NA ), 
    e_ur_11 = lag( mean_e_ur_0, n=11, default=NA ), 
    e_ur_12 = lag( mean_e_ur_0, n=12, default=NA ), 
    #
    e_ur_d_0 = lag( mean_e_ur_d_0, n=0, default=NA ), 
    e_ur_d_1 = lag( mean_e_ur_d_0, n=1, default=NA ), 
    e_ur_d_2 = lag( mean_e_ur_d_0, n=2, default=NA ), 
    e_ur_d_3 = lag( mean_e_ur_d_0, n=3, default=NA ), 
    e_ur_d_4 = lag( mean_e_ur_d_0, n=4, default=NA ), 
    e_ur_d_5 = lag( mean_e_ur_d_0, n=5, default=NA ), 
    e_ur_d_6 = lag( mean_e_ur_d_0, n=6, default=NA ), 
    e_ur_d_7 = lag( mean_e_ur_d_0, n=7, default=NA ), 
    e_ur_d_8 = lag( mean_e_ur_d_0, n=8, default=NA ), 
    e_ur_d_9 = lag( mean_e_ur_d_0, n=9, default=NA ), 
    e_ur_d_10 = lag( mean_e_ur_d_0, n=10, default=NA ), 
    e_ur_d_11 = lag( mean_e_ur_d_0, n=11, default=NA ), 
    e_ur_d_12 = lag( mean_e_ur_d_0, n=12, default=NA ), 
    #
    e_hpi_dlog_0 = lag( mean_e_hpi_dlog_0, n=0, default=NA ), 
    e_hpi_dlog_1 = lag( mean_e_hpi_dlog_0, n=1, default=NA ), 
    e_hpi_dlog_2 = lag( mean_e_hpi_dlog_0, n=2, default=NA ), 
    e_hpi_dlog_3 = lag( mean_e_hpi_dlog_0, n=3, default=NA ), 
    e_hpi_dlog_4 = lag( mean_e_hpi_dlog_0, n=4, default=NA ), 
    e_hpi_dlog_5 = lag( mean_e_hpi_dlog_0, n=5, default=NA ), 
    e_hpi_dlog_6 = lag( mean_e_hpi_dlog_0, n=6, default=NA ), 
    e_hpi_dlog_7 = lag( mean_e_hpi_dlog_0, n=7, default=NA ), 
    e_hpi_dlog_8 = lag( mean_e_hpi_dlog_0, n=8, default=NA ), 
    e_hpi_dlog_9 = lag( mean_e_hpi_dlog_0, n=9, default=NA ), 
    e_hpi_dlog_10 = lag( mean_e_hpi_dlog_0, n=10, default=NA ), 
    e_hpi_dlog_11 = lag( mean_e_hpi_dlog_0, n=11, default=NA ), 
    e_hpi_dlog_12 = lag( mean_e_hpi_dlog_0, n=12, default=NA ), 
    #
    e_emp_dlog_0 = lag( mean_e_emp_dlog_0, n=0, default=NA ), 
    e_emp_dlog_1 = lag( mean_e_emp_dlog_0, n=1, default=NA ), 
    e_emp_dlog_2 = lag( mean_e_emp_dlog_0, n=2, default=NA ), 
    e_emp_dlog_3 = lag( mean_e_emp_dlog_0, n=3, default=NA ), 
    e_emp_dlog_4 = lag( mean_e_emp_dlog_0, n=4, default=NA ), 
    e_emp_dlog_5 = lag( mean_e_emp_dlog_0, n=5, default=NA ), 
    e_emp_dlog_6 = lag( mean_e_emp_dlog_0, n=6, default=NA ), 
    e_emp_dlog_7 = lag( mean_e_emp_dlog_0, n=7, default=NA ), 
    e_emp_dlog_8 = lag( mean_e_emp_dlog_0, n=8, default=NA ), 
    e_emp_dlog_9 = lag( mean_e_emp_dlog_0, n=9, default=NA ), 
    e_emp_dlog_10 = lag( mean_e_emp_dlog_0, n=10, default=NA ), 
    e_emp_dlog_11 = lag( mean_e_emp_dlog_0, n=11, default=NA ), 
    e_emp_dlog_12 = lag( mean_e_emp_dlog_0, n=12, default=NA ), 
    #
    d_storm_winter_0 = lag( mean_d_storm_winter_0, n=0, default=NA ), 
    d_storm_winter_1 = lag( mean_d_storm_winter_0, n=1, default=NA ), 
    d_storm_winter_2 = lag( mean_d_storm_winter_0, n=2, default=NA ), 
    d_storm_winter_3 = lag( mean_d_storm_winter_0, n=3, default=NA ), 
    d_storm_winter_4 = lag( mean_d_storm_winter_0, n=4, default=NA ), 
    d_storm_winter_5 = lag( mean_d_storm_winter_0, n=5, default=NA ), 
    d_storm_winter_6 = lag( mean_d_storm_winter_0, n=6, default=NA ), 
    d_storm_winter_7 = lag( mean_d_storm_winter_0, n=7, default=NA ), 
    d_storm_winter_8 = lag( mean_d_storm_winter_0, n=8, default=NA ), 
    d_storm_winter_9 = lag( mean_d_storm_winter_0, n=9, default=NA ), 
    d_storm_winter_10 = lag( mean_d_storm_winter_0, n=10, default=NA ), 
    d_storm_winter_11 = lag( mean_d_storm_winter_0, n=11, default=NA ), 
    d_storm_winter_12 = lag( mean_d_storm_winter_0, n=12, default=NA ), 
    d_storm_winter_p1 = lead( mean_d_storm_winter_0, n=1, default=NA ), 
    d_storm_winter_p2 = lead( mean_d_storm_winter_0, n=2, default=NA ), 
    d_storm_winter_p3 = lead( mean_d_storm_winter_0, n=3, default=NA ), 
    d_storm_winter_p4 = lead( mean_d_storm_winter_0, n=4, default=NA ), 
    #
    d_storm_summer_0 = lag( mean_d_storm_summer_0, n=0, default=NA ), 
    d_storm_summer_1 = lag( mean_d_storm_summer_0, n=1, default=NA ), 
    d_storm_summer_2 = lag( mean_d_storm_summer_0, n=2, default=NA ), 
    d_storm_summer_3 = lag( mean_d_storm_summer_0, n=3, default=NA ), 
    d_storm_summer_4 = lag( mean_d_storm_summer_0, n=4, default=NA ), 
    d_storm_summer_5 = lag( mean_d_storm_summer_0, n=5, default=NA ), 
    d_storm_summer_6 = lag( mean_d_storm_summer_0, n=6, default=NA ), 
    d_storm_summer_7 = lag( mean_d_storm_summer_0, n=7, default=NA ), 
    d_storm_summer_8 = lag( mean_d_storm_summer_0, n=8, default=NA ), 
    d_storm_summer_9 = lag( mean_d_storm_summer_0, n=9, default=NA ), 
    d_storm_summer_10 = lag( mean_d_storm_summer_0, n=10, default=NA ), 
    d_storm_summer_11 = lag( mean_d_storm_summer_0, n=11, default=NA ), 
    d_storm_summer_12 = lag( mean_d_storm_summer_0, n=12, default=NA ), 
    d_storm_summer_p1 = lead( mean_d_storm_summer_0, n=1, default=NA ), 
    d_storm_summer_p2 = lead( mean_d_storm_summer_0, n=2, default=NA ), 
    d_storm_summer_p3 = lead( mean_d_storm_summer_0, n=3, default=NA ), 
    d_storm_summer_p4 = lead( mean_d_storm_summer_0, n=4, default=NA ), 
    #
    d_fire_0 = lag( mean_d_fire_0, n=0, default=NA ), 
    d_fire_1 = lag( mean_d_fire_0, n=1, default=NA ), 
    d_fire_2 = lag( mean_d_fire_0, n=2, default=NA ), 
    d_fire_3 = lag( mean_d_fire_0, n=3, default=NA ), 
    d_fire_4 = lag( mean_d_fire_0, n=4, default=NA ), 
    d_fire_5 = lag( mean_d_fire_0, n=5, default=NA ), 
    d_fire_6 = lag( mean_d_fire_0, n=6, default=NA ), 
    d_fire_7 = lag( mean_d_fire_0, n=7, default=NA ), 
    d_fire_8 = lag( mean_d_fire_0, n=8, default=NA ), 
    d_fire_9 = lag( mean_d_fire_0, n=9, default=NA ), 
    d_fire_10 = lag( mean_d_fire_0, n=10, default=NA ), 
    d_fire_11 = lag( mean_d_fire_0, n=11, default=NA ), 
    d_fire_12 = lag( mean_d_fire_0, n=12, default=NA ), 
    d_fire_p1 = lead( mean_d_fire_0, n=1, default=NA ), 
    d_fire_p2 = lead( mean_d_fire_0, n=2, default=NA ), 
    d_fire_p3 = lead( mean_d_fire_0, n=3, default=NA ), 
    d_fire_p4 = lead( mean_d_fire_0, n=4, default=NA ), 
    #
    d_flood_0 = lag( mean_d_flood_0, n=0, default=NA ), 
    d_flood_1 = lag( mean_d_flood_0, n=1, default=NA ), 
    d_flood_2 = lag( mean_d_flood_0, n=2, default=NA ), 
    d_flood_3 = lag( mean_d_flood_0, n=3, default=NA ), 
    d_flood_4 = lag( mean_d_flood_0, n=4, default=NA ), 
    d_flood_5 = lag( mean_d_flood_0, n=5, default=NA ), 
    d_flood_6 = lag( mean_d_flood_0, n=6, default=NA ), 
    d_flood_7 = lag( mean_d_flood_0, n=7, default=NA ), 
    d_flood_8 = lag( mean_d_flood_0, n=8, default=NA ), 
    d_flood_9 = lag( mean_d_flood_0, n=9, default=NA ), 
    d_flood_10 = lag( mean_d_flood_0, n=10, default=NA ), 
    d_flood_11 = lag( mean_d_flood_0, n=11, default=NA ), 
    d_flood_12 = lag( mean_d_flood_0, n=12, default=NA ), 
    d_flood_p1 = lead( mean_d_flood_0, n=1, default=NA ), 
    d_flood_p2 = lead( mean_d_flood_0, n=2, default=NA ), 
    d_flood_p3 = lead( mean_d_flood_0, n=3, default=NA ), 
    d_flood_p4 = lead( mean_d_flood_0, n=4, default=NA ), 
    #
    d_biological_0 = lag( mean_d_biological_0, n=0, default=NA ), 
    d_biological_1 = lag( mean_d_biological_0, n=1, default=NA ), 
    d_biological_2 = lag( mean_d_biological_0, n=2, default=NA ), 
    d_biological_3 = lag( mean_d_biological_0, n=3, default=NA ), 
    d_biological_4 = lag( mean_d_biological_0, n=4, default=NA ), 
    d_biological_5 = lag( mean_d_biological_0, n=5, default=NA ), 
    d_biological_6 = lag( mean_d_biological_0, n=6, default=NA ), 
    d_biological_7 = lag( mean_d_biological_0, n=7, default=NA ), 
    d_biological_8 = lag( mean_d_biological_0, n=8, default=NA ), 
    d_biological_9 = lag( mean_d_biological_0, n=9, default=NA ), 
    d_biological_10 = lag( mean_d_biological_0, n=10, default=NA ), 
    d_biological_11 = lag( mean_d_biological_0, n=11, default=NA ), 
    d_biological_12 = lag( mean_d_biological_0, n=12, default=NA ), 
    #
    d_other_0 = lag( mean_d_other_0, n=0, default=NA ), 
    d_other_1 = lag( mean_d_other_0, n=1, default=NA ), 
    d_other_2 = lag( mean_d_other_0, n=2, default=NA ), 
    d_other_3 = lag( mean_d_other_0, n=3, default=NA ), 
    d_other_4 = lag( mean_d_other_0, n=4, default=NA ), 
    d_other_5 = lag( mean_d_other_0, n=5, default=NA ), 
    d_other_6 = lag( mean_d_other_0, n=6, default=NA ), 
    d_other_7 = lag( mean_d_other_0, n=7, default=NA ), 
    d_other_8 = lag( mean_d_other_0, n=8, default=NA ), 
    d_other_9 = lag( mean_d_other_0, n=9, default=NA ), 
    d_other_10 = lag( mean_d_other_0, n=10, default=NA ), 
    d_other_11 = lag( mean_d_other_0, n=11, default=NA ), 
    d_other_12 = lag( mean_d_other_0, n=12, default=NA ), 
  )
data_zip3$mean_e_ur_0 			<- NULL;
data_zip3$mean_e_ur_d_0 		<- NULL;
data_zip3$mean_e_hpi_dlog_0 		<- NULL;
data_zip3$mean_e_emp_dlog_0 		<- NULL;
data_zip3$mean_d_storm_winter_0 	<- NULL;
data_zip3$mean_d_storm_summer_0 	<- NULL;
data_zip3$mean_d_fire_0 		<- NULL;
data_zip3$mean_d_flood_0		<- NULL;
data_zip3$mean_d_biological_0		<- NULL;
data_zip3$mean_d_other_0		<- NULL;
# removing observations with a lot of missing data
NROW(data_zip3)
data_zip3 			<- data_zip3[which( !is.na( data_zip3$e_ur_0 * data_zip3$e_hpi_dlog_0 * data_zip3$e_emp_dlog_0 * data_zip3$d_storm_summer_0 * data_zip3$d_storm_winter_0 * data_zip3$d_biological_0 * data_zip3$d_fire_0 * data_zip3$d_flood_0 * data_zip3$d_other_0 ) ),];
NROW(data_zip3)
# saving
#View(data_zip3)
write.csv( data_zip3, "F:/Honours/Data/Mortgage Data/Outputs/data_zip3.csv", row.names=FALSE, na="" )
# merging credit data
data_zip3_del			<- merge( data_del_zip3_yvintage, data_zip3, by=c("state","zip3","year","quarter"), all.x=TRUE );
# saving
#View(data_zip3_del)
write.csv( data_zip3_del, "F:/Honours/Data/Mortgage Data/Outputs/data_zip3_del.csv", row.names=FALSE, na="" )
####################################################################

