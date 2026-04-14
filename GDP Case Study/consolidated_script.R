# Case Study: GDP and components

## Part 1: Input data

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#install.packages("officer")
#install.packages("flextable")
#install.packages("sjPlot")
library(officer)
library(flextable)

############################################################################################################################################
# 1.process Oxford tracker daily indexes - input file "OxCGRT_compact_national_v1.csv"
############################################################################################################################################

# Set working directory
#Path1 <- "\\\\FS19-MB-2\\SdataSDD\\Applic\\QNA\\A.Case study GDP and components\\Covid\\input data"
#setwd(Path1)

setwd("C:/Users/Sacksferrari_S/OneDrive - OECD/Working_Paper_GDP_method")

# Read the data
tracker_data_D <- read.csv("OxCGRT_compact_national_v1.csv")
tracker_data_A = tracker_data_D
# drop unnecessary variables
tracker_data_D <- subset(tracker_data_D, select=-c(CountryName, RegionName, RegionCode,Jurisdiction))

# Convert numeric dates to character format
character_dates <- as.character(tracker_data_D$Date)

# Convert character dates to Date format
tracker_data_D$Date <- as.Date(character_dates, format = "%Y%m%d")

# Create quarters variable
tracker_data_D$quarters <- paste0(format(tracker_data_D$Date, "%Y"), "-",quarters(tracker_data_D$Date) )


# Select numeric columns
numeric_cols <- c("CountryCode","quarters", "Date", names(tracker_data_D)[sapply(tracker_data_D, is.numeric)])


# Derive quarterly average from daily indexes for the numeric columns 
tracker_data_Q <- aggregate(. ~ CountryCode + quarters , data = tracker_data_D[, numeric_cols], FUN = mean, na.action = na.pass)

# Sort the dataframe by CountryCode and Date to have the quarters ordered to use Lag
tracker_data_Q <- tracker_data_Q %>%
  arrange(CountryCode, Date)


# Calculate the Difference between Q and Q-1 for all the numeric variables
tracker_data_Q_Diff <- tracker_data_Q %>%
  arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
  mutate(across(where(is.numeric), ~ . - dplyr::lag(.)))

#Rename variables to be consistent with df_QNA_growth
tracker_data_Q_Diff <- rename(tracker_data_Q_Diff, country = CountryCode)
tracker_data_Q_Diff <- rename(tracker_data_Q_Diff, period = quarters)

#drop Date
tracker_data_Q_Diff <- subset(tracker_data_Q_Diff, select = -Date)



################################################################################
#2. extract yearly data
################################################################################

# drop unnecessary variables
tracker_data_A <- subset(tracker_data_A, select=-c(CountryName, RegionName, RegionCode, Jurisdiction))

# Convert numeric dates to character format
character_dates <- as.character(tracker_data_A$Date)

# Convert character dates to Date format
tracker_data_A$Date <- as.Date(character_dates, format = "%Y%m%d")

# Create years variable
tracker_data_A$years <- format(tracker_data_A$Date, "%Y")

# Select numeric columns
numeric_cols <- c("CountryCode", "years", "Date", names(tracker_data_A)[sapply(tracker_data_A, is.numeric)])

# Derive yearly average from daily indexes for the numeric columns 
tracker_data_Y <- aggregate(. ~ CountryCode + years , data = tracker_data_A[, numeric_cols], FUN = mean, na.action = na.pass)

# Sort the dataframe by CountryCode and Date to have the quarters ordered to use Lag
tracker_data_Y <- tracker_data_Y %>%
  arrange(CountryCode, Date)

# Calculate the Difference between Y and Y-1 for all the numeric variables
tracker_data_Y_Diff <- tracker_data_Y %>%
  arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
  mutate(across(where(is.numeric), ~ . - dplyr::lag(.)))

# Rename variables to be consistent with df_QNA_growth
tracker_data_Y_Diff <- rename(tracker_data_Y_Diff, country = CountryCode)
tracker_data_Y_Diff <- rename(tracker_data_Y_Diff, period = years)

# Drop Date
tracker_data_Y_Diff <- subset(tracker_data_Y_Diff, select = -Date)

## Add tracker data (not diff) to consolidated database
# Rename variables to be consistent with df_QNA_growth
tracker_data_Y <- rename(tracker_data_Y, country = CountryCode)
tracker_data_Y <- rename(tracker_data_Y, period = years)

# Drop Date
tracker_data_Y <- subset(tracker_data_Y, select = -Date)

tracker_data_Y$period = as.integer(tracker_data_Y$period)

############################################################################################################################################
# 3. RETRIEVE ELS EXCESS MORTALITY WEEKLY DATA FROM V8
############################################################################################################################################

url="https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_MORTALITY@DF_MORTALITY/.W.EM._T._T.PC_DT_A?startPeriod=2020-W01&endPeriod=2022-W52&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_ELS<-read.csv(url)

#clean table
ELS_columns_to_keep<-c("REF_AREA","Measure","TIME_PERIOD","OBS_VALUE")

df_ELS<- df_ELS[,ELS_columns_to_keep]

df_ELS <- rename(df_ELS, country = REF_AREA)
df_ELS <- rename(df_ELS, period = TIME_PERIOD)


# Function to convert weekly period to quarterly format
convert_to_quarterly <- function(period) {
  year_week <- strsplit(period, "-")[[1]]
  year <- year_week[1]
  week <- as.integer(substring(year_week[2], 2))
  quarter <- ceiling(week / 13)
  
  # Adjust quarter if it exceeds 4
  if (quarter > 4) {
    quarter <- 4
  }
  
  return(paste0(year, "-Q", quarter))
}


# Convert to quarterly format
df_ELS$Quarter <- sapply(df_ELS$period, convert_to_quarterly)


# Select numeric columns
numeric_cols <- c("country", "Quarter", names(df_ELS)[sapply(df_ELS, is.numeric)])

# Derive sum from weekly data for the numeric columns 
df_ELS_Q <- aggregate(. ~ country + Quarter , data = df_ELS[, numeric_cols], FUN = sum, na.action = na.pass)

df_ELS_Q <- rename(df_ELS_Q, excess_mortality = OBS_VALUE)
df_ELS_Q <- rename(df_ELS_Q, period = Quarter)

############################################################################################################################################
#4. RETRIEVE ELS EXCESS MORTALITY WEEKLY DATA FROM V8 - yearly
########################################################

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_MORTALITY@DF_MORTALITY/.W.EM._T._T.PC_DT_A?startPeriod=2020-W01&endPeriod=2022-W52&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_ELS <- read.csv(url)

# Clean table
ELS_columns_to_keep <- c("REF_AREA", "Measure", "TIME_PERIOD", "OBS_VALUE")
df_ELS <- df_ELS[, ELS_columns_to_keep]
df_ELS <- rename(df_ELS, country = REF_AREA)
df_ELS$TIME_PERIOD <- as.integer(substring(df_ELS$TIME_PERIOD, 1, 4))

df_ELS <- rename(df_ELS, year = TIME_PERIOD)

# Select numeric columns
numeric_cols <- c("country", "year", names(df_ELS)[sapply(df_ELS, is.numeric)])

# Derive sum from weekly data for the numeric columns 
df_ELS_Y <- aggregate(. ~ country + year, data = df_ELS[, numeric_cols], FUN = sum, na.action = na.pass)

df_ELS_Y <- rename(df_ELS_Y, excess_mortality = OBS_VALUE)
df_ELS_Y <- rename(df_ELS_Y, period = year)
df_ELS_Y$year.1 <- NULL

############################################################################################################################################
# 5. RETRIEVE QNA DATA FROM V8 FOR: adjustment=Y,price_base=L or Q, tables T0101/0102,transfo=N
############################################################################################################################################

url="https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA/Q.Y.........L+Q.N.T0101+T0102?startPeriod=2019-Q4&endPeriod=2022-Q4&&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_QNA<-read.csv(url)

#with labels
#QNA_columns_to_keep<-c("REF_AREA","SECTOR","TRANSACTION","Transaction","ACTIVITY","Economic.activity","UNIT_MEASURE","Unit.of.measure","ADJUSTMENT","PRICE_BASE","Price.base","TRANSFORMATION","TABLE_IDENTIFIER","TIME_PERIOD","OBS_VALUE","UNIT_MULT","CURRENCY")
QNA_columns_to_keep<-c("REF_AREA","SECTOR","TRANSACTION","ACTIVITY","TIME_PERIOD","OBS_VALUE","TABLE_IDENTIFIER", "INSTR_ASSET")
#QNA_measure_to_keep<-c("L","Q","XDC", "V" --> B9 )
#QNA_table_id_to_keep<-"T0102"
QNA_subject_to_keep<-c("B1GQ","P3","P31","P32","P5", "P51G","P6", "P61" ,"P62" ,"P7", "P71" ,"P72","B1G") #"B5G B9)
QNA_instrasset_to_keep<-c("_Z", "N11G")

df_QNA1<- df_QNA[,QNA_columns_to_keep]

df_QNA1 <- df_QNA1%>%
  filter(TRANSACTION %in% QNA_subject_to_keep)

df_QNA1 <- df_QNA1 %>%
  filter((INSTR_ASSET %in% QNA_instrasset_to_keep & TRANSACTION != "P51G") | 
           (INSTR_ASSET == "N11G" & TRANSACTION == "P51G"))


df_QNA1 <- rename(df_QNA1, country = REF_AREA)
df_QNA1 <- rename(df_QNA1, period = TIME_PERIOD)

#keep only GDP data from T0102 because duplicates with T0102
df_QNA1 <- subset(df_QNA1, !(TABLE_IDENTIFIER =="T0101" & TRANSACTION=="B1GQ") )
#data for Russia removed cause not allowed to publish data for RUS
df_QNA1 <- subset(df_QNA1, country !="RUS")

# CHECK for duplicates
# check <- df_QNA %>%
#  filter(country == "FRA" & TRANSACTION == "B1GQ")
# df_QNA <- subset(df_QNA, 
#                 TRANSACTION %in% QNA_subject_to_keep)

# derive Quarter-on-quarter growth rates from volume SA levels
df_QNA_growth <- df_QNA1 %>%
  arrange(country,TRANSACTION,SECTOR,ACTIVITY,TABLE_IDENTIFIER) %>%
  group_by(country,TRANSACTION,SECTOR,ACTIVITY,TABLE_IDENTIFIER) %>%
  mutate(growth = 100*(OBS_VALUE - dplyr::lag(OBS_VALUE))/dplyr::lag(OBS_VALUE))


#concatenate the subject
df_QNA_growth$SECTOR <- replace(df_QNA_growth$SECTOR, df_QNA_growth$SECTOR=="S1", NA)
df_QNA_growth$ACTIVITY <- replace(df_QNA_growth$ACTIVITY, df_QNA_growth$ACTIVITY=="_Z"|df_QNA_growth$ACTIVITY=="_T", NA)

df_QNA_growth$subject <- ifelse(is.na(df_QNA_growth$SECTOR) & is.na(df_QNA_growth$ACTIVITY),
                                paste(df_QNA_growth$TRANSACTION),
                                ifelse(is.na(df_QNA_growth$SECTOR),
                                       paste(df_QNA_growth$TRANSACTION, df_QNA_growth$ACTIVITY, sep = "."),
                                       ifelse(is.na(df_QNA_growth$ACTIVITY),
                                              paste(df_QNA_growth$TRANSACTION, df_QNA_growth$SECTOR, sep = "."),
                                              paste(df_QNA_growth$TRANSACTION, df_QNA_growth$SECTOR, df_QNA_growth$ACTIVITY, sep = "."))))


df_QNA_growth <- subset(df_QNA_growth, select=c("country", "period", "subject","OBS_VALUE", "growth"))

df_QNA_growth_reshaped <- pivot_wider(data = df_QNA_growth, 
                                      id_cols = c("country", "period"), 
                                      names_from = subject, 
                                      values_from = c(growth, OBS_VALUE))

###############################################################
#6. Import annual data in volumes for GVA_Q and GVA_P
##############################################################

url_ana="https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6/A....B1G..P+Q+_T...L+V..?startPeriod=2009&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_ANA<-read.csv(url_ana)

ANA_columns_to_keep<-c("REF_AREA","SECTOR","TRANSACTION","ACTIVITY","TIME_PERIOD","OBS_VALUE","TABLE_IDENTIFIER", "INSTR_ASSET", "PRICE_BASE")

df_ANA1<- df_ANA[,ANA_columns_to_keep]

df_ANA1 <- rename(df_ANA1, country = REF_AREA)
df_ANA1 <- rename(df_ANA1, period = TIME_PERIOD)

#keep only GDP data from T0102 because duplicates with T0102
#data for Russia removed cause not allowed to publish data for RUS
df_ANA1 <- subset(df_ANA1, country !="RUS")

# derive Quarter-on-quarter growth rates from volume SA levels
df_ANA_growth <- df_ANA1 %>%
  arrange(country,period,TRANSACTION,SECTOR,ACTIVITY,TABLE_IDENTIFIER,PRICE_BASE) %>%
  group_by(country,TRANSACTION,SECTOR,ACTIVITY,TABLE_IDENTIFIER,PRICE_BASE) %>%
  mutate(growth = 100*(OBS_VALUE - dplyr::lag(OBS_VALUE))/dplyr::lag(OBS_VALUE))


#concatenate the subject
df_ANA_growth$SECTOR <- replace(df_ANA_growth$SECTOR, df_ANA_growth$SECTOR=="S1", NA)
df_ANA_growth$ACTIVITY <- replace(df_ANA_growth$ACTIVITY, df_ANA_growth$ACTIVITY=="_Z"|df_ANA_growth$ACTIVITY=="_T", NA)

df_ANA_growth$subject <- ifelse(is.na(df_ANA_growth$SECTOR) & is.na(df_ANA_growth$ACTIVITY),
                                paste(df_ANA_growth$TRANSACTION, df_ANA_growth$PRICE_BASE, sep = "."),
                                ifelse(is.na(df_ANA_growth$SECTOR),
                                       paste(df_ANA_growth$TRANSACTION, df_ANA_growth$ACTIVITY, df_ANA_growth$PRICE_BASE, sep = "."),
                                       ifelse(is.na(df_ANA_growth$ACTIVITY),
                                              paste(df_ANA_growth$TRANSACTION, df_ANA_growth$SECTOR, df_ANA_growth$PRICE_BASE, sep = "."),
                                              paste(df_ANA_growth$TRANSACTION, df_ANA_growth$SECTOR, df_ANA_growth$ACTIVITY, df_ANA_growth$PRICE_BASE, sep = "."))))

df_ANA_growth <- subset(df_ANA_growth, select=c("country", "period", "subject","growth", "OBS_VALUE"))


df_ANA_growth_reshaped <- pivot_wider(data = df_ANA_growth, 
                                      id_cols = c("country", "period"), 
                                      names_from = subject, 
                                      values_from = c(growth, OBS_VALUE))


###############################################################
#7. Import population data
##############################################################

url_pop="https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,/..PS._T._T+Y20T64.?startPeriod=2010&endPeriod=2022&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_pop<-read.csv(url_pop)

# Assuming your dataset is named 'df'
# Convert TIME_PERIOD to character to match with AGE and SEX
df_pop$TIME_PERIOD <- as.character(df_pop$TIME_PERIOD)

# Subset the data for OBS_STATUS for Y2064 (AGE == "Y20T64") and _T (SEX == "_T")
obs_y2064 <- subset(df_pop, AGE == "Y20T64" & SEX == "_T", select=c(REF_AREA, TIME_PERIOD, OBS_VALUE))
obs_total <- subset(df_pop, SEX == "_T" & AGE == "_T", select=c(REF_AREA, TIME_PERIOD, OBS_VALUE))

# Merge the two subsets by REF_AREA and TIME_PERIOD
pop_data <- merge(obs_y2064, obs_total, by=c("REF_AREA", "TIME_PERIOD"), suffixes=c("_Y2064", "_Total"))

# Calculate the ratio of OBS_STATUS for Y2064 to _T
pop_data$OBS_VALUE <- pop_data$OBS_VALUE_Y2064 / pop_data$OBS_VALUE_Total
pop_data$OBS_VALUE = pop_data$OBS_VALUE*100

pop_data <- rename(pop_data, country = REF_AREA)
pop_data <- rename(pop_data, period = TIME_PERIOD)
pop_data <- rename(pop_data, working_age = OBS_VALUE)

pop_data <- subset(pop_data, select=c("country", "period", "working_age"))



################################################################
## 8.Combined Oxford data, QNA data and ELS data
################################################################

# Combine the 3 dataframes by country and period
#combined_df <- merge(df_QNA_growth_reshaped, tracker_data_Q_Diff,df_ELS_Q, by = c("country", "period"), all = TRUE)

combined_df_q <- merge(df_QNA_growth_reshaped, 
                       tracker_data_Q_Diff, 
                       df_ELS_Q, 
                       by.x = c("country", "period"),
                       by.y = c("country", "period"),
                       all = TRUE)

combined_df_a1 <- merge(df_ANA_growth_reshaped, 
                        tracker_data_Y, 
                        by = c("country", "period"), 
                        all = TRUE)

combined_df_a2 <- merge(combined_df_a1, 
                       df_ELS_Y, 
                       by = c("country", "period"), 
                       all = TRUE)

combined_df_a <- merge(combined_df_a2, 
                       pop_data, 
                       by = c("country", "period"), 
                       all = TRUE)

combined_df_a$growth_B1G.P.P<-combined_df_a$growth_B1G.P.V-combined_df_a$growth_B1G.P.L
combined_df_a$growth_B1G.Q.P<-combined_df_a$growth_B1G.Q.V-combined_df_a$growth_B1G.Q.L


###############################################################
#9. Import annual data for income levels
##############################################################

url_ppp="https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE_HVPVOB/A....B1GQ_POP.......?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_ppp<-read.csv(url_ppp)

ppp_columns_to_keep<-c("REF_AREA","TIME_PERIOD","OBS_VALUE")

ppp_data<- df_ppp[,ppp_columns_to_keep]

ppp_data <- rename(ppp_data, country = REF_AREA)
ppp_data <- rename(ppp_data, period = TIME_PERIOD)

#keep only GDP data from T0102 because duplicates with T0102
#data for Russia removed cause not allowed to publish data for RUS

ppp_data <- subset(ppp_data, select=c("country", "period", "OBS_VALUE"))

ppp_data <- rename(ppp_data, gdp_percap = OBS_VALUE)

combined_df_a <- merge(combined_df_a, 
                       ppp_data, 
                       by = c("country", "period"), 
                       all = TRUE)

###############################################################
#10. Import annual data for education government share
##############################################################

url_edu_finance="https://sdmx.oecd.org/public/rest/data/OECD.EDU.IMEP,DSD_EAG_UOE_FIN@DF_UOE_FIN_SOURCE_GV_PR_NDOM,3.0/..ISCED11_1T8.S13.INST_EDU.._Z.PT_EXP.?startPeriod=2009&endPeriod=2022&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
df_edu_char<-read.csv(url_edu_finance)

edu_keep<-c("REF_AREA","TIME_PERIOD","OBS_VALUE", "EDUCATION_LEV")

edu_data<- df_edu_char[,edu_keep]

edu_data1 <- edu_data %>%
  group_by(REF_AREA, TIME_PERIOD) %>%
  summarise(average_OBS_VALUE = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")


# Fill missing OBS_VALUE using the closest available year within each REF_AREA and EDUCATION_LEV
edu_data2 <- edu_data1 %>%
  arrange(REF_AREA,TIME_PERIOD) %>% # Ensure data is ordered
  group_by(REF_AREA) %>% # Group by REF_AREA and EDUCATION_LEV
  mutate(
    OBS_VALUE = zoo::na.approx(average_OBS_VALUE, TIME_PERIOD, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup()

edu_data2 <- rename(edu_data2, country = REF_AREA)
edu_data2 <- rename(edu_data2, period = TIME_PERIOD)
edu_data2 <- rename(edu_data2, educ_share = OBS_VALUE)


combined_df_a <- merge(combined_df_a, 
                       edu_data2, 
                       by = c("country", "period"), 
                       all = TRUE)

###############################################################
#11. Import annual data for government expenditure in health
##############################################################
# Keep only relevant columns
#health_exp_keep <- c("REF_AREA", "TIME_PERIOD", "OBS_VALUE", "EXP_SOURCE")
#health_exp <- health_exp[, health_exp_keep]

# Calculate the ratio
#health_exp_ratio <- health_exp %>%
#  filter(EXP_SOURCE %in% c("S14", "S13")) %>%
#  select(REF_AREA, TIME_PERIOD, EXP_SOURCE, OBS_VALUE) %>%
#  pivot_wider(names_from = EXP_SOURCE, values_from = OBS_VALUE) %>%
#  mutate(ratio = S14 / S13) %>%
#  select(REF_AREA, TIME_PERIOD, ratio)

# Impute missing values
#health_exp_ratio_imputed <- health_exp_ratio %>%
#  arrange(REF_AREA, TIME_PERIOD) %>%  # Ensure data is sorted by REF_AREA and TIME_PERIOD
#  group_by(REF_AREA) %>%             # Group by REF_AREA to handle each country separately
#  mutate(
#    health_ratio = ifelse(is.na(ratio), NA_real_, ratio)  # Prepare column for filling
#  ) %>%
#  fill(health_ratio, .direction = "down") %>%  # Fill missing values from prior years
#  ungroup()

# Prepare final data
#final_data <- health_exp_ratio_imputed %>%
#  rename(country = REF_AREA, period = TIME_PERIOD, health_exp_ratio = health_ratio) %>%
#  arrange(country, period)


#keep only GDP data from T0102 because duplicates with T0102
#data for Russia removed cause not allowed to publish data for RUS

#combined_df_a <- merge(combined_df_a, 
#                       final_data, 
#                       by = c("country", "period"), 
#                       all = TRUE)
###############################################################
#11. Import annual data for income inequality
##############################################################

#url_gini="https://sdmx.oecd.org/public/rest/data/OECD.WISE.INE,DSD_WISE_IDD@DF_IDD,1.0/.A.INC_DISP_GINI..._T.METH2012.D_CUR.?startPeriod=2010&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
#df_gini<-read.csv(url_gini)

#gini_columns_to_keep<-c("REF_AREA","TIME_PERIOD","OBS_VALUE")

#gini_data<- df_gini[,gini_columns_to_keep]

#gini_data <- rename(gini_data, country = REF_AREA)
#gini_data <- rename(gini_data, period = TIME_PERIOD)

#gini_data <- subset(gini_data, select=c("country", "period", "OBS_VALUE"))

#gini_data <- rename(gini_data, gini = OBS_VALUE)

#combined_df_a <- merge(combined_df_a, 
#                       gini_data, 
#                       by = c("country", "period"), 
#                      all = TRUE)

###############################################################
## Import Health classification data
###############################################################

library(readxl)
health_sys <- read_excel("C:/Users/sacksferrari_s/OneDrive - OECD/Working_Paper_GDP_method/health_system_els.xlsx")

# Merge usa_growth data into combined_df_a and update relevant columns for USA
combined_df_a <- merge(
  combined_df_a, 
  health_sys, 
  by = "country", 
  all.x = TRUE, 
)

# Update columns for USA and remove temporary columns in one step
combined_df_a$reference_area = NULL
combined_df_a$system_type = NULL 
# View the updated structure
str(combined_df_a)


###############################################################
#12. Add category for health and education method of estimation
###############################################################

#4 categories
#combined_df_q <- combined_df_q %>%
#  mutate(method_health = case_when(
#    country %in% c("AUS", "CHL", "COL", "CZE", "DNK", "POL", "KOR") ~ 1,
#    country %in% c("CAN", "IRL", "LVA", "MEX", "NZL", "SVK", "ESP") ~ 2,
#    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ 3,
#    country %in% c("BEL", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "PRT", "SVN", "SWE", "GBR") ~ 4,
#    TRUE ~ NA_integer_
#  ))


# 4 categories
combined_df_a <- combined_df_a %>%
  mutate(method_health = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR", "ISR") ~ "Deflation_Input",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator_Input",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation_Output",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))


#combined_df_q <- combined_df_q %>%
#  mutate(method_edu = case_when(
#    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
#    country %in% c("") ~ 2,
#    country %in% c("IRL", "LVA", "ESP") ~ 3,
#    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 4,
#    TRUE ~ NA_integer_
#  ))

#2 categories
#combined_df_a <- combined_df_a %>%
#  mutate(method_edu = case_when(
#    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
#    country %in% c("") ~ 2,
#    country %in% c("IRL", "LVA", "ESP") ~ 1,
#    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 2,
#    TRUE ~ NA_character_
#  ))

#3 categories
combined_df_a <- combined_df_a %>%
  mutate(method_edu_3 = case_when(
    country %in% c("JPN", "KOR", "COL", "USA", "ISR") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("CAN", "IRL", "LVA", "ESP", "MEX", "SVK") ~ "Input_Indicators",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "NOR", "NLD", "NZL", "POL", "PRT", "SVN", "SWE", "ZAF", "GBR") ~ "Output_Indicators",
    TRUE ~ NA_character_
  ))


#####################################
# Import imputed US data
#####################################

library(readxl)
usa_growth <- read_excel("C:/Users/sacksferrari_s/OneDrive - OECD/Working_Paper_GDP_method/usa_growth.xlsx")

# Merge usa_growth data into combined_df_a and update relevant columns for USA
combined_df_a <- merge(
  combined_df_a, 
  usa_growth, 
  by = c("country", "period"), 
  all.x = TRUE, 
  suffixes = c("", "_usa")
)

# Update columns for USA and remove temporary columns in one step
combined_df_a <- within(combined_df_a, {
  growth_B1G.P.L <- ifelse(country == "USA" & !is.na(growth_B1G.P.L_usa), growth_B1G.P.L_usa, growth_B1G.P.L)
  growth_B1G.Q.L <- ifelse(country == "USA" & !is.na(growth_B1G.Q.L_usa), growth_B1G.Q.L_usa, growth_B1G.Q.L)
  rm(growth_B1G.P.L_usa, growth_B1G.Q.L_usa) # Remove temporary columns
})

# View the updated structure
str(combined_df_a)




###################################################################

## Part 2: Data manipulation

####Clean for empty columns-rows and keep some variables only

##Annual data

combined_df_ss <- subset(combined_df_a, select=c("country", "period", "method_health", "growth_B1G.P.L", "growth_B1G.P.V", "growth_B1G.Q.L", "growth_B1G.Q.V", "growth_B1G.L", "growth_B1G.V", "method_edu_3", 
                                                 "excess_mortality", 
                                                 "working_age", "gdp_percap", 
                                                 "educ_share", "health_sys"))

data <- combined_df_ss %>%
  mutate(method_health_2 = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR", "ISR") ~ "Deflation",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator",
    TRUE ~ NA_character_
  ))

data <- data %>%
  mutate(method_health_input = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR", "ISR") ~ "input",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "input",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "output",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "output",
    TRUE ~ NA_character_
  ))


data <- data %>%
  mutate(year_factor = case_when(
    period <= 2019 ~ "pre-covid",
    period == 2020 ~ "2020",
    period == 2021 ~ "2021",
   period == 2022 ~ "2022",
    TRUE ~ NA_character_  # Optional: Handle years not specified in the categories
  ))

data <- data %>%
  filter(period >= 2010)
         
#& period <= 2021)



data$method_health = as.factor(data$method_health)
data$method_health_rlv <- relevel(data$method_health,"Indicator_Output")
data$method_health_2 = as.factor(data$method_health_2)
data$method_health_2 <- relevel(data$method_health_2,"Indicator")
data$method_health_input = as.factor(data$method_health_input)
data$method_health_input <- relevel(data$method_health_input,"output")

data$method_edu_3 <- as.factor(data$method_edu_3)
data$method_edu_3 <- relevel(data$method_edu_3,"Output_Indicators")


data$method_edu_3 <- as.factor(data$method_edu_3)
data$method_edu_3_rl <- relevel(data$method_edu_3,"Deflation_Input")

data$year_factor <- relevel(as.factor(data$year_factor),"pre-covid")


data$health_sys <- as.factor(data$health_sys)
data$health_sys <- relevel(data$health_sys,"nhi")
#######################################################
#Incorporate contribution and non market output data:


library(readxl)
contributions <- read_excel("C:/Users/sacksferrari_s/OneDrive - OECD/Working_Paper_GDP_method/contributions.xlsx")

# Assuming your dataset is called contributions
contrib <- contributions %>%
  # Select only the relevant columns
  select(country, measure, average) %>%
  # Pivot the data to make measures as columns
  pivot_wider(
    names_from = measure, 
    values_from = average
  )


contrib <- contrib %>% mutate_all(~ifelse(is.nan(.), NA, .))

data <- data %>%
  left_join(contrib, by = c("country"))


delete = c("combined_df_a1", "combined_df_a2", "combined_df_q", "contrib", 
           "contributions", "df_ANA", "df_ANA_growth", "df_ANA_growth_reshaped", 
           "df_ANA1", "df_ELS", "df_edu_char",
           "df_ELS_Q", "df_ELS_Y", "df_QNA", "df_QNA_growth", "df_QNA1", 
           "tracker_data_D", "tracker_data_Q", "tracker_data_Q_Diff", 
           "df_pop", "df_ppp", "df_QNA_growth_reshaped",
           "edu_data", "edu_data1", "edu_data2", "obs_total", "obs_y2064", 
           "pop_data", "ppp_data", "tracker_data_A", "tracker_data_Y",
           "tracker_data_Y_Diff", "usa_growth")

rm(list=delete)

data$`NA` = NULL

data <- data %>%
  filter(rowSums(is.na(.)) < 15)

##### Convert non market output variables to groups


data$excess_mortality[data$year_factor == "pre-covid"] <- 0


# View the result
print(data)
####################################
# UP TO HERE, THIS IS DATA MANIPULATION.
####################################

#alternatively, import data:
library(openxlsx)
write.xlsx(data, "complete_data_21jan.xlsx")
save(data, file = "complete_data_21jan.RData")
#to load:
#load("complete_data_november25.RData")


##########################################
#Part 3: Different visualizations
##########################################

# #First, stringency vs B1GQ separated by health and period
# #Same but adding trend line / plot all groups in one pane
# plot2 <- ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health_4))) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size=2) +
#   ggtitle("GVA for Health Sector and Year") +
#   scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Add custom labels
# 
# plot3 <-ggplot(data = combined_df_ss %>% filter(!is.na(growth_B1G.P.L)), 
#        aes(x = period, y = growth_B1G.P.L, color = factor(method_edu))) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size = 2) +
#   ggtitle("GVA for Education Sector and Year") +
#   scale_color_discrete(name = "Estimation method", labels = c("Indirect", "Direct"))  #
# 
# plot4 <- ggplot(data = combined_df_ss %>% filter(!is.na(method_edu)), 
#                 aes(x = period, y = growth_B1G.P.L, color = factor(method_edu_3))) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size = 2) +
#   ggtitle("GVA for Education Sector and Year") +
#   scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-direct"))  # Add custom labels
# 
# 
# plot5 <- ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health))) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size=2) +
#   ggtitle("GVA for Health Sector and Year") +
#   scale_color_discrete(name = "Estimation method", labels = c("Indirect", "Direct"))  # Add custom labels
# 
# 
# #Same but adding trend line / plot all groups in one pane
# ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health))) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size=2) +
#   geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
#   ggtitle("GVA for Health Sector and Year") +
#   facet_grid(combined_df_ss$method_health) +
#   scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Add custom labels
# 
# #RD - with prices
# 
# #Health
# #ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.P, color = factor(method_health), group = factor(method_health))) +
# #  geom_point(alpha = 0.5) +
# #  geom_text(aes(label = country), vjust = -0.5, size=2) +
# #  ggtitle("GVA.Q (implicit prices) and Year by Health Est. Methods") +
# #  scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Custom color labels
# 
# #Education
# #ggplot(combined_df_ss, aes(x = period, y = B1G.P.P, color = factor(method_edu), group = factor(method_edu))) +
# #  geom_point(alpha = 0.5) +
# #  geom_text(aes(label = country), vjust = -0.5, size=2) +
# #  ggtitle("GVA.P (implcit prices) and Year by Education Est. Methods") +
# #  scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Custom color labels




###################################
# Part IV: Regressions
###################################



####Trying out a fixed/random/mixed effects model

## summary(gpa_mixed)

#model = lm(growth_B1G.Q.L ~ factor(method_health)*period, data = combined_df_a)
#summary(model)




#random_effects <- plm(growth_B1G.Q.V ~ as.factor(method_health),
#                    index = c("period"),
#                    data = combined_df_a, 
#                    model = "random")

#summary(random_effects)



######### FIGURES

#combined_df_a$period <- as.factor(combined_df_a$period)

#plot = subset(combined_df_a, select = c("country", "period", "growth_B1G.L", "growth_B1G.V", "growth_B1G.Q.L", "growth_B1G.Q.V", "growth_B1G.P.L", "growth_B1G.P.V",  "method_health", "method_edu", "method_health_4", "method_edu_3", "excess_mortality"))
#plot$method_health = as.factor(plot$method_health)
#plot$period = as.factor(plot$period)
#plot = drop_na(plot)
# Assuming 'method_health' is a column in your dataset combined_df_a

# Replace 'method_health' with the actual column name if different
#plot$period <- as.factor(plot$period)

# Plot the boxplot with 'period' as x-axis
# Define color palette
#color_palette <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")


#plot6 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health_4))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
#  theme(legend.position = "bottom") +
# scale_fill_manual(name = "Estimation method", 
#                    labels = c("Deflation input prices", "Input indicators", "Deflation output prices", "Output indicators"))

#print(plot6)


#plot7 <- ggplot(plot, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu_3))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Output indicators"), values = color_palette)


#plot8 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


#plot9 <- ggplot(plot, aes(x = factor(period), y = growth_B1G.P.L, fill = factor(method_edu))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)



######BOX PLOTS CONTRIBUTIONS TO GROWTH
#install.packages("ggforce")
#library(ggforce)
#ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth Share in GVA Education", title = "Boxplot ofgrowth of Share of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") 


#library(ggplot2)
#library(ggforce)

#ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth Share in GVA Education", title = "Boxplot of Growth of Share of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") +
#  facet_zoom(ylim = c(-1, 1), zoom.data = contrib_B1G_P_V >= -1 & contrib_B1G_P_V <= 1)


#library(ggplot2)
#library(grid)

# Main plot without outliers
#main_plot <- ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75), outlier.shape = NA) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth Share in GVA Education") +
#  coord_cartesian(ylim = c(-0.5, 0.5))

# Outliers plot
#outliers <- subset(data, contrib_B1G_P_V < -1 | contrib_B1G_P_V > 1)
#outlier_plot <- ggplot(outliers, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
#  geom_point(position = position_dodge(width = 0.75), alpha = 1) +
#  theme_minimal() +
#  labs(x = NULL, y = NULL) +
#  coord_cartesian(ylim = c(-4, -1)) +
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Arrange the plots using grid
#grid.newpage()
#pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(2, 1), "null"))))

#print(main_plot, vp = viewport(layout.pos.row = 1))
#print(outlier_plot, vp = viewport(layout.pos.row = 2))

data2 <- data[!is.na(data$method_health) & !is.na(data$method_edu_3), ]


#Models

model_1 = lm(growth_B1G.Q.L ~  year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q+working_age+gdp_percap, data = data)
summary(model_1)

model_1 = lm(growth_B1G.Q.L ~  year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q+working_age+gdp_percap+as.factor(health_sys), data = data)
summary(model_1)


model_2 = lm(growth_B1G.Q.L ~  year_factor*method_health_2+growth_B1G.Q.V + share_nmo_Q+working_age+gdp_percap++as.factor(health_sys), data = data)
summary(model_2)


model_3 = lm(growth_B1G.Q.L ~  year_factor*method_health_input+growth_B1G.Q.V + share_nmo_Q+working_age+gdp_percap+as.factor(health_sys), data = data)
summary(model_3)


model_4 = lm(growth_B1G.P.L ~  year_factor*method_edu_3+growth_B1G.P.V+share_nmo_P + working_age+gdp_percap +educ_share, data = data)
summary(model_4)


model_5 = lm(growth_B1G.L ~  year_factor*method_health_rlv+growth_B1G.V+share_nmo_Q + working_age + gdp_percap + as.factor(health_sys), data = data)
summary(model_5)

model_6 = lm(growth_B1G.L ~  year_factor*method_edu_3+growth_B1G.V+share_nmo_P + working_age + gdp_percap + educ_share, data = data)
summary(model_6)



#Histograms
library(scales)

data_clean <- data %>%
  filter(!is.na(method_health))

#FIGURE 2

# Density plots
p1 <- ggplot(data_clean, aes(x = growth_B1G.Q.L, color = method_health, fill = method_health)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Growth GVA Health",
       y = "Density") +
  theme_minimal()

p1 + guides(color = guide_legend(title = "Estimation Method - Health"),
            fill = guide_legend(title = "Estimation Method - Health"),
            override.aes = list(fill = scales::alpha("white", 0.5))) 

#Histograms
library(dplyr)
library(ggplot2)
library(scales)

# Remove rows with missing values in the specified columns
filtered_data <- data %>%
  filter(!is.na(growth_B1G.P.L) & !is.na(method_edu_3_rl) & !is.na(year_factor))

filtered_data <- data %>%
  filter(!is.na(growth_B1G.Q.L) & !is.na(method_health_rlv) & !is.na(year_factor))


# Define the colors in the desired order
my_colors <- c("#C77CFF", "#F8766D", "#00BFC4")

# Update your ggplot code to use these colors
##FIGURE 4

p2 <- ggplot(filtered_data, aes(x = growth_B1G.P.L, color = method_edu_3, fill = method_edu_3)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Growth GVA Education",
       y = "Density of Obs") +
  theme_minimal() +
  scale_color_manual(values = setNames(my_colors, unique(filtered_data$method_edu_3))) +
  scale_fill_manual(values = setNames(my_colors, unique(filtered_data$method_edu_3)))

# Adjust the legend titles and transparency
p2 + guides(color = guide_legend(title = "Estimation Method - Education"),
            fill = guide_legend(title = "Estimation Method - Education"),
            override.aes = list(fill = scales::alpha("white", 0.5))) 

### density plots with adjusted axis

# Calculate the maximum density for both datasets
max_density_health <- max(ggplot_build(p1)$data[[1]]$density)
max_density_edu <- max(ggplot_build(p2)$data[[1]]$density)

# Set the maximum density across both plots
max_density <- max(max_density_health, max_density_edu)

# Adjust the y-axis for the first plot
p1 <- ggplot(data_clean, aes(x = growth_B1G.Q.L, color = method_health, fill = method_health)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max_density)) +
  labs(x = "Growth GVA Health",
       y = "Density") +
  theme_minimal() +
  guides(color = guide_legend(title = "Estimation Method - Health"),
         fill = guide_legend(title = "Estimation Method - Health"),
         override.aes = list(fill = scales::alpha("white", 0.5)))

# Adjust the y-axis for the second plot
p2 <- ggplot(filtered_data, aes(x = growth_B1G.P.L, color = method_edu_3, fill = method_edu_3)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max_density)) +
  labs(x = "Growth GVA Education",
       y = "Density of Obs") +
  theme_minimal() +
  scale_color_manual(values = setNames(my_colors, unique(filtered_data$method_edu_3))) +
  scale_fill_manual(values = setNames(my_colors, unique(filtered_data$method_edu_3))) +
  guides(color = guide_legend(title = "Estimation Method - Education"),
         fill = guide_legend(title = "Estimation Method - Education"),
         override.aes = list(fill = scales::alpha("white", 0.5)))

# Print the plots
p1
p2


# Variance plots

variance_data_health <- data %>%
  group_by(method_health, period) %>%
  summarise(across(starts_with("growth"), ~ var(.x, na.rm = TRUE), .names = "var_{col}"))


variance_data_edu <- data %>%
  group_by(method_edu_3_rl, period) %>%
  summarise(across(starts_with("growth"), ~ var(.x, na.rm = TRUE), .names = "var_{col}"))



variance_data_clean <- variance_data_edu %>%
  filter(!is.na(method_edu_3_rl))

variance_data_clean <- variance_data_clean %>%
  filter(!is.na(var_growth_B1G.P.L))


variance_data_clean_H <- variance_data_health %>%
  filter(!is.na(var_growth_B1G.Q.L))

variance_data_clean_H <- variance_data_clean_H %>%
  filter(!is.na(method_health))

# Assuming `variance_data_clean` has the required structure
#FIGURE 5

p <- (ggplot(variance_data_clean_H, aes(x = as.factor(period), y = var_growth_B1G.Q.L, color = method_health, group = method_health)) +
        geom_line() +
        geom_point() +
        labs(x = "Period",
             y = "Variance of GVA in Health")) + 
        theme_minimal()
p + guides(color = guide_legend(title = "Estimation Method - Health")) +
  coord_cartesian(ylim = c(0, 90)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))



#FIGURE 6

p <- (ggplot(variance_data_clean, aes(x = as.factor(period), y = var_growth_B1G.P.L, color = method_edu_3_rl, group = method_edu_3_rl)) +
        geom_line() +
        geom_point() +
        labs(x = "Period",
             y = "Variance of GVA in Education")) +
  theme_minimal() +
  scale_color_manual(values = setNames(my_colors, unique(variance_data_clean$method_edu_3_rl))) +
  scale_fill_manual(values = setNames(my_colors, unique(variance_data_clean$method_edu_3_rl)))
p + guides(color = guide_legend(title = "Estimation Method - Education")) +
  coord_cartesian(ylim = c(0, 90)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))


#Boxplots for each estimation method


# Plot the boxplot with 'period' as x-axis
# Define color palette
#color_palette <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")


#plot6 <- ggplot(data, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health  by Period and Method Health") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Deflation output prices", "Output indicators"), values = color_palette)

# plot7 <- ggplot(data, aes(x = factor(period), y = growth_B1G.P.L, fill = factor(method_edu_3))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Output indicators"))


#plot8 <- ggplot(data, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health_2))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


#plot9 <- ggplot(data, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu))) +
#  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
#  theme_minimal() +
#  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
#  theme(legend.position = "bottom") +
#  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


###########
#Diagnostic tests
###########
#ggplot(data = model_1, aes(x = model_1$residuals)) +
#  geom_histogram(fill = 'steelblue', color = 'black') +
#  labs(title = 'Histogram of Residuals: Model 1', x = 'Residuals', y = 'Frequency')


### Separating data into 2010-2019 

data_historical <- data2 %>% 
  filter(period >= 2010 & period <= 2019)

data_2020 <- data2 %>% 
  filter(period >= 2010 & period <= 2021)








#################################### THESE ARE THE ANOVA TO USE

library(rstatix)


#t_test_result <- anova_test(growth_B1G.Q.L ~ as.factor(method_health_input)+growth_B1G.Q.V, data = data_historical)

#t_test_result <- anova_test(growth_B1G.Q.L ~ period*method_health_rlv+growth_B1G.Q.V + share_nmo_Q, data = data_historical)
#t_test_result <- anova_test(growth_B1G.Q.L ~ period*method_health_rlv+growth_B1G.Q.V + share_nmo_Q + working_age + gdp_percap, data = data_historical)
health_complete <- anova_test(growth_B1G.Q.L ~ year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q + working_age + gdp_percap + as.factor(health_sys), data = data)
health_covid <- anova_test(growth_B1G.Q.L ~ year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q + working_age + gdp_percap + as.factor(health_sys), data = data_2020)
health_baseline <- anova_test(growth_B1G.Q.L ~ period*method_health+growth_B1G.Q.V + share_nmo_Q + working_age + gdp_percap, data = data_historical)



t_test_result <- anova_test(growth_B1G.P.L ~ period*method_edu_3+growth_B1G.P.V + share_nmo_P, data = data_historical)
t_test_result
t_test_result <- anova_test(growth_B1G.P.L ~ period*method_edu_3+growth_B1G.P.V + share_nmo_P, data = filtered_data)
t_test_result

edu_complete <-anova_test(growth_B1G.P.L ~ period*method_edu_3_rl+growth_B1G.P.V + share_nmo_P + working_age + gdp_percap + educ_share, data = data_2020)
edu_baseline <-anova_test(growth_B1G.P.L ~ period*method_edu_3_rl+growth_B1G.P.V + share_nmo_P + working_age + gdp_percap + educ_share, data = data_historical)

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()

# Add sheets and write data to them
addWorksheet(wb, "ANOVA Table 1")
writeData(wb, "ANOVA Table 1", health_complete)

addWorksheet(wb, "ANOVA Table 2")
writeData(wb, "ANOVA Table 2", health_baseline)

# Add sheets and write data to them
addWorksheet(wb, "ANOVA Table 3")
writeData(wb, "ANOVA Table 3", edu_complete)

addWorksheet(wb, "ANOVA Table 4")
writeData(wb, "ANOVA Table 4", edu_baseline)



# Save the workbook
saveWorkbook(wb, "anova_tables_15jan25.xlsx", overwrite = TRUE)

#######################################################



#########
#Descriptive statistics
library(vtable)

data_summary = filter(data2)
sumtable(data_summary,
         out="csv",
         file="summary_stats.csv")


##################
# extract
##################

library(jtools)
export_summs(model_1, model_2, model_3, scale = F, 
             error_format = "[{conf.low}, {conf.high}]", 
             to.file = "docx", file.name = "results_models1-3_new.docx")

library(jtools)
export_summs(model_4, model_5, model_6, scale = FALSE, 
             error_format = "[{conf.low}, {conf.high}]", 
             to.file = "docx", file.name = "results_models4-6_new.docx")


library(jtools)
export_summs(model_1, model_4, scale = FALSE, 
             error_format = "[{conf.low}, {conf.high}]", 
             to.file = "docx", file.name = "results_models1-4.docx")




######## WEIGHTED MODEL

install.packages("lmtest")
library(lmtest)
library(dpylr)


# Filter out NA values and select relevant columns



model_1 = lm(growth_B1G.Q.L ~  year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q+working_age+gdp_percap+health_sys, data = data)
summary(model_1)

model_4 = lm(growth_B1G.P.L ~  year_factor*method_edu_3+growth_B1G.P.V+share_nmo_P + working_age+gdp_percap +educ_share, data = data)
summary(model_4)


model_5 = lm(growth_B1G.L ~  year_factor*method_health_rlv+growth_B1G.V+share_nmo_Q + working_age + gdp_percap + as.factor(health_sys), data = data)
summary(model_5)

model_6 = lm(growth_B1G.L ~  year_factor*method_edu_3+growth_B1G.V+share_nmo_P + working_age + gdp_percap + educ_share, data = data)
summary(model_6)

# Calculate weights based on the standard deviation of growth_B1G.Q.L
# Replicate the weight for each row in data_filtered


###model wt for education as well. 
# Extract the data used in model_1
model_data <- model.frame(model_1)

# Calculate the absolute residuals and fitted values
abs_resid <- abs(model_1$residuals)
fitted_vals <- model_1$fitted.values

# Fit a model to estimate the variance function
variance_model <- lm(abs_resid ~ fitted_vals)

# Compute the weights
wt <- 1 / (variance_model$fitted.values^2)

# Fit the weighted regression model using model_data and wt
model_wt <- lm(growth_B1G.Q.L ~ year_factor * method_health_rlv + 
                 growth_B1G.Q.V + share_nmo_Q + working_age + gdp_percap + health_sys, 
               data = model_data, weights = wt)
summary(model_wt)

model_data4 <- model.frame(model_4)

# Calculate the absolute residuals and fitted values
abs_resid <- abs(model_4$residuals)
fitted_vals <- model_4$fitted.values

# Fit a model to estimate the variance function
variance_model <- lm(abs_resid ~ fitted_vals)

# Compute the weights
wt4 <- 1 / (variance_model$fitted.values^2)

# Fit the weighted regression model using model_data and wt
model_wt4 <- lm(growth_B1G.P.L ~  year_factor*method_edu_3+growth_B1G.P.V+share_nmo_P + working_age+gdp_percap +educ_share, 
               data = model_data4, weights = wt4)


library(jtools)

jtools::summ(model_wt, scale = F, confint = TRUE, digits = 2)
export_summs(model_1, model_wt, model_4, model_wt4, scale = FALSE, 
             error_format = "[{conf.low}, {conf.high}]", 
             to.file = "docx", file.name = "results_jan_weightedreg.docx")

#### How to see included countries

included_countries <- unique(data$country[rownames(data) %in% rownames(model_1$model)])
print(included_countries)

detach("package:rstatix", unload=TRUE)


# Extract the countries included in the analysis
included_countries <- unique(included_data$country)

# View the included countries
print(included_countries)




#####
#Regression plots
library(jtools)
library(ggplot2)
library(gridExtra)
plot_1 <- plot_coefs(model_1) +
  ggtitle("Model 1")

plot_2 <- plot_coefs(model_2) +
  ggtitle("Model 2")

plot_3 <- plot_coefs(model_3) +
  ggtitle("Model 3")

# Combine the plots into a single screen
grid.arrange(plot_1, plot_2, plot_3, ncol = 1)




summ(model_1, confint = TRUE, digits=3)
summ(model_2, confint = TRUE, digits=3)
