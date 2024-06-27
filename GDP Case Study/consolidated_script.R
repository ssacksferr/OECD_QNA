# Case Study: GDP and components

## Part 1: Input data

.libPaths("g:/r/win-library/4.3")
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
Path1 <- "\\\\FS19-MB-2\\SdataSDD\\Applic\\QNA\\A.Case study GDP and components\\Covid\\input data"
setwd(Path1)

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
# extract yearly data
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
# 2. RETRIEVE ELS EXCESS MORTALITY WEEKLY DATA FROM V8
############################################################################################################################################

url="https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_MORTALITY@DF_MORTALITY,1.0/.W.EM._T._T.PC_DT_A?startPeriod=2020-W01&endPeriod=2022-W52&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
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
# RETRIEVE ELS EXCESS MORTALITY WEEKLY DATA FROM V8 - yearly
########################################################

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_MORTALITY@DF_MORTALITY,1.0/.W.EM._T._T.PC_DT_A?startPeriod=2020-W01&endPeriod=2022-W52&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
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
# 3. RETRIEVE QNA DATA FROM V8 FOR: adjustment=Y,price_base=L or Q, tables T0101/0102,transfo=N
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
#Import annual data in volumes for GVA_Q and GVA_P
##############################################################

url_ana="https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0/A....B1G..P+Q+_T...L+V..?startPeriod=2009&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
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


################################################################
## 3.Combined Oxford data, QNA data and ELS data
################################################################

# Combine the 3 dataframes by country and period
# combined_df <- merge(df_QNA_growth_reshaped, tracker_data_Q_Diff,df_ELS_Q, by = c("country", "period"), all = TRUE)

#combined_df_q <- merge(df_QNA_growth_reshaped, 
#                       tracker_data_Q_Diff, 
#                       df_ELS_Q, 
#                       by.x = c("country", "period"),
#                       by.y = c("country", "period"),
#                       all = TRUE)

combined_df_a1 <- merge(df_ANA_growth_reshaped, 
                        tracker_data_Y, 
                        by = c("country", "period"), 
                        all = TRUE)

combined_df_a <- merge(combined_df_a1, 
                       df_ELS_Y, 
                       by = c("country", "period"), 
                       all = TRUE)

combined_df_a$growth_B1G.P.P<-combined_df_a$growth_B1G.P.V-combined_df_a$growth_B1G.P.L
combined_df_a$growth_B1G.Q.P<-combined_df_a$growth_B1G.Q.V-combined_df_a$growth_B1G.Q.L


# Add category for health and education method of estimation

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
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "Deflation_Input",
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
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("IRL", "LVA", "ESP", "MEX") ~ "Input_Indicators",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ "Output_Indicators",
    TRUE ~ NA_character_
  ))


###################################################################

## Part 2: Data manipulation

####Clean for empty columns-rows and keep some variables only

##Annual data

combined_df_ss <- subset(combined_df_a, select=c("country", "period", "method_health", "growth_B1G.P.L", "growth_B1G.P.V", "growth_B1G.Q.L", "growth_B1G.Q.V", "growth_B1G.L", "growth_B1G.V", "growth_B1G.Q.P", "growth_B1G.P.P", "method_edu_3", "excess_mortality"))

data <- combined_df_ss %>%
  mutate(method_health_2 = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "Deflation",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator",
    TRUE ~ NA_character_
  ))

data <- data %>%
  mutate(method_health_input = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "input",
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
  filter(period >= 2010 & period <= 2021)

data$method_health = as.factor(data$method_health)
data$method_health_rlv <- relevel(data$method_health,"Indicator_Output")
data$method_health_2 = as.factor(data$method_health_2)
data$method_health_2 <- relevel(data$method_health_2,"Indicator")
data$method_health_input = as.factor(data$method_health_input)
data$method_health_input <- relevel(data$method_health_input,"output")

data$method_edu_3 <- as.factor(data$method_edu_3)
data$method_edu_3 <- relevel(data$method_edu_3,"Output_Indicators")

data$year_factor <- relevel(as.factor(data$year_factor),"pre-covid")

#######################################################
#Incorporate contribution and non market output data:


library(readxl)
contributions <- read_excel("C:/Users/sacksferrari_s/OneDrive - OECD/Working_Paper_GDP_method/contributions.xlsx")
View(contributions)

# Pivot the dataset to long format
contrib <- contributions %>%
  pivot_longer(cols = starts_with("20"), names_to = "period", values_to = "value") %>%
  mutate(period = as.integer(period))

# Step 2: Pivot wider to create columns for each measure
contrib <- contrib %>%
  pivot_wider(names_from = measure, values_from = value)


data <- data %>%
  left_join(contrib, by = c("country", "period"))

delete = c("combined_df_a1", "combined_df_q", "df_ANA", "df_ANA_growth", "df_ANA1", "df_ELS", 
           "df_ELS_Q", "df_ELS_Y", "df_QNA", "df_QNA_growth", "df_QNA1", 
           "tracker_data_D", "tracker_data_Q", "tracker_data_Q_Diff")

rm(list=delete)

data$`NA` = NULL

data <- data %>%
  filter(rowSums(is.na(.)) < 15)

##### Convert non market output variables to groups

data$nmo_q_group <- cut(data$share_nmo_Q, 
                   breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                   labels = c("0 to 0.2", "0.21 to 0.4", "0.41 to 0.6", "0.61 to 0.8", "0.81 to 1"),
                   include.lowest = TRUE)

# Convert to factor
data$nmo_q_group <- as.factor(data$nmo_q_group)

# View the result
print(data$nmo_q_group)

data$nmo_p_group <- cut(data$share_nmo_P, 
                        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                        labels = c("0 to 0.2", "0.21 to 0.4", "0.41 to 0.6", "0.61 to 0.8", "0.81 to 1"),
                        include.lowest = TRUE)

# Convert to factor
data$nmo_p_group <- as.factor(data$nmo_p_group)

# View the result
#print(data$nmo_p_group)

data <- data %>%
  arrange(country, year_factor) %>%
  group_by(country) %>%
  mutate(share_nmo_Q = ifelse(year_factor == 2021 & is.na(share_nmo_Q),
                              share_nmo_Q[year_factor == 2020],
                              share_nmo_Q)) %>%
  ungroup()



# Impute missing values for the filtered countries


data <- data %>%
  arrange(country, year_factor) %>%
  group_by(country) %>%
  mutate(share_nmo_P = ifelse(year_factor == 2021 & is.na(share_nmo_P),
                              share_nmo_P[year_factor == 2020],
                              share_nmo_P)) %>%
  ungroup()


# View the result
print(df)
####################################
# UP TO HERE, THIS IS DATA MANIPULATION.
####################################

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


plot6 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health_4))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", 
                    labels = c("Deflation input prices", "Input indicators", "Deflation output prices", "Output indicators"))

print(plot6)


plot7 <- ggplot(plot, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Output indicators"), values = color_palette)


plot8 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


plot9 <- ggplot(plot, aes(x = factor(period), y = growth_B1G.P.L, fill = factor(method_edu))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)



######BOX PLOTS CONTRIBUTIONS TO GROWTH
install.packages("ggforce")
library(ggforce)
ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth Share in GVA Education", title = "Boxplot ofgrowth of Share of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") 


library(ggplot2)
library(ggforce)

ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth Share in GVA Education", title = "Boxplot of Growth of Share of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  facet_zoom(ylim = c(-1, 1), zoom.data = contrib_B1G_P_V >= -1 & contrib_B1G_P_V <= 1)


library(ggplot2)
library(grid)

# Main plot without outliers
main_plot <- ggplot(data, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_minimal() +
  labs(x = "Period", y = "Growth Share in GVA Education") +
  coord_cartesian(ylim = c(-0.5, 0.5))

# Outliers plot
outliers <- subset(data, contrib_B1G_P_V < -1 | contrib_B1G_P_V > 1)
outlier_plot <- ggplot(outliers, aes(x = factor(period), y = contrib_B1G_P_V, fill = factor(method_edu_3))) +
  geom_point(position = position_dodge(width = 0.75), alpha = 1) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(-4, -1)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Arrange the plots using grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(2, 1), "null"))))

print(main_plot, vp = viewport(layout.pos.row = 1))
print(outlier_plot, vp = viewport(layout.pos.row = 2))



#Models

model_1 = lm(growth_B1G.Q.L ~  year_factor*method_health_rlv+growth_B1G.Q.V + share_nmo_Q, data = data)
summary(model_1)

# Filter out rows where method_health is NA
filtered_data <- data[!is.na(data$method_health), ]

ggplot(filtered_data, aes(x = growth_B1G.Q.L, y = share_nmo_Q, color = method_health)) +
  geom_point(size = 3, alpha = 0.7) +  # Use points for each data point with increased size and transparency
  geom_text(aes(label = country), size = 3, vjust = -0.5) +  # Add country labels with slightly larger size
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Add linear trend lines with dashed lines
  xlim(-40, 40) +
  ylim(0, 1) +
  geom_vline(xintercept = 0, linetype = "solid", size = 1, color = "black") +  # Add bold vertical line at x = 0
  labs(x = "Growth B1G.Q.V", y = "Share NMO Q", color = "Health Method") +  # Improve axis and legend labels
  facet_wrap(~ year_factor, scales = "free") +  # Facet by year_factor
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and bold plot title
    axis.title = element_text(size = 12, face = "bold"),  # Bold axis titles
    axis.text = element_text(size = 10),  # Increase axis text size
    legend.title = element_text(size = 10),  # Increase legend title size
    legend.text = element_text(size = 9),  # Increase legend text size
    strip.text = element_text(size = 10)  # Increase facet label size
  )

plot(model_1)

anova1 = aov(model_1)
summary(anova1)

model_2 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_2+growth_B1G.Q.V, data = data)
summary(model_2)


model_3 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_input+growth_B1G.Q.V + data = data)
summary(model_3)


model_4 = lm(growth_B1G.P.L ~  as.factor(year_factor_rlv)*method_edu_3+growth_B1G.P.V, data = data)
summary(model_4)


model_5 = lm(growth_B1G.L ~  as.factor(year_factor_rlv)*method_health_rlv+growth_B1G.V, data = data)
summary(model_5)

model_6 = lm(growth_B1G.L ~  as.factor(year_factor_rlv)*method_edu_3+growth_B1G.V, data = data)
summary(model_6)



#Models

model_1 = lm(growth_B1G.Q.L ~  year_factor*method_health_rlv+growth_B1G.Q.V+share_nmo_Q, data = data)
summary(model_1)

anova1 = aov(model_1)
summary(anova1)

model_2 = lm(growth_B1G.Q.L ~  year_factor*method_health_2+growth_B1G.Q.V+share_nmo_Q, data = data)
summary(model_2)


model_3 = lm(growth_B1G.Q.L ~  year_factor*method_health_input+growth_B1G.Q.V +share_nmo_Q, data = data)
summary(model_3)


model_4 = lm(growth_B1G.P.L ~  year_factor*method_edu_3+growth_B1G.P.V+share_nmo_P, data = data)
summary(model_4)


model_5 = lm(growth_B1G.L ~  as.factor(year_factor_rlv)*method_health_rlv+growth_B1G.V, data = data)
summary(model_5)

model_6 = lm(growth_B1G.L ~  as.factor(year_factor_rlv)*method_edu_3+growth_B1G.V, data = data)
summary(model_6)



#Histograms

data <- data %>%
  group_by(method_health, year_factor_rlv) %>%
  mutate(total_count = n()) %>%
  ungroup() %>%
  group_by(method_health, year_factor_rlv, growth_B1G.L) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(percentage = count / total_count)

data_clean <- data %>%
  filter(!is.na(growth_B1G.Q.L) & !is.na(method_health) & !is.na(year_factor_rlv))


# Create the histogram
ggplot(data_clean, aes(x = growth_B1G.Q.L, color = method_health, fill = method_health)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor_rlv) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Density Plot of growth_B1G.Q.L",
       x = "Growth B1G.Q,L",
       y = "Density") +
  theme_minimal()

# Variance plots

variance_data <- data %>%
  group_by(method_health_rlv, period) %>%
  summarise(across(starts_with("growth"), ~ var(.x, na.rm = TRUE), .names = "var_{col}"))



variance_data_clean <- variance_data %>%
  filter(!is.na(period) & !is.na(var_growth_B1G.Q.L))

result <- variance_data_clean %>%
  select(method_health_rlv, period, var_growth_B1G.Q.L) %>%
  group_by(method_health_rlv, period) %>%
  summarize(value = first(var_growth_B1G.Q.L), .groups = 'drop') %>%
  pivot_wider(names_from = period, values_from = value)


result <- data %>%
  select(method_health_rlv, period, growth_B1G.Q.L) %>%
  group_by(method_health_rlv, period) %>%
  summarize(value = first(growth_B1G.Q.L), .groups = 'drop') %>%
  pivot_wider(names_from = period, values_from = value)

result
plot(result)

variance_of_averages <- average_growth %>%
  group_by(period) %>%
  summarize(variance = var(avg_growth), .groups = 'drop')

# Print the variance of averages table
print(variance_of_averages)

write_xlsx(result, "output_table_avg.xlsx")

p <- (ggplot(variance_data_edu, aes(x = as.factor(period), y = var_growth_B1G.P.L, color = method_edu_3, group = method_edu_3)) +
        geom_line() +
        geom_point() +
        labs(title = "Fig 1: Variance of GVA over time period for each estimation method group",
             x = "Period",
             y = "Variance of growth_B1G.P.L")) +
  theme_minimal()

p + scale_fill_discrete(name = "Estimation method - Education")


# Calculate the average growth for each method and period
average_growth <- data %>%
  group_by(method_health_rlv, period) %>%
  summarize(avg_growth = mean(growth_B1G.Q.L), .groups = 'drop')

# Create the bar chart with average labels on top of each bar
ggplot(data, aes(x = period, y = growth_B1G.Q.L, fill = method_health_rlv)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(data = average_growth, aes(x = period, y = avg_growth, label = round(avg_growth, 2)),
            position = position_dodge(width = 0.9), vjust = -1, size = 3) +
  labs(title = "Growth by Method and Period", x = "Period", y = "Growth", fill = "Method") +
  theme_minimal()


#Boxplots for each estimation method


# Plot the boxplot with 'period' as x-axis
# Define color palette
color_palette <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")


plot6 <- ggplot(data, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health  by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Deflation output prices", "Output indicators"), values = color_palette)

plot7 <- ggplot(data, aes(x = factor(period), y = growth_B1G.P.L, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Output indicators"))


plot8 <- ggplot(data, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health_2))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


plot9 <- ggplot(data, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


###########
#Diagnostic tests
###########
ggplot(data = model_1, aes(x = model_1$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals: Model 1', x = 'Residuals', y = 'Frequency')

bartlett.test()

# Tukey test to study each pair of treatment :


TukeyHSD(aov1)

aov1 = aov(growth_B1G.Q.L ~ method_health, data=data)

## Anova test

anova1 = anova_test(growth_B1G.Q.L ~ method_health_rlv, data = data_historical)
ssb <- anova1$`Sum Sq`[1]
sst <- sum(anova1$`Sum Sq`)
R2 <- ssb/sst

anova1


### Separating data into 2010-2019 

data_historical <- data %>% 
  filter(period >= 2010 & period <= 2019)

leveneTest(growth_B1G.Q.L ~ interaction(period,method_health), data=data_2020)
leveneTest(growth_B1G.Q.L ~ interaction(period,method_health), data=data)

#Isolating 2020

data_2020 <- data %>% 
  filter(period == 2020)

model.metrics <- augment(reg4)  # Remove details

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

reg4= lm(growth_B1G.Q.L ~ factor(method_health) + growth_B1G.Q.V, data = data_2020)

t_test_result <- anova_test(growth_B1G.Q.L ~ as.factor(method_health_input)+growth_B1G.Q.V, data = data_historical)
t_test_result <- anova_test(growth_B1G.Q.L ~ as.factor(method_health)+growth_B1G.Q.V, data = data_2020)


mean_table <- data %>%
  group_by(method_health) %>%
  summarise(growth_B1G.Q.L = median(growth_B1G.Q.L, na.rm = TRUE))
mean_table_2020 <- data_2020 %>% 
  group_by(method_health) %>%
  summarise(growth_B1G.Q.L = median(growth_B1G.Q.L, na.rm = TRUE))

mean_data2020 <- data_2020 %>%
  group_by(method_health_rlv) %>%
  summarise(across(starts_with("growth"), ~ median(.x, na.rm = TRUE), .names = "median{col}"))

t_test_result <- aov(mediangrowth_B1G.Q.L ~ method_health_rlv, data = mean_data2020)
TukeyHSD(t_test_result)
reg = lm(growth_B1G.Q.L ~ method_health_rlv + growth_B1G.Q.V, data = data_2020)
summary(reg)

boxplot(growth_B1G.Q.L ~ method_health_rlv, data = data_2020,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07", "#A020F0"))



#########
#Descriptive statistics
library(vtable)
sumtable(data,
         out="csv",
         file="file.csv")


