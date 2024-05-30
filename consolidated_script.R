# Case Study: GDP and components

## Part 1: Input data

.libPaths("g:/r/win-library/4.3")
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(officer)
library(flextable)
library(zoo)
library(sjPlot)
library(sjmisc)


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

url_ana="https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0/A....B1G..P+Q+_T...L+V..?startPeriod=2010&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
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
  arrange(country,TRANSACTION,SECTOR,ACTIVITY,TABLE_IDENTIFIER,PRICE_BASE) %>%
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

# Clean the data to remove rows with NA values
df_ANA_growth <- na.omit(df_ANA_growth)


df_ANA_growth_reshaped <- pivot_wider(data = df_ANA_growth, 
                                      id_cols = c("country", "period"), 
                                      names_from = subject, 
                                      values_from = c(growth, OBS_VALUE))


################################################################
## 3.Combined Oxford data, QNA data and ELS data
################################################################

# Combine the 3 dataframes by country and period
# combined_df <- merge(df_QNA_growth_reshaped, tracker_data_Q_Diff,df_ELS_Q, by = c("country", "period"), all = TRUE)

combined_df_q <- merge(df_QNA_growth_reshaped, 
                       tracker_data_Q_Diff, 
                       df_ELS_Q, 
                       by.x = c("country", "period"),
                       by.y = c("country", "period"),
                       all = TRUE)

combined_df_a <- merge(df_ANA_growth_reshaped, 
                       tracker_data_Y, 
                       df_ELS_Y, 
                       by.x = c("country", "period"),
                       by.y = c("country", "period"),
                       all = TRUE)

combined_df_a$growth_B1G.P.P<-combined_df_a$growth_B1G.P.V-combined_df_a$growth_B1G.P.L
combined_df_a$growth_B1G.Q.P<-combined_df_a$growth_B1G.Q.V-combined_df_a$growth_B1G.Q.L


######Clean house
df_ANA = NULL
df_QNA = NULL
df_QNA_growth = NULL
df_QNA_growth_reshaped = NULL
df_ANA_growth = NULL
tracker_data_D = NULL

# Add category for health and education method of estimation

#4 categories
combined_df_q <- combined_df_q %>%
  mutate(method_health = case_when(
    country %in% c("AUS", "CHL", "COL", "CZE", "DNK", "POL", "KOR") ~ 1,
    country %in% c("CAN", "IRL", "LVA", "MEX", "NZL", "SVK", "ESP") ~ 2,
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ 3,
    country %in% c("BEL", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "PRT", "SVN", "SWE", "GBR") ~ 4,
    TRUE ~ NA_integer_
  ))

#4 categories
combined_df_a <- combined_df_a %>%
  mutate(method_health = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "Deflation_Input",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator_Input",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation_Output",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))

# 2 categories
combined_df_a <- combined_df_a %>%
  mutate(method_health_2 = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "Deflation",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator",
    TRUE ~ NA_character_
  ))


combined_df_q <- combined_df_q %>%
  mutate(method_edu = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("IRL", "LVA", "ESP") ~ "Indicator_Input",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))

combined_df_a <- combined_df_a %>%
  mutate(method_edu_3 = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("IRL", "LVA", "ESP") ~ "Indicator_Input",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))

#2 categories
combined_df_a <- combined_df_a %>%
  mutate(method_edu = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
    country %in% c("") ~ 2,
    country %in% c("IRL", "LVA", "ESP") ~ 1,
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 2,
    TRUE ~ NA_integer_
  ))

#3 categories
combined_df_a <- combined_df_a %>%
  mutate(method_edu_3 = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
    country %in% c("") ~ 2,
    country %in% c("IRL", "LVA", "ESP") ~ 3,
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 4,
    TRUE ~ NA_integer_
  ))


###################################################################

## Part 2: Data manipulation

####Clean for empty columns-rows and keep some variables only

##Annual data
#combined_df_a = remove_empty(combined_df_a, which=c("cols", "rows"))

combined_df_ss <- subset(combined_df_a, select=c("country", "period", "method_health", "growth_B1G.P.L", "growth_B1G.P.V", "growth_B1G.Q.L", "growth_B1G.Q.V", "growth_B1G.L", "growth_B1G.V", "growth_B1G.Q.P", "growth_B1G.P.P", "method_edu", "method_health_2", "method_edu_3"))

combined_df_ss <- drop_na(combined_df_ss)

##Quarterly data

#combined_df_q_ss <- subset(combined_df_q, select=c("country", "period", "method_health", "B1GQ", "StringencyIndex_Average", "method_edu"))

#combined_df_q_ss <- drop_na(combined_df_q_ss)



##########################################
#Part 3: Different visualizations
##########################################

# #First, stringency vs B1GQ separated by health and period
# #Same but adding trend line / plot all groups in one pane
#plot2 <- ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health_4))) +
#  geom_point(alpha = 0.5) +
#   geom_text(aes(label = country), vjust = -0.5, size=2) +
#   ggtitle("GVA for Health Sector and Year") +
#   scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Add custom labels

plot2 <- ggplot(combined_df_ss, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health_2))) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = country), vjust = -0.5, size=6) +
  ggtitle("GVA for Health Sector and Year") +
  scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct")) 


#plot2 <- ggplot(combined_df_a, aes(x = period, y = growth_B1G.Q.L, color = factor(method_health_4))) +
#  geom_point(alpha = 0.5) +
#  geom_text(aes(label = country), vjust = -0.5, size=2) +
#  ggtitle("GVA for Health Sector and Year") +
#  scale_color_discrete(name = "Estimation method", labels = c("Input - indirect", "Input - direct", "Output-indirect", "Output-direct"))  # Add custom labels


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


combined_df_ss$method_health_relevel <- relevel(combined_df_ss$method_health,"Indicators")
data = combined_df_ss
#combined_df_a$share_gva = (combined_df_a$OBS_VALUE_B1G.Q.V*100/combined_df_a$OBS_VALUE_B1G.V)



ggplot(data = model_1, aes(x = model_1$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals: Model 1', x = 'Residuals', y = 'Frequency')

######### BOX PLOTS


plot = subset(combined_df_ss, select = c("country", "period", "growth_B1G.L", "growth_B1G.V", "growth_B1G.Q.L", "growth_B1G.Q.V", "growth_B1G.P.L", "growth_B1G.P.V",  "method_health", "method_edu", "method_health_2", "method_edu_3"))
plot$method_health = as.factor(plot$method_health)
plot$period = as.factor(plot$period)
plot = drop_na(plot)
# Assuming 'method_health' is a column in your dataset combined_df_a

# Replace 'method_health' with the actual column name if different
plot$period <- as.factor(plot$period)

# Plot the boxplot with 'period' as x-axis
# Define color palette
color_palette <- c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3")


plot6 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health  by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Deflation output prices", "Output indicators"), values = color_palette)

plot7 <- ggplot(plot, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu_3))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Deflation input prices", "Input indicators", "Output indicators"), values = color_palette)


plot8 <- ggplot(plot, aes(x = period, y = growth_B1G.Q.L, fill = factor(method_health_2))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Health", title = "Boxplot of real growth of GVA Health by Period and Method Health") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)


plot9 <- ggplot(plot, aes(x = period, y = growth_B1G.P.L, fill = factor(method_edu))) +
  geom_boxplot(alpha = 1, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(x = "Period", y = "Growth GVA Education", title = "Boxplot of real growth of GVA Education by Period and Method Education") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Estimation method", labels = c("Indirect (deflation)", "Direct (indicators)"), values = color_palette)

############
#model eval
############

bartlett.test()

bartlett.test(growth_B1G.Q.L ~ factor(method_health), data = plot)

bartlett.test(growth_B1G.L ~ factor(method_health), data = combined_df_a)

############ MODELS


# What is the effect of the treatment on the value ?
model_1 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health+growth_B1G.Q.V, data = filtered_df)
summary(model_1)

model_1$coefficients

anova1 = aov(model_1)
summary(anova1)

##########################################################
#now we try to group years 

combined_df_ss$covid_status <- ifelse(combined_df_ss$period %in% c(2020, 2021), "covid", "nocovid")

combined_df_ss <- combined_df_ss %>%
  mutate(year_factor = case_when(
    period <= 2019 ~ "pre-covid",
    period == 2020 ~ "2020",
    period == 2021 ~ "2021",
    period == 2022 ~ "2022",
    TRUE ~ NA_character_  # Optional: Handle years not specified in the categories
  ))

filtered_combined_df_ss <- combined_df_ss %>%
  filter(period >= 2010 & period <= 2021)

filtered_combined_df_ss$year_factor_rlv <- relevel(filtered_combined_df_ss$year_factor,"pre-covid")
filtered_combined_df_ss$method_health = as.factor(filtered_combined_df_ss$method_health)
filtered_combined_df_ss$method_health_rlv <- relevel(filtered_combined_df_ss$method_health,"Indicator_Output")

data <- filtered_combined_df_ss %>%
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
  mutate(method_edu = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("IRL", "LVA", "ESP") ~ "Indicator_Input",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))

data <- data %>%
  mutate(method_edu_3 = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ "Deflation_Input",
    country %in% c("") ~ "Deflation_Output",
    country %in% c("IRL", "LVA", "ESP") ~ "Indicator_Input",
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ "Indicator_Output",
    TRUE ~ NA_character_
  ))

#2 categories
#combined_df_a <- combined_df_a %>%
#  mutate(method_edu = case_when(
#    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
#    country %in% c("") ~ 2,
#    country %in% c("IRL", "LVA", "ESP") ~ 1,
#    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 2,
#    TRUE ~ NA_integer_
#  ))

#3 categories
data <- data %>%
  mutate(method_edu_3 = case_when(
    country %in% c("CAN", "JPN", "KOR", "COL", "USA") ~ 1,
    country %in% c("") ~ 2,
    country %in% c("IRL", "LVA", "ESP") ~ 3,
    country %in% c("AUS", "AUT", "BEL", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU", "HUN", "ITA", "LUX", "MEX", "NLD", "NZL", "POL", "PRT", "SVK", "SVN", "SWE", "ZAF", "GBR") ~ 4,
    TRUE ~ NA_integer_
  ))


###############variance regression

variance_data <- filtered_combined_df_ss %>%
  group_by(method_health, period) %>%
  summarise(across(starts_with("growth"), ~ var(.x, na.rm = TRUE), .names = "var_{col}"))


mean_data <- combined_df_ss %>%
  group_by(country, year_factor) %>%
  summarise(across(starts_with("growth"), ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


variance_data_clean <- variance_data %>%
  filter(!is.na(period) & !is.na(var_growth_B1G.Q.L))

print(ggplot(variance_data_clean, aes(x = as.factor(period), y = var_growth_B1G.L, color = method_health, group = method_health)) +
        geom_line() +
        geom_point() +
        labs(title = "Fig 1: Variance of GVA over time period for each estimation method group",
             x = "Period",
             y = "Variance of growth_B1G.L") +
        theme_minimal())


#### Variance graph
print(ggplot(data, aes(x = period, y = growth_B1G.Q.L, color = country, shape = method_health, group = country)) +
        geom_line() +
        geom_point() +
        labs(title = "Variance of growth_B1G.Q.L over Period for each method_health Group",
             x = "Period",
             y = "Variance of growth_B1G.Q.L") +
        theme_minimal() +
        theme(legend.position = "bottom"))


########################
 
#we want to look at year 2007, 2008, 2013 and 2020

#results for current prices in equation
#include variance
#quantile regression


model_1 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_rlv+growth_B1G.Q.V, data = data)
summary(model_1)

anova1 = aov(model_1)
summary(anova1)

data$method_health_2 = as.factor(data$method_health_2)
data$method_health_2 <- relevel(data$method_health_2,"Indicator")
data$method_health_input = as.factor(data$method_health_input)
data$method_health_input <- relevel(data$method_health_input,"output")


model_2 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_2+growth_B1G.Q.V, data = data)
summary(model_2)


model_3 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_input+growth_B1G.Q.V, data = data)
summary(model_3)

#Histograms
# Assuming your data frame is named `data`
# Calculate the percentage within each year_factor
data <- combined_df_ss %>%
  group_by(method_health, year_factor_rlv) %>%
  mutate(total_count = n()) %>%
  ungroup() %>%
  group_by(method_health, year_factor_rlv, growth_B1G.Q.L) %>%
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
       x = "Growth B1G.Q.L",
       y = "Density") +
  theme_minimal()


# Assuming your data frame is named `data`
# Calculate the percentage within each year_factor
data <- filtered_combined_df_ss %>%
  group_by(method_edu_3, year_factor_rlv) %>%
  mutate(total_count = n()) %>%
  ungroup() %>%
  group_by(method_edu_3, year_factor_rlv, growth_B1G.P.L) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(percentage = count / total_count)

data_clean <- data %>%
  filter(!is.na(growth_B1G.P.L) & !is.na(method_edu_3) & !is.na(year_factor_rlv))


# Create the histogram
ggplot(data_clean, aes(x = growth_B1G.P.L, color = as.factor(method_edu_3), fill = as.factor(method_edu_3))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year_factor_rlv) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Density Plot of growth_B1G.P.L",
       x = "Growth B1G.P.L",
       y = "Density") +
  theme_minimal()

