#First, relevel groups
combined_df_a_subset <- combined_df_a %>%
  select(country, period, excess_mortality)

# Merge the two datasets on the 'country' and 'period' columns
data <- data %>%
  left_join(combined_df_a_subset, by = c("country", "period"))


filtered_combined_df_ss = data

#data <- filtered_combined_df_ss %>%
#  mutate(method_health_2 = case_when(
#    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "Deflation",
#    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "Indicator",
#    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "Deflation",
#    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "Indicator",
#    TRUE ~ NA_character_
#  ))

data <- combined_df_ss %>%
  mutate(method_health_input = case_when(
    country %in% c("AUT", "CHL", "COL", "CZE", "POL", "KOR") ~ "input",
    country %in% c("CAN", "IRL", "LVA", "MEX", "SVK", "ESP") ~ "input",
    country %in% c("DEU", "JPN", "LUX", "ZAF", "USA") ~ "output",
    country %in% c("AUS", "BEL", "DNK", "FIN", "FRA", "HUN", "ITA", "NLD", "NOR", "NZL", "PRT", "SVN", "SWE", "GBR") ~ "output",
    TRUE ~ NA_character_
  ))


data$method_health_relevel <- relevel(data$method_health,"Indicators")
#filtered_combined_df_ss$year_factor_rlv <- relevel(filtered_combined_df_ss$year_factor,"pre-covid")
#filtered_combined_df_ss$method_health = as.factor(filtered_combined_df_ss$method_health)
#filtered_combined_df_ss$method_health_rlv <- relevel(filtered_combined_df_ss$method_health,"Indicator_Output")
#data$method_health_2 = as.factor(data$method_health_2)
#data$method_health_2 <- relevel(data$method_health_2,"Indicator")
#data$method_health_input = as.factor(data$method_health_input)
#data$method_health_input <- relevel(data$method_health_input,"output")
#combined_df_ss$covid_status <- ifelse(combined_df_ss$period %in% c(2020, 2021), "covid", "nocovid")

data$method_edu_3 <- as.factor(data$method_edu_3)
data$method_edu_3 <- relevel(data$method_edu_3,"Indicator_Output")

#combined_df_ss <- combined_df_ss %>%
#  mutate(year_factor = case_when(
#    period <= 2019 ~ "pre-covid",
#    period == 2020 ~ "2020",
#    period == 2021 ~ "2021",
#    period == 2022 ~ "2022",
#    TRUE ~ NA_character_  # Optional: Handle years not specified in the categories
#  ))

#filtered_combined_df_ss <- combined_df_ss %>%
#  filter(period >= 2010 & period <= 2021)

#Models

model_1 = lm(growth_B1G.Q.L ~  as.factor(year_factor_rlv)*method_health_rlv+growth_B1G.Q.V + excess_mortality, data = data)
summary(model_1)

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
         file="descriptive_file.csv")


model_1 = lm(growth_B1G.Q.L ~  year_factor_rlv*method_health_rlv+growth_B1G.Q.V, data = data)
summary(model_1)


###################
#Contributions

library(readxl)
contributions <- read_excel("C:/Users/sacksferrari_s/OneDrive - OECD/Working_Paper_GDP_method/contributions.xlsx")
View(contributions)

# Pivot the dataset to long format
contrib <- contributions %>%
  pivot_longer(cols = `2010`:`2022`, names_to = "period", values_to = "value") %>%
  mutate(period = as.numeric(period))

# Step 2: Pivot wider to create columns for each measure
contrib <- contrib %>%
  pivot_wider(names_from = measure, values_from = value)

data <- data %>%
  left_join(contrib, by = c("country", "period"))