# The goal of this project is to use different economic factors (inflation, unemployment etc.) to predict
# If the country is in a downturn (defined as a negative annual GDP growth) or not.

# Install the required libraries and packages. The WDI library contains tools to effectively
# Download and merge different tables, maintained by the World Bank, into one table.
if(!require(WDI)) install.packages("WDI")
install.packages("tidyverse")
install.packages("randomForest")
library("tidyverse")
library("dplyr")
library("caret")

# This option forces R to display numbers in the full format (1000000 instead of 1*e6).
options(scipen=999)

#----
# DATA PREPARATION

# Download the relevant tables (annual GDP growth, unemployment rates, two inflation types, levels
# Of government debt and savings levels, values of government reserves, account balance and external debt).
# The function WDI:WDI also merges these separate tables into one convenient table, useful for further work.
# The value commented out is growth per capita, which is ignored due to a direct connection to the examined
# annual GDP growth.
my_dataset = WDI::WDI(indicator = 
                c("SL.UEM.TOTL.ZS", 
                  "NY.GDP.MKTP.KD.ZG", 
                  #"NY.GDP.PCAP.KD.ZG", 
                  "NY.GDP.DEFL.KD.ZG", 
                  "FP.CPI.TOTL.ZG",
                  "GC.DOD.TOTL.GD.ZS",
                  "NY.GNS.ICTR.ZS",
                  "FI.RES.TOTL.CD", #current $
                  "BN.CAB.XOKA.CD", #current $
                  "DT.DOD.DECT.CD"), #current $
              extra = TRUE)

# Rename the columns. Ignore the growth per capita, as it is too closely related to the value we are
# Predicting, which is "GDP_growth_annual".
my_dataset_renamed = my_dataset %>% rename(., 
                      unemployment_rate = SL.UEM.TOTL.ZS,
                      GDP_growth_annual = NY.GDP.MKTP.KD.ZG,
                      #GDP_growth_per_capita = NY.GDP.PCAP.KD.ZG,
                      inflation_GDP_deflator = NY.GDP.DEFL.KD.ZG,
                      inflation = FP.CPI.TOTL.ZG,
                      govt_debt_share_of_gdp = GC.DOD.TOTL.GD.ZS,
                      gross_savings_share_of_gdp = NY.GNS.ICTR.ZS,
                      total_reserves = FI.RES.TOTL.CD,
                      current_account_balance = BN.CAB.XOKA.CD,
                      external_debt_stocks = DT.DOD.DECT.CD)

# Review the created and renamed dataset.
head(my_dataset_renamed)

# View the number of rows available at the onset of the data preparation.
nrow(my_dataset_renamed)
no_at_beginning = nrow(my_dataset_renamed)

# Economic troubles generally don't just appear out of the blue, but follow larger periods of poor
# Economic performance. Thus, the following creates a parallel dataset, where the year value is
# Increased by 1 and then merged back into the main dataset. So a value for 1990 is set to 1991
# And then merged back into the main dataset, where the year 1991 contains the values for 1991
# (Original data, suffix .x) as well as the data for 1990 (previous year data, using the suffix .y).
my_dataset_renamed_year = my_dataset_renamed %>% mutate(year = year + 1)

my_dataset_renamed_joined = merge(x = my_dataset_renamed, y = my_dataset_renamed_year,
                                 by.x=c("country", "year"), by.y=c("country", "year"))

# Review the renamed and joined dataset.
head(my_dataset_renamed_joined)

# We can see the data from the World Bank include data for entire regions, which is not something we want.
# We want the data to reflect only the actual countries. Thus, we remove the data which do not have entries
# For the capital city, making them regional data. In addition we recode the data for economic growth
# (GDP_growth_annual.x), creating a column named economic downturn, which returns the value 1 for downturn
# And 0 for growth or stagnation. This will be necessary later for classification methods.
my_dataset_almost_final = my_dataset_renamed_joined %>% 
  filter(capital.x != "") %>% 
  mutate(economic_downturn = ifelse(GDP_growth_annual.x < 0, 1, 0)) %>% 
  select(country, year, GDP_growth_annual.x, economic_downturn, unemployment_rate.x, unemployment_rate.y, GDP_growth_annual.y,
         inflation_GDP_deflator.x, inflation_GDP_deflator.y, inflation.x, inflation.y,
         govt_debt_share_of_gdp.x, govt_debt_share_of_gdp.y, gross_savings_share_of_gdp.x, gross_savings_share_of_gdp.y,
         total_reserves.x, total_reserves.y, current_account_balance.x, current_account_balance.y,
         external_debt_stocks.x, external_debt_stocks.y)

# The countries have individual random numbers assigned to them. A developed country would potentially 
# Achieve lower GDP growth given the same economic indicators as opposed to a developing country (having
# Less room for growth than a developed country). Thus, all countries are not (statistically) equal, and
# This will be taken into account in the algorithm development.
countries_recoded = my_dataset_almost_final %>% select(country)
distinct_country = distinct(countries_recoded)

set.seed(1, sample.kind = "Rounding")
distinct_country_numbers = data.frame(distinct_country, country_number = sample(seq(1, nrow(distinct_country), 1), nrow(distinct_country), replace = FALSE))
distinct_country_numbers %>% arrange(country_number)

my_dataset_final = merge(my_dataset_almost_final, distinct_country_numbers)

# Certain of the used algorithms do not work with empty (NA value) cells. Thus, these are removed for each
# Column, reducing the number of rows remaining drastically. This last operation completes the data
# Preparation phase.
my_dataset_final = my_dataset_final %>% 
  filter(!is.na(economic_downturn) & !is.na(unemployment_rate.x) & !is.na(unemployment_rate.y) 
         & !is.na(GDP_growth_annual.x) & !is.na(GDP_growth_annual.y) 
         & !is.na(inflation_GDP_deflator.x) & !is.na(inflation_GDP_deflator.y)
         & !is.na(inflation.x) & !is.na(inflation.y)
         & !is.na(govt_debt_share_of_gdp.x) & !is.na(govt_debt_share_of_gdp.y) 
         & !is.na(gross_savings_share_of_gdp.x) & !is.na(gross_savings_share_of_gdp.y)
         & !is.na(total_reserves.x) & !is.na(total_reserves.y) 
         & !is.na(current_account_balance.x) & !is.na(current_account_balance.y)
         & !is.na(external_debt_stocks.x) & !is.na(external_debt_stocks.y))

# Review the final dataset.
head(my_dataset_final)

# View the number of rows remaining after data preparation.
nrow(my_dataset_final)
no_at_end = nrow(my_dataset_final)

#----
# DATA VISUALISATION

# GDP growth annual vs. unemployment rate according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# There appears a relevant correlation between the two values, with the unemployment rising when
# GDP growth is low or negative, though this trend is somewhat spoiled from about 2003 on, where
# The values appear to move in tandem.
unemployment_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_unemployment = mean(unemployment_rate.x))
unemployment_coeff = max(unemployment_coeff$mean_gdp_growth)/max(unemployment_coeff$mean_unemployment)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_unemployment = mean(unemployment_rate.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_unemployment*unemployment_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./unemployment_coeff, name="Mean unemployment")) +
  ggtitle("Mean GDP growth annual vs. mean unemployment per year") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. inflation GDP deflator according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# The values again appear to move opposite to one another, with high inflation GDP deflator 
# associated with low GDP growth. As with the previous graph, this relationship appears to 
# break down with the economic Crisis of the late 2010s, and is only reestablished with the 
# last data points, for 2016.
deflator_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_deflator = mean(inflation_GDP_deflator.x))
deflator_coeff = max(deflator_coeff$mean_gdp_growth)/max(deflator_coeff$mean_deflator)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_infl_GDP_defl = mean(inflation_GDP_deflator.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_infl_GDP_defl*deflator_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./deflator_coeff, name="Mean inflation GDP deflator")) +
  ggtitle("Mean GDP growth annual vs. Mean inflation GDP deflator") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. inflation according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# This graph is very similar to the previous one, since the inflation and inflation GDP deflator are
# Closely related to one another. Again we see the inverse movement of the two values up to 2009,
# With the inversion dissapearing for the following 5 years.
inflation_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_inflation = mean(inflation.x))
inflation_coeff = max(inflation_coeff$mean_gdp_growth)/max(inflation_coeff$mean_inflation)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_inflation = mean(inflation.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_inflation*inflation_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./inflation_coeff, name="Mean inflation")) +
  ggtitle("Mean GDP growth annual vs. Mean inflation") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. government debt as share of GDP according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# The two values on display generally mirror each other, which is odd (low GDP growth should cause
# grater levels of government debt?). Just before the crisis the two lines diverge, which makes sense.
# Oddly, during the crisis, debt increases only marginally, and then slowly rises through the uneven
# Economic recovery following.
debt_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_debt = mean(govt_debt_share_of_gdp.x))
debt_coeff = max(debt_coeff$mean_gdp_growth)/max(debt_coeff$mean_debt)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_govt_debt_share_of_gdp = mean(govt_debt_share_of_gdp.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_govt_debt_share_of_gdp*debt_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./debt_coeff, name="Mean government debt as share of GDP")) +
  ggtitle("Mean GDP growth annual vs. Mean government debt as share of GDP") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. savings as share of GDP according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# The two values neatly follow each other. when GDP grows, so do the savings in a society, and vice versa.
savings_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_savings = mean(gross_savings_share_of_gdp.x))
savings_coeff = max(savings_coeff$mean_gdp_growth)/max(savings_coeff$mean_savings)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_gross_savings_share_of_gdp = mean(gross_savings_share_of_gdp.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_gross_savings_share_of_gdp*savings_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./savings_coeff, name="Mean gross savings as share of GDP")) +
  ggtitle("Mean GDP growth annual vs. Mean gross savings as share of GDP") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. total reserves according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# The values in this graph are confusing, as they indicate a massive jump in the absolute value of
# Reserves just before the crisis. In fact, this is due to the fact that the values for several 
# Large economies (Russian Federation, Indonesia, Brasil) only appear during this period, massively
# Increasing the mean values displayed in the graph. After the crisis, the values behave more logically,
# falling with the reduced economic activity.
reserves_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_reserves = mean(total_reserves.x))
reserves_coeff = max(reserves_coeff$mean_gdp_growth)/max(reserves_coeff$mean_reserves)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_total_reserves = mean(total_reserves.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_total_reserves*reserves_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./reserves_coeff, name="Mean total reserves")) +
  ggtitle("Mean GDP growth annual vs. Mean total reserves") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. current account balance according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# Due to the nature of the current account balance, a correlation between the two values is not to be
# Expected. Current account balance is a measure of the flow of money into or out of a given country.
# If we had the perfect data for the entire planet, the mean value of the current account balance would
# Always be zero, as each dollar that goes out of one country enters another country.
# Given that we do not actually have perfect data, an in depth analysis shows that the appearance of
# The Russian Federation hugely boosts the value before the crisis (the country being a massive)
# Exporter of oil and gas. Afterwards, the large importers Brasil and India drag the value down.
current_account_balance_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_current_account_balance = mean(current_account_balance.x))
current_account_balance_coeff = max(current_account_balance_coeff$mean_gdp_growth)/max(current_account_balance_coeff$mean_current_account_balance)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_current_account_balance = mean(current_account_balance.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_current_account_balance*current_account_balance_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./current_account_balance_coeff, name="Mean current account balance")) +
  ggtitle("Mean GDP growth annual vs. Mean current account balance") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

# GDP growth annual vs. external debt according to the year of measurement.
# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.

# Similarly, this graph reflects the inclusion of the Russian Federation, Brasil and Indonesia
# In the mid 2010s. Even without these three outliers, the debt still increases with years.
external_debt_coeff = my_dataset_final %>% group_by(year) %>%
  summarise(mean_gdp_growth = mean(GDP_growth_annual.x), mean_external_debt = mean(external_debt_stocks.x))
external_debt_coeff = max(external_debt_coeff$mean_gdp_growth)/max(external_debt_coeff$mean_external_debt)

my_dataset_final %>% group_by(year) %>%
  summarise(mean_GDP_growth_annual = mean(GDP_growth_annual.x), mean_external_debt = mean(external_debt_stocks.x)) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = mean_GDP_growth_annual), col="blue") +
  geom_line(aes(y = mean_external_debt*external_debt_coeff), col="red") +
  scale_y_continuous(name = "Mean GDP growth annual", sec.axis = sec_axis(~./external_debt_coeff, name="Mean external debt")) +
  ggtitle("Mean GDP growth annual vs. Mean external debt") +
  xlab("Year") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))

#----
# Development of the algorithm

# Create the test set and the validation set. Due to the limited number of rows remaining, We will
# Allocate as many entries as possible to the train set (90 %) and as little as possible (10 %)
# To the validation set. This should return an algorithm which will achieve better accuracy/RMSE,
# At the threat of overtraining and with the added problem of lower validation accuracy (the lowest
# 'step' for the validation accuracy will be cca. 2% (1/(10%*507))).
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = my_dataset_final$economic_downturn, 
                                    times = 1, p = 0.1, list = FALSE)
train_set <- my_dataset_final[-test_index,]
validation_set <- my_dataset_final[test_index,]

# Purge the memory and remove unnecessary stored tables and values.
gc()
rm(countries_recoded,distinct_country,distinct_country_numbers,my_dataset,my_dataset_almost_final,
   my_dataset_renamed,my_dataset_renamed_joined,my_dataset_renamed_year,test_index)

control <- trainControl(method = "cv", number = 7)

# Below, the typical methods learned during the course will be used, both regressions and classifications.

# Glm method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_glm <- train(GDP_growth_annual.x ~ 
                     +country
                     +country_number
                     +unemployment_rate.x
                     +unemployment_rate.y
                     +inflation_GDP_deflator.x 
                     +inflation_GDP_deflator.y
                     +inflation.x
                     +inflation.y
                     +govt_debt_share_of_gdp.x
                     +govt_debt_share_of_gdp.y
                     +gross_savings_share_of_gdp.x
                     +gross_savings_share_of_gdp.y
                     +total_reserves.x
                     +total_reserves.y
                     +current_account_balance.x
                     +current_account_balance.y
                     +external_debt_stocks.x
                     +external_debt_stocks.y
                     , method = "glm",
                     na.action = na.pass,
                     data = train_set
                     ,trControl = control
)
# Look over the results
train_glm
# Round the value to 5 decimals
RMSE_initial_glm = round(train_glm$results["RMSE"], 5)

# Assess the initial RMSE
RMSE_initial_glm

# Run the algorithm on the validation data set to obtain the final RMSE.
# Round the final validation RMSE, to 5 decimals.
validation_prediction_glm = predict(train_glm, validation_set)
RMSE_final_glm = round(RMSE(validation_prediction_glm, validation_set$GDP_growth_annual.x), 5)
RMSE_final_glm

# Calculate the accuracy of the given algorithm, defining the downturn as a negative
# Projected or actual negative annual GDP growth.
# Bind the projected validation and actual validation downturn values, compare them and calculate the final accuracy.
accuracy_glm = round(data.frame(validation_prediction_glm) %>% 
  mutate(predicted_economic_downturn_glm = ifelse(validation_prediction_glm < 0, 1, 0)) %>% 
  cbind(actual = validation_set$economic_downturn) %>% 
  select(predicted_economic_downturn_glm, actual) %>%
  mutate(glm_same = ifelse(predicted_economic_downturn_glm == actual, 1, 0)) %>%
  summarise(glm_final_accuracy = mean(glm_same)), 5)
accuracy_glm

# Create the table results, which will contain all the relevant results for each algorithm.
# Input the glm values into the results table.
glm_results = c("glm", "RMSE", RMSE_initial_glm, RMSE_final_glm, accuracy_glm)

# Knn method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_knn <- train(GDP_growth_annual.x ~ 
                     +country
                   +country_number
                   +unemployment_rate.x
                   +unemployment_rate.y
                   +inflation_GDP_deflator.x 
                   +inflation_GDP_deflator.y
                   +inflation.x
                   +inflation.y
                   +govt_debt_share_of_gdp.x
                   +govt_debt_share_of_gdp.y
                   +gross_savings_share_of_gdp.x
                   +gross_savings_share_of_gdp.y
                   +total_reserves.x
                   +total_reserves.y
                   +current_account_balance.x
                   +current_account_balance.y
                   +external_debt_stocks.x
                   +external_debt_stocks.y
                   , method = "knn",
                   na.action = na.pass,
                   data = train_set
                   ,trControl = control
)
# Look over the results
train_knn
# Round the value to 5 decimals
RMSE_initial_knn = round(train_knn$results["RMSE"], 5)

# The knn algorithm calculates the RMSEs for 3 numbers of k (5, 7 and 9).
# The best RMSE is the result of k = 5, so that value is used further on.
# The minimim value of the three initial RMSEs is designated the initial RMSE for the knn method.
RMSE_initial_knn = min(RMSE_initial_knn)

# Run the algorithm on the validation data set to obtain the final RMSE.
# Round the final validation RMSE, to 5 decimals.
validation_prediction_knn = predict(train_knn, validation_set)
RMSE_final_knn = round(RMSE(validation_prediction_knn, validation_set$GDP_growth_annual.x), 5)
RMSE_final_knn

# Calculate the accuracy of the given algorithm, defining the downturn as a negative
# Projected or actual negative annual GDP growth. 
# Bind the projected validation and actual validation downturn values, compare them and calculate the final accuracy.
accuracy_knn = round(data.frame(validation_prediction_knn) %>% 
  mutate(predicted_economic_downturn_knn = ifelse(validation_prediction_knn < 0, 1, 0)) %>% 
  cbind(actual = validation_set$economic_downturn) %>% 
  select(predicted_economic_downturn_knn, actual) %>%
  mutate(knn_same = ifelse(predicted_economic_downturn_knn == actual, 1, 0)) %>%
  summarise(knn_final_accuracy = mean(knn_same)), 5)
accuracy_knn

# Input the knn values into the results table.
knn_results = c("knn", "RMSE", RMSE_initial_knn, RMSE_final_knn, accuracy_knn)

results = rbind(glm_results, knn_results)
results

# GamLoess method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_gamLoess <- train(GDP_growth_annual.x ~ 
                     +country
                   +country_number
                   +unemployment_rate.x
                   +unemployment_rate.y
                   +inflation_GDP_deflator.x 
                   +inflation_GDP_deflator.y
                   +inflation.x
                   +inflation.y
                   +govt_debt_share_of_gdp.x
                   +govt_debt_share_of_gdp.y
                   +gross_savings_share_of_gdp.x
                   +gross_savings_share_of_gdp.y
                   +total_reserves.x
                   +total_reserves.y
                   +current_account_balance.x
                   +current_account_balance.y
                   +external_debt_stocks.x
                   +external_debt_stocks.y
                   , method = "gamLoess",
                   na.action = na.pass,
                   data = train_set
                   ,trControl = control
)
# Look over the results
train_gamLoess

# Disregard the result of the RMSE returned above, calculate the correct RMSE by predicting
# The algorithm back on the train set.
set.seed(1, sample.kind = "Rounding")
predict_loess_check = predict(train_gamLoess, train_set)
RMSE_initial_gamLoess = round(RMSE(predict_loess_check, train_set$GDP_growth_annual.x), 5)

# Run the algorithm on the validation data set to obtain the final RMSE.
# Round the final validation RMSE, to 5 decimals.
validation_prediction_gamLoess = predict(train_gamLoess, validation_set)
RMSE_final_gamLoess = round(RMSE(validation_prediction_gamLoess, validation_set$GDP_growth_annual.x), 5)
RMSE_final_gamLoess

# Calculate the accuracy of the given algorithm, defining the downturn as a negative
# Projected or actual negative annual GDP growth. 
# Bind the projected validation and actual validation downturn values, compare them and calculate the final accuracy.
accuracy_gamLoess = round(data.frame(validation_prediction_gamLoess) %>% 
  mutate(predicted_economic_downturn_gamLoess = ifelse(validation_prediction_gamLoess < 0, 1, 0)) %>% 
  cbind(actual = validation_set$economic_downturn) %>% 
  select(predicted_economic_downturn_gamLoess, actual) %>%
  mutate(gamLoess_same = ifelse(predicted_economic_downturn_gamLoess == actual, 1, 0)) %>%
  summarise(gamLoess_final_accuracy = mean(gamLoess_same)), 5)
accuracy_gamLoess

# Input the gamLoess values into the results table.
gamLoess_results = c("gamLoess", "RMSE", RMSE_initial_gamLoess, RMSE_final_gamLoess, accuracy_gamLoess)

results = rbind(results, gamLoess_results)
results

# Lda method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_lda <- train(as.character(economic_downturn) ~ 
                          +country
                        +country_number
                        +unemployment_rate.x
                        +unemployment_rate.y
                        +inflation_GDP_deflator.x 
                        +inflation_GDP_deflator.y
                        +inflation.x
                        +inflation.y
                        +govt_debt_share_of_gdp.x
                        +govt_debt_share_of_gdp.y
                        +gross_savings_share_of_gdp.x
                        +gross_savings_share_of_gdp.y
                        +total_reserves.x
                        +total_reserves.y
                        +current_account_balance.x
                        +current_account_balance.y
                        +external_debt_stocks.x
                        +external_debt_stocks.y
                        , method = "lda"
                        ,na.action = na.pass
                        ,data = train_set
                        ,trControl = control
)
# Look over the results
train_lda
accuracy_initial_lda = round(train_lda$results["Accuracy"], 5)

# Run the algorithm on the validation data set to obtain the final accuracy.
# Round the final validation accuracy, to 5 decimals.
validation_prediction_lda = predict(train_lda, validation_set)
accuracy_final_lda = round(mean(validation_prediction_lda == validation_set$economic_downturn), 5)
accuracy_final_lda

# Input the lda values into the results table. 
# For classification methods the validation and final accuracies are the same.
lda_results = c("lda", "accuracy", accuracy_initial_lda, accuracy_final_lda, accuracy_final_lda)

results = rbind(results, lda_results)
results

# # Qda method - returns a bizzare error, no idea why, ignore the method and be content with lda.
# # Set the random seed and train the algorithm
# set.seed(1, sample.kind = "Rounding")
# train_qda <- train(as.character(economic_downturn) ~ 
#                      +country
#                    +country_number
#                    +unemployment_rate.x
#                    +unemployment_rate.y
#                    +inflation_GDP_deflator.x 
#                    +inflation_GDP_deflator.y
#                    +inflation.x
#                    +inflation.y
#                    +govt_debt_share_of_gdp.x
#                    +govt_debt_share_of_gdp.y
#                    +gross_savings_share_of_gdp.x
#                    +gross_savings_share_of_gdp.y
#                    +total_reserves.x
#                    +total_reserves.y
#                    +current_account_balance.x
#                    +current_account_balance.y
#                    +external_debt_stocks.x
#                    +external_debt_stocks.y
#                    ,method = "qda"
#                    #,tuneGrid = grid
#                    #,na.action = na.pass
#                    ,data = train_set
#                    #,trControl = trainControl(method = "cv", number = 1)
# )
# # Look over the results
# train_qda
# accuracy_initial_qda = round(train_qda$results["Accuracy"], 5)
# 
# # Run the algorithm on the validation data set to obtain the final accuracy.
# # Round the final validation accuracy, to 5 decimals.
# validation_prediction_qda = predict(train_qda, validation_set)
# accuracy_final_qda = round(mean(validation_prediction_qda == validation_set$economic_downturn), 5)
# accuracy_final_qda
# 
# # Input the qda values into the results table. 
# # For classification methods the validation and final accuracies are the same.
# qda_results = c("qda", "accuracy", accuracy_initial_qda, accuracy_final_qda, accuracy_final_qda)
# 
# results = rbind(results, qda_results)
# results

# Decision tree method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(as.character(economic_downturn) ~ 
                     +country
                   +country_number
                   +unemployment_rate.x
                   +unemployment_rate.y
                   +inflation_GDP_deflator.x 
                   +inflation_GDP_deflator.y
                   +inflation.x
                   +inflation.y
                   +govt_debt_share_of_gdp.x
                   +govt_debt_share_of_gdp.y
                   +gross_savings_share_of_gdp.x
                   +gross_savings_share_of_gdp.y
                   +total_reserves.x
                   +total_reserves.y
                   +current_account_balance.x
                   +current_account_balance.y
                   +external_debt_stocks.x
                   +external_debt_stocks.y
                   , method = "rpart"
                   ,na.action = na.pass
                   ,data = train_set
                   ,trControl = control
)
# Look over the results
train_rpart
# The calculation uses three different complexity parameters to decide on the best algorithm version.
# The one returning the highest accuracy is used.
accuracy_initial_rpart = round(max(train_rpart$results["Accuracy"]), 5)

# Run the algorithm on the validation data set to obtain the final accuracy.
# Round the final validation accuracy, to 5 decimals.
validation_prediction_rpart = predict(train_rpart, validation_set)
accuracy_final_rpart = round(mean(validation_prediction_rpart == validation_set$economic_downturn), 5)
accuracy_final_rpart

# Input the rpart values into the results table. 
# For classification methods the validation and final accuracies are the same.
rpart_results = c("decision tree", "accuracy", accuracy_initial_rpart, accuracy_final_rpart, accuracy_final_rpart)

results = rbind(results, rpart_results)
results

# Random forest method
# Set the random seed and train the algorithm
set.seed(1, sample.kind = "Rounding")
train_rf <- train(as.character(economic_downturn) ~ 
                       +country
                     +country_number
                     +unemployment_rate.x
                     +unemployment_rate.y
                     +inflation_GDP_deflator.x 
                     +inflation_GDP_deflator.y
                     +inflation.x
                     +inflation.y
                     +govt_debt_share_of_gdp.x
                     +govt_debt_share_of_gdp.y
                     +gross_savings_share_of_gdp.x
                     +gross_savings_share_of_gdp.y
                     +total_reserves.x
                     +total_reserves.y
                     +current_account_balance.x
                     +current_account_balance.y
                     +external_debt_stocks.x
                     +external_debt_stocks.y
                     , method = "rf"
                     ,na.action = na.pass
                     ,data = train_set
                     ,trControl = control
)
# Look over the results
train_rf
# The calculation uses three different mtry parameters (numbers of variables sampled at each split) 
# To decide on the best algorithm version.
# The one returning the highest accuracy is used.
accuracy_initial_rf = round(max(train_rf$results["Accuracy"]), 5)

# Run the algorithm on the validation data set to obtain the final accuracy.
# Round the final validation accuracy, to 5 decimals.
validation_prediction_rf = predict(train_rf, validation_set)
accuracy_final_rf = round(mean(validation_prediction_rf == validation_set$economic_downturn), 5)
accuracy_final_rf

# Input the rf values into the results table. 
# For classification methods the validation and final accuracies are the same.
rf_results = c("random forest", "accuracy", accuracy_initial_rf, accuracy_final_rf, accuracy_final_rf)

results = rbind(results, rf_results)
colnames(results) = c('algorithm_name','initial_metric','test_value','validation_value','validation_accuracy')
results