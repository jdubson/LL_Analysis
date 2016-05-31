# Create model using sample data
# Author: Jason Wu

# Load libraries
#
#
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)

# Import sample data
#
#
ll_data <- read.csv("~/Desktop/LL_data.csv", header=TRUE, sep=",")
ll_data_clean <- ll_data

# Format data frame
#
#
ll_data_clean$Date <- as.Date(ll_data_clean$Date, format = "%m/%d/%y")
ll_data_clean$ARPU <- as.numeric(gsub("\\$4.99", 4.99, ll_data_clean$ARPU))
ll_data_clean$Trial.Conversion <- as.numeric( gsub("\\%", "", ll_data_clean$Trial.Conversion))
ll_data_clean$Trial.Conversion <- ll_data_clean$Trial.Conversion / 100
#ll_data_clean$Trial.Temp <- append(ll_data_clean$Trial.Adds[-1],NA)

# Add new metrics
#
#
#ll_data_clean$Total.Paid.Users <- append(ll_data_clean$Paid.Subs[-1],NA) + ll_data_clean$Paid.Adds

# Daily Churn Rate
ll_data_clean <- ll_data_clean %>%
  mutate(Revenue = Paid.Subs * ARPU, Total.Paid.Users = lag(Paid.Subs) + Paid.Adds, 
         Daily.Churn = Total.Paid.Users - Paid.Subs, Daily.Churn.Rate = Daily.Churn / Total.Paid.Users) %>%
  select(Date, Trial.Conversion, Trial.Adds, Trial.Subs, Paid.Adds, Paid.Subs, Total.Paid.Users, Daily.Churn, Daily.Churn.Rate, ARPU, Revenue)

#ll_data_clean$Trial.New <- as.numeric(gsub("Inf", NA, ll_data_clean$Trial.New))

# Summarize into monthly data frame
#
#
ll_data_monthly <- ll_data_clean
ll_data_monthly$Month <- as.Date(as.yearmon(ll_data_monthly$Date))

ll_data_monthly_summary <- ll_data_monthly %>%
  group_by(Month) %>%
  summarise(avg_trial_conv = median(Trial.Conversion), tot_trial_adds = sum(Trial.Adds), f_trial_subs = first(Trial.Subs), 
            l_trial_subs = last(Trial.Subs), tot_paid_adds = sum(Paid.Adds), f_paid_subs = first(Paid.Subs), l_paid_subs = last(Paid.Subs), 
            days = n(), tot_revenue = sum(Revenue), ARPU = first(ARPU), tot_churn = sum(Daily.Churn)) %>%
  mutate(monthly_churn = tot_churn / (tot_paid_adds + lag(l_paid_subs))) %>%

# Part 1
#
#

# Exploratory plot removing the most recent month
ggplot(ll_data_monthly_summary[-65,], aes(Month, tot_revenue)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Monthly product line revenues model
fit <- lm(tot_revenue ~ Month, ll_data_monthly_summary[-65,])
summary(fit)

# Monthly Churn Rate and LTV of a Subscriber
ll_monthly_metrics <- ll_data_monthly_summary %>%
  summarise(avg_monthly_churn = mean(na.omit(monthly_churn)), arpu = first(ARPU)) %>%
  mutate(avg_monthly_ret = 1 - avg_monthly_churn, exp_monthly_lifetime = 1 / avg_monthly_churn, monthly_arpu = arpu * 30) %>%
  mutate(ltv = exp_monthly_lifetime * monthly_arpu) %>%
  select(avg_monthly_churn, avg_monthly_ret, exp_monthly_lifetime, monthly_arpu, ltv)



# Part 2
#
#

# Forecast annual run rate by May 2017
ll_forecast <- read.csv("~/Desktop/LL_forecast.csv", header=TRUE, sep=",")
ll_forecast$Month <- as.Date(ll_forecast$Month, format = "%m/%d/%y")

rev_forecast <- predict(fit, ll_forecast)
run_rate <- rev_forecast * 12

# Part 3
#
#

fit_churn <- lm(monthly_churn ~ Month, ll_data_monthly_summary[-65,])
churn_forecast <- predict(fit_churn, ll_forecast)

fit_adds <- lm(tot_trial_adds ~ Month, ll_data_monthly_summary[-65,])
adds_forecast <- predict(fit_adds, ll_forecast)

fit_conv <- lm(avg_trial_conv ~ Month, ll_data_monthly_summary[-65,])
conv_forecast <- predict(fit_conv, ll_forecast)

forecast <- cbind(ll_forecast, churn_forecast, adds_forecast, conv_forecast)

impact_forecast <- forecast %>%
  mutate(new_paid_adds = adds_forecast * conv_forecast, new_paid_sub = new_paid_adds * (1-churn_forecast), new_rev = new_paid_sub * 149.7,
         adds_rev_change = ((adds_forecast*1.1 * conv_forecast) * (1-churn_forecast) * 149.7) - new_rev, 
         churn_rev_change = ((adds_forecast * conv_forecast) * (1-(churn_forecast-(churn_forecast*0.1))) * 149.7) - new_rev,
         conv_rev_change = ((adds_forecast * conv_forecast*1.1) * (1-churn_forecast) * 149.7) - new_rev)

# Part 4
#
#


