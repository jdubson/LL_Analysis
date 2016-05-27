# Create model using sample data
# Author: Jason Wu

# Load libraries
library(tidyr)
library(dplyr)
library(zoo)

# Import sample data
ll_data <- read.csv("~/Desktop/LL_data.csv", header=TRUE, sep=",")
ll_data_clean <- ll_data

# Format data frame
ll_data_clean$Date <- as.Date(ll_data_clean$Date, format = "%m/%d/%y")
ll_data_clean$ARPU <- as.numeric(gsub("\\$4.99", 4.99, ll_data_clean$ARPU))
ll_data_clean$Trial.Conversion <- as.numeric( gsub("\\%", "", ll_data_clean$Trial.Conversion))
ll_data_clean$Trial.Conversion <- ll_data_clean$Trial.Conversion / 100
temporary <- as.vector(ll_data_clean$Trial.Adds[-1])
temporary <- append(temporary,NA)
ll_data_clean$Trial.Temp <- temporary

ll_data_clean <- ll_data_clean %>%
  mutate(Revenue = Paid.Subs * ARPU, Trial.New = round(Trial.Temp/Trial.Conversion,0)) %>%
  select(Date, Trial.Conversion, Trial.New, Trial.Adds, Trial.Subs, Paid.Adds, Paid.Subs, Revenue)

ll_data_clean$Trial.New <- as.numeric(gsub("Inf", NA, ll_data_clean$Trial.New))

# Summarize into monthly data frame
ll_data_monthly <- ll_data_clean
ll_data_monthly$Month <- as.Date(as.yearmon(ll_data_monthly$Date))

ll_data_monthly_summary <- ll_data_monthly %>%
  group_by(Month) %>%
  summarise(avg_trial_conv = median(Trial.Conversion), tot_trial_new = sum(Trial.New, na.rm = TRUE), tot_trial_adds = sum(Trial.Adds),
            tot_trial_subs = sum(Trial.Subs), tot_paid_adds = sum(Paid.Adds), tot_paid_subs = sum(Paid.Subs), tot_revenue = sum(Revenue))