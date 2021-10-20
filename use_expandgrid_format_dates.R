# Clean HHS Data from _____ ----------------------------------------------------
# Author: Marty Masek
# Date Created: August 10, 2021
# Purpose: We have facility-reported data that are missing reports for some days
# and have multiple reports for other days. When a facility doesn't report, they 
# don't have a row for that day, so we need to give them a row for each possible
# day and fill their values with previously reported values for up to four days.
# When they have multiple reports for a day, we need to take the most recent.

library(readxl)
library(tidyverse)
library(lubridate)
library(RODBC)

# Load data
data1 <- read_xlsx("")

# Convert dates to date formats
data2 <- data1 %>%
  mutate(Submission_Time_UTC = as.POSIXct(strptime(Submission_Time_UTC,
                                                   "%H:%M %p, %b %d, %Y")),
         Data_Time_UTC = as.Date(Data_Time_UTC, "%b %d, %Y"),
         # make a duplicate date column for later determining the number of days 
         # that passed since report. Join with this column so R will keep the 
         # original column after join
         Data_Time_UTC2 = Data_Time_UTC) 

# Keep the most recent entry for each day when hospitals have multiple entries 
# for a data date
data3 <- data2 %>%
  group_by(hospital_id, Data_Time_UTC) %>%
  slice(which.max(Submission_Time_UTC))
  
# get unique hospital ids and dates to use for expand.grid (next)
hospital_ids <- unique(data1$hospital_id)
dates <- unique(data2$Data_Time_UTC) 
# summary(dates)

# Create base table then join rest of data
data4 <- expand.grid(hospital_ids, dates) %>%
  rename(hospital_id = Var1,
         date_base   = Var2) %>%
  # change this start date as needed:
  filter(date_base >= as.Date("2020-11-01")) %>% 
  # make new date field without year (for use in epi-year comparisons)
  mutate(month_day = format(date_base, "%b-%d")) %>%
  arrange(hospital_id, date_base) %>% # important for fill() below
  left_join(data3,
            by = c("hospital_id" = "hospital_id",
                   "date_base" = "Data_Time_UTC2")) %>%
  group_by(hospital_id) %>%
  # Fill NAs with previous values (downward)
  fill(everything()) %>%
  # calculate the number of days separating the submission date and date_base
  mutate(days_since_report = as.numeric(difftime(date_base, 
                                                 Data_Time_UTC, 
                                                 units = "days")),
         days_since_report = ifelse(is.na(days_since_report), 
                                    99, 
                                    days_since_report),
         date_base = gsub("-","",date_base))

# Replace values with NA when the numbers are carried for more than 4 days
data4[data4$days_since_report > 4, 3:162] <- NA

# Remove some problematic columns
hss_for_sql <- data4[, -c(17, 19, 124)]

# Write to SQL Server ----
connection <- odbcConnect('fill_in')

sqlSave(channel   = connection, 
        dat       = hss_for_sql,
        tablename = "hhs_covid_fac", 
        rownames  = FALSE,
        safer     = FALSE,
        fast      = FALSE,
        varTypes  = c(date_base = "varchar(8)")
)

# Done! ------------------------------------------------------------------------
