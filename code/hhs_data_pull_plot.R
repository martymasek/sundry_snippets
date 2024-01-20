# Get HHS COVID time series data -----------------------------------------------
# Check a few fields with quick plots
# Author:       Marty Masek
# Date created: August 3, 2021

# Set up ----
library(RSocrata)
library(tidyverse)

# State-level data ----------------------------------------------------------
state <- read.socrata(
  "https://healthdata.gov/resource/g62h-syeh.csv?state=FL", # only get FL data
  app_token = getOption("hhs_app_token"),
  email     = getOption("rsocrata_email"),
  password  = getOption("rsocrata_pw")
)

"
Note: to set your credentials above, enter new credentials into the 
global options using:
      options(hhs_app_token = 'fillinapptoken')
etc.
"

# prepare data for plotting
plot_state <- state %>%
  mutate(date = as.Date(date),
         covid_hosp_conf = inpatient_beds_used_covid,
         covid_hosp_conf2 = 
           total_adult_patients_hospitalized_confirmed_covid +
           total_pediatric_patients_hospitalized_confirmed_covid,
         covid_hosp_conf3 = inpatient_bed_covid_utilization,
         covid_hosp_conf_susp = 
           total_adult_patients_hospitalized_confirmed_and_suspected_covid +
           total_pediatric_patients_hospitalized_confirmed_and_suspected_covid,
         covid_admits = 
           previous_day_admission_adult_covid_confirmed +
           previous_day_admission_adult_covid_suspected +
           previous_day_admission_pediatric_covid_confirmed +
           previous_day_admission_pediatric_covid_suspected,
         covid_admits_conf = 
           previous_day_admission_adult_covid_confirmed +
           previous_day_admission_pediatric_covid_confirmed) 

## plot # hospitalized ----
ggplot(plot_state,
       aes(x = date)) +
  geom_line(aes(y = covid_hosp_conf), color = "red") +
  geom_line(aes(y = covid_hosp_conf2), color = "blue") +
  geom_line(aes(y = covid_hosp_conf_susp), color = "green") 

## plot # hospitalized only one line ----
ggplot(plot_state,
       aes(x = date,
           y = covid_hosp_conf)) +
  geom_line() 

# plot data completeness over time
ggplot(plot_state,
       aes(x = date)) +
  geom_line(aes(y = inpatient_beds_used_covid_coverage), color = "purple") +
  geom_line(aes(y = total_adult_patients_hospitalized_confirmed_and_suspected_covid_coverage), color = "orange") +
  geom_line(aes(y = previous_day_admission_adult_covid_confirmed_coverage), color = "black")
  
## plot admissions ----
ggplot(plot_state,
       aes(x = date)) +
  geom_line(aes(y = covid_admits), color = "cornflower blue") +
  geom_line(aes(y = covid_admits_conf), color = "purple") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Facility Data ----------------------------------------------------------------
fac <- read.socrata(
  "https://healthdata.gov/resource/anag-cw7u.csv?state=FL", # only get FL data
  app_token = HHS_app_token,
  email     = RSocrata_email,
  password  = RSocrata_pw
)

# replace suppressed values with NA. Values under 4 are suppressed.
fac[fac == -999999] <- NA

# prepare data for plotting
plot_fac <- fac %>%
  mutate(collection_week = as.Date(collection_week),
         covid_hosp_conf = 
           total_adult_patients_hospitalized_confirmed_covid_7_day_avg +
           total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg,
         covid_hosp_conf_sus = 
           total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg +
           total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg) %>%
  group_by(collection_week) %>%
  summarize(covid_hosp_conf = sum(covid_hosp_conf, na.rm = TRUE),
            covid_hosp_conf_sus = sum(covid_hosp_conf_sus, na.rm = TRUE))
# plot
ggplot(plot_fac,
       aes(x = collection_week)) +
  geom_line(aes(y = covid_hosp_conf), color = "red") 

