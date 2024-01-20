# Get FL SNF COVID Data from CMS Website via API -------------------------------

# Set up ----
library(jsonlite)

# Change these two objects:
data_id <- "b290adc2-7646-4a4d-b54d-ee7765916f36" # changes each week
data_date <- "2021-07-25" # obviously, changes each week


# set objects/values ----
#specify the data url
data_url <- 
  paste0("https://data.cms.gov/data-api/v1/dataset/",
         data_id,
         "/data")

# create filters to be used in the API call
filter_state <- "filter[provider_state]=FL"
filter_quality <- "filter[passed_quality_assurance_check]=Y"
filter_week <- 
  paste0("filter[week_filter][condition][path]=week_ending",
         "&filter[week_filter][condition][operator]=STARTS_WITH",
         "&filter[week_filter][condition][value]=", data_date)

# request the data ----
# Sometimes this times out and has to be run again.
snf_df <- 
  fromJSON(
    paste0(data_url,
           "?",
           filter_state,
           "&",
           filter_quality,
           "&",
           filter_week))
