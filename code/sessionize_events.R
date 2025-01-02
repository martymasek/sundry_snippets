# Code for combining events within time window and allowing multiple spans per person.
# AKA "Session windows" or "Sessionizing".

# Author: Marty Masek

# Objective ----
# Span is event plus some number of days (time window).
# When there is another event in the time window, the span runout is re-anchored 
# at the new event date, increasing the span length and combining the events.

# Strategy
## 1. Arrange by date.
## 2. Make a span ID column with cumsum() to keep other events within the time window.
## 3. Keep rows with the first span ID.
## 4. Summarize event names (paste together) and min and max event dates in the span.
## 5. Calculate span end date by adding the time window to the max event date.
## 6. For subsequent spans, join original df to the above resulting df and keep
##   records after span end date. Then repeat steps.
## 7. Repeat until no data remain.

# Setup ----
library(dplyr)
library(lubridate)

# Data in long format
fake_df <- data.frame(
  person_id = sample(1:25,           250, replace = TRUE),
  event_nm  = sample(c("A","B","C"), 250, replace = TRUE),
  event_dt = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 250)
)

# Function for each span ----
span_fun <- function(df, time_window) {
  
  df |>
    # 1. Arrange by date
    group_by(person_id) |>
    arrange(event_dt) |>
    # 2. Make a span ID column with cumsum() to keep other events within the time window
    mutate(
      day_diff = as.numeric(event_dt -  lag(event_dt)),
      is_new_span = case_when(
        is.na(day_diff)         ~ 0, # NA is the record with the first date
        day_diff <  time_window ~ 0, # using `<` because date subtraction cuts out one day
        day_diff >= time_window ~ 1
      ),
      span_id = cumsum(is_new_span)
    ) |>
    ungroup() |>
    # 3. Keep rows with the first span ID
    filter(span_id == 0) |>
    # 4. Summarize event names (paste together) and min and max event dates in the span
    group_by(person_id) |>
    arrange(event_nm) |>
    summarize(
      span_nm = paste(unique(event_nm), collapse = ", "),
      span_start_dt = min(event_dt),
      max_event_dt  = max(event_dt)
    ) |>
    # 5. Calculate span end date
    mutate(
      span_end_dt = max_event_dt + days(time_window-1), # minus 1 because event day is already included
      time_window_allowed = time_window
    )
    
}

# Loop for multiple spans ----

span_list <- list()

for (i in 1:100) {
  
  # filter to correct data. For the first iteration, use all data
  if (i == 1) {
    df <- fake_df
  } else if (i > 1) {
    df <- fake_df |>
      # 6. For subsequent spans, join original df to the previous resulting df and keep
      ##   records after span end date.
      left_join(
        span_list[[i-1]] |> select(person_id, span_end_dt),
        by = "person_id"
      ) |>
      filter(event_dt > span_end_dt)
  }
  
  # apply the function if the df has any records
  if (nrow(df) > 0) {
    span_list[[i]] <- span_fun(df = df, time_window = 30) |>
      mutate(span_id = i)
  } else {
    break # 7. Repeat until no data remain
  }
}

# Final data ----
final_spans <- bind_rows(span_list)

# check combined event names
# table(final_spans$span_nm)
