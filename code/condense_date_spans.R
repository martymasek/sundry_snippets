# condense multiple overlapping spans (drug spans, enrollment spans, etc.)
# into discrete spans.

library(dplyr)
library(purrr)

# start with long data
## create sample data
set.seed(789)
df <- data.frame(
  person_id = rep(letters[1:10], each = 20),
  start_dt  = sample(
    seq(
      from = as.Date("2025-01-01"), 
      to   = as.Date("2025-12-31"), 
      by   = "day"),
    size = 200,
    replace = TRUE
  ),
  duration = sample(1:14, size = 200, replace = TRUE)
) |>
  mutate(
    end_dt = start_dt + duration
  )

condensed_df <- df |>
  group_by(person_id) |>
  arrange(start_dt, .by_group = TRUE) |>
  mutate(
    running_max_end_dt = purrr::accumulate(end_dt, max),
    # flag new spans when:
    is_new_span = case_when(
      # no previous start date
      is.na(lag(start_dt)) ~ 1,
      # there is more than 1 day gap between the previous end date and the current start date
      as.numeric(start_dt - lag(running_max_end_dt)) > 1 ~ 1,
      TRUE ~ 0
    ),
    # cumsum to make span ids
    span_id = cumsum(is_new_span)
  ) |>
  group_by(person_id, span_id) |>
  summarize(
    start_dat = min(start_dt),
    end_dt    = max(end_dt)
  ) |>
  ungroup()
