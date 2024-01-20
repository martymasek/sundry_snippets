# Goal -------------------------------------------------------------------------
# Compare the sun rise and set times in different locations.
# Get data from U.S. Navy via API https://aa.usno.navy.mil/data/index.
# Plot times.
# Does not account for Daylight Saving Time.

# Load packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(httr)
library(lubridate)
library(glue)
library(tidyr)
library(purrr)

# Define dates and places of interest ------------------------------------------
date_vec <- seq(as.Date('2022-01-01'), as.Date('2022-12-31'), by = 'days')
place_list <- list(
  'Camas'       = 'coords=45.59,-122.41&tz=-8',
  'Sacramento'  = 'coords=38.56,-121.46&tz=-8',
  'Tallahassee' = 'coords=30.44,-84.29&tz=-5',
  'Erie'        = 'coords=42.12,-80.08&tz=-5',
  'PortofSpain' = 'coords=10.66,-61.51&tz=-4')

# Function to get data from API ------------------------------------------------
get_sun_times <- function(date_place) {
  
  result <- GET(glue('https://aa.usno.navy.mil/api/rstt/oneday?date={date_place}')) |>
    content()
  
  sun_data <- bind_rows(result$properties$data$sundata)
  
  return(sun_data)
  
}

# Run function and clean data --------------------------------------------------
sun_df <- expand_grid(place = unlist(place_list),
                      date = date_vec) |>
  mutate(date_place = paste0(date, '&', place)) |>
  mutate(sun_data = map(date_place, get_sun_times)) |> 
  unnest(sun_data) |>
  filter(phen %in% c('Rise','Set')) |>
  mutate(dtm = as.POSIXct(time, format = "%H:%M"))

# make wide for plotting lines separately
sun_df_wide = sun_df |>
  select(place, date, phen, dtm) |>
  pivot_wider(id_cols = c(place, date),
              names_from = phen,
              values_from = dtm)

# Define values for pretty plotting --------------------------------------------

x_breaks <- c(glue('{year(date_vec[1])}-01-01'),
              glue('{year(date_vec[1])}-04-01'),
              glue('{year(date_vec[1])}-07-01'),
              glue('{year(date_vec[1])}-10-01'),
              glue('{year(date_vec[1])}-12-31'))
x_labels <- c('Jan','Apr','Jul','Oct','Jan')
y_time_breaks <- c('00:00:00','06:00:00','12:00:00','18:00:00','23:59:59')
y_breaks <- c(as.POSIXct(paste(as.Date(sun_df$dtm[1]), y_time_breaks)))
y_labels <- c('12 AM','6 AM', '12 PM', '6 PM', '12 AM')

# Plot -------------------------------------------------------------------------

sun_df_wide |>
  # reduce the number of lines on the plot
  filter(names(place) %in% c('Tallahassee','Sacramento')) |>
  ggplot(aes(x = date,
             group = place,
             color = place)) +
  geom_line(aes(y = Rise),
            linewidth = 1.5,
            alpha = .3) +
  geom_line(aes(y = Set),
            linewidth = 1.5,
            alpha = .3) +
  scale_x_date(name = '\nMonth',
               breaks = as.Date(x_breaks),
               labels = x_labels) +
  scale_y_datetime(name = 'Time\n',
                   breaks = y_breaks,
                   labels = y_labels,
                   limits = c(min(y_breaks), max(y_breaks))) +
  scale_color_discrete(name = 'Place',
                       breaks = unlist(place_list),
                       labels = names(place_list)) +
  labs(title = 'Sun Rise and Set Times\n') +
  theme(panel.background = element_rect(fill = NA, color = 'grey'),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', linetype = 'dashed'),
        plot.title.position = 'plot')

# save plot for repo's readme
ggsave(filename = 'sun_rise_set.png',
       path = 'output/plots',
       device = 'png')

# Done -------------------------------------------------------------------------
