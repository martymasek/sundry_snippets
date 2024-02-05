# Setup ------------------------------------------------------------------------
## packages 
library(tidyverse)
library(sf) # for spatial data
library(glue)

## define 5-level diverging color palette
div_5 <- c("#d7191c","#fdae61","#f1f192","#abd9e9","#2c7bb6")

## specify WA counties of interest
counties_of_interest <- c("Thurston") # enter one or more county names

## specify coordinate reference system
crs_desired <- 4269

## read shapefiles
### WA precinct polygons with votes by precinct - use 2016 presidential election data
#### retrieved from https://redistrictingdatahub.org/dataset/vest-2016-washington-precinct-and-election-results/
precincts <- read_sf("input/data/wa_vest_16/wa_vest_16.shp")

### WA road lines to help orient the map
#### retrieved the data from https://www2.census.gov/geo/tiger/TIGER2023/PRISECROADS/
roads <- read_sf("input/data/tl_2023_53_prisecroads/tl_2023_53_prisecroads.shp")

### WA city points to help orient the map
#### retrieved from https://geo.wa.gov/datasets/cfc2b6503ecd45efa03052d7b9e28a95/about
cities <- read_sf("input/data/WSDOT_-_City_Points/WSDOT_-_City_Points.shp")

# Clean data -------------------------------------------------------------------
## filter to county of interest, make % left measure, and set CRS
precincts_clean <- precincts |>
  mutate(n_right = G16PRERTRU + G16PRELJOH + G16PRECCAS,
         n_left  = G16PREDCLI + G16PREGSTE + G16PREOKEN + G16PREOLAR,
         p_left = n_left/(n_left + n_right),
         p_left_bins = cut(p_left, 5)
         ) |>
  filter(COUNTY %in% counties_of_interest) |>
  st_transform(crs_desired)

# make a county boundary to speed up roads filtering below and fix invalid geometry error
county_boundary <- precincts |>
  filter(COUNTY %in% counties_of_interest) |>
  group_by(COUNTY) |>
  summarize() |>
  st_buffer(dist = 100) |>
  st_transform(crs_desired)

## roads: set CRS and keep only lines within the county boundary
roads_clean <- roads |>
  st_transform(crs_desired) |>
  st_intersection(county_boundary)

# city points/names: set CRS and keep major cities within the county boundary
cities_clean <- cities |>
  # filter(MajorCity == 'yes') |>
  st_transform(crs_desired) |>
  st_intersection(county_boundary)

# Plot -------------------------------------------------------------------------

ggplot() +
  geom_sf(data = precincts_clean,
          aes(fill = p_left_bins,
              color = p_left_bins),
          # color = "white" # uncomment for lines around precincts
            ) +
  geom_sf(data = roads_clean,
          color = "white",
          size = .3) +
  geom_sf_text(data = cities_clean,
               aes(label = NAME),
               color = "black",
               alpha = .6,
               size = 4) +
  scale_fill_manual(name = "Proportion\nLeft Voters",
                    breaks = levels(precincts_clean$p_left_bins),
                    labels = # make first level prettier for the legend
                      c("[0.0,0.2]",levels(precincts_clean$p_left_bins)[2:5]),
                    values = div_5) +
  scale_color_manual(name = "Proportion\nLeft Voters",
                     breaks = levels(precincts_clean$p_left_bins),
                     labels = # make first level prettier for the legend
                       c("[0.0,0.2]",levels(precincts_clean$p_left_bins)[2:5]),
                     values = div_5) +
  labs(title = "2016 Proportion of Voters Casting 'Left' Presidential Vote by Precinct",
       subtitle = glue("{paste0(counties_of_interest, ' County', collapse = ', ')} ", 
                       "in Washington State"),
       caption = str_wrap(paste("Sources: Precinct polygon shapefile with vote data from Redistricting Data Hub.",
                       "Road line shapefile from census.gov.",
                       "City point shapefile from wa.gov."), 150)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.caption = element_text(hjust = 0)) 

# create plot file name
plot_filename <- paste0("precinct_votes_map_",
                       paste0(str_to_lower(counties_of_interest), 
                                                   collapse = '_'),
                              ".png")

# save plot
ggsave(filename = plot_filename,
       path = "output/plots",
       device = "png"
       )

# Done -------------------------------------------------------------------------
