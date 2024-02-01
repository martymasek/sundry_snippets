# Setup ------------------------------------------------------------------------
## packages 
library(tidyverse)
library(sf)
library(glue)

## define 5-level diverging color palette
div_5 <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6")

## specify WA counties of interest
counties_of_interest <- c("Thurston") # enter one of more county names

## specify coordinate reference system
crs_desired <- 4269

## read shapefiles
### precinct polygons with votes by precinct - use 2016 preseidential election data
#### retrieved from https://redistrictingdatahub.org/dataset/vest-2016-washington-precinct-and-election-results/
precincts <- read_sf("input/data/wa_vest_16/wa_vest_16.shp")

### road lines to help orient the map
#### retrieved the data from https://www2.census.gov/geo/tiger/TIGER2023/PRISECROADS/
roads <- read_sf("input/data/tl_2023_53_prisecroads/tl_2023_53_prisecroads.shp")

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

## set CRS and keep only lines within the county of interest
roads_clean <- roads |>
  st_set_crs(crs_desired) |>
  st_intersection(county_boundary)

# Plot -------------------------------------------------------------------------

ggplot() +
  geom_sf(data = precincts_clean,
          aes(fill = p_left_bins),
          color = "white") +
  geom_sf(data = roads_clean,
          color = "dark grey") +
  scale_fill_manual(name = "Proportion\nLeft Voters",
                    breaks = levels(precincts_clean$p_left_bins),
                    labels = # make first level prettier for the legend
                      c("[0.0,0.2]",levels(precincts_clean$p_left_bins)[2:5]),
                    values = div_5) +
  theme_void() +
  labs(title = "2016 Proportion of Voters Casting 'Left' Presidential Vote by Precinct",
       subtitle = glue("{paste0(counties_of_interest, ' County', collapse = ', ')}, ", 
                       "Washington State"))

# save plot
ggsave(filename = "precinct_votes_map.png",
       path = "output/plots",
       device = "png"
       )
