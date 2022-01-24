# Florida: County-Based Distances

# People in which couties would not be within an acceptable distance of
# a _____ facility?
## Acceptable distance = 60 miles or less, "as the crow flies"


# Set up -----------------------------------------------------------------------

## Libraries
library(tidyverse)
library(tigris) # import county shapefiles
library(sf)     # work with spatial data

# Get and prepare data ---------------------------------------------------------

## Import Florida county shapefiles using the tigris package
counties <- tigris::counties(state = "12", cb = TRUE, class = "sf")

## Make column names lower case
names(counties) <- tolower(names(counties))

## Add a column with the centroid of each county
### Note: Centroids will be the secondary geometry column
counties$centroid <- st_centroid(counties$geometry)

## Visually check that the centroids are correctly placed
ggplot(data = counties) +
  geom_sf() +
  geom_sf(aes(geometry = centroid))

## Make a new object that will have the centroids as the primary geometry
centroids <- counties %>%
  select(name, centroid)

## Set centroids as primary geometry
st_geometry(centroids) <- "centroid"

## Make sure the centroids are now the primary geometry by default plotting
ggplot(data = centroids) +
  geom_sf()

# Calculate distances ----------------------------------------------------------

## Calculate distances between centroids in meters
### Note: This loses the associated county names as it goes into a quasi-matrix
distance_matrix <- st_distance(centroids, by_element = FALSE) %>%
  as.data.frame()

## Reassociate the county names with the columns
names(distance_matrix) <- centroids$name

## Reassociate the county names with the rows
distance_matrix$county <- centroids$name

## Remove the units (meters)
distance_matrix[-68] <- lapply(distance_matrix[-68], 
                               function(x) as.numeric(sub("\\s+\\D+$", "", x))) 

## Reshape to long to subset and join other county data
county_pairs <- distance_matrix %>%
  pivot_longer(cols = 1:67,
               names_to = "comparison_county",
               values_to = "distance"
               ) %>%
  mutate(dist_in_miles = distance / 1609.34) %>%
  filter(county != comparison_county,
         # keep only the county pairs that are within 60 miles of each other
         dist_in_miles <= 60)

# Check visually ---------------------------------------------------------------
## Make a data frame to plot
check_data <- county_pairs %>%
  select(county, comparison_county) %>%
  left_join(centroids, by = c("comparison_county" = "name")) %>%
  mutate(comparison_geometry = geometry) %>%
  select(-geometry, -centroid) %>%
  left_join(centroids, by = c("county" = "name"))

## Plot the df
ggplot(data = filter(check_data, 
                     county %in% c("Leon", "Alachua", "Miami-Dade", "Pasco",
                                   "Duval", "Orange", "Baker", "Polk"))) +
  geom_sf(aes(geometry = geometry), fill = "cornflower blue", color = "white") +
  geom_sf(aes(geometry = comparison_geometry), color = "white") +
  facet_wrap(~ county, nrow = 2) +
  theme_void()

# Save the plot
ggsave(path = "",
       filename = "test_county_distances.png",
       device = "png",
       dpi = 300,
       height = 5,
       units = "in")

# Find the counties left out ---------------------------------------------------
## Which counties would not be within the acceptable distance if facilities
## were located only in: "Miami-Dade", "Broward", "Leon", "Palm Beach",
## "Hillsborough", "Orange", "Duval", "Walton?

facility_counties <- c("Miami-Dade", 
                       "Broward", 
                       "Leon", 
                       "Palm Beach", 
                       "Hillsborough", 
                       "Orange", 
                       "Duval",
                       "Walton",
                       "Alachua")

included_counties <- 
  subset(county_pairs, 
         comparison_county %in% facility_counties 
         | county %in% facility_counties
           ) %>%
  select(county) %>%
  distinct()

## People living in the following counties would not have access:
excluded_counties <- filter(counties,
                            !(name %in% included_counties$county))$name
excluded_counties

# Final map of counties --------------------------------------------------------
## Make df to map
final_map_df <- counties %>%
  mutate(access = ifelse(name %in% excluded_counties, 
                           "Inaccessible", "Accessible")) %>%
  select(name, access, geometry)

## Final map
ggplot(data = final_map_df,
       aes(fill = access)) +
  geom_sf(color = "white") +
  scale_fill_brewer(type = "qual",
                    palette = 5) +
  geom_sf_text(data = filter(final_map_df, 
                      access == "Inaccessible"),
               aes(label = name),
               alpha = .7) +
  theme_void() +
  theme(legend.title = element_blank()) 

## Save the map
ggsave(path = "C:/Users/heyma/OneDrive - Florida State University/Projects/Put On GitHub",
       filename = "county_access_map.png",
       device = "png",
       dpi = 300,
       height = 7,
       units = "in")

# End --------------------------------------------------------------------------