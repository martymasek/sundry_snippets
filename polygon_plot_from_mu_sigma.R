# Make polygon plot from two mus and sigmas for several groups -----------------

## Plot diamond polygons where 
## x and y are numeric variables, 
## each diamond represents a group,
## x mu and y mu are the center points of the diamonds,
## x sigma and y sigma provide the outer points of the diamonds (by being added 
## to and subtracted from the respective mu.)

# Set up -----------------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------------
# Create example data with random values
example_df <- data.frame(g = c("group 1", "group 2", "group 3", "group 4"),
                         x_mu = sample(5:10, 4, replace = FALSE),
                         y_mu = sample(5:10, 4, replace = FALSE),
                         x_sigma = runif(n = 4, min = 1, max = 3),
                         y_sigma = runif(n = 4, min = 1, max = 3)
                         )  

# Transform each x,y mu and x,y sigma into 4 x,y coord pairs
plot_df <- example_df %>%
  mutate(c1_x = x_mu, # specify the calculation for each coordinate
         c2_x = x_mu + x_sigma,  
         c3_x = x_mu,
         c4_x = x_mu - x_sigma,
         c1_y = y_mu + y_sigma,
         c2_y = y_mu, # note the different pattern for x and y. c1_x starts with mu, and c1_y starts with mu + sigma. Offsetting them makes the polygons correctly.
         c3_y = y_mu - y_sigma,
         c4_y = y_mu) %>%
  # get rid of these columns since we no longer need them for plotting
  select(-c(x_mu, y_mu, x_sigma, y_sigma)) %>% 
  pivot_longer(cols = !g,  # pivot every column except the column named "g"
               names_to = c("coord_num", ".value"), # name one column "coord_num" and name the other columns from the non-extracted part of the previous column names
               names_sep = "_") # this tells the line above what symbol separates the value that goes into the "coord_num" column from part of the column names that should remain column names


# Plot -------------------------------------------------------------------------
ggplot(plot_df) + 
  geom_polygon(aes(x = x,      # set of x coordinates
                   y = y,      # set of y coordinates
                   group = g,  # group coordinates so you don't get one big polygon
                   fill  = g), # values for color fill
               alpha = .5      # make them transparent in case of overlap
               ) + 
  lims(x = c(0, NA),   # begin axes at 0 with an unspecified max
       y = c(0, NA)) +
  theme(legend.title = element_blank())

# End! -------------------------------------------------------------------------
