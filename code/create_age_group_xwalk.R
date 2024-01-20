# Make 5-yr age group crosswalk, ages 0 to 120 ---------------------------------

# Create vectors for the lower and upper age bounds of the age group ranges
lower_age_bounds <- rep(seq(0,80,5), each = 5)
upper_age_bounds <- rep(seq(4,84,5), each = 5)

# Paste the vectors together, add values for the 85+ open interval group
age_groups <- c(paste0(lower_age_bounds, "-", upper_age_bounds),
                rep("85+", 36))

# Make the crosswalk
## with an age column 
## and another column with the corresponding age groups from the vector above
age_xwalk <- dplyr::bind_cols("age" = 0:120,
                       "age_group_5yr" = age_groups)

# End --------------------------------------------------------------------------