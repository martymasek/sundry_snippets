# load required packages
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)

# make example data
set.seed(3)
test_data <- data.frame(age = abs(round(rnorm(n = 10000, mean = 25, sd=20),0)),
                        sex = sample(c('M','F'), 1000, replace = TRUE))

# create popn pyramid function
popn_pyramid <- function(df, 
                         ageCol,
                         sexCol,
                         mColor = "#e52b50", 
                         fColor = "#2bade5"){
  plot_colors <- c(mColor, fColor)
  pyr_title <- glue::glue(
    "Age Distribution of 
    <span style='color:{plot_colors[2]};'>Males</span>
    and 
    <span style='color:{plot_colors[1]};'>Females</span> 
    " )
  plot_popn <- substitute(
    
    df %>%
      group_by(ageCol, sexCol) %>%
      summarize(n = as.numeric(n())) %>%
      mutate(n = case_when(sexCol == "F" ~ n,
                           sexCol == "M" ~ n*-1)) %>%
      ggplot(aes(x    = ageCol,
                 y    = n,
                 fill = sexCol)) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(0,110, 10),
                         name = "Age") +
      scale_fill_manual(values = plot_colors) +
      coord_flip() +
      labs(title = pyr_title) +
      theme(panel.background = element_rect(fill = "grey96"),
            plot.title = ggtext::element_markdown(color = "grey30"),
            legend.position = "none",
            plot.title.position = "plot") 
    
  )
  eval(plot_popn)
}

# use function to make plot with example data
popn_pyramid(df = test_data,
             ageCol = age,
             sexCol = sex)

# save plot
ggsave(filename = 'popn_pyramid.png',
       path = 'output/plots',
       device = 'png')
