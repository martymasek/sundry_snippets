# function to plot trees from h2o model ----
## requires tidyverse to be loaded, h2o to be running, igraph to be installed (no need to load)
if (!require(igraph)) install.packages('igraph')
if (!require(tidyverse)) install.packages('tidyverse')
# if (!require(h2o)) install.packages('h2o')
library(tidyverse)

plot_h2o_tree <- function(model, 
                          tree_number, 
                          label_length = 25, 
                          max_depth = 6, # trees that are too deep cannot be human-viewed
                          min_depth = 0) {
  # call tree data
  tree <- h2o::h2o.getModelTree(model, tree_number = tree_number)
  
  # put data into df
  node_df <- data.frame(node        = tree@node_ids,
                        feature     = tree@features,
                        # prediction  = tree@predictions,
                        prediction  = 1 / (1 + exp(-tree@predictions)),
                        left_child  = tree@left_children,
                        right_child = tree@right_children,
                        threshold   = tree@thresholds,
                        na_direction= tree@nas) |>
    mutate(label = ifelse(is.na(feature), paste0("p=",round(prediction, 3)), substr(feature,1,label_length)),
           left_thresh_txt = case_when(
             is.na(threshold) ~ "",
             round(threshold,9) == -0.000000954 & na_direction == "LEFT"  ~ "1, NA",
             round(threshold,9) == -0.000000954 & na_direction == "RIGHT" ~ "1",
             na_direction == "LEFT"  ~ paste0("<", round(threshold,1), ", NA"),
             na_direction == "RIGHT" ~ paste0("<", round(threshold,1))
           ),
           right_thresh_txt = case_when(
             is.na(threshold) ~ "",
             round(threshold,9) == -0.000000954 & na_direction == "LEFT"  ~ "0",
             round(threshold,9) == -0.000000954 & na_direction == "RIGHT" ~ "0, NA",
             na_direction == "LEFT"  ~ paste0(">=", round(threshold,1)),
             na_direction == "RIGHT" ~ paste0(">=", round(threshold,1), ", NA")
           ),
           thresh_text = paste0(right_thresh_txt,"\n\n\n",left_thresh_txt)
    )
  
  # make long version of df
  node_df_long <- node_df |>
    select(node, left_child, right_child) |>
    rename(origin_node = node) |>
    pivot_longer(cols = c(left_child, 
                          right_child),
                 names_to  = NULL, # don't need split direction
                 values_to = "child_node") |>
    filter(child_node != -1) # remove rows where a terminal node is the "from" node
  
  # make graph object in order to assign depth "coordinates"
  g <- node_df_long |> 
    igraph::graph_from_data_frame(directed = TRUE)
  
  # retrieve graph depth coordinates (x-axis)
  node_coords_x <- data.frame(node = as.integer(igraph::vertex_attr(g, "name")),
                              # y = igraph::layout_as_tree(g, mode = "out")[,1],
                              x = igraph::layout_as_tree(g, mode = "out", flip.y = FALSE)[,2])
  
  # make final plot coordinates
  ## make custom y-axis coordinates that separate terminal nodes better than igraph does
  y_axis_range <- node_coords_x |> filter(x == max_depth) |> count() |> pull()
  node_coords <- node_coords_x |>
    filter(x <= max_depth) |>
    arrange(node) |>
    group_by(x) |> # group by depth
    mutate(n = n(),
           y_spacing_at_depth = y_axis_range/n(),
           cumsum_y_spacing = cumsum(y_spacing_at_depth),
           y = cumsum_y_spacing-(y_spacing_at_depth/2)) |>
    select(node, x, y)
  
  # final df for plotting
  node_plot_df <- node_df_long |>
    mutate(row_number = row_number()) |>
    pivot_longer(cols = c(origin_node, child_node),
                 names_to = "place",
                 values_to = "node") |>
    distinct() |>
    left_join(node_coords, by = "node") |>
    left_join(node_df, by = "node") |>
    mutate(node_type = ifelse(is.na(feature), "terminal", "non-terminal")) |>
    select(row_number, node, x, y, label, node_type, thresh_text) |>
    # plot only between a certain depth
    filter(x <= max_depth & x >= min_depth)
  
  # make separate df to plot threshold values
  thresh_long <- node_df |>
    rename(origin_node = node,
           right_thresh = right_thresh_txt,
           left_thresh = left_thresh_txt) |>
    select(origin_node, left_child, right_child, left_thresh, right_thresh) |>
    pivot_longer(cols = 2:5,
                 names_to = c("direction", ".value"),
                 names_sep = "_"
    ) |>
    filter(child != -1) |>
    select(-direction) |>
    rename(child_node = child)
  
  threshold_plot_df <- node_df_long |>
    left_join(node_coords, by = c("child_node" = "node")) |>
    select(-x) |>
    rename(child_y = y) |>
    left_join(node_coords, by = c("origin_node" = "node")) |>
    rename(origin_y = y) |>
    mutate(thresh_y = (origin_y + child_y)/2) |>
    select(-c(origin_y, child_y)) |>
    left_join(thresh_long, by = c("origin_node", "child_node")) |>
    filter(!is.na(thresh_y))
  
  # plot
  plot <- ggplot(data    = node_plot_df,
                 mapping = aes(x = x,
                               y = y,
                               label = label)) +
    # draw network lines
    geom_path(mapping = aes(group = row_number)) +
    # add nodes
    geom_label(mapping = aes(label = label,
                             fill = node_type),
               label.size = 0,
               size = 3) +
    # add threshold values
    geom_label(data    = threshold_plot_df,
               mapping = aes(x = x+.5,
                             y = thresh_y,
                             label = thresh),
               label.size = 0,
               size = 2,
               alpha = .7) +
    # extra space for labels
    scale_x_continuous(expand = c(.1,.1)) +
    # blank background
    theme_void() +
    theme(legend.position = "none")
  
  return(plot)
  
}
