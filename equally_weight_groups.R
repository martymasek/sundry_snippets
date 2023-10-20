# generate some data
data.frame(binary_group = sample(c(0,1), size = 100, prob = c(.7,.3), replace = TRUE)) |>
  # make weights
  mutate(n_tot = n()) |>
  group_by(binary_group) |>
  mutate(n_grp = n()) |>
  ungroup() |>
  mutate(
    weights = case_when(
      binary_group == 1 ~ 1, # smaller group
      binary_group == 0 ~ (n_tot-n_grp)/n_grp))
