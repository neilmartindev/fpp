fit <- prison_gts |>
  filter(year(Quarter) <= 2014) |>
  model(base = ETS(Count)) |>
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )
fc <- fit |> forecast(h = 8)
fc |>
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) |>
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(y = "Number of prisoners ('000)",
       title = "Australian prison population (total)")

fc |>
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal),
    is_aggregated(Gender)
  ) |>
  autoplot(
    prison_gts |> filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by state)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fc |>
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) |>
  accuracy(data = prison_gts,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)
           )
  ) |>
  group_by(.model) |>
  summarise(mase = mean(mase), sspc = mean(ss) * 100)
