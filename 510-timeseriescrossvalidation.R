fb_stretch <- fb_stock |>
  stretch_tsibble(.init = 3, .step = 1) |>
  filter(.id != max(.id)) 

fb_stretch

fit_cv <- fb_stretch |>
  model(RW(Close ~ drift()))

fit_cv

fc_cv <- fit_cv |>
  forecast(h = 1)

fc_cv |> accuracy(fb_stock)

fb_stock |> 
  model(RW(Close ~ drift())) |>
  accuracy()