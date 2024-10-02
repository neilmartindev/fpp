ETS(y ~ error("A") + trend("N") + season("N"))
ETS(y ~ error("M") + trend("N") + season("N"))

trend("A", beta = 0.004)
trend("A", beta_range = c(0, 0.1))