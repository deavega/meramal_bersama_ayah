source("kode/00_persiapan.R")
inflasi <- baca_inflasi()
print(inflasi)
inflasi |> autoplot(inflasi_yoy) +
  labs(title = "Inflasi IHK Indonesia, tahunan (yoy), 2003–2019",
       x = NULL, y = "Persen")
simpan("b11_inflasi")

# 11.1 Stasioneritas dan differencing ---------------------------------------
print(inflasi |> features(inflasi_yoy, unitroot_kpss))
print(inflasi |> features(inflasi_yoy, unitroot_ndiffs))
print(inflasi |> mutate(d = difference(inflasi_yoy)) |> features(d, unitroot_kpss))
print(inflasi |> features(inflasi_yoy, unitroot_nsdiffs))
gambar_d <- inflasi |> gg_tsdisplay(difference(inflasi_yoy), plot_type = "partial", lag_max = 36)
gambar_d
simpan("b11_tsdisplay", gambar_d, tinggi = 4.6)

# 11.2 Model ARIMA: manual vs otomatis ----------------------------------------
latih <- inflasi |> filter_index(~ "2019 Jun")
fit <- latih |>
  model(`ARIMA(1,1,0)` = ARIMA(inflasi_yoy ~ pdq(1, 1, 0) + PDQ(0, 0, 0)),
        `ARIMA(0,1,1)(0,0,1)` = ARIMA(inflasi_yoy ~ pdq(0, 1, 1) + PDQ(0, 0, 1)),
        Bertahap = ARIMA(inflasi_yoy),
        Menyeluruh = ARIMA(inflasi_yoy, stepwise = FALSE, approximation = FALSE))
print(fit |> pivot_longer(everything(), names_to = "Nama", values_to = "Model"))
print(glance(fit) |> arrange(AICc) |> select(.model, sigma2, AICc, BIC))
fit |> select(Menyeluruh) |> report()
gambar_resid <- fit |> select(Menyeluruh) |> gg_tsresiduals(lag_max = 36)
gambar_resid
simpan("b11_resid", gambar_resid, tinggi = 4.4)
print(augment(fit) |> filter(.model == "Menyeluruh") |> features(.innov, ljung_box, lag = 24, dof = 3))

# 11.3 Uji pada Juli–Desember 2019 ----------------------------------------------
fit2 <- latih |>
  model(ARIMA = ARIMA(inflasi_yoy, stepwise = FALSE, approximation = FALSE),
        ETS = ETS(inflasi_yoy),
        Naif = NAIVE(inflasi_yoy))
fc <- fit2 |> forecast(h = 6)
print(accuracy(fc, inflasi) |> select(.model, RMSE, MAE, MASE) |> arrange(RMSE))
print(fc |> filter(.model == "ARIMA") |> hilo(80) |> as_tibble() |> select(bulan, .mean, `80%`))
print(inflasi |> filter_index("2019 Jul" ~ .))
fc |> autoplot(inflasi |> filter_index("2016 Jan" ~ .), level = 80, alpha = 0.5) +
  labs(title = "Forecast inflasi Juli–Desember 2019 dari data s.d. Juni 2019",
       x = NULL, y = "Persen", colour = "Model", fill = "Model")
simpan("b11_fc_2019", tinggi = 3.6)

# 11.4 ARIMA untuk kurs: kembali ke random walk -----------------------------------
kurs <- baca_kurs_bulanan() |> filter_index("2010 Jan" ~ .)
fit_kurs <- kurs |> model(ARIMA(log(kurs)))
report(fit_kurs)

# 11.5 ARIMA vs ETS dengan validasi silang (CPO) ------------------------------------
cpo <- baca_komoditas() |> filter_index("2005 Jan" ~ "2025 Dec") |> select(bulan, cpo)
cv <- cpo |> stretch_tsibble(.init = 120, .step = 6) |>
  model(ARIMA = ARIMA(log(cpo)), ETS = ETS(log(cpo)), Naif = NAIVE(log(cpo))) |>
  forecast(h = 6)
print(accuracy(cv, cpo) |> select(.model, RMSE, MAE, MAPE, MASE) |> arrange(RMSE))
