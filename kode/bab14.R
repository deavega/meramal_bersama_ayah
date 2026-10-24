source("kode/00_persiapan.R")
library(fable.prophet)
set.seed(11)
cpo <- baca_komoditas() |> filter_index("2005 Jan" ~ "2025 Dec") |> select(bulan, cpo, brent)
latih <- cpo |> filter_index(~ "2023 Dec")

# 14.1 Kekuatan kombinasi --------------------------------------------------------
fit <- latih |>
  model(ETS = ETS(log(cpo)),
        ARIMA = ARIMA(log(cpo)),
        NNAR = NNETAR(log(cpo)),
        Prophet = prophet(log(cpo) ~ growth("linear") + season("year", type = "multiplicative"))) |>
  mutate(Kombinasi = (ETS + ARIMA + NNAR) / 3)
set.seed(11)
fc <- fit |> forecast(h = 24, times = 300)
print(accuracy(fc, cpo) |> select(.model, RMSE, MAE, MAPE) |> arrange(RMSE))
print(accuracy(fc, cpo, list(crps = CRPS)) |> select(.model, crps) |> arrange(crps))
fc |> autoplot(cpo |> filter_index("2019 Jan" ~ .), level = NULL, linewidth = 0.75) +
  labs(title = "Lima pendekatan untuk CPO 2024–2025", x = NULL, y = "USD per ton", colour = "Model")
simpan("b14_kombinasi", tinggi = 3.7)
fit |> select(NNAR) |> report()

# 14.2 Musiman kompleks: data harian -----------------------------------------------
telur <- baca_pangan() |>
  filter(komoditas == "telur_ayam") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal) |> fill_gaps() |> tidyr::fill(harga)
telur_latih <- telur |> filter(tanggal <= as.Date("2025-08-31"))
telur_uji <- telur |> filter(tanggal > as.Date("2025-08-31"))
fit_harian <- telur_latih |>
  model(`STL + ETS` = decomposition_model(
          STL(log(harga) ~ season(period = "week") + season(period = "year")),
          ETS(season_adjust ~ season("N"))),
        `Harmonik dinamis` = ARIMA(log(harga) ~ fourier(period = "week", K = 3) +
                                    fourier(period = "year", K = 5) + PDQ(0, 0, 0)),
        Naif = NAIVE(harga))
fc_h <- fit_harian |> forecast(h = nrow(telur_uji))
print(accuracy(fc_h, telur) |> select(.model, RMSE, MAE, MAPE) |> arrange(RMSE))
fc_h |> autoplot(telur |> filter(tanggal >= as.Date("2025-01-01")), level = NULL, linewidth = 0.7) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Harga telur ayam harian: forecast Sep 2025–Feb 2026",
       x = NULL, y = "Rupiah per kg", colour = "Model")
simpan("b14_harian")

# 14.3 VAR: CPO dan Brent ---------------------------------------------------------
cpo_d <- cpo |>
  mutate(d_cpo = difference(log(cpo)) * 100, d_brent = difference(log(brent)) * 100) |>
  filter_index("2005 Feb" ~ .) |> select(bulan, d_cpo, d_brent)
fit_var <- cpo_d |> model(VAR = VAR(vars(d_cpo, d_brent) ~ AR(1:6)))
report(fit_var)
print(fit_var |> glance())
fc_var <- fit_var |> forecast(h = 12)
print(fc_var |> as_tibble() |> select(-.model) |> head(4))
fc_var |> autoplot(cpo_d |> filter_index("2022 Jan" ~ .)) +
  labs(title = "Forecast VAR untuk perubahan bulanan CPO dan Brent (%)", x = NULL, y = "%")
simpan("b14_var", tinggi = 4)

# 14.4 Bagging ETS -------------------------------------------------------------
set.seed(7)
sim <- latih |> model(stl = STL(log(cpo))) |>
  generate(new_data = latih, times = 30, bootstrap_block_size = 24) |>
  select(-.model, -cpo)
ggplot() +
  geom_line(aes(bulan, .sim, group = .rep), data = as_tibble(sim), colour = "grey75", linewidth = 0.25) +
  geom_line(aes(bulan, cpo), data = latih, colour = "black", linewidth = 0.5) +
  labs(title = "30 versi CPO hasil bootstrap blok (hitam = asli)", x = NULL, y = "USD per ton")
simpan("b14_bootstrap_seri")
fc_bag <- sim |> model(ets = ETS(log(.sim))) |> forecast(h = 24) |>
  as_tibble() |> summarise(bagged = mean(.mean), .by = bulan)
bag <- fc_bag |> mutate(cpo_bag = bagged) |>
  left_join(as_tibble(cpo) |> select(bulan, cpo), by = "bulan")
print(bag |> summarise(RMSE_bagged = sqrt(mean((cpo - cpo_bag)^2))))
