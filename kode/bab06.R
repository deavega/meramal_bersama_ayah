source("kode/00_persiapan.R")
kurs <- baca_kurs_bulanan() |> filter_index("2010 Jan" ~ .)
latih <- kurs |> filter_index(~ "2023 Dec")

# 6.1 Empat metode tolok ukur -------------------------------------------
fit <- latih |>
  model(Rerata = MEAN(kurs),
        Naif = NAIVE(kurs),
        `Naif musiman` = SNAIVE(kurs ~ lag("year")),
        Drift = RW(kurs ~ drift()))
print(fit)
fc <- fit |> forecast(h = "33 months")
print(fc)
fc |> autoplot(latih, level = NULL, linewidth = 0.8) +
  autolayer(kurs |> filter_index("2024 Jan" ~ .), kurs, colour = "black") +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Empat metode tolok ukur untuk kurs (hitam = data aktual 2024–2026)",
       x = NULL, y = "Rupiah per USD", colour = "Metode")
simpan("b06_benchmark", tinggi = 3.8)

# 6.2 Nilai cocok dan residual -----------------------------------------
fit_naif <- latih |> model(NAIVE(log(kurs)))
print(augment(fit_naif) |> tail())
gambar_resid <- fit_naif |> gg_tsresiduals()
gambar_resid
simpan("b06_resid_naif", gambar_resid, tinggi = 4.6)
print(augment(fit_naif) |> features(.innov, ljung_box, lag = 24))
fit_rerata <- latih |> model(MEAN(kurs))
print(augment(fit_rerata) |> features(.innov, ljung_box, lag = 24))

# 6.3 Interval prediksi ------------------------------------------------
fc_naif <- fit_naif |> forecast(h = 12)
print(fc_naif |> hilo(level = c(80, 95)))
fc_naif |> autoplot(latih |> filter_index("2019 Jan" ~ .)) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Forecast naif log-kurs 12 bulan dengan interval 80% dan 95%",
       x = NULL, y = "Rupiah per USD")
simpan("b06_interval")

# 6.4 Interval bootstrap -------------------------------------------------
set.seed(17)
sim <- fit_naif |> generate(h = 12, times = 5, bootstrap = TRUE)
print(sim |> head())
latih |> filter_index("2021 Jan" ~ .) |>
  ggplot(aes(x = bulan)) + geom_line(aes(y = kurs)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Lima jalur masa depan hasil bootstrap residual",
       x = NULL, y = "Rupiah per USD", colour = "Simulasi") +
  guides(colour = "none")
simpan("b06_bootstrap_jalur")
fc_boot <- fit_naif |> forecast(h = 12, bootstrap = TRUE, times = 2000)
print(fc_boot |> hilo(95) |> select(bulan, `95%`) |> head(3))
print(fc_naif |> hilo(95) |> select(bulan, `95%`) |> head(3))

# 6.5 Transformasi & bias -----------------------------------------------
fc_log <- latih |> model(RW(log(kurs) ~ drift())) |>
  forecast(h = 24, point_forecast = list(.median = median, .mean = mean))
print(fc_log |> as_tibble() |> select(bulan, .median, .mean) |> slice(c(1, 12, 24)))
