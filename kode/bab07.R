source("kode/00_persiapan.R")
kurs <- baca_kurs_bulanan() |> filter_index("2010 Jan" ~ .)
latih <- kurs |> filter_index(~ "2023 Dec")

# 7.1 Akurasi pada data uji kurs ------------------------------------------
fit <- latih |>
  model(Rerata = MEAN(kurs), Naif = NAIVE(kurs),
        `Naif musiman` = SNAIVE(kurs ~ lag("year")), Drift = RW(kurs ~ drift()))
fc <- fit |> forecast(h = "33 months")
akurasi <- accuracy(fc, kurs) |> select(.model, RMSE, MAE, MAPE, MASE, RMSSE) |> arrange(RMSE)
print(akurasi)
print(accuracy(fc, kurs, list(skor_winkler = winkler_score, crps = CRPS), level = 95) |>
        select(.model, skor_winkler, crps))

# 7.2 Cabai: data uji 2025 ------------------------------------------------
cabai <- pangan_bulanan("cabai_rawit")
cabai_latih <- cabai |> filter_index(~ "2024 Dec")
fit_c <- cabai_latih |>
  model(Rerata = MEAN(harga), Naif = NAIVE(harga),
        `Naif musiman` = SNAIVE(harga), Drift = RW(harga ~ drift()))
fc_c <- fit_c |> forecast(h = 13)
print(accuracy(fc_c, cabai) |> select(.model, RMSE, MAE, MAPE, MASE) |> arrange(MASE))
fc_c |> filter(.model %in% c("Rerata", "Naif musiman")) |>
  autoplot(cabai, level = NULL, linewidth = 0.8) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Cabai rawit: forecast 2025 dari metode rerata dan naif musiman",
       x = NULL, y = "Rupiah per kg", colour = "Metode")
simpan("b07_cabai_uji")

# 7.3 Validasi silang deret waktu ------------------------------------------
kurs_cv <- kurs |> filter_index(~ "2025 Dec") |>
  stretch_tsibble(.init = 60, .step = 1)
print(kurs_cv)
print(n_distinct(kurs_cv$.id))
cv1 <- kurs_cv |>
  model(Rerata = MEAN(kurs), Naif = NAIVE(kurs), Drift = RW(kurs ~ drift())) |>
  forecast(h = 1)
print(accuracy(cv1, kurs) |> select(.model, RMSE, MAE, MAPE, MASE))

# Akurasi menurut horizon
cvh <- kurs_cv |>
  model(Naif = NAIVE(kurs), Drift = RW(kurs ~ drift())) |>
  forecast(h = 12) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |> ungroup() |>
  as_fable(response = "kurs", distribution = kurs)
akurasi_h <- cvh |> accuracy(kurs, by = c("h", ".model")) |> select(h, .model, RMSE, MAPE)
print(akurasi_h |> filter(h %in% c(1, 3, 6, 12)))
akurasi_h |> ggplot(aes(h, RMSE, colour = .model)) + geom_line(linewidth = 0.8) + geom_point() +
  scale_x_continuous(breaks = 1:12) + scale_y_continuous(labels = rupiah) +
  labs(title = "Galat forecast kurs naik seiring horizon (validasi silang)",
       x = "Horizon (bulan ke depan)", y = "RMSE (rupiah)", colour = "Metode")
simpan("b07_cv_horizon", tinggi = 3.3)
