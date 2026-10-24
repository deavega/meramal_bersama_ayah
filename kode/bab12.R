source("kode/00_persiapan.R")
komoditas <- baca_komoditas() |> filter_index("2005 Jan" ~ "2025 Dec")

# 12.1 Regresi dengan galat ARIMA -------------------------------------------------
fit_dr <- komoditas |> model(ARIMA(log(cpo) ~ log(brent) + log(karet)))
report(fit_dr)
bind_rows(
  `Galat regresi (ηt)` = as_tibble(residuals(fit_dr, type = "regression")),
  `Galat ARIMA (εt)` = as_tibble(residuals(fit_dr, type = "innovation")),
  .id = "jenis") |>
  ggplot(aes(bulan, .resid)) + geom_line(colour = warna[["biru"]]) +
  facet_grid(jenis ~ ., scales = "free_y") +
  labs(title = "Dua jenis residual pada regresi dinamis CPO", x = NULL, y = NULL)
simpan("b12_dua_residual", tinggi = 3.8)
print(augment(fit_dr) |> features(.innov, ljung_box, lag = 24, dof = 3))

# 12.2 Uji 2024–2025: TSLM vs ARIMA saja vs regresi dinamis --------------------------
latih <- komoditas |> filter_index(~ "2023 Dec")
uji <- komoditas |> filter_index("2024 Jan" ~ .)
fit3 <- latih |>
  model(`Regresi biasa` = TSLM(log(cpo) ~ log(brent) + log(karet)),
        `ARIMA saja` = ARIMA(log(cpo)),
        `Regresi dinamis` = ARIMA(log(cpo) ~ log(brent) + log(karet)))
fc3 <- forecast(fit3, new_data = uji)
print(accuracy(fc3, komoditas) |> select(.model, RMSE, MAE, MAPE) |> arrange(RMSE))
fc3 |> autoplot(komoditas |> filter_index("2021 Jan" ~ .), level = NULL, linewidth = 0.8) +
  labs(title = "CPO 2024–2025: forecast ex-post (Brent & karet aktual diketahui)",
       x = NULL, y = "USD per ton", colour = "Model")
simpan("b12_uji")

# 12.3 Prediktor tertunda (lag) ----------------------------------------------------------
lag_fit <- komoditas |>
  mutate(brent1 = lag(brent), brent2 = lag(brent, 2), brent3 = lag(brent, 3)) |>
  filter_index("2005 Apr" ~ .) |>
  model(`lag 0` = ARIMA(log(cpo) ~ log(brent)),
        `lag 0–1` = ARIMA(log(cpo) ~ log(brent) + log(brent1)),
        `lag 0–2` = ARIMA(log(cpo) ~ log(brent) + log(brent1) + log(brent2)),
        `lag 0–3` = ARIMA(log(cpo) ~ log(brent) + log(brent1) + log(brent2) + log(brent3)))
print(glance(lag_fit) |> select(.model, AICc) |> arrange(AICc))

# 12.4 Regresi harmonik dinamis untuk data harian ------------------------------------------
idulfitri <- as.Date(c("2022-05-02", "2023-04-22", "2024-04-10", "2025-03-31", "2026-03-20"))
tandai_lebaran <- function(tgl) {
  tibble(jelang = as.integer(sapply(tgl, \(t) any(t >= idulfitri - 14 & t <= idulfitri))),
         pasca  = as.integer(sapply(tgl, \(t) any(t >  idulfitri & t <= idulfitri + 7))))
}
ayam <- baca_pangan() |>
  filter(komoditas == "daging_ayam") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal) |> fill_gaps() |> tidyr::fill(harga)
ayam <- bind_cols(ayam, tandai_lebaran(ayam$tanggal))
fit_dhr <- ayam |>
  model(DHR = ARIMA(log(harga) ~ trend() + fourier(period = "year", K = 6) + jelang + pasca + PDQ(0, 0, 0)))
report(fit_dhr)
print(tidy(fit_dhr) |> filter(term %in% c("jelang", "pasca")))
gambar_resid <- fit_dhr |> gg_tsresiduals()
gambar_resid
simpan("b12_dhr_resid", gambar_resid, tinggi = 4.2)
masa_depan <- new_data(ayam, 80)
masa_depan <- bind_cols(masa_depan, tandai_lebaran(masa_depan$tanggal))
fc_dhr <- forecast(fit_dhr, new_data = masa_depan)
print(fc_dhr |> hilo(80) |> as_tibble() |> filter(tanggal %in% as.Date(c("2026-03-01", "2026-03-20", "2026-03-25", "2026-04-15"))) |> select(tanggal, .mean, `80%`))
ayam |> filter(tanggal >= as.Date("2024-09-01")) |> autoplot(harga, linewidth = 0.4) +
  autolayer(fc_dhr, level = c(80, 95)) +
  geom_vline(xintercept = idulfitri[4:5], linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Regresi harmonik dinamis: daging ayam menjelang Idulfitri 2026",
       x = NULL, y = "Rupiah per kg")
simpan("b12_dhr_fc")

# 12.5 Skenario dengan regresi dinamis: asumsi ICP APBN 2027 (US$75) ----------------
skenario_dr <- scenarios(
  `Brent 60` = new_data(komoditas, 12) |> mutate(brent = 60, karet = 1.75),
  `Brent 75 (setara asumsi APBN)` = new_data(komoditas, 12) |> mutate(brent = 75, karet = 1.75),
  `Brent 90` = new_data(komoditas, 12) |> mutate(brent = 90, karet = 1.75),
  names_to = "Skenario")
fc_dr <- forecast(fit_dr, new_data = skenario_dr)
print(fc_dr |> hilo(80) |> as_tibble() |> filter(bulan == yearmonth("2026 Dec")) |> select(Skenario, .mean, `80%`))
komoditas |> filter_index("2022 Jan" ~ .) |> autoplot(cpo) +
  autolayer(fc_dr, level = NULL, linewidth = 0.8) +
  labs(title = "Skenario CPO 2026 dengan regresi dinamis", x = NULL, y = "USD per ton", colour = "Skenario")
simpan("b12_skenario")
