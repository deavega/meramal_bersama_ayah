source("kode/00_persiapan.R")

# 10.1 Simple exponential smoothing -------------------------------------------
cabai <- pangan_bulanan("cabai_rawit")
fit_ses <- cabai |> model(ETS(harga ~ error("A") + trend("N") + season("N")))
report(fit_ses)
fc_ses <- fit_ses |> forecast(h = 6)
fc_ses |> autoplot(cabai) +
  geom_line(aes(y = .fitted), data = augment(fit_ses), colour = warna[["oranye"]]) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "SES pada harga cabai rawit (oranye = nilai cocok)", x = NULL, y = "Rupiah per kg")
simpan("b10_ses")
# efek alpha
alpha_uji <- purrr::map_dfr(c(0.1, 0.5, 0.9), function(a) {
  cabai |>
    model(ETS(harga ~ error("A") + trend("N", alpha = a) + season("N"))) |>
    augment() |> mutate(.model = paste("α =", format(a, decimal.mark = ",")))
})
alpha_uji |> ggplot(aes(bulan)) +
  geom_line(aes(y = harga), colour = "grey55") +
  geom_line(aes(y = .fitted, colour = .model), linewidth = 0.7) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Pengaruh α terhadap nilai cocok SES", x = NULL, y = "Rupiah per kg", colour = NULL)
simpan("b10_alpha")

# 10.2 Tren: Holt dan damped pada penduduk -------------------------------------
penduduk <- read_csv("data/makro_tahunan.csv", show_col_types = FALSE) |>
  select(tahun, penduduk) |> mutate(juta = penduduk / 1e6) |>
  as_tsibble(index = tahun)
fit_pend <- penduduk |>
  model(SES = ETS(juta ~ error("A") + trend("N") + season("N")),
        Holt = ETS(juta ~ error("A") + trend("A") + season("N")),
        `Holt teredam` = ETS(juta ~ error("A") + trend("Ad", phi = 0.9) + season("N")))
fc_pend <- fit_pend |> forecast(h = 15)
print(fc_pend |> as_tibble() |> filter(tahun %in% c(2030, 2040)) |> select(.model, tahun, .mean))
fc_pend |> autoplot(penduduk |> filter(tahun >= 1990), level = NULL, linewidth = 0.8) +
  labs(title = "Proyeksi penduduk Indonesia: SES, Holt, Holt teredam",
       x = NULL, y = "Juta jiwa", colour = "Metode")
simpan("b10_penduduk")
cv_pend <- penduduk |> stretch_tsibble(.init = 30) |>
  model(SES = ETS(juta ~ error("A") + trend("N") + season("N")),
        Holt = ETS(juta ~ error("A") + trend("A") + season("N")),
        `Holt teredam` = ETS(juta ~ error("A") + trend("Ad") + season("N"))) |>
  forecast(h = 5)
print(accuracy(cv_pend, penduduk) |> select(.model, RMSE, MAE, MAPE))
fit_damp <- penduduk |> model(ETS(juta ~ error("A") + trend("Ad") + season("N")))
report(fit_damp)

# 10.3 Holt-Winters pada cabai ---------------------------------------------------
cabai_latih <- cabai |> filter_index(~ "2024 Dec")
fit_hw <- cabai_latih |>
  model(`HW aditif` = ETS(harga ~ error("A") + trend("N") + season("A")),
        `HW multiplikatif` = ETS(harga ~ error("M") + trend("N") + season("M")),
        `SES` = ETS(harga ~ error("A") + trend("N") + season("N")),
        `Naif musiman` = SNAIVE(harga))
fc_hw <- fit_hw |> forecast(h = 13)
print(accuracy(fc_hw, cabai) |> select(.model, RMSE, MAE, MAPE, MASE) |> arrange(MASE))
fc_hw |> filter(.model %in% c("HW multiplikatif", "SES")) |>
  autoplot(cabai, level = NULL, linewidth = 0.8) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Holt-Winters multiplikatif vs SES untuk cabai rawit 2025",
       x = NULL, y = "Rupiah per kg", colour = "Model")
simpan("b10_hw")

# 10.4 ETS otomatis ----------------------------------------------------------------
kurs <- baca_kurs_bulanan() |> filter_index("2010 Jan" ~ .)
fit_auto <- kurs |> model(ETS(log(kurs)))
report(fit_auto)
components(fit_auto) |> autoplot() + labs(title = "Komponen ETS untuk log kurs")
simpan("b10_ets_komponen", tinggi = 4.2)
fit_auto |> forecast(h = 24) |> autoplot(kurs |> filter_index("2020 Jan" ~ .)) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Forecast ETS otomatis kurs 24 bulan", x = NULL, y = "Rupiah per USD")
simpan("b10_ets_kurs")
print(fit_auto |> forecast(h = 24) |> hilo(80) |> as_tibble() |> slice(c(3, 15)) |> select(bulan, .mean, `80%`))
cpo <- baca_komoditas() |> filter_index("2005 Jan" ~ .)
report(cpo |> model(ETS(cpo)))
