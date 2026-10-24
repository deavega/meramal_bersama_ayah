source("kode/00_persiapan.R")
set.seed(2027)
kurs <- baca_kurs_bulanan() |> filter_index("2010 Jan" ~ .) |> select(bulan, kurs)
print(tail(kurs, 3))
asumsi_apbn <- 17500

# Langkah 1: evaluasi kandidat untuk horizon 4–15 bulan (rata-rata setahun) ------------
# Asal forecast: setiap September 2014..2024, target: rata-rata Jan–Des tahun berikutnya
asal <- yearmonth(paste(2014:2024, "Sep"))
hasil_cv <- purrr::map_dfr(asal, function(a) {
  latih <- kurs |> filter(bulan <= a)
  fit <- latih |>
    model(Naif = NAIVE(log(kurs)), Drift = RW(log(kurs) ~ drift()),
          ETS = ETS(log(kurs)), ARIMA = ARIMA(log(kurs))) |>
    mutate(Kombinasi = (Naif + Drift + ETS + ARIMA) / 4)
  fc <- fit |> forecast(h = 15)
  tahun_target <- year(a) + 1
  fc |> as_tibble() |> filter(year(bulan) == tahun_target) |>
    summarise(prediksi = mean(.mean), .by = .model) |>
    mutate(tahun = tahun_target)
})
aktual <- kurs |> as_tibble() |> mutate(tahun = year(bulan)) |>
  summarise(aktual = mean(kurs), .by = tahun)
eval <- hasil_cv |> left_join(aktual, by = "tahun") |>
  mutate(galat = prediksi - aktual, galat_persen = 100 * galat / aktual)
print(eval |> summarise(MAE = mean(abs(galat)), MAPE = mean(abs(galat_persen)),
                        bias = mean(galat_persen), .by = .model) |> arrange(MAE))
eval |> ggplot(aes(tahun, galat_persen, colour = .model)) +
  geom_hline(yintercept = 0, colour = "grey50") + geom_line(linewidth = 0.7) + geom_point() +
  scale_x_continuous(breaks = 2015:2025) +
  labs(title = "Galat forecast rata-rata kurs tahunan, dibuat tiap September",
       x = "Tahun yang diramal", y = "Galat (%)", colour = "Model")
simpan("b16_cv", tinggi = 3.5)

# Langkah 2: model akhir & simulasi 2027 ----------------------------------------
fit_akhir <- kurs |>
  model(Naif = NAIVE(log(kurs)), Drift = RW(log(kurs) ~ drift()),
        ETS = ETS(log(kurs)), ARIMA = ARIMA(log(kurs)))
print(fit_akhir)
sim <- fit_akhir |> generate(h = 15, times = 1000, bootstrap = TRUE)
rata_2027 <- sim |> as_tibble() |> filter(year(bulan) == 2027) |>
  summarise(rata = mean(.sim), .by = c(.model, .rep))
ringkas <- rata_2027 |>
  summarise(median = median(rata), p10 = quantile(rata, 0.1), p90 = quantile(rata, 0.9),
            peluang_di_atas_asumsi = mean(rata > asumsi_apbn), .by = .model)
semua <- rata_2027 |> summarise(median = median(rata), p10 = quantile(rata, 0.1),
                                p90 = quantile(rata, 0.9),
                                peluang_di_atas_asumsi = mean(rata > asumsi_apbn)) |>
  mutate(.model = "Gabungan (semua simulasi)")
print(bind_rows(ringkas, semua))
rata_2027 |> ggplot(aes(rata, fill = .model)) +
  geom_density(alpha = 0.35, colour = NA) +
  geom_vline(xintercept = asumsi_apbn, linetype = "dashed") +
  annotate("text", x = asumsi_apbn, y = 0, label = " Asumsi APBN 2027", hjust = 0, vjust = -0.6, size = 3) +
  scale_x_continuous(labels = rupiah) +
  labs(title = "Sebaran rata-rata kurs 2027 dari 4.000 jalur simulasi",
       x = "Rupiah per USD", y = "Kepadatan", fill = "Model")
simpan("b16_sebaran", tinggi = 3.6)

jalur <- sim |> as_tibble() |>
  summarise(median = median(.sim), p10 = quantile(.sim, 0.1), p90 = quantile(.sim, 0.9),
            p025 = quantile(.sim, 0.025), p975 = quantile(.sim, 0.975), .by = bulan)
print(jalur |> filter(bulan %in% yearmonth(c("2026 Dec", "2027 Jun", "2027 Dec"))))
ggplot(jalur, aes(x = bulan)) +
  geom_ribbon(aes(ymin = p025, ymax = p975), fill = warna[["biru"]], alpha = 0.18) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = warna[["biru"]], alpha = 0.3) +
  geom_line(aes(y = median), colour = warna[["biru"]], linewidth = 0.8) +
  geom_line(aes(y = kurs), data = kurs |> filter_index("2022 Jan" ~ .)) +
  geom_hline(yintercept = asumsi_apbn, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Kipas forecast kurs Okt 2026–Des 2027, gabungan empat model",
       subtitle = "Pita: 80% dan 95%. Garis putus-putus: asumsi APBN 2027 (Rp17.500)",
       x = NULL, y = "Rupiah per USD")
simpan("b16_jalur")

# Langkah 3: analisis sensitivitas (dipakai di Bab 16) -----------------------------
kurs_harian <- read_csv("data/kurs_harian.csv", show_col_types = FALSE)
print(kurs_harian |> filter(tanggal >= as.Date("2026-01-01")) |>
        summarise(rata_2026_ytd = mean(kurs), terakhir = last(kurs), tanggal_terakhir = max(tanggal)))
