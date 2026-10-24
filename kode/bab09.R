source("kode/00_persiapan.R")
komoditas <- baca_komoditas() |> filter_index("2005 Jan" ~ "2025 Dec")

# 9.1 Regresi sederhana: log CPO terhadap log Brent -------------------------
fit_cpo <- komoditas |> model(TSLM(log(cpo) ~ log(brent)))
report(fit_cpo)
augment(fit_cpo) |>
  ggplot(aes(x = bulan)) +
  geom_line(aes(y = cpo, colour = "Aktual")) +
  geom_line(aes(y = .fitted, colour = "Nilai cocok")) +
  scale_colour_manual(values = c(Aktual = "grey50", `Nilai cocok` = warna[["oranye"]])) +
  labs(title = "Regresi log CPO terhadap log Brent", x = NULL, y = "USD per ton", colour = NULL)
simpan("b09_cpo_fit")
gambar_resid <- fit_cpo |> gg_tsresiduals()
gambar_resid
simpan("b09_cpo_resid", gambar_resid, tinggi = 4.4)
print(augment(fit_cpo) |> features(.innov, ljung_box, lag = 24))

# 9.2 Memilih prediktor ------------------------------------------------------
pilihan <- komoditas |>
  model(`Brent` = TSLM(log(cpo) ~ log(brent)),
        `Brent + batubara` = TSLM(log(cpo) ~ log(brent) + log(batubara)),
        `Brent + batubara + karet` = TSLM(log(cpo) ~ log(brent) + log(batubara) + log(karet)),
        `Brent + tren` = TSLM(log(cpo) ~ log(brent) + trend()),
        `Semua + tren` = TSLM(log(cpo) ~ log(brent) + log(batubara) + log(karet) + trend()))
print(glance(pilihan) |> select(.model, adj_r_squared, CV, AIC, AICc, BIC) |> arrange(AICc))

# 9.3 Forecast skenario ------------------------------------------------------
fit_pilih <- komoditas |> model(TSLM(log(cpo) ~ log(brent) + log(karet)))
report(fit_pilih)
skenario <- scenarios(
  `Brent 55` = new_data(komoditas, 12) |> mutate(brent = 55, karet = 1.75),
  `Brent 70` = new_data(komoditas, 12) |> mutate(brent = 70, karet = 1.75),
  `Brent 90` = new_data(komoditas, 12) |> mutate(brent = 90, karet = 1.75),
  names_to = "Skenario")
fc_sk <- forecast(fit_pilih, new_data = skenario)
print(fc_sk |> as_tibble() |> filter(bulan == yearmonth("2026 Jun")) |> select(Skenario, .mean))
komoditas |> filter_index("2021 Jan" ~ .) |> autoplot(cpo) +
  autolayer(fc_sk, level = 80) +
  labs(title = "Forecast skenario harga CPO 2026 untuk tiga asumsi Brent",
       x = NULL, y = "USD per ton", colour = "Skenario", fill = "Skenario")
simpan("b09_skenario", tinggi = 3.6)

# 9.4 Data harian: daging ayam, Fourier, dan dummy Lebaran -------------------
idulfitri <- as.Date(c("2022-05-02", "2023-04-22", "2024-04-10", "2025-03-31", "2026-03-20"))
tandai_lebaran <- function(tgl) {
  tibble(jelang = as.integer(sapply(tgl, \(t) any(t >= idulfitri - 14 & t <= idulfitri))),
         pasca  = as.integer(sapply(tgl, \(t) any(t >  idulfitri & t <= idulfitri + 7))))
}
pangan <- baca_pangan()
ayam <- pangan |>
  filter(komoditas == "daging_ayam") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal) |>
  fill_gaps() |> tidyr::fill(harga)
ayam <- bind_cols(ayam, tandai_lebaran(ayam$tanggal))
pilih_k <- ayam |>
  model(K1 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 1) + jelang + pasca),
        K2 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 2) + jelang + pasca),
        K3 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 3) + jelang + pasca),
        K4 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 4) + jelang + pasca),
        K6 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 6) + jelang + pasca),
        K8 = TSLM(log(harga) ~ trend() + fourier(period = "year", K = 8) + jelang + pasca))
print(glance(pilih_k) |> select(.model, adj_r_squared, AICc, CV) |> arrange(AICc))
fit_ayam <- ayam |> model(TSLM(log(harga) ~ trend() + fourier(period = "year", K = 6) + jelang + pasca))
print(tidy(fit_ayam) |> filter(term %in% c("jelang", "pasca", "trend()")))
masa_depan <- new_data(ayam, 80)
masa_depan <- bind_cols(masa_depan, tandai_lebaran(masa_depan$tanggal))
fc_ayam <- forecast(fit_ayam, new_data = masa_depan)
print(fc_ayam |> as_tibble() |> filter(tanggal %in% as.Date(c("2026-03-01", "2026-03-20", "2026-03-25", "2026-04-15"))) |> select(tanggal, .mean))
ayam |> filter(tanggal >= as.Date("2024-09-01")) |> autoplot(harga, linewidth = 0.4) +
  autolayer(fc_ayam, level = 80, colour = warna[["oranye"]]) +
  geom_vline(xintercept = idulfitri[4:5], linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Daging ayam menjelang Idulfitri 2026: regresi Fourier + dummy",
       x = NULL, y = "Rupiah per kg")
simpan("b09_ayam_fc")

# 9.5 Regresi palsu -----------------------------------------------------------
makro <- read_csv("data/makro_tahunan.csv", show_col_types = FALSE)
listrik <- read_csv("data/listrik_sumber.csv", show_col_types = FALSE) |>
  summarise(twh = sum(twh), .by = tahun)
palsu <- inner_join(listrik, makro, by = "tahun") |> as_tsibble(index = tahun)
fit_palsu <- palsu |> model(TSLM(twh ~ kurs_rata2))
report(fit_palsu)
print(augment(fit_palsu) |> features(.innov, ljung_box, lag = 6))
