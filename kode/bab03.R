source("kode/00_persiapan.R")

# 3.1 Membuat tsibble dari CSV -----------------------------------------
kurs_mentah <- read_csv("data/kurs_bulanan.csv", show_col_types = FALSE)
print(kurs_mentah)
kurs <- kurs_mentah |>
  mutate(bulan = yearmonth(bulan)) |>
  as_tsibble(index = bulan)
print(kurs)

# 3.2 Plot waktu ---------------------------------------------------------
komoditas <- baca_komoditas()
komoditas |> autoplot(cpo) +
  labs(title = "Harga minyak sawit mentah (CPO), Bank Dunia",
       x = NULL, y = "USD per metrik ton")
simpan("b03_cpo")

# 3.3 Pola musiman -------------------------------------------------------
cabai <- pangan_bulanan("cabai_rawit")
cabai |> gg_season(harga, linewidth = 0.8) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Plot musiman harga cabai rawit", x = NULL, y = "Rupiah per kg")
simpan("b03_cabai_season")
cabai |> gg_subseries(harga) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Plot sub-deret harga cabai rawit", x = NULL, y = "Rupiah per kg")
simpan("b03_cabai_subseries", tinggi = 3.3)

# Data harian
pangan <- baca_pangan()
cabai_harian <- pangan |>
  filter(komoditas == "cabai_rawit") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal)
print(cabai_harian)
print(has_gaps(cabai_harian)); print(count_gaps(cabai_harian))
lebaran <- as.Date(c("2022-05-02", "2023-04-22", "2024-04-10", "2025-03-31"))
cabai_harian |> autoplot(harga, linewidth = 0.35) +
  geom_vline(xintercept = lebaran, linetype = "dashed", colour = warna[["oranye"]]) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Harga harian cabai rawit (garis putus-putus = Idulfitri)",
       x = NULL, y = "Rupiah per kg")
simpan("b03_cabai_harian")

# 3.4 Hubungan antar-deret ----------------------------------------------
komoditas |>
  filter_index("2005 Jan" ~ .) |>
  ggplot(aes(x = brent, y = cpo)) +
  geom_point(alpha = 0.6, colour = warna[["biru"]]) +
  labs(title = "Harga CPO vs minyak Brent, 2005–2026",
       x = "Brent (USD per barel)", y = "CPO (USD per ton)")
simpan("b03_cpo_brent", lebar = 5.2, tinggi = 3.6)
print(komoditas |> filter_index("2005 Jan" ~ .) |> as_tibble() |>
        summarise(r = cor(cpo, brent)))

# 3.5 Plot lag & ACF -----------------------------------------------------
cabai |> mutate(ribu = harga / 1000) |>
  gg_lag(ribu, geom = "point", lags = c(1, 2, 6, 12)) +
  labs(title = "Plot lag harga cabai rawit (ribu rupiah/kg)",
       x = "lag(harga, k)", y = "harga", colour = "Bulan") +
  theme(legend.position = "right")
simpan("b03_cabai_lag", tinggi = 4.4)
print(cabai |> ACF(harga, lag_max = 18))
cabai |> ACF(harga, lag_max = 24) |> autoplot() +
  labs(title = "ACF harga cabai rawit bulanan")
simpan("b03_cabai_acf", tinggi = 3)
kurs |> filter_index("2010 Jan" ~ .) |> ACF(kurs, lag_max = 36) |> autoplot() +
  labs(title = "ACF kurs rupiah 2010–2026 (data bertren)")
simpan("b03_kurs_acf", tinggi = 3)

# 3.6 White noise --------------------------------------------------------
set.seed(2026)
derau <- tsibble(waktu = 1:60, y = rnorm(60), index = waktu)
derau |> autoplot(y) + labs(title = "Derau putih (white noise) buatan", x = "waktu", y = NULL)
simpan("b03_wn", tinggi = 2.6)
derau |> ACF(y) |> autoplot() + labs(title = "ACF derau putih")
simpan("b03_wn_acf", tinggi = 2.6)
perubahan <- kurs |> filter_index("2010 Jan" ~ .) |>
  mutate(ubah = difference(log(kurs)) * 100)
perubahan |> ACF(ubah, lag_max = 24) |> autoplot() +
  labs(title = "ACF perubahan kurs bulanan (%), 2010–2026")
simpan("b03_kurs_ubah_acf", tinggi = 3)
