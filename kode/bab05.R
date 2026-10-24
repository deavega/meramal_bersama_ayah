source("kode/00_persiapan.R")
pangan <- baca_pangan()

# Semua komoditas x provinsi, bulanan --------------------------------------
pangan_bln <- pangan |>
  filter(tanggal < as.Date("2026-02-01")) |>
  mutate(bulan = yearmonth(tanggal)) |>
  summarise(harga = mean(harga), .by = c(bulan, komoditas, provinsi)) |>
  as_tsibble(index = bulan, key = c(komoditas, provinsi))
print(pangan_bln)

# 5.1 Statistik sederhana
ringkas <- pangan_bln |>
  filter(komoditas == "cabai_rawit") |>
  features(harga, list(rerata = mean, sd = sd)) |>
  mutate(cv = sd / rerata) |> arrange(desc(cv))
print(ringkas, n = 34)

# 5.2 Fitur ACF dan STL
fitur <- pangan_bln |> features(harga, feat_stl)
print(fitur |> select(komoditas, provinsi, trend_strength, seasonal_strength_year) |> head())
nasional <- pangan_bln |>
  as_tibble() |> summarise(harga = mean(harga), .by = c(bulan, komoditas)) |>
  as_tsibble(index = bulan, key = komoditas)
fitur_nas <- nasional |> features(harga, feat_stl)
print(fitur_nas |> select(komoditas, trend_strength, seasonal_strength_year, seasonal_peak_year, seasonal_trough_year))
print(nasional |> features(harga, feat_acf))

fitur |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, colour = komoditas)) +
  geom_point(size = 1.6, alpha = 0.8) +
  scale_colour_manual(values = c(unname(warna), "#7a7974", "#2c2c2a")) +
  labs(title = "Kekuatan tren vs musiman: 10 komoditas × 34 provinsi",
       x = "Kekuatan tren (Ft)", y = "Kekuatan musiman (Fs)", colour = NULL) +
  guides(colour = guide_legend(nrow = 2))
simpan("b05_fitur_scatter", tinggi = 4.4)

# Provinsi paling musiman untuk cabai rawit
paling <- fitur |> filter(komoditas == "cabai_rawit") |>
  arrange(desc(seasonal_strength_year))
print(paling |> select(provinsi, trend_strength, seasonal_strength_year, seasonal_peak_year), n = 34)
pangan_bln |>
  filter(komoditas == "cabai_rawit",
         provinsi %in% c(head(paling$provinsi, 2), tail(paling$provinsi, 2))) |>
  autoplot(harga) + facet_wrap(~provinsi, scales = "free_y") +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Cabai rawit: dua provinsi paling musiman vs dua paling tidak musiman",
       x = NULL, y = "Rupiah per kg") + theme(legend.position = "none")
simpan("b05_cabai_ekstrem", tinggi = 4)

# 5.3 Banyak fitur sekaligus + PCA
semua <- pangan_bln |>
  features(harga, feature_set(pkgs = "feasts"))
print(dim(semua))
pcs <- semua |> select(-komoditas, -provinsi) |>
  select(where(~ all(is.finite(.x)) && sd(.x) > 0)) |>
  prcomp(scale. = TRUE) |>
  broom::augment(semua)
pcs |>
  ggplot(aes(.fittedPC1, .fittedPC2, colour = komoditas)) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_colour_manual(values = c(unname(warna), "#7a7974", "#2c2c2a")) +
  labs(title = "Peta 340 deret harga pangan berdasarkan puluhan fitur (PCA)",
       x = "PC1", y = "PC2", colour = NULL) +
  guides(colour = guide_legend(nrow = 2))
simpan("b05_pca", tinggi = 4.4)
outlier <- pcs |> arrange(desc(abs(.fittedPC1))) |> select(komoditas, provinsi, .fittedPC1, .fittedPC2) |> head(5)
print(outlier)
