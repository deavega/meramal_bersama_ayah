source("kode/00_persiapan.R")
komoditas <- baca_komoditas()
kurs <- baca_kurs_bulanan()

# 4.1 Penyesuaian per kapita ----------------------------------------------
makro <- read_csv("data/makro_tahunan.csv", show_col_types = FALSE) |>
  as_tsibble(index = tahun)
makro |> filter(tahun >= 1967) |>
  mutate(pdb_triliun = pdb_usd / 1e9) |>
  select(tahun, `PDB (miliar USD)` = pdb_triliun, `PDB per kapita (USD)` = pdb_per_kapita_usd) |>
  pivot_longer(-tahun) |>
  ggplot(aes(tahun, value)) + geom_line(colour = warna[["biru"]]) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(title = "PDB dan PDB per kapita Indonesia (harga berlaku)", x = NULL, y = NULL)
simpan("b04_pdb_perkapita", tinggi = 4.2)

# 4.2 Transformasi matematis ---------------------------------------------
cpo <- komoditas |> select(bulan, cpo) |> filter_index("2000 Jan" ~ .)
lambda <- cpo |> features(cpo, features = guerrero) |> pull(lambda_guerrero)
print(lambda)
cpo |>
  mutate(`Asli (USD/ton)` = cpo, `Logaritma` = log(cpo),
         `Box-Cox (λ terpilih)` = box_cox(cpo, lambda)) |>
  select(-cpo) |>
  pivot_longer(-bulan) |>
  mutate(name = factor(name, levels = c("Asli (USD/ton)", "Logaritma", "Box-Cox (λ terpilih)"))) |>
  ggplot(aes(bulan, value)) + geom_line(colour = warna[["biru"]], linewidth = 0.45) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(title = "Harga CPO sebelum dan sesudah transformasi", x = NULL, y = NULL)
simpan("b04_cpo_transform", tinggi = 4.6)

# 4.3 Rata-rata bergerak ---------------------------------------------------
kurs10 <- kurs |> filter_index("2010 Jan" ~ .) |>
  mutate(ma12 = slider::slide_dbl(kurs, mean, .before = 5, .after = 6, .complete = TRUE),
         ma2x12 = slider::slide_dbl(ma12, mean, .before = 1, .after = 0, .complete = TRUE))
print(kurs10 |> select(bulan, kurs, ma12, ma2x12) |> slice(5:10))
kurs10 |> autoplot(kurs, colour = "grey60") +
  geom_line(aes(y = ma2x12), colour = warna[["oranye"]], linewidth = 0.9) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Kurs rupiah dan rata-rata bergerak 2×12", x = NULL, y = "Rupiah per USD")
simpan("b04_kurs_ma")

# 4.4 Dekomposisi klasik dan STL -------------------------------------------
cabai <- pangan_bulanan("cabai_rawit")
cabai |> model(classical_decomposition(harga, type = "additive")) |>
  components() |> autoplot() + labs(title = "Dekomposisi klasik aditif: cabai rawit")
simpan("b04_cabai_klasik", tinggi = 5)

dcmp <- cabai |> model(stl = STL(harga ~ trend(window = 13) + season(window = "periodic"), robust = TRUE))
print(components(dcmp))
components(dcmp) |> autoplot() + labs(title = "Dekomposisi STL: cabai rawit")
simpan("b04_cabai_stl", tinggi = 5)
cabai |> autoplot(harga, colour = "grey60") +
  autolayer(components(dcmp), season_adjust, colour = warna[["biru"]]) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Harga cabai rawit: asli (abu-abu) dan setelah penyesuaian musiman (biru)",
       x = NULL, y = "Rupiah per kg")
simpan("b04_cabai_sa")

# STL harian dengan dua musim (mingguan & tahunan)
pangan <- baca_pangan()
telur_harian <- pangan |>
  filter(komoditas == "telur_ayam") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal) |>
  fill_gaps() |>
  tidyr::fill(harga)
telur_harian |>
  model(STL(harga ~ season(period = "week") + season(period = "year"), robust = TRUE)) |>
  components() |> autoplot() + labs(title = "STL harian harga telur ayam: musim mingguan & tahunan")
simpan("b04_telur_stl", tinggi = 5.4)
