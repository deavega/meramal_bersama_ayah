source("kode/00_persiapan.R")

# 15.1 Celah data dan imputasi -----------------------------------------------
telur <- baca_pangan() |>
  filter(komoditas == "telur_ayam") |>
  summarise(harga = mean(harga), .by = tanggal) |>
  as_tsibble(index = tanggal)
print(count_gaps(telur) |> summarise(celah = n(), hari_hilang = sum(.n)))
telur_isi <- telur |> fill_gaps()
print(telur_isi |> filter(is.na(harga)) |> head(3))
telur_isi <- telur_isi |>
  model(ARIMA(log(harga))) |>
  interpolate(telur_isi)
print(telur_isi |> filter(tanggal %in% as.Date(c("2022-01-15", "2022-01-16"))))

# 15.2 Pencilan --------------------------------------------------------------
kurs <- baca_kurs_bulanan() |> filter_index("2005 Jan" ~ .)
dcmp <- kurs |> model(STL(log(kurs) ~ season(window = "periodic"), robust = TRUE)) |> components()
batas <- dcmp |> as_tibble() |> summarise(q1 = quantile(remainder, 0.25), q3 = quantile(remainder, 0.75)) |>
  mutate(bawah = q1 - 3 * (q3 - q1), atas = q3 + 3 * (q3 - q1))
pencilan <- dcmp |> filter(remainder < batas$bawah | remainder > batas$atas)
print(pencilan |> select(bulan, `log(kurs)`, remainder))
kurs |> autoplot(kurs) +
  geom_point(data = kurs |> filter(bulan %in% pencilan$bulan), colour = warna[["merah"]], size = 2) +
  scale_y_continuous(labels = rupiah) +
  labs(title = "Pencilan kurs terdeteksi dari residual STL (titik merah)", x = NULL, y = "Rupiah per USD")
simpan("b15_pencilan")

# 15.3 Forecast yang dibatasi: porsi listrik terbarukan --------------------------
listrik <- read_csv("data/listrik_sumber.csv", show_col_types = FALSE) |>
  summarise(total = sum(twh), terbarukan = sum(twh[kelompok == "terbarukan"]), .by = tahun) |>
  mutate(porsi = terbarukan / total * 100) |>
  as_tsibble(index = tahun)
print(tail(listrik))
logit_terskala <- function(x, bawah = 0, atas = 100) log((x - bawah) / (atas - x))
inv_logit_terskala <- function(x, bawah = 0, atas = 100) (atas - bawah) * exp(x) / (1 + exp(x)) + bawah
logit_porsi <- new_transformation(logit_terskala, inv_logit_terskala)
fit_porsi <- listrik |>
  model(`Tanpa batas` = ETS(porsi ~ trend("A")),
        `Logit 0–100` = ETS(logit_porsi(porsi) ~ trend("A")))
fc_porsi <- fit_porsi |> forecast(h = 16)
print(fc_porsi |> hilo(95) |> as_tibble() |> filter(tahun %in% c(2030, 2040)) |> select(.model, tahun, .mean, `95%`))
fc_porsi |> autoplot(listrik, level = 95, alpha = 0.4) +
  labs(title = "Porsi listrik terbarukan (%) dengan dan tanpa batas",
       x = NULL, y = "Persen dari produksi", colour = "Model", fill = "Model")
simpan("b15_porsi")

# 15.4 Forecast agregat: total 2026 dari model bulanan --------------------------------
cpo <- baca_komoditas() |> filter_index("2005 Jan" ~ "2025 Dec") |> select(bulan, cpo)
fit_cpo <- cpo |> model(ETS(log(cpo)))
set.seed(3)
jalur <- fit_cpo |> generate(h = 12, times = 2000)
rerata_2026 <- jalur |> as_tibble() |> summarise(rata = mean(.sim), .by = .rep)
print(quantile(rerata_2026$rata, c(0.1, 0.5, 0.9)))
ggplot(rerata_2026, aes(rata)) +
  geom_histogram(bins = 40, fill = warna[["biru"]], colour = "white") +
  labs(title = "Distribusi rata-rata harga CPO 2026 dari 2.000 simulasi",
       x = "USD per ton", y = "Jumlah simulasi")
simpan("b15_agregat", tinggi = 3)

# 15.5 Banyak deret sekaligus: 34 provinsi -----------------------------------------
cabai_prov <- baca_pangan() |>
  filter(komoditas == "cabai_rawit", tanggal < as.Date("2026-02-01")) |>
  mutate(bulan = yearmonth(tanggal)) |>
  summarise(harga = mean(harga), .by = c(bulan, provinsi)) |>
  as_tsibble(index = bulan, key = provinsi)
mulai <- Sys.time()
fit_prov <- cabai_prov |> model(ETS = ETS(log(harga)), SNaif = SNAIVE(harga))
print(Sys.time() - mulai)
print(fit_prov |> head(4))
fc_prov <- fit_prov |> forecast(h = 3)
print(fc_prov |> filter(.model == "ETS", bulan == yearmonth("2026 Mar")) |>
        as_tibble() |> arrange(desc(.mean)) |> select(provinsi, .mean) |> head(5))
print(fit_prov |> select(ETS) |> mutate(jenis = format(ETS)) |> as_tibble() |> count(jenis, sort = TRUE))

# 15.6 Deret sangat pendek: inflasi bulanan nasional 2024–2026 ----------------------------
mtm <- read_csv("data/inflasi_provinsi_mtm.csv", show_col_types = FALSE) |>
  filter(wilayah == "Indonesia") |>
  mutate(bulan = yearmonth(bulan)) |> as_tsibble(index = bulan)
print(nrow(mtm)); print(range(mtm$bulan))
report(mtm |> model(ETS(inflasi_mtm)))
mtm |> model(Rerata = MEAN(inflasi_mtm), `Naif musiman` = SNAIVE(inflasi_mtm)) |>
  forecast(h = 6) |> autoplot(mtm, level = NULL, linewidth = 0.8) +
  labs(title = "Inflasi bulanan (m-to-m) nasional: hanya 2 tahun data",
       x = NULL, y = "Persen", colour = "Metode")
simpan("b15_pendek")
