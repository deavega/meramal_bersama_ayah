source("kode/00_persiapan.R")
listrik <- read_csv("data/listrik_sumber.csv", show_col_types = FALSE) |>
  as_tsibble(index = tahun, key = c(kelompok, sumber))
print(listrik)

# 13.1 Membangun hierarki -----------------------------------------------------
listrik_h <- listrik |>
  aggregate_key(kelompok / sumber, twh = sum(twh))
print(listrik_h)
print(listrik_h |> filter(tahun == 2024) |> arrange(kelompok, sumber), n = 20)
listrik_h |> filter(is_aggregated(sumber)) |>
  mutate(Tingkat = if_else(is_aggregated(kelompok), "Total nasional", as.character(kelompok))) |>
  ggplot(aes(tahun, twh, colour = Tingkat)) + geom_line(linewidth = 0.8) +
  labs(title = "Produksi listrik: total, fosil, dan terbarukan", x = NULL, y = "TWh", colour = NULL)
simpan("b13_tingkat_atas")
listrik |> autoplot(twh) + facet_wrap(~ sumber, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = c(2005, 2015, 2025)) +
  labs(title = "Delapan deret paling bawah (per sumber energi)", x = NULL, y = "TWh") +
  theme(legend.position = "none")
simpan("b13_bawah", tinggi = 3.8)

# 13.2 Bottom-up, top-down, MinT ------------------------------------------------
latih <- listrik_h |> filter(tahun <= 2019)
fit <- latih |>
  model(base = ETS(twh)) |>
  reconcile(`Bottom-up` = bottom_up(base),
            `Top-down` = top_down(base),
            `MinT` = min_trace(base, method = "mint_shrink"))
fc <- fit |> forecast(h = 5)
print(fc |> filter(is_aggregated(kelompok), tahun == 2024) |> as_tibble() |> select(.model, .mean))
print(listrik_h |> filter(is_aggregated(kelompok), tahun == 2024))
label_tingkat <- function(d) d |> filter(is_aggregated(sumber)) |>
  as_tibble() |>
  mutate(Tingkat = if_else(is_aggregated(kelompok), "Total", as.character(kelompok)))
ggplot() +
  geom_line(aes(tahun, twh), data = label_tingkat(listrik_h) |> filter(tahun >= 2008)) +
  geom_line(aes(tahun, .mean, colour = .model), data = label_tingkat(fc), linewidth = 0.7) +
  facet_wrap(vars(Tingkat), scales = "free_y") +
  labs(title = "Forecast 2020–2024 dari data s.d. 2019 (hitam = aktual)",
       x = NULL, y = "TWh", colour = "Metode")
simpan("b13_fc", tinggi = 3.6)
akurasi <- fc |> accuracy(listrik_h)
tabel <- akurasi |>
  mutate(Tingkat = case_when(is_aggregated(kelompok) ~ "Total",
                             is_aggregated(sumber) ~ "Kelompok",
                             TRUE ~ "Sumber")) |>
  summarise(RMSE = mean(RMSE), MASE = mean(MASE), .by = c(Tingkat, .model)) |>
  arrange(Tingkat, MASE)
print(tabel, n = 20)
