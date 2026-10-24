source("kode/00_persiapan.R")
kurs <- baca_kurs_bulanan()
print(kurs)
kurs |> autoplot(kurs) +
  labs(title = "Kurs rupiah terhadap dolar AS, akhir bulan",
       x = NULL, y = "Rupiah per USD") +
  scale_y_continuous(labels = rupiah)
simpan("b01_kurs")

listrik <- read_csv(file.path(folder_data, "listrik_sumber.csv"), show_col_types = FALSE) |>
  summarise(twh = sum(twh), .by = tahun) |>
  as_tsibble(index = tahun)
print(tail(listrik))
listrik |> autoplot(twh) + geom_point(size = 1.2) +
  labs(title = "Produksi listrik Indonesia", x = NULL, y = "Terawatt-jam (TWh)")
simpan("b01_listrik")

pangan <- baca_pangan()
cabai_bulanan <- pangan_bulanan("cabai_rawit", pangan)
print(cabai_bulanan, n = 50)
cabai_bulanan |> autoplot(harga) + geom_point(size = 1) +
  labs(title = "Harga cabai rawit, rata-rata 34 provinsi",
       x = NULL, y = "Rupiah per kg") +
  scale_y_continuous(labels = rupiah)
simpan("b01_cabai")
