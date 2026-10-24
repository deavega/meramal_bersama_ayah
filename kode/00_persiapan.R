# =====================================================================
# 00_persiapan.R — dijalankan di awal setiap bab
# Buku: "Meramal Bersama Ayah" — forecasting time series dengan R & Positron
# =====================================================================

if (!interactive()) options(device = function(...) ragg::agg_png(tempfile(fileext = ".png")))

suppressWarnings(suppressPackageStartupMessages({
  library(fpp3)      # memuat tsibble, fable, feasts, fabletools, dplyr, ggplot2, lubridate
  library(readr)
}))

# Folder data: ubah jika Anda menyimpan data di tempat lain
folder_data <- if (dir.exists("data")) "data" else "../data"

# --- Tema grafik buku (opsional, hanya agar grafik seragam) ---------
warna <- c(biru = "#2a78d6", oranye = "#eb6834", hijau_laut = "#1baf7a",
           kuning = "#eda100", magenta = "#e87ba4", hijau = "#008300",
           ungu = "#6250d6", merah = "#e34948")
options(ggplot2.discrete.colour = unname(warna),
        ggplot2.discrete.fill   = unname(warna))
theme_set(
  theme_minimal(base_size = 11, base_family = "Plus Jakarta Sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e5e0", linewidth = 0.3),
          axis.title = element_text(colour = "#52514e", size = 9.5),
          axis.text  = element_text(colour = "#52514e", size = 8.5),
          plot.title = element_text(face = "bold", size = 11.5),
          plot.subtitle = element_text(colour = "#52514e", size = 9),
          legend.position = "bottom",
          legend.title = element_text(size = 9),
          strip.text = element_text(face = "bold", size = 9))
)
update_geom_defaults("line", list(colour = "#2c2c2a", linewidth = 0.55))
update_geom_defaults("point", list(colour = "#2c2c2a"))

# simpan() menyimpan grafik terakhir ke folder gambar (dipakai penulis buku)
simpan <- function(nama, plot = last_plot(), lebar = 6.6, tinggi = 3.5) {
  dir.create("gambar", showWarnings = FALSE)
  ggsave(file.path("gambar", paste0(nama, ".png")), plot,
         width = lebar, height = tinggi, dpi = 220, bg = "white",
         device = ragg::agg_png)
  invisible(plot)
}

# --- Fungsi pemuat data (dipakai berulang di semua bab) ---------------
baca_kurs_bulanan <- function() {
  read_csv(file.path(folder_data, "kurs_bulanan.csv"), show_col_types = FALSE) |>
    mutate(bulan = yearmonth(bulan)) |>
    as_tsibble(index = bulan)
}
baca_komoditas <- function() {
  read_csv(file.path(folder_data, "komoditas_bulanan.csv"), show_col_types = FALSE) |>
    mutate(bulan = yearmonth(bulan)) |>
    as_tsibble(index = bulan)
}
baca_inflasi <- function() {
  read_csv(file.path(folder_data, "inflasi_bulanan.csv"), show_col_types = FALSE) |>
    mutate(bulan = yearmonth(bulan)) |>
    as_tsibble(index = bulan)
}
baca_pangan <- function() {
  read_csv(file.path(folder_data, "pangan_harian.csv"), show_col_types = FALSE)
}

# Harga bulanan satu komoditas pangan, rata-rata 34 provinsi.
# Februari 2026 belum lengkap (data berakhir 12 Feb 2026), jadi dibuang.
pangan_bulanan <- function(nama_komoditas, data = baca_pangan()) {
  data |>
    filter(komoditas == nama_komoditas, tanggal < as.Date("2026-02-01")) |>
    mutate(bulan = yearmonth(tanggal)) |>
    summarise(harga = mean(harga), .by = bulan) |>
    as_tsibble(index = bulan)
}
rupiah <- scales::label_number(big.mark = ".", decimal.mark = ",")
