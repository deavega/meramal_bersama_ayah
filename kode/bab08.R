source("kode/00_persiapan.R")

# Forecast model (dari Bab 16: median simulasi gabungan untuk Desember 2027)
model_des27 <- 18655

# Hasil putaran terakhir Delphi (ilustrasi, 7 anggota panel, anonim)
panel <- c(17800, 18100, 18300, 18400, 18500, 18900, 19200)
median(panel)

# Penyesuaian: bobot 70% model, 30% panel, dengan alasan tertulis
forecast_akhir <- 0.7 * model_des27 + 0.3 * median(panel)
log_penyesuaian <- tibble(
  tanggal = Sys.Date(),
  variabel = "kurs akhir Des 2027",
  forecast_model = model_des27,
  forecast_panel = median(panel),
  forecast_akhir = forecast_akhir,
  alasan = "Panel mempertimbangkan rencana penerbitan global bond awal 2027"
)
log_penyesuaian
