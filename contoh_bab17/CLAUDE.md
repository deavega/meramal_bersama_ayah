# Proyek: Meramal Bersama Ayah

- Bahasa: R. Paket utama: fpp3 (tsibble, fable, feasts). Jangan pakai paket forecast lama.
- Data ada di data/*.csv. Muat dengan fungsi di kode/00_persiapan.R.
- Setiap model baru WAJIB dibandingkan dengan NAIVE dan RW(~drift()) memakai
  stretch_tsibble() dan accuracy(). Laporkan RMSE, MAE, MASE.
- Grafik memakai tema dari 00_persiapan.R; sumbu rupiah pakai labels = rupiah.
- Jangan pernah mengukur akurasi di data latih.
- Tulis komentar kode dalam bahasa Indonesia.
