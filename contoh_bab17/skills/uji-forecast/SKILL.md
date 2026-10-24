---
name: uji-forecast
description: Menjalankan uji forecast standar (tolok ukur, validasi silang, residual) untuk satu deret tsibble. Gunakan saat pengguna meminta mengevaluasi atau membandingkan model forecast.
---

1. Tanyakan nama objek tsibble, kolom respons, dan horizon (h) jika belum jelas.
2. Buat validasi silang dengan stretch_tsibble(.init = 60, .step = 3).
3. Pasang NAIVE, RW(~ drift()), ETS(), ARIMA() pada log respons, plus kombinasi rata-ratanya.
4. Hitung accuracy() per horizon; tampilkan tabel RMSE, MAE, MASE terurut.
5. Untuk model terbaik, jalankan gg_tsresiduals() dan uji Ljung-Box.
6. Tulis ringkasan tiga kalimat: pemenang, selisih dengan naif, dan keterbatasan.
