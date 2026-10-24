# Meramal Bersama Ayah — kode dan data hands-on

Repositori pendamping buku **Meramal Bersama Ayah: Forecasting Deret Waktu dengan R dan Positron, Studi Kasus Indonesia** (Dea Avega Editya, 2026).

Materi buku mengikuti *Forecasting: Principles and Practice* (3rd ed.) karya Rob J. Hyndman dan George Athanasopoulos, OTexts, 2021 — https://otexts.com/fpp3/.

## Cara memakai

1. Clone repositori ini, atau unduh sebagai ZIP (tombol **Code → Download ZIP**):
   ```bash
   git clone https://github.com/deavega/meramal_bersama_ayah.git
   ```
2. Di Positron: **File → Open Folder** → pilih folder `meramal_bersama_ayah`.
3. Pasang paket sekali saja:
   ```r
   install.packages(c("fpp3", "readr", "slider", "fable.prophet"))
   ```
4. Buka `kode/babXX.R`, lalu jalankan dengan Ctrl/Cmd+Enter atau tombol **Source**.

## Isi

| Folder/berkas | Isi |
|---|---|
| `data/` | Semua data studi kasus (CSV) |
| `kode/00_persiapan.R` | Paket, tema grafik, dan fungsi pembantu; dipanggil di awal setiap bab |
| `kode/bab01.R` … `kode/bab16.R` | Skrip setiap bab |
| `laporan/kurs-apbn-2027.qmd` | Contoh laporan Quarto (Bab 16) |
| `gambar/` | Tempat grafik disimpan oleh fungsi `simpan()` |
| `contoh_bab17/` | Contoh CLAUDE.md dan skill `uji-forecast` dari Bab 17 (tidak aktif otomatis) |

## Data

| Berkas | Isi | Rentang | Sumber |
|---|---|---|---|
| `kurs_bulanan.csv` | Kurs rupiah per USD, akhir bulan | Jan 1990–Sep 2026 | IMF IFS; dilanjutkan kurs referensi ECB |
| `kurs_harian.csv` | Kurs rupiah per USD harian | Okt 2021–Okt 2026 | Kurs referensi ECB |
| `komoditas_bulanan.csv` | CPO, batu bara, Brent, nikel, karet, kopi, beras | Jan 1990–Jan 2026 | World Bank Pink Sheet |
| `pangan_harian.csv` | Harga 10 komoditas pangan, 34 provinsi | Jan 2022–Feb 2026 | PIHPS Bank Indonesia (dataset terbuka berlisensi MIT) |
| `inflasi_bulanan.csv` | Inflasi IHK yoy | 2003–2019 | Bank Indonesia |
| `inflasi_provinsi_mtm.csv` | Inflasi m-to-m, 38 provinsi + nasional | Jan 2024–Agu 2026 | BPS |
| `makro_tahunan.csv` | Inflasi, PDB, penduduk, kurs, pengangguran | 1960–2025 | World Bank Open Data (CC BY 4.0) |
| `listrik_sumber.csv` | Produksi listrik per sumber, TWh | 2000–2024 | Our World in Data (Energy Institute, Ember) |

Rincian sumber, lisensi, dan cara memperbarui data (termasuk dari Trading Economics) ada di Lampiran C buku. Simpan berkas unduhan mentah di `data/unduhan/`.

Semua forecast di repositori ini adalah latihan statistik untuk tujuan pendidikan, bukan rekomendasi investasi atau pandangan resmi lembaga mana pun.
