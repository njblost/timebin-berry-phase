# Reproducibility Notes — Time-bin Photonic Qudit: Geometric (Berry) Phase

This repository accompanies the manuscript **“Geometric-Phase (Pancharatnam--Berry) Correction for Time-Bin Photonic Qudits: A Calibration and Feed-Forward Algorithm.”**  
It contains Mathematica notebooks and a headless driver that regenerate the paper’s figures/tables.

---

## System used for the main runs

- **Wolfram Mathematica:** 13.3 (Kernel: Wolfram Language 13.3.0.0)  
- **Platform:** macOS (Apple Silicon / ARM, 64-bit)  
- See the exact snapshot in `wolfram-version.txt`.

---

## Repository layout (repro subset)

src/
run-all.wls # headless driver (kernel only; no FE needed)
results/
figs/ # auto-generated figures
tables/ # auto-generated CSVs
logs/ # run logs
notebooks_run-all WLS/ # batch-friendly copies of notebooks (you edit these)
notebooks/ # original interactive notebooks (unchanged)