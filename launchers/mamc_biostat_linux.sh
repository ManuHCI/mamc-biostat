#!/bin/bash
# Start MAMC BioStat on Linux. Install R first:  sudo apt install r-base   (Ubuntu/Debian)
cd "$(dirname "$0")/.."
command -v Rscript >/dev/null || { echo "R not found. Install with: sudo apt install r-base"; exit 1; }
Rscript launcher.R
