#!/bin/bash
# Double-click to start MAMC BioStat on macOS (needs R from https://cran.r-project.org/bin/macosx/)
cd "$(dirname "$0")/.."
if ! command -v Rscript >/dev/null 2>&1; then
  if [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ]; then PATH="/Library/Frameworks/R.framework/Resources/bin:$PATH"
  else osascript -e 'display alert "MAMC BioStat" message "Please install R from https://cran.r-project.org/bin/macosx/ first."'; exit 1; fi
fi
Rscript launcher.R
