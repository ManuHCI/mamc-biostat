"""
Build the macOS edition of MAMC BioStat (a .dmg containing "MAMC BioStat.app").

The .app contains the program, every R package it needs (pre-installed, so no
internet is needed) and the official R installer. On first opening, if R is
not yet on the Mac, the app offers to install the bundled R (one click +
the Mac password); after that it always opens directly.

Run on a Mac (Apple Silicon -> arm64 build, Intel -> x86_64 build):
    python3 build_tools/build_mac.py
Needs: Python 3.8+, internet, Xcode command-line tools (sips, iconutil, hdiutil), sudo for installing R on the build Mac.
Output: dist/MAMC_BioStat_<version>_macOS-<arch>.dmg
"""
import os, platform, plistlib, re, shutil, subprocess, sys, tarfile, io, urllib.request
from pathlib import Path

CRAN = "https://cloud.r-project.org"
ROOT = Path(__file__).resolve().parent.parent
APP_VERSION = re.search(r'APP_VERSION <- "([^"]+)"', (ROOT / "app" / "app.R").read_text(encoding="utf-8")).group(1)
APP_PACKAGES = ["shiny", "readxl", "writexl", "DT", "ggplot2", "bslib", "jsonlite", "survival"]
ARCH = "arm64" if platform.machine() == "arm64" else "x86_64"
RFW = "/Library/Frameworks/R.framework/Resources"
MAC_R_VERSION = "4.5.3"


def log(m):
    print(f"[build] {m}", flush=True)


def fetch(url, binary=True):
    with urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "MAMC-BioStat-builder"}), timeout=300) as r:
        d = r.read()
    return d if binary else d.decode("utf-8", "replace")


def latest_r_pkg():
    html = fetch(f"{CRAN}/bin/macosx/", binary=False)
    m = re.search(rf"(big-sur-{ARCH}/base/R-(\d+\.\d+\.\d+)-{ARCH}\.pkg)", html)
    if m:
        return f"{CRAN}/bin/macosx/{m.group(1)}", m.group(2)
    # fallback: same version as the current Windows release
    v = re.search(r"R-(\d+\.\d+\.\d+)-win\.exe", fetch(f"{CRAN}/bin/windows/base/release.html", binary=False)).group(1)
    return f"{CRAN}/bin/macosx/big-sur-{ARCH}/base/R-{v}-{ARCH}.pkg", v


def run(cmd, **kw):
    log(" ".join(map(str, cmd)))
    subprocess.run(cmd, check=True, **kw)


LAUNCHER = r'''#!/bin/bash
# MAMC BioStat for macOS - starts the program (installs the bundled R the first time if needed)
RES="$(cd "$(dirname "$0")/../Resources" && pwd)"
RSCRIPT=""
for c in /Library/Frameworks/R.framework/Resources/bin/Rscript /usr/local/bin/Rscript /opt/homebrew/bin/Rscript; do
  [ -x "$c" ] && RSCRIPT="$c" && break
done
if [ -z "$RSCRIPT" ]; then
  ans=$(osascript -e 'button returned of (display dialog "MAMC BioStat needs R, the free statistics engine (included with this app).\n\nClick Install R, follow the installer (you will be asked for your Mac password), then open MAMC BioStat again." buttons {"Cancel", "Install R"} default button "Install R" with title "MAMC BioStat" with icon note)' 2>/dev/null)
  [ "$ans" = "Install R" ] && open "$RES/R-installer.pkg"
  exit 0
fi
LOG="$HOME/Library/Logs/MAMC BioStat.log"
cd "$RES"
"$RSCRIPT" "$RES/launcher.R" > "$LOG" 2>&1
rc=$?
if [ $rc -ne 0 ]; then
  osascript -e "display dialog \"MAMC BioStat could not start.\n\nDetails are saved in:\n$LOG\" buttons {\"OK\"} with title \"MAMC BioStat\" with icon stop" >/dev/null 2>&1
fi
'''


def make_icns(dest):
    src = ROOT / "installer" / "mamc_icon_512.png"
    iconset = dest.parent / "AppIcon.iconset"
    shutil.rmtree(iconset, ignore_errors=True)
    iconset.mkdir()
    for s in (16, 32, 64, 128, 256, 512):
        run(["sips", "-z", str(s), str(s), str(src), "--out", str(iconset / f"icon_{s}x{s}.png")], stdout=subprocess.DEVNULL)
        if s <= 256:
            run(["sips", "-z", str(2 * s), str(2 * s), str(src), "--out", str(iconset / f"icon_{s}x{s}@2x.png")], stdout=subprocess.DEVNULL)
    run(["iconutil", "-c", "icns", str(iconset), "-o", str(dest)])
    shutil.rmtree(iconset)


def main():
    # R 4.5.x is the newest series built for macOS 11 (Big Sur) and later, so it runs on the most student Macs
    r_version = os.environ.get("R_VERSION", MAC_R_VERSION)
    pkg_url = f"{CRAN}/bin/macosx/big-sur-{ARCH}/base/R-{r_version}-{ARCH}.pkg"
    r_minor = ".".join(r_version.split(".")[:2])
    log(f"MAMC BioStat {APP_VERSION} for macOS {ARCH}, R {r_version}")
    work = ROOT / "build" / f"mac-{ARCH}"
    shutil.rmtree(work, ignore_errors=True)
    work.mkdir(parents=True)
    rpkg = work / "R-installer.pkg"
    log(f"downloading {pkg_url}")
    rpkg.write_bytes(fetch(pkg_url))

    # install R on the build Mac (needed to install the packages and to test)
    if not Path(f"{RFW}/bin/Rscript").exists() or os.environ.get("FORCE_R_INSTALL"):
        run(["sudo", "installer", "-pkg", str(rpkg), "-target", "/"])

    app = work / "MAMC BioStat.app"
    res = app / "Contents" / "Resources"
    (app / "Contents" / "MacOS").mkdir(parents=True)
    res.mkdir(parents=True)
    shutil.copytree(ROOT / "app", res / "app")
    shutil.copy(ROOT / "launcher.R", res / "launcher.R")
    shutil.copy(rpkg, res / "R-installer.pkg")
    shutil.copy(ROOT / "LICENSE", res / "LICENSE.txt")

    lib = res / "library"
    lib.mkdir()
    log("installing R packages (binary) into the app")
    run([f"{RFW}/bin/Rscript", "-e",
         f"install.packages(c({', '.join(repr(p) for p in APP_PACKAGES)}), lib = '{lib}', repos = '{CRAN}', type = 'binary', dependencies = c('Depends','Imports','LinkingTo'))"])
    run([f"{RFW}/bin/Rscript", "-e",
         f".libPaths(c('{lib}', .libPaths())); for (p in c({', '.join(repr(p) for p in APP_PACKAGES)})) stopifnot(requireNamespace(p))"])
    (lib / "R_VERSION").write_text(r_minor + "\n")
    for pkgdir in lib.iterdir():
        if pkgdir.is_dir():
            for sub in ("doc", "tests", "html"):
                shutil.rmtree(pkgdir / sub, ignore_errors=True)

    exe = app / "Contents" / "MacOS" / "MAMCBioStat"
    exe.write_text(LAUNCHER.replace("__RMINOR__", r_minor))
    exe.chmod(0o755)
    make_icns(res / "AppIcon.icns")
    plist = {
        "CFBundleName": "MAMC BioStat", "CFBundleDisplayName": "MAMC BioStat", "CFBundleIdentifier": "in.ac.mamc.biostat",
        "CFBundleVersion": APP_VERSION, "CFBundleShortVersionString": APP_VERSION, "CFBundlePackageType": "APPL",
        "CFBundleExecutable": "MAMCBioStat", "CFBundleIconFile": "AppIcon", "LSMinimumSystemVersion": "11.0",
        "NSHumanReadableCopyright": "Maulana Azad Medical College, New Delhi - GNU GPL-3.0", "NSHighResolutionCapable": True,
    }
    with open(app / "Contents" / "Info.plist", "wb") as f:
        plistlib.dump(plist, f)
    # ad-hoc signature (required for apps containing native code on Apple Silicon)
    subprocess.run(["codesign", "--force", "--deep", "--sign", "-", str(app)], check=False)

    # disk image: app + Applications shortcut + read-me
    dmgsrc = work / "dmg"
    dmgsrc.mkdir()
    shutil.move(str(app), str(dmgsrc / app.name))
    os.symlink("/Applications", dmgsrc / "Applications")
    shutil.copy(ROOT / "docs" / "MAMC_BioStat_Quick_Start_Guide.docx", dmgsrc / "Quick Start Guide.docx")
    (dmgsrc / "READ ME FIRST.txt").write_text(
        "MAMC BioStat - Maulana Azad Medical College & Lok Nayak Hospital, New Delhi\n\n"
        "1. Drag 'MAMC BioStat' onto the 'Applications' folder.\n"
        "2. Open Applications, RIGHT-click MAMC BioStat and choose Open (first time only - macOS asks because the app is not from the App Store).\n"
        "3. If R is not yet on this Mac, the app offers to install it (included - no internet needed). Then open MAMC BioStat again.\n")
    dist = ROOT / "dist"
    dist.mkdir(exist_ok=True)
    out = dist / f"MAMC_BioStat_{APP_VERSION}_macOS-{'AppleSilicon' if ARCH == 'arm64' else 'Intel'}.dmg"
    out.unlink(missing_ok=True)
    run(["hdiutil", "create", "-volname", "MAMC BioStat", "-srcfolder", str(dmgsrc), "-ov", "-format", "UDZO", str(out)])
    log(f"done -> {out}")


if __name__ == "__main__":
    main()
