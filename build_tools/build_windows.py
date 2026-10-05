"""
Build the stand-alone Windows installer for MAMC BioStat.

The installer contains its own copy of R and every R package the app needs,
so the user installs nothing else and the software works fully offline.

Profiles
  modern : Windows 10 / 11   -> latest R (4.5.x or newer)
  win7   : Windows 7 / 8.1   -> R 4.4.3 (the last R series tested on Windows 7/8.1)

Usage (on Windows, from the repository folder):
    python build_tools/build_windows.py --profile modern
    python build_tools/build_windows.py --profile win7
Needs: Python 3.8+, internet, NSIS (makensis) on PATH  (choco install nsis)
Also runs on Linux if `innoextract` and `makensis` are installed.
Output: dist/MAMC_BioStat_Setup_<version>_<profile>.exe  and a portable .zip
"""
import argparse, io, os, platform, re, shutil, subprocess, sys, urllib.request, zipfile
from pathlib import Path

CRAN = "https://cloud.r-project.org"
APP_PACKAGES = ["shiny", "readxl", "writexl", "DT", "ggplot2", "bslib", "jsonlite", "survival"]
ROOT = Path(__file__).resolve().parent.parent
APP_VERSION = re.search(r'APP_VERSION <- "([^"]+)"', (ROOT / "app" / "app.R").read_text(encoding="utf-8")).group(1)
PROFILES = {
    "modern": {"label": "Windows 10/11", "suffix": "Windows10-11", "r_version": None},   # None = latest release
    "win7":   {"label": "Windows 7/8.1", "suffix": "Windows7-8",   "r_version": "4.4.3"},
}


def log(msg):
    print(f"[build] {msg}", flush=True)


def fetch(url, binary=True):
    req = urllib.request.Request(url, headers={"User-Agent": "MAMC-BioStat-builder"})
    with urllib.request.urlopen(req, timeout=300) as r:
        data = r.read()
    return data if binary else data.decode("utf-8", "replace")


def latest_r_version():
    html = fetch(f"{CRAN}/bin/windows/base/release.html", binary=False)
    return re.search(r"R-(\d+\.\d+\.\d+)-win\.exe", html).group(1)


def download_r_installer(version, dest):
    for url in (f"{CRAN}/bin/windows/base/R-{version}-win.exe", f"{CRAN}/bin/windows/base/old/{version}/R-{version}-win.exe"):
        try:
            log(f"downloading {url}")
            dest.write_bytes(fetch(url))
            return
        except Exception as e:  # try the next location
            log(f"  not found there ({e})")
    sys.exit(f"Could not download R {version} for Windows")


def extract_r(installer, target):
    """Unpack R into `target` (a portable R, nothing is registered on the build machine)."""
    if target.exists():
        shutil.rmtree(target)
    if platform.system() == "Windows":
        subprocess.run([str(installer), "/VERYSILENT", "/SUPPRESSMSGBOXES", "/CURRENTUSER", "/NOICONS",
                        "/COMPONENTS=main,x64", f"/DIR={target}"], check=True)
    else:
        tmp = target.parent / "innoextract_tmp"
        shutil.rmtree(tmp, ignore_errors=True)
        subprocess.run(["innoextract", "-q", "-d", str(tmp), str(installer)], check=True)
        shutil.move(str(tmp / "app"), str(target))
        shutil.rmtree(tmp, ignore_errors=True)
    if not (target / "bin").exists():
        sys.exit("R extraction failed")


def parse_dcf(text):
    pkgs = {}
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    for block in re.split(r"\n[ \t]*\n", text):
        rec, key = {}, None
        for line in block.splitlines():
            if line[:1].isspace() and key:
                rec[key] += " " + line.strip()
            elif ":" in line:
                key, val = line.split(":", 1)
                rec[key] = val.strip()
        if "Package" in rec:
            pkgs[rec["Package"]] = rec
    return pkgs


FALLBACK_R = "4.5.3"


def packages_complete(r_minor):
    """True if every needed package (and dependency) has a Windows binary for this R series."""
    try:
        index = parse_dcf(fetch(f"{CRAN}/bin/windows/contrib/{r_minor}/PACKAGES", binary=False))
    except Exception:
        return False
    seen, queue = set(), list(APP_PACKAGES)
    while queue:
        p = queue.pop()
        if p in seen or p in BASE_PKGS:
            continue
        if p not in index:
            log(f"  {p} has no Windows binary for R {r_minor} yet")
            return False
        seen.add(p)
        for f in ("Depends", "Imports", "LinkingTo"):
            queue += dep_names(index[p].get(f))
    return True


BASE_PKGS = {"base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats",
             "stats4", "tcltk", "tools", "utils", "survival", "MASS", "lattice", "Matrix", "nlme", "mgcv", "boot", "class",
             "cluster", "codetools", "foreign", "KernSmooth", "nnet", "rpart", "spatial"}


def dep_names(field):
    out = []
    for part in (field or "").split(","):
        name = re.sub(r"\(.*?\)", "", part).strip()
        if name and name != "R":
            out.append(name)
    return out


def install_packages(r_dir, r_minor):
    """Download Windows binary packages (plus all dependencies) from CRAN and unzip them into R/library."""
    lib = r_dir / "library"
    already = {p.name for p in lib.iterdir() if p.is_dir()}
    index = parse_dcf(fetch(f"{CRAN}/bin/windows/contrib/{r_minor}/PACKAGES", binary=False))
    needed, queue = set(), list(APP_PACKAGES)
    while queue:
        p = queue.pop()
        if p in needed or p in already:
            continue
        if p not in index:
            sys.exit(f"Package {p} not available for R {r_minor} on CRAN")
        needed.add(p)
        rec = index[p]
        for f in ("Depends", "Imports", "LinkingTo"):
            queue += dep_names(rec.get(f))
    log(f"{len(needed)} packages to bundle: {', '.join(sorted(needed))}")
    for p in sorted(needed):
        ver = index[p]["Version"]
        data = fetch(f"{CRAN}/bin/windows/contrib/{r_minor}/{p}_{ver}.zip")
        zipfile.ZipFile(io.BytesIO(data)).extractall(lib)
    (lib / "R_VERSION").write_text(r_minor + "\n")
    # slim down: package manuals/tests are not needed by the app
    for pkgdir in lib.iterdir():
        for sub in ("doc", "tests", "html"):
            shutil.rmtree(pkgdir / sub, ignore_errors=True)


VBS = r'''' MAMC BioStat - starts the program without a black console window
Set fso = CreateObject("Scripting.FileSystemObject")
Set sh = CreateObject("WScript.Shell")
d = fso.GetParentFolderName(WScript.ScriptFullName)
sh.CurrentDirectory = d
rs = d & "\R\bin\x64\Rscript.exe"
If Not fso.FileExists(rs) Then rs = d & "\R\bin\Rscript.exe"
logDir = sh.ExpandEnvironmentStrings("%LOCALAPPDATA%") & "\MAMC BioStat"
If Not fso.FolderExists(logDir) Then fso.CreateFolder(logDir)
logFile = logDir & "\last_run.log"
rc = sh.Run("cmd /c """"" & rs & """ """ & d & "\launcher.R"" > """ & logFile & """ 2>&1""", 0, True)
If rc <> 0 Then
  MsgBox "MAMC BioStat could not start." & vbCrLf & vbCrLf & "Details are saved in:" & vbCrLf & logFile, 16, "MAMC BioStat"
End If
'''

BAT = "@echo off\r\ntitle MAMC BioStat\r\ncd /d \"%~dp0\"\r\nif exist \"R\\bin\\x64\\Rscript.exe\" (\"R\\bin\\x64\\Rscript.exe\" launcher.R) else (\"R\\bin\\Rscript.exe\" launcher.R)\r\npause\r\n"

NSI = r'''Unicode true
!include "MUI2.nsh"
Name "MAMC BioStat"
OutFile "{outfile}"
InstallDir "$LOCALAPPDATA\Programs\MAMC BioStat"
RequestExecutionLevel user
SetCompressor /SOLID lzma
BrandingText "MAMC BioStat {version} - Maulana Azad Medical College, New Delhi"
VIProductVersion "{version4}"
VIAddVersionKey "ProductName" "MAMC BioStat"
VIAddVersionKey "CompanyName" "Maulana Azad Medical College, New Delhi"
VIAddVersionKey "FileDescription" "MAMC BioStat installer ({label})"
VIAddVersionKey "FileVersion" "{version}"
VIAddVersionKey "LegalCopyright" "GNU GPL-3.0"
!define MUI_ICON "{icon}"
!define MUI_UNICON "{icon}"
!define MUI_WELCOMEPAGE_TITLE "MAMC BioStat {version}"
!define MUI_WELCOMEPAGE_TEXT "Free, open-source biostatistics software for medical students, residents and researchers.$\r$\n$\r$\nMaulana Azad Medical College & Lok Nayak Hospital, New Delhi.$\r$\n$\r$\nThis edition is for {label}. Everything needed (including R) is included - no internet is required.$\r$\n$\r$\nClick Next to continue."
!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_FUNCTION LaunchApp
!define MUI_FINISHPAGE_RUN_TEXT "Start MAMC BioStat now"
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "{license}"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Function LaunchApp
  Exec '"$WINDIR\System32\wscript.exe" "$INSTDIR\MAMC BioStat.vbs"'
FunctionEnd

Section "Install"
  SetOutPath "$INSTDIR"
  File /r "{stage}\*.*"
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  CreateDirectory "$SMPROGRAMS\MAMC BioStat"
  CreateShortCut "$SMPROGRAMS\MAMC BioStat\MAMC BioStat.lnk" "$WINDIR\System32\wscript.exe" '"$INSTDIR\MAMC BioStat.vbs"' "$INSTDIR\mamc_biostat.ico"
  CreateShortCut "$SMPROGRAMS\MAMC BioStat\Sample data.lnk" "$INSTDIR\app\sample_data"
  CreateShortCut "$SMPROGRAMS\MAMC BioStat\Uninstall MAMC BioStat.lnk" "$INSTDIR\Uninstall.exe"
  CreateShortCut "$DESKTOP\MAMC BioStat.lnk" "$WINDIR\System32\wscript.exe" '"$INSTDIR\MAMC BioStat.vbs"' "$INSTDIR\mamc_biostat.ico"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "DisplayName" "MAMC BioStat"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "DisplayVersion" "{version}"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "Publisher" "Maulana Azad Medical College, New Delhi"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "DisplayIcon" "$INSTDIR\mamc_biostat.ico"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat" "NoModify" 1
SectionEnd

Section "Uninstall"
  Delete "$DESKTOP\MAMC BioStat.lnk"
  RMDir /r "$SMPROGRAMS\MAMC BioStat"
  RMDir /r "$INSTDIR"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\MAMCBioStat"
SectionEnd
'''


def stage_app(stage, r_dir):
    log("assembling program folder")
    shutil.copytree(ROOT / "app", stage / "app", ignore=shutil.ignore_patterns("*.Rhistory", ".RData"))
    shutil.copy(ROOT / "launcher.R", stage / "launcher.R")
    shutil.copy(ROOT / "LICENSE", stage / "LICENSE.txt")
    shutil.copy(ROOT / "installer" / "mamc_biostat.ico", stage / "mamc_biostat.ico")
    shutil.copy(ROOT / "docs" / "MAMC_BioStat_Quick_Start_Guide.docx", stage / "MAMC_BioStat_Quick_Start_Guide.docx")
    (stage / "MAMC BioStat.vbs").write_text(VBS.replace("\n", "\r\n"), encoding="utf-8")
    (stage / "Start MAMC BioStat (with console).bat").write_bytes(BAT.encode())
    # the bundled R lives next to the app; its own library holds the packages
    shutil.move(str(r_dir), str(stage / "R"))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", choices=PROFILES, default="modern")
    ap.add_argument("--r-version", help="override the R version")
    a = ap.parse_args()
    prof = PROFILES[a.profile]
    r_version = a.r_version or prof["r_version"] or latest_r_version()
    if not packages_complete(".".join(r_version.split(".")[:2])):
        log(f"some packages are not yet built for R {r_version} - using R {FALLBACK_R}")
        r_version = FALLBACK_R
    r_minor = ".".join(r_version.split(".")[:2])
    log(f"MAMC BioStat {APP_VERSION} for {prof['label']} with R {r_version}")

    work = ROOT / "build" / a.profile
    shutil.rmtree(work, ignore_errors=True)
    work.mkdir(parents=True)
    installer = work / f"R-{r_version}-win.exe"
    download_r_installer(r_version, installer)
    r_dir = work / "R"
    extract_r(installer, r_dir)
    install_packages(r_dir, r_minor)

    stage = work / "MAMC_BioStat"
    stage.mkdir()
    stage_app(stage, r_dir)

    dist = ROOT / "dist"
    dist.mkdir(exist_ok=True)
    base = f"MAMC_BioStat_{APP_VERSION}_{prof['suffix']}"
    log("making portable zip")
    shutil.make_archive(str(dist / f"{base}_portable"), "zip", root_dir=work, base_dir="MAMC_BioStat")

    log("making installer with NSIS")
    nsi = work / "installer.nsi"
    v4 = ".".join((APP_VERSION.split(".") + ["0", "0", "0"])[:4])
    nsi.write_text(NSI.format(outfile=str(dist / f"MAMC_BioStat_Setup_{APP_VERSION}_{prof['suffix']}.exe"), version=APP_VERSION,
                              version4=v4, label=prof["label"], icon=str(ROOT / "installer" / "mamc_biostat.ico"),
                              license=str(ROOT / "LICENSE"), stage=str(stage)), encoding="utf-8")
    makensis = shutil.which("makensis") or r"C:\Program Files (x86)\NSIS\makensis.exe"
    subprocess.run([makensis, "-V2", str(nsi)], check=True)
    log(f"done -> {dist}")


if __name__ == "__main__":
    main()
