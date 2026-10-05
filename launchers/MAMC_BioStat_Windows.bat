@echo off
title MAMC BioStat
cd /d "%~dp0\.."
set "RS="
REM 1) bundled R (installer version)
if exist "R\bin\Rscript.exe" set "RS=%CD%\R\bin\Rscript.exe"
if defined RS goto run
REM 2) Rscript on PATH
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RS set "RS=%%i"
if defined RS goto run
REM 3) standard install location (newest version wins)
for /d %%d in ("%ProgramFiles%\R\R-*") do set "RS=%%d\bin\Rscript.exe"
if defined RS goto run
echo R was not found. Please install R from https://cran.r-project.org/bin/windows/base/ and run this file again.
pause
exit /b 1
:run
"%RS%" launcher.R
if errorlevel 1 pause
