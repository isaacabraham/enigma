@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

robocopy paket-files\fsprojects\FsReveal . build.fsx /njh /njs /nfl /ndl
del host-slides.fsx
rename build.fsx host-slides.fsx
packages\FAKE\tools\FAKE.exe host-slides.fsx %*