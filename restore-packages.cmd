@echo off
cls

IF NOT EXIST packages\FAKE\tools\FAKE.exe  (
    .paket\paket.bootstrapper.exe
    .paket\paket.exe restore
)