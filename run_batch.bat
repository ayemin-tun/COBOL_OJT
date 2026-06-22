@echo off
cd /d "%~dp0"

cd src

set COB_LIBRARY_PATH=bin

.\bin\BATCHRUN.exe >> ..\batch_result.log 2>&1