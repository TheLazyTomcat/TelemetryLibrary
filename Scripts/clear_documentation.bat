@echo off

pushd .\

cd "..\Telemetry Library\Documentation"

for /F "delims=" %%i in ('dir /b') do (
  rd "%%i" /S /Q . 2> NUL || del "%%i" /S /Q . 2> NUL 
)
 
copy NUL .gitkeep

popd