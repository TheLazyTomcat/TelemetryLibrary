@echo off
SET THEFILE=E:\Programy\Libraries\TelemetryLibrary\Dev\Telemetry Library\Tester\Lazarus\win_x86\Telemetry_TestPrg.exe
echo Linking %THEFILE%
D:\Lazarus\fpc\2.6.4\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections  -s  --entry=_mainCRTStartup    -o "E:\Programy\Libraries\TelemetryLibrary\Dev\Telemetry Library\Tester\Lazarus\win_x86\Telemetry_TestPrg.exe" "E:\Programy\Libraries\TelemetryLibrary\Dev\Telemetry Library\Tester\Lazarus\win_x86\link.res"
if errorlevel 1 goto linkend
D:\Lazarus\fpc\2.6.4\bin\i386-win32\postw32.exe --subsystem console --input "E:\Programy\Libraries\TelemetryLibrary\Dev\Telemetry Library\Tester\Lazarus\win_x86\Telemetry_TestPrg.exe" --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
