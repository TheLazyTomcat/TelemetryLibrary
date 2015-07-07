@echo off

pushd .\

cd "..\Telemetry Library\Examples\BinaryLogger\Delphi"
dcc32.exe -Q -B SCSTelemetry_BinaryLogger.dpr

cd "..\Lazarus"
lazbuild -B --bm=Release_win_x86 SCSTelemetry_BinaryLogger.lpi
lazbuild -B --bm=Release_win_x64 SCSTelemetry_BinaryLogger.lpi
lazbuild -B --bm=Debug_win_x86 SCSTelemetry_BinaryLogger.lpi
lazbuild -B --bm=Debug_win_x64 SCSTelemetry_BinaryLogger.lpi



cd "..\..\TextLogger\Delphi"
dcc32.exe -Q -B SCSTelemetry_TextLogger.dpr

cd ..\Lazarus
lazbuild -B --bm=Release_win_x86 SCSTelemetry_TextLogger.lpi
lazbuild -B --bm=Release_win_x64 SCSTelemetry_TextLogger.lpi
lazbuild -B --bm=Debug_win_x86 SCSTelemetry_TextLogger.lpi
lazbuild -B --bm=Debug_win_x64 SCSTelemetry_TextLogger.lpi



cd "..\..\LogConverter" 
dcc32.exe -Q -B LogConverter.dpr

popd