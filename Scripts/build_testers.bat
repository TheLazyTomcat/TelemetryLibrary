@echo off

pushd .\

cd "..\Headers Tester\Delphi"
dcc32.exe -Q -B HT_SCS_TELEMETRY_DEV.dpr

cd "..\Lazarus"
lazbuild -B --bm=Devel_win_x86 HT_SCS_TELEMETRY_DEV.lpi
lazbuild -B --bm=Devel_win_x64 HT_SCS_TELEMETRY_DEV.lpi



cd "..\..\Condensed Tester\Delphi"
dcc32.exe -Q -B Condensed_tester.dpr

cd "..\Lazarus"
lazbuild -B --bm=Devel_win_x86 Condensed_tester.lpi
lazbuild -B --bm=Devel_win_x64 Condensed_tester.lpi



cd "..\..\Telemetry Library\Tester\Delphi"
dcc32.exe -Q -B Telemetry_TestPrg.dpr

cd "..\Lazarus"
lazbuild -B --bm=Devel_win_x86 Telemetry_TestPrg.lpi
lazbuild -B --bm=Devel_win_x64 Telemetry_TestPrg.lpi

popd