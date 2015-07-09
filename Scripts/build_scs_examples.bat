@echo off

pushd .\

cd "..\Telemetry Library\SCS Examples\telemetry\Delphi"
dcc32.exe -Q -B telemetry.dpr

cd "..\Lazarus"
lazbuild -B --bm=Release_win_x86 telemetry.lpi
lazbuild -B --bm=Release_win_x64 telemetry.lpi
lazbuild -B --bm=Debug_win_x86 telemetry.lpi
lazbuild -B --bm=Debug_win_x64 telemetry.lpi



cd "..\..\telemetry_position\Delphi" 
dcc32.exe -Q -B telemetry_position.dpr

cd "..\Lazarus"
lazbuild -B --bm=Release_win_x86 telemetry_position.lpi
lazbuild -B --bm=Release_win_x64 telemetry_position.lpi
lazbuild -B --bm=Debug_win_x86 telemetry_position.lpi
lazbuild -B --bm=Debug_win_x64 telemetry_position.lpi



cd "..\..\telemetry_mem\Delphi" 
dcc32.exe -Q -B telemetry_mem.dpr

cd "..\Lazarus"
lazbuild -B --bm=Release_win_x86 telemetry_mem.lpi
lazbuild -B --bm=Release_win_x64 telemetry_mem.lpi
lazbuild -B --bm=Debug_win_x86 telemetry_mem.lpi
lazbuild -B --bm=Debug_win_x64 telemetry_mem.lpi



cd "..\..\telemetry_mem_reader\Delphi" 
dcc32.exe -Q -B TelemetryMemReader.dpr

cd "..\Lazarus"
lazbuild -B --bm=Release_win_x86 telemetry_mem_reader.lpi
lazbuild -B --bm=Release_win_x64 telemetry_mem_reader.lpi
lazbuild -B --bm=Debug_win_x86 telemetry_mem_reader.lpi
lazbuild -B --bm=Debug_win_x64 telemetry_mem_reader.lpi

popd