@echo off
cd "SCS Examples\telemetry"
dcc32.exe -Q -B telemetry.dpr
cd "..\..\SCS Examples\telemetry_position" 
dcc32.exe -Q -B telemetry_position.dpr
cd "..\..\SCS Examples\telemetry_mem" 
dcc32.exe -Q -B telemetry_mem.dpr
cd "..\..\SCS Examples\telemetry_mem_reader" 
dcc32.exe -Q -B TelemetryMemReader.dpr