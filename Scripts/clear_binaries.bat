@echo off

rem Headers tester
del "..\API Headers\Headers Tester\Delphi\HT_SCS_TELEMETRY_DEV.exe" /S /Q
del "..\API Headers\Headers Tester\Lazarus\win_x86\HT_SCS_TELEMETRY_DEV.exe" /S /Q
del "..\API Headers\Headers Tester\Lazarus\win_x64\HT_SCS_TELEMETRY_DEV.exe" /S /Q

rem Condensed headers tester
del "..\API Headers\Condensed Headers Tester\Delphi\Condensed_tester.exe" /S /Q
del "..\API Headers\Condensed Headers Tester\Lazarus\win_x86\Condensed_tester.exe" /S /Q
del "..\API Headers\Condensed Headers Tester\Lazarus\win_x64\Condensed_tester.exe" /S /Q

rem Condenser
del "..\API Headers\Condenser\Condenser.exe" /S /Q

rem Telemetry library source tester
del "..\Telemetry Library\Tester\Delphi\win_x86\Telemetry_TestPrg.exe" /S /Q
del "..\Telemetry Library\Tester\Lazarus\win_x86\Telemetry_TestPrg.exe" /S /Q
del "..\Telemetry Library\Tester\Lazarus\win_x64\Telemetry_TestPrg.exe" /S /Q

rem Telemetry library documentation postprocessor
del "..\Telemetry Library\AutoDocumentation\PostProcessor\PostProcessor.exe" /S /Q 

rem Telemetry library examples - Text logger
del "..\Telemetry Library\Examples\TextLogger\Delphi\Release\win_x86\SCSTelemetry_TextLogger.dll" /S /Q
del "..\Telemetry Library\Examples\TextLogger\Lazarus\Debug\win_x86\SCSTelemetry_TextLogger.dll" /S /Q
del "..\Telemetry Library\Examples\TextLogger\Lazarus\Debug\win_x64\SCSTelemetry_TextLogger.dll" /S /Q
del "..\Telemetry Library\Examples\TextLogger\Lazarus\Release\win_x86\SCSTelemetry_TextLogger.dll" /S /Q
del "..\Telemetry Library\Examples\TextLogger\Lazarus\Release\win_x64\SCSTelemetry_TextLogger.dll" /S /Q

rem Telemetry library examples - Binary logger
del "..\Telemetry Library\Examples\BinaryLogger\Delphi\Release\win_x86\SCSTelemetry_BinaryLogger.dll" /S /Q
del "..\Telemetry Library\Examples\BinaryLogger\Lazarus\Debug\win_x86\SCSTelemetry_BinaryLogger.dll" /S /Q
del "..\Telemetry Library\Examples\BinaryLogger\Lazarus\Debug\win_x64\SCSTelemetry_BinaryLogger.dll" /S /Q
del "..\Telemetry Library\Examples\BinaryLogger\Lazarus\Release\win_x86\SCSTelemetry_BinaryLogger.dll" /S /Q
del "..\Telemetry Library\Examples\BinaryLogger\Lazarus\Release\win_x64\SCSTelemetry_BinaryLogger.dll" /S /Q

rem Telemetry library examples - Log converter
del "..\Telemetry Library\Examples\LogConverter\Delphi\Release\win_x86\LogConverter.exe" /S /Q
del "..\Telemetry Library\Examples\LogConverter\Lazarus\Debug\win_x86\LogConverter.exe" /S /Q
del "..\Telemetry Library\Examples\LogConverter\Lazarus\Debug\win_x64\LogConverter.exe" /S /Q
del "..\Telemetry Library\Examples\LogConverter\Lazarus\Release\win_x86\LogConverter.exe" /S /Q
del "..\Telemetry Library\Examples\LogConverter\Lazarus\Release\win_x64\LogConverter.exe" /S /Q

rem Telemetry library translated SDK examples - telemetry
del "..\Telemetry Library\SCS Examples\telemetry\Delphi\Release\win_x86\telemetry.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry\Lazarus\Debug\win_x86\telemetry.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry\Lazarus\Debug\win_x64\telemetry.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry\Lazarus\Release\win_x86\telemetry.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry\Lazarus\Release\win_x64\telemetry.dll" /S /Q   

rem Telemetry library translated SDK examples - telemetry_position
del "..\Telemetry Library\SCS Examples\telemetry_position\Delphi\Release\win_x86\telemetry_position.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_position\Lazarus\Debug\win_x86\telemetry_position.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_position\Lazarus\Debug\win_x64\telemetry_position.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_position\Lazarus\Release\win_x86\telemetry_position.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_position\Lazarus\Release\win_x64\telemetry_position.dll" /S /Q 

rem Telemetry library translated SDK examples - telemetry_mem
del "..\Telemetry Library\SCS Examples\telemetry_mem\Delphi\Release\win_x86\telemetry_mem.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem\Lazarus\Debug\win_x86\telemetry_mem.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem\Lazarus\Debug\win_x64\telemetry_mem.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem\Lazarus\Release\win_x86\telemetry_mem.dll" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem\Lazarus\Release\win_x64\telemetry_mem.dll" /S /Q   

rem Telemetry library translated SDK examples - telemetry_mem_reader
del "..\Telemetry Library\SCS Examples\telemetry_mem_reader\Delphi\Release\win_x86\telemetry_mem_reader.exe" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem_reader\Lazarus\Debug\win_x86\telemetry_mem_reader.exe" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem_reader\Lazarus\Debug\win_x64\telemetry_mem_reader.exe" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem_reader\Lazarus\Release\win_x86\telemetry_mem_reader.exe" /S /Q
del "..\Telemetry Library\SCS Examples\telemetry_mem_reader\Lazarus\Release\win_x64\telemetry_mem_reader.exe" /S /Q 