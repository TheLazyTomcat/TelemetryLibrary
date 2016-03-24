@echo off

pushd .\

cd "..\API Headers\Condenser"
dcc32.exe -Q -B Condenser.dpr

cd "..\..\Telemetry Library\AutoDocumentation\PostProcessor"
dcc32.exe -Q -B PostProcessor.dpr

popd