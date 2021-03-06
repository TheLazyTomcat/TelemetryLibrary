@echo off

pushd .\

cd "..\Telemetry Library\AutoDocumentation"

start /wait .\PasDoc\pasdoc.exe^
 --title "Telemetry library"^
 --format html^
 --output "..\Documentation"^
 --language=en^
 --write-uses-list^
 --use-tipue-search^
 --auto-abstract^
 --auto-link^
 --include "..\Source"^
 --define Documentation^
 --implicit-visibility=public^
 --visible-members private,protected,public,published,automated^
 --css=pasdoc.css^
 --graphviz-uses^
 --graphviz-classes^
 --link-gv-uses png^
 --link-gv-classes png^
 --marker :^
 ..\Source\TelemetryCommon.pas^
 ..\Source\TelemetryIDs.pas^
 ..\Source\TelemetryConversions.pas^
 ..\Source\TelemetryStrings.pas^
 ..\Source\TelemetryValueTypeUtils.pas^
 ..\Source\TelemetryLists.pas^
 ..\Source\TelemetryStreaming.pas^
 ..\Source\TelemetryVersionObjects.pas^
 ..\Source\TelemetryInfoProvider.pas^
 ..\Source\TelemetryRecipient.pas^
 ..\Source\TelemetryRecipientBinder.pas^
 ..\Source\SCS\TelemetrySCSExample_telemetry.pas^
 ..\Source\SCS\TelemetrySCSExample_telemetry_mem.pas^
 ..\Source\SCS\TelemetrySCSExample_telemetry_position.pas^
 ..\Source\Log\TelemetryLogText.pas^
 ..\Source\Log\TelemetryLogBinary.pas^
 ..\Source\Log\TelemetryLogBinaryParser.pas 

popd