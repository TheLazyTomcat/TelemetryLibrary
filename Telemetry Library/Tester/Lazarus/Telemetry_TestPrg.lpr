program Telemetry_TestPrg;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Classes,

  TelemetryCommon,
  TelemetryIDs,
  TelemetryConversions,
  TelemetryStrings,
  TelemetryValueTypeUtils,
  TelemetryLists,
  TelemetryStreaming,
  TelemetryVersionObjects,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryRecipientBinder,

  TelemetrySCSExample_telemetry,
  TelemetrySCSExample_telemetry_position,
  TelemetrySCSExample_telemetry_mem,

  TelemetryLogText,
  TelemetryLogBinary,
  TelemetryLogBinaryParser;

begin
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.

