program telemetry_mem_reader;

{$mode objfpc}{$H+}
{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  Interfaces, // this includes the LCL widgetset

{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed,
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_telemetry_event,
  scssdk_telemetry_channel,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2,
{$ENDIF}

  CRC32,
  MD5,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}
  SimpleLog,
  BitOps,

  TelemetryCommon,
  TelemetryIDs,
  TelemetryConversions,
  TelemetryStrings,
  TelemetryValueTypeUtils,
  TelemetryLists,
  TelemetryVersionObjects,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryRecipientBinder,

  TelemetrySCS_Examples_telemetry_mem,

  Forms, ImgDraw, MainForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Telemetry Mem Reader';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.Run;
end.

