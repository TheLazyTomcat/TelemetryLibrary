program Telemetry_TestPrg;

{$mode objfpc}{$H+}
{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,

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

  TelemetryCommon,                                           //*
  TelemetryIDs,                                              //*
  TelemetryConversions,                                      //*
  TelemetryStrings,                                          //*
  TelemetryValueTypeUtils,                                   //*
  TelemetryLists,                                            //*
  TelemetryStreaming,                                        //*
  TelemetryVersionObjects,                                   //*
  TelemetryInfoProvider,                                     //*
  TelemetryRecipient,                                        //*
  TelemetryRecipientBinder,

  TelemetrySCS_Examples_telemetry,                           //*
  TelemetrySCS_Examples_telemetry_position,                  //*
  TelemetrySCS_Examples_telemetry_mem,                       //*

  TelemetryLogText,                                          //*
  TelemetryLogBinary,                                        //*
  TelemetryLogBinaryParser,                                  //*

  TelemetryCommCommon,                                       //*
  TelemetryCommPackets,                                      //*
  TelemetryCommCircularBuffers,                              //*
  TelemetryCommPacketsAllocator,                             //*
  TelemetryCommPacketsBuilder,                               //*
  TelemetryCommPacketsResolving,                             //*
  TelemetryCommRemoteRecipient,                              //*
  TelemetryCommTransmitter,                                  //*
  TelemetryCommReceiver,                                     //*
  TelemetryCommCommunicator;                                 //*

begin
end.

