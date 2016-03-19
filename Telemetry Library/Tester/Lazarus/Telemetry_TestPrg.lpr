{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
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

