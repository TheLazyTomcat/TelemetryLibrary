{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program Telemetry_TestPrg;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
  Classes,

  TelemetryCommon           in '..\..\Source\TelemetryCommon.pas',
  TelemetryIDs              in '..\..\Source\TelemetryIDs.pas',
  TelemetryConversions      in '..\..\Source\TelemetryConversions.pas',
  TelemetryStrings          in '..\..\Source\TelemetryStrings.pas',
  TelemetryValueTypeUtils   in '..\..\Source\TelemetryValueTypeUtils.pas',
  TelemetryLists            in '..\..\Source\TelemetryLists.pas',
  TelemetryStreaming        in '..\..\Source\TelemetryStreaming.pas',
  TelemetryVersionObjects   in '..\..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\..\Source\TelemetryRecipientBinder.pas',

  TelemetrySCSExample_telemetry          in '..\..\Source\SCS\TelemetrySCSExample_telemetry.pas',
  TelemetrySCSExample_telemetry_position in '..\..\Source\SCS\TelemetrySCSExample_telemetry_position.pas',
  TelemetrySCSExample_telemetry_mem      in '..\..\Source\SCS\TelemetrySCSExample_telemetry_mem.pas',

  TelemetryLogText          in '..\..\Source\Log\TelemetryLogText.pas',
  TelemetryLogBinary        in '..\..\Source\Log\TelemetryLogBinary.pas',
  TelemetryLogBinaryParser  in '..\..\Source\Log\TelemetryLogBinaryParser.pas';

begin
  with TTelemetryInfoProvider.CreateCurrent('eut2') do
  try
  finally
    Free;
  end;
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.
