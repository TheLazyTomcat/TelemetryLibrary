{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
library SCSTelemetry_BinaryLogger;

{$mode objfpc}{$H+}

uses
  SysUtils,

  SCS_Telemetry_Condensed,

  TelemetryRecipient,

  TelemetryLogBinary;

var
  Recipient:    TTelemetryRecipient = nil;
  BinaryLogger: TTelemetryLogBinaryFile = nil;


Function TelemetryLibraryInit(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;
begin
If not TTelemetryRecipient.SupportsTelemetryAndGameVersionParam(version,params^) then
  begin
    Result := SCS_RESULT_unsupported;
  end
else
  begin
    Recipient := TTelemetryRecipient.Create(version,params^);
    Recipient.ManageIndexedChannels := True;
    If not Assigned(BinaryLogger) then
      BinaryLogger := TTelemetryLogBinaryFile.Create(Recipient);
    BinaryLogger.SaveItemIDOnly := True;
    Recipient.EventRegisterAll;
    Recipient.ChannelRegisterAll;
    Recipient.Log(SCS_LOG_TYPE_message,'== Binary logger loaded ==');
    Result := SCS_RESULT_ok;
  end;
end;

procedure TelemetryLibraryFinal; stdcall;
begin
FreeAndNil(BinaryLogger);
FreeAndNil(Recipient);
end;

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';


begin
end.
