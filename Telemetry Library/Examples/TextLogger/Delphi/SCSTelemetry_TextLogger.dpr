{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
library SCSTelemetry_TextLogger;

uses
  FastMM4,
  SysUtils,

  SCS_Telemetry_Condensed,

  TelemetryRecipient,
  
  TelemetryLogText;

var
  Recipient:  TTelemetryRecipient = nil;
  TextLogger: TTelemetryLogText = nil;

{$R *.res}

Function TelemetryLibraryInit(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;
begin
If not Assigned(TextLogger) then TextLogger := TTelemetryLogText.Create;
If not TTelemetryRecipient.SupportsTelemetryAndGameVersionParam(version,params^) then
  begin
    Result := SCS_RESULT_unsupported;
    TextLogger.AddLog('Version not supported (' + SCSGetVersionAsString(version) + ' ' +
                      TelemetryStringDecode(APIStringToTelemetryString(params^.common.game_id)) + ' ' +
                      SCSGetVersionAsString(params^.common.game_version) + ')');
    FreeAndNil(TextLogger);
  end
else
  begin
    TextLogger.Logger.AddHeader;
    TextLogger.AddLog('Telemetry version: ' + SCSGetVersionAsString(version));
    TextLogger.AddLog('Game: ' + APIStringToTelemetryString(params^.common.game_name) + ' (' +
                      TelemetryStringDecode(APIStringToTelemetryString(params^.common.game_id)) + ' ' +
                      SCSGetVersionAsString(params^.common.game_version) + ')');
    TextLogger.AddLog('Creating recipient...');
    Recipient := TTelemetryRecipient.Create(version,params^);
    Recipient.ManageIndexedChannels := True;
    TextLogger.Recipient := Recipient;
    TextLogger.AssignHandlers;
    Recipient.EventRegisterAll;
    Recipient.ChannelRegisterAll;    
    Recipient.Log(SCS_LOG_TYPE_message,'== Text logger loaded ==');    
    Result := SCS_RESULT_ok;
  end;
end;

procedure TelemetryLibraryFinal; stdcall;
begin
FreeAndNil(TextLogger);
FreeAndNil(Recipient);
end;

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';

begin
end.
