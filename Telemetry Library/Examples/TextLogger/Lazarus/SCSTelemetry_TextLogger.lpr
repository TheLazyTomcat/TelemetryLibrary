library SCSTelemetry_TextLogger;

{$mode objfpc}{$H+}

{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  SysUtils,
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

  TelemetryLogText;

var
  Recipient:  TTelemetryRecipient = nil;
  TextLogger: TTelemetryLogText = nil;

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

