library SCSTelemetry_ServerLogger;

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  FastMM4 in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',
  SysUtils,
  SimpleLog in 'Libs\SimpleLog.pas',
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed in '..\..\..\Condensed\SCS_Telemetry_Condensed.pas',
{$ELSE}
  scssdk in '..\..\..\Headers\scssdk.pas',
  scssdk_value in '..\..\..\Headers\scssdk_value.pas',
  scssdk_telemetry in '..\..\..\Headers\scssdk_telemetry.pas',
  scssdk_telemetry_event in '..\..\..\Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel in '..\..\..\Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry_common_configs in '..\..\..\Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels in '..\..\..\Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels in '..\..\..\Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels in '..\..\..\Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2 in '..\..\..\Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2 in '..\..\..\Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
{$ENDIF}
  CRC32 in '..\..\Telemetry\Libs\CRC32.pas',
  MD5 in '..\..\Telemetry\Libs\MD5.pas',
  TelemetryCommon in '..\..\Telemetry\TelemetryCommon.pas',
  TelemetryVersionObjects in '..\..\Telemetry\TelemetryVersionObjects.pas',
  TelemetryLists in '..\..\Telemetry\TelemetryLists.pas',
  TelemetryInfoProvider in '..\..\Telemetry\TelemetryInfoProvider.pas',
  TelemetryRecipient in '..\..\Telemetry\TelemetryRecipient.pas',
  TelemetryRecipientAux in '..\..\Telemetry\TelemetryRecipientAux.pas',
  TelemetryNetCommon in '..\..\Telemetry\NET\TelemetryNetCommon.pas',
  TelemetryNetHashing in '..\..\Telemetry\NET\TelemetryNetHashing.pas',
  TelemetryNetLists in '..\..\Telemetry\NET\TelemetryNetLists.pas',
  TelemetryNetIncomingStream in '..\..\Telemetry\NET\TelemetryNetIncomingStream.pas',
  TelemetryNetCircularBuffers in '..\..\Telemetry\NET\TelemetryNetCircularBuffers.pas',
  TelemetryNetSockets in '..\..\Telemetry\NET\TelemetryNetSockets.pas',  
  TelemetryNetPackets in '..\..\Telemetry\NET\TelemetryNetPackets.pas',
  TelemetryNetPacketsBuilding in '..\..\Telemetry\NET\TelemetryNetPacketsBuilding.pas',
  TelemetryNetPacketsResolving in '..\..\Telemetry\NET\TelemetryNetPacketsResolving.pas',
  TelemetryNetServer in '..\..\Telemetry\NET\TelemetryNetServer.pas';

type
  TRecipientLog = class(TSimpleLog)
  public
    constructor Create;
    procedure LogEventReg(Sender: TObject; Event: scs_event_t);
    procedure LogEventUnreg(Sender: TObject; Event: scs_event_t);
    procedure LogEvent(Sender: TObject; Event: scs_event_t; Data: Pointer);
    procedure LogChannelReg(Sender: TObject; Name: String; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
    procedure LogChannelUnreg(Sender: TObject; Name: String; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
    procedure LogChannel(Sender: TObject; Name: String; ID: TItemID; Index: scs_u32_t; Value: p_scs_value_t);
    procedure LogConfig(Sender: TObject; Name: String; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
  end;

var
  Server: TTelemetryNetServer = nil;
  Logger: TRecipientLog = nil;

{$R *.res}

{=== TRecipientLog implementation =============================================}

{--- Public Methods -----------------------------------------------------------}

constructor TRecipientLog.Create;
begin
inherited;
StreamFileName := ExtractFilePath(GetModuleName(hInstance)) + 'log.txt';
InMemoryLog := False;
StreamToFile := True;
StreamAppend := False;
IndentNewLines := True;
end;

procedure TRecipientLog.LogEventReg(Sender: TObject; Event: scs_event_t);
begin
If Sender is TTelemetryNetServer then
  AddLog('Event registered: '  + TTelemetryNetServer(Sender).TelemetryRecipient.TelemetryInfoProvider.EventGetName(Event) + '(' + IntToStr(Event) + ')');
end;

procedure TRecipientLog.LogEventUnreg(Sender: TObject; Event: scs_event_t);
begin
If Sender is TTelemetryNetServer then
  AddLog('Event unregistered: '  + TTelemetryNetServer(Sender).TelemetryRecipient.TelemetryInfoProvider.EventGetName(Event) + '(' + IntToStr(Event) + ')');
end;

procedure TRecipientLog.LogEvent(Sender: TObject; Event: scs_event_t; Data: Pointer);
var
  TempStr:  String;
begin
If Sender is TTelemetryNetServer then
  begin
    TempStr := TTelemetryNetServer(Sender).TelemetryRecipient.EventGetDataAsString(Event,Data,False);
    If TempStr <> '' then
      AddLog(TTelemetryNetServer(Sender).TelemetryRecipient.TelemetryInfoProvider.EventGetName(Event) + sLineBreak + TempStr)
    else
      AddLog(TTelemetryNetServer(Sender).TelemetryRecipient.TelemetryInfoProvider.EventGetName(Event));
  end;
end;

procedure TRecipientLog.LogChannelReg(Sender: TObject; Name: String; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
If Sender is TTelemetryNetServer then
  begin
    If Index <> SCS_U32_NIL then
      AddLog('Channel registered: (' + IntToHex(ID,8) + ')' + Name + ', index ' + IntToStr(Index) + ', ' + SCSValueTypeToStr(ValueType) + ', ' + IntToHex(Flags,8))
    else
      AddLog('Channel registered: (' + IntToHex(ID,8) + ')' + Name + ', ' + SCSValueTypeToStr(ValueType) + ', ' + IntToHex(Flags,8));
  end;
end;

procedure TRecipientLog.LogChannelUnreg(Sender: TObject; Name: String; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
If Sender is TTelemetryNetServer then
  begin
    If Index <> SCS_U32_NIL then
      AddLog('Channel unregistered: (' + IntToHex(ID,8) + ')' + Name + ', index ' + IntToStr(Index) + ', ' + SCSValueTypeToStr(ValueType))
    else
      AddLog('Channel unregistered: (' + IntToHex(ID,8) + ')' + Name + ', ' + SCSValueTypeToStr(ValueType));
  end;
end;

procedure TRecipientLog.LogChannel(Sender: TObject; Name: String; ID: TItemID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If Sender is TTelemetryNetServer then
  begin
    If Index <> SCS_U32_NIL then
      AddLog('(' + IntToHex(ID,8) + ')' + Name + '[' + IntToStr(Index) + ']: ' + TTelemetryNetServer(Sender).TelemetryRecipient.ChannelGetValueAsString(Value,False))
    else
      AddLog('(' + IntToHex(ID,8) + ')' + Name + ': ' + TTelemetryNetServer(Sender).TelemetryRecipient.ChannelGetValueAsString(Value,False));
  end;
end;

procedure TRecipientLog.LogConfig(Sender: TObject; Name: String; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If Index <> SCS_U32_NIL then
  AddLog('(' + IntToHex(ID,8) + ')' + Name + '[' + IntToStr(Index) + ']: ' + SCSValueLocalizedToStr(Value))
else
  AddLog('(' + IntToHex(ID,8) + ')' + Name + ': ' + SCSValueLocalizedToStr(Value));
end;

{=== Library exported routines ================================================}

Function TelemetryLibraryInit(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;
begin
If not Assigned(Logger) then Logger := TRecipientLog.Create;
If not TTelemetryRecipient.SupportsTelemetryAndGameVersion(
  Version,params^.common.game_id,params^.common.game_version) then
  begin
    Result := SCS_RESULT_unsupported;
    Logger.AddLog('Version not supported (' + SCSGetVersionAsString(version) + ' ' +
                  UTF8ToString(params^.common.game_id) + ' ' +
                  SCSGetVersionAsString(params^.common.game_version) + ')');    
  end
else
  begin
    Logger.AddHeader;
    Logger.AddLog('Telemetry version: ' + SCSGetVersionAsString(version));
    Logger.AddLog('Game: ' + UTF8ToString(params^.common.game_name) + ' (' +
                  UTF8ToString(params^.common.game_id) + ' ' +
                  SCSGetVersionAsString(params^.common.game_version) + ')');
    Logger.AddLog('Creating server and recipient..');
    Server := TTelemetryNetServer.Create(TTelemetryRecipient.Create(version,params^));
    Server.ActiveMode := False;
    Server.TelemetryRecipient.Log('== Server running ==');
    Server.TelemetryRecipient.StoreChannelValues := True;
    Server.TelemetryRecipient.ManageIndexedChannels := True;
    Server.OnEventRegister := Logger.LogEventReg;
    Server.OnEventUnregister := Logger.LogEventUnreg;
    Server.OnEvent := Logger.LogEvent;
    Server.OnChannelRegister := Logger.LogChannelReg;
    Server.OnChannelUnregister := Logger.LogChannelUnreg;
    Server.OnChannel := Logger.LogChannel;
    Server.OnConfig := Logger.LogConfig;
    Result := SCS_RESULT_ok;
  end;
end;

procedure TelemetryLibraryFinal;
begin
Server.TelemetryRecipient.Free;
FreeAndNil(Logger);
end;

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';

begin
end.
