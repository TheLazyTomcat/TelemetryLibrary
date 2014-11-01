// todo: documentation
unit TelemetrySCS_Examples_telemetry;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  SysUtils,
  SimpleLog,
  TelemetryCommon,
  TelemetryIDs,
  TelemetryStrings,
  TelemetryRecipient,
  TelemetryRecipientBinder,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2;
{$ENDIF}

const
  def_LogFileName = 'telemetry.log';

type
  TSCSExm_TelemetryState = record
    Timestamp:                    scs_timestamp_t;
    RawRenderingTimestamp:        scs_timestamp_t;
    RawSimulationTimestamp:       scs_timestamp_t;
    RawPausedSimulationTimestamp: scs_timestamp_t;
    OrientationAvailable:         Boolean;
    Heading:                      Single;
    Pitch:                        Single;
    Roll:                         Single;
    Speed:                        Single;
    RPM:                          Single;
    Gear:                         Integer;
  end;

{==============================================================================}
{    TSCSExm_Telemetry // Class declaration                                    }
{==============================================================================}  
  TSCSExm_Telemetry = class(TTelemetryRecipientBinder)
  private
    fFormatSettings:  TFormatSettings;
    fLog:             TSimpleLog;
    fLogFileName:     String;
    fOutputPaused:    Boolean;
    fPrintHeader:     Boolean;
    fLastTimestamp:   scs_timestamp_t;
    fTelemetry:       TSCSExm_TelemetryState;
  protected
    Function InitLog: Boolean; virtual;
    procedure FinishLog; virtual;
  public
    constructor Create(Recipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
    destructor Destroy; override;
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t); override;
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); override;
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); override;
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  end;

implementation

uses
  Windows;

{==============================================================================}
{    TSCSExm_Telemetry // Class implementation                                 }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TSCSExm_Telemetry // Protected methods                                    }
{------------------------------------------------------------------------------}

Function TSCSExm_Telemetry.InitLog: Boolean;
begin
try
  fLog.InMemoryLog := False;
  fLog.StreamFileAccessRights := fmShareDenyNone;
  fLog.StreamFileName := ExtractFilePath(GetModuleName(hInstance)) + 'telemetry.log';
  fLog.StreamToFile := True;
  fLog.AddLogNoTime('Log opened');
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.FinishLog;
begin
fLog.AddLogNoTime('Log ended');
end;

{------------------------------------------------------------------------------}
{    TSCSExm_Telemetry // Public methods                                       }
{------------------------------------------------------------------------------}

constructor TSCSExm_Telemetry.Create(Recipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
begin
inherited Create(Recipient);
If not Assigned(Recipient) then
  raise Exception.Create('TSCSExm_Telemetry.Create: Recipient is not assigned.');
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
fFormatSettings.DecimalSeparator := '.';
Recipient.KeepUtilityEvents := False;
Recipient.StoreConfigurations := False;
Recipient.EventUnregisterAll;
fLogFileName := LogFileName;
fLog := TSimpleLog.Create;
If not InitLog then
  begin
    Recipient.Log(SCS_LOG_TYPE_error,'Unable to initialize the log file');
    raise Exception.Create('TSCSExm_Telemetry.Create: Log initialization failed.');
  end;
fLog.AddLogNoTime('Game ''' + TelemetryStringDecode(Recipient.GameID) + ''' '
                            + IntToStr(SCSGetMajorVersion(Recipient.GameVersion)) + '.'
                            + IntToStr(SCSGetMinorVersion(Recipient.GameVersion)));
If not TelemetrySameStr(Recipient.GameID, SCS_GAME_ID_EUT2) then
  begin
    fLog.AddLogNoTime('WARNING: Unsupported game, some features or values might behave incorrectly');
  end
else
  begin
    If Recipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_00 then
      fLog.AddLogNoTime('WARNING: Too old version of the game, some features might behave incorrectly');
    If SCSGetMajorVersion(Recipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT) then
      fLog.AddLogNoTime('WARNING: Too new major version of the game, some features might behave incorrectly');
  end;
If not (Recipient.EventRegister(SCS_TELEMETRY_EVENT_frame_start) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_frame_end) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_paused) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_started)) then
  begin
    Recipient.Log(SCS_LOG_TYPE_error,'Unable to register event callbacks');
    raise Exception.Create('TSCSExm_Telemetry.Create: Events registration failed.');
  end;
Recipient.EventRegister(SCS_TELEMETRY_EVENT_configuration);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,SCS_U32_NIL,SCS_VALUE_TYPE_euler,SCS_TELEMETRY_CHANNEL_FLAG_no_value);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_speed,SCS_U32_NIL,SCS_VALUE_TYPE_float);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,SCS_U32_NIL,SCS_VALUE_TYPE_float);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,SCS_U32_NIL,SCS_VALUE_TYPE_s32);
Recipient.Log('Initializing telemetry log example');
ZeroMemory(@fTelemetry,SizeOf(TSCSExm_TelemetryState));
fPrintHeader := True;
fLastTimestamp := scs_timestamp_t(-1);
fOutputPaused := True;
end;

//------------------------------------------------------------------------------

destructor TSCSExm_Telemetry.Destroy;
begin
If Assigned(fLog) then FinishLog;
fLog.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.EventRegisterHandler(Sender: TObject; Event: scs_event_t);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.EventUnregisterHandler(Sender: TObject; Event: scs_event_t);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer);

  procedure WriteDataToLog;
  var
    TempStr:  String;
  begin
    TempStr := IntToStr(fTelemetry.Timestamp) + ';' + IntToStr(fTelemetry.RawRenderingTimestamp) + ';' +
               IntToStr(fTelemetry.RawSimulationTimestamp) + ';' + IntToStr(fTElemetry.RawPausedSimulationTimestamp);
    If fTelemetry.OrientationAvailable then
      TempStr := TempStr + ';' + FloatToStrF(fTelemetry.Heading,ffFixed,6,6,fFormatSettings) +
                           ';' + FloatToStrF(fTelemetry.Pitch,ffFixed,6,6,fFormatSettings) +
                           ';' + FloatToStrF(fTelemetry.Roll,ffFixed,6,6,fFormatSettings)
    else
      TempStr := TempStr + ';---;---;---';
    TempStr := TempStr + ';' + FloatToStrF(fTelemetry.Speed,ffFixed,6,6,fFormatSettings) +
                         ';' + FloatToStrF(fTelemetry.RPM,ffFixed,6,6,fFormatSettings) +
                         ';' + IntToStr(fTelemetry.Gear);
    fLog.AddLogNoTime(TempStr);
  end;

  procedure WriteConfigToLog(Config: scs_telemetry_configuration_t);
  var
    TempStr:  String;
    TempAttr: p_scs_named_value_t;
  begin
    TempStr := 'Configuration: ' + TelemetryStringDecode(APIStringToTelemetryString(Config.id));
    TempAttr := Config.attributes;
    while Assigned(TempAttr^.name) do
      begin
        TempStr := TempStr + sLineBreak + '  ' + TelemetryStringDecode(APIStringToTelemetryString(TempAttr^.name));
        If TempAttr^.index <> SCS_U32_NIL then
          TempStr := TempStr + '[' + IntToStr(TempAttr^.index) + ']';
        TempStr := TempStr + ' : ';
        case TempAttr^.value._type of
          SCS_VALUE_TYPE_INVALID:
            TempStr := TempStr + 'none';
          SCS_VALUE_TYPE_bool:
            TempStr := TempStr + 'bool = ' + BoolToStr(TempAttr^.value.value_bool.value <> 0,True);
          SCS_VALUE_TYPE_s32:
            TempStr := TempStr + 's32 = ' + IntToStr(TempAttr^.value.value_s32.value);
          SCS_VALUE_TYPE_u32:
            TempStr := TempStr + 'u32 = ' + IntToStr(TempAttr^.value.value_u32.value);
          SCS_VALUE_TYPE_u64:
            TempStr := TempStr + 'u64 = ' + IntToStr(TempAttr^.value.value_u64.value);
          SCS_VALUE_TYPE_float:
            TempStr := TempStr + 'float = ' + FloatToStrF(TempAttr^.value.value_float.value,ffFixed,6,6,fFormatSettings);
          SCS_VALUE_TYPE_double:
            TempStr := TempStr + 'double = ' + FloatToStrF(TempAttr^.value.value_double.value,ffFixed,6,6,fFormatSettings);
          SCS_VALUE_TYPE_fvector:
            TempStr := TempStr + 'fvector = (' + FloatToStrF(TempAttr^.value.value_fvector.x,ffFixed,6,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_fvector.y,ffFixed,6,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_fvector.z,ffFixed,6,6,fFormatSettings) + ')';
          SCS_VALUE_TYPE_dvector:
            TempStr := TempStr + 'dvector = (' + FloatToStrF(TempAttr^.value.value_dvector.x,ffFixed,6,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_dvector.y,ffFixed,6,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_dvector.z,ffFixed,6,6,fFormatSettings) + ')';
          SCS_VALUE_TYPE_euler:
            TempStr := TempStr + 'euler = h:' + FloatToStrF(TempAttr^.value.value_euler.heading * 360,ffFixed,6,6,fFormatSettings) +
                                        ' p:' + FloatToStrF(TempAttr^.value.value_euler.pitch * 360,ffFixed,6,6,fFormatSettings) +
                                        ' r:' + FloatToStrF(TempAttr^.value.value_euler.roll * 360,ffFixed,6,6,fFormatSettings);
          SCS_VALUE_TYPE_fplacement:
            TempStr := TempStr + 'fplacement = (' + FloatToStrF(TempAttr^.value.value_fplacement.position.x ,ffFixed,6,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_fplacement.position.y,ffFixed,6,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_fplacement.position.z,ffFixed,6,6,fFormatSettings) +
                                           ') h:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.heading * 360,ffFixed,6,6,fFormatSettings) +
                                            ' p:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.pitch * 360,ffFixed,6,6,fFormatSettings) +
                                            ' r:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.roll * 360,ffFixed,6,6,fFormatSettings);
          SCS_VALUE_TYPE_dplacement:
            TempStr := TempStr + 'dplacement = (' + FloatToStrF(TempAttr^.value.value_dplacement.position.x ,ffFixed,6,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_dplacement.position.y,ffFixed,6,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_dplacement.position.z,ffFixed,6,6,fFormatSettings) +
                                           ') h:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.heading * 360,ffFixed,6,6,fFormatSettings) +
                                            ' p:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.pitch * 360,ffFixed,6,6,fFormatSettings) +
                                            ' r:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.roll * 360,ffFixed,6,6,fFormatSettings);
          SCS_VALUE_TYPE_string:
            TempStr := TempStr + 'string = ' + TelemetryStringDecode(APIStringToTelemetryString(TempAttr^.value.value_string.value));
        else
          TempStr := TempStr + 'unknown';
        end;
        Inc(TempAttr);
      end;
    fLog.AddLogNoTime(TempStr);
  end;

begin
case Event of
  SCS_TELEMETRY_EVENT_frame_start:
    begin
      If fLastTimeStamp = scs_timestamp_t(-1) then
        fLastTimeStamp := scs_telemetry_frame_start_t(Data^).paused_simulation_time;
      If (scs_telemetry_frame_start_t(Data^).flags and SCS_TELEMETRY_FRAME_START_FLAG_timer_restart) <> 0 then
        fLastTimeStamp := 0;
      fTelemetry.Timestamp := fTelemetry.Timestamp + (scs_telemetry_frame_start_t(Data^).paused_simulation_time - fLastTimestamp);
      fLastTimestamp := scs_telemetry_frame_start_t(Data^).paused_simulation_time;
      fTelemetry.RawRenderingTimestamp := scs_telemetry_frame_start_t(Data^).render_time;
      fTelemetry.RawSimulationTimestamp := scs_telemetry_frame_start_t(Data^).simulation_time;
      fTelemetry.RawPausedSimulationTimestamp := scs_telemetry_frame_start_t(Data^).paused_simulation_time;
    end;
  SCS_TELEMETRY_EVENT_frame_end:
    begin
      If fOutputPaused then Exit;
      If fPrintHeader then
        begin
          fPrintHeader := False;
          fLog.AddLogNoTime('timestamp[us];raw rendering timestamp[us];raw simulation timestamp[us];raw paused simulation timestamp[us];heading[deg];pitch[deg];roll[deg];speed[m/s];rpm;gear');
        end;
      WriteDataToLog;
    end;
  SCS_TELEMETRY_EVENT_paused,
  SCS_TELEMETRY_EVENT_started:
    begin
      fOutputPaused := Event = SCS_TELEMETRY_EVENT_paused;
      If fOutputPaused then
        fLog.AddLogNoTime('Telemetry paused')
      else
        fLog.AddLogNoTime('Telemetry unpaused');
      fPrintHeader := True;
    end;
  SCS_TELEMETRY_EVENT_configuration:
    begin
      WriteConfigToLog(scs_telemetry_configuration_t(Data^));
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If ID = SCS_TELEMETRY_TRUCK_CHANNEL_ID_world_placement then
  begin
    If not Assigned(Value) then
      fTelemetry.OrientationAvailable := False
    else
      begin
        If scs_value_t(Value^)._type = SCS_VALUE_TYPE_euler then
          begin
            fTelemetry.OrientationAvailable := True;
            fTelemetry.Heading := scs_value_t(Value^).value_euler.heading * 360;
            fTelemetry.Pitch := scs_value_t(Value^).value_euler.pitch * 360;
            fTelemetry.Roll := scs_value_t(Value^).value_euler.roll * 360;
          end;
      end;
  end
else If ID = SCS_TELEMETRY_TRUCK_CHANNEL_ID_speed then
  begin
    If Assigned(Value) and (scs_value_t(Value^)._type = SCS_VALUE_TYPE_float) then
      fTelemetry.Speed := scs_value_t(Value^).value_float.value;
  end
else If ID = SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_rpm then
  begin
    If Assigned(Value) and (scs_value_t(Value^)._type = SCS_VALUE_TYPE_float) then
      fTelemetry.RPM := scs_value_t(Value^).value_float.value;
  end
else If ID = SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_gear then
  begin
    If Assigned(Value) and (scs_value_t(Value^)._type = SCS_VALUE_TYPE_s32) then
      fTelemetry.Gear := scs_value_t(Value^).value_s32.value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
// nothing to do here
end;     

end.
