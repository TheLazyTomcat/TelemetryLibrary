{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
//:todo: documentation
unit TelemetrySCSExample_telemetry_position;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
{$ENDIF}
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
  scssdk_telemetry_common_configs,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2;
{$ENDIF}

const
  def_LogFileName = 'telemetry_position.log';

type
  TSCSExm_TelemetryPositionState = record
    CabinPosition:  scs_value_fvector_t;
    HeadPosition:   scs_value_fvector_t;
    TruckPlacement: scs_value_dplacement_t;
    CabinOffset:    scs_value_fplacement_t;
    HeadOffset:     scs_value_fplacement_t;
  end;

{==============================================================================}
{   TSCSExm_TelemetryPosition // Class declaration                             }
{==============================================================================}  
  TSCSExm_TelemetryPosition = class(TTelemetryRecipientBinder)
  private
    fFormatSettings:  TFormatSettings;
    fLog:             TSimpleLog;
    fLogFileName:     String;
    fOutputPaused:    Boolean;
    fTelemetry:       TSCSExm_TelemetryPositionState;
  protected
    Function InitLog: Boolean; virtual;
    procedure FinishLog; virtual;
  public
    constructor Create(aRecipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
    destructor Destroy; override;
    procedure LogHandler(Sender: TObject; {%H-}LogType: scs_log_type_t; const {%H-}LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; {%H-}Event: scs_event_t; {%H-}UserData: Pointer); override;
    procedure EventUnregisterHandler(Sender: TObject; {%H-}Event: scs_event_t; {%H-}UserData: Pointer); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; {%H-}UserData: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; {%H-}ValueType: scs_value_type_t; {%H-}Flags: scs_u32_t; {%H-}UserData: Pointer); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; {%H-}ValueType: scs_value_type_t; {%H-}UserData: Pointer); override;
    procedure ChannelHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
    procedure ConfigHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TConfigID; {%H-}Index: scs_u32_t; {%H-}Value: scs_value_localized_t); override;
  end;


implementation

uses
  Windows;

{==============================================================================}
{   TSCSExm_TelemetryPosition // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryPosition // Protected methods                             }
{------------------------------------------------------------------------------}

Function TSCSExm_TelemetryPosition.InitLog: Boolean;
begin
try
  fLog.InternalLog := False;
  fLog.StreamFileAccessRights := fmShareDenyNone;
  fLog.StreamFileName := ExtractFilePath(GetModuleName(hInstance)) + fLogFileName;
  fLog.StreamToFile := True;
  fLog.AddLogNoTime('Log opened');
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.FinishLog;
begin
fLog.AddLogNoTime('Log ended');
end;

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryPosition // Public methods                                }
{------------------------------------------------------------------------------}

constructor TSCSExm_TelemetryPosition.Create(aRecipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
begin
inherited Create(aRecipient);
If not Assigned(aRecipient) then
  raise ETLNilReference.Create('TSCSExm_TelemetryPosition.Create: Recipient is not assigned.');
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fFormatSettings.DecimalSeparator := '.';
aRecipient.KeepUtilityEvents := False;
aRecipient.StoreConfigurations := False;
aRecipient.EventUnregisterAll;
fLogFileName := LogFileName;
fLog := TSimpleLog.Create;
If not InitLog then
  begin
    aRecipient.Log(SCS_LOG_TYPE_error,'Unable to initialize the log file');
    raise ETLInitFailed.Create('TSCSExm_TelemetryPosition.Create: Log initialization failed.');
  end;
fLog.AddLogNoTime('Game ''' + TelemetryStringDecode(aRecipient.GameID) + ''' '
                            + IntToStr(SCSGetMajorVersion(aRecipient.GameVersion)) + '.'
                            + IntToStr(SCSGetMinorVersion(aRecipient.GameVersion)));
If not TelemetrySameStr(aRecipient.GameID, SCS_GAME_ID_EUT2) then
  begin
    fLog.AddLogNoTime('WARNING: Unsupported game, some features or values might behave incorrectly');
  end
else
  begin
    If aRecipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_00 then
      fLog.AddLogNoTime('WARNING: Too old version of the game, some features might behave incorrectly');
    If SCSGetMajorVersion(aRecipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT) then
      fLog.AddLogNoTime('WARNING: Too new major version of the game, some features might behave incorrectly');
  end;
If not (aRecipient.EventRegister(SCS_TELEMETRY_EVENT_frame_end) and
        aRecipient.EventRegister(SCS_TELEMETRY_EVENT_paused) and
        aRecipient.EventRegister(SCS_TELEMETRY_EVENT_started) and
        aRecipient.EventRegister(SCS_TELEMETRY_EVENT_configuration)) then
  begin
    aRecipient.Log(SCS_LOG_TYPE_error,'Unable to register event callbacks');
    raise ETLRegFailed.Create('TSCSExm_TelemetryPosition.Create: Events registration failed.');
  end;
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,SCS_U32_NIL,SCS_VALUE_TYPE_dplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.TruckPlacement));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset,SCS_U32_NIL,SCS_VALUE_TYPE_fplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.CabinOffset));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset,SCS_U32_NIL,SCS_VALUE_TYPE_fplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.HeadOffset));
ZeroMemory(@fTelemetry,SizeOf(TSCSExm_TelemetryPositionState));
fOutputPaused := True;
end;

//------------------------------------------------------------------------------

destructor TSCSExm_TelemetryPosition.Destroy;
begin
If Assigned(fLog) then FinishLog;
fLog.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
var
  Config:               p_scs_named_value_t;
  HeadPosCabinSpace:    scs_value_fvector_t;
  HeadPosVehicleSpace:  scs_value_fvector_t;
  HeadPosWorldSpace:    scs_value_dvector_t;

  Function Add(VecA,VecB: scs_value_fvector_t): scs_value_fvector_t; overload;
  begin
    Result.x := VecA.x + VecB.x;
    Result.y := VecA.y + VecB.y;
    Result.z := VecA.z + VecB.z;
  end;

  Function Add(VecA: scs_value_dvector_t; VecB: scs_value_fvector_t): scs_value_dvector_t; overload;
  begin
    Result.x := VecA.x + VecB.x;
    Result.y := VecA.y + VecB.y;
    Result.z := VecA.z + VecB.z;
  end;

  Function Rotate(Orientation: scs_value_euler_t; Vector: scs_value_fvector_t): scs_value_fvector_t;
  var
    HeadRad,PitchRad,RollRad: Single;
    SinHeading,CosHeading:    Single;
    SinPitch,CosPitch:        Single;
    SinRoll,CosRoll:          Single;
    PostRoll,PostPitch:       scs_value_fvector_t;
  begin
    HeadRad := Orientation.heading * (2 * Pi);
    PitchRad := Orientation.pitch * (2 * Pi);
    RollRad := Orientation.roll * (2 * Pi);
    SinHeading := Sin(HeadRad);
    CosHeading := Cos(HeadRad);
    SinPitch := Sin(PitchRad);
    CosPitch := Cos(PitchRad);
    SinRoll := Sin(RollRad);
    CosRoll := Cos(RollRad);
    PostRoll.x := Vector.x * CosRoll - Vector.y * SinRoll;
    PostRoll.y := Vector.x * SinRoll + Vector.y * CosRoll;
    PostRoll.z := Vector.z;
	  PostPitch.x := PostRoll.x;
	  PostPitch.y := PostRoll.y * CosPitch - PostRoll.z * SinPitch;
	  PostPitch.z := PostRoll.y * SinPitch + PostRoll.z * CosPitch;
	  Result.x := PostPitch.x * CosHeading + PostPitch.z * SinHeading;
	  Result.y := PostPitch.y;
	  Result.z := -PostPitch.x * SinHeading + PostPitch.z * CosHeading;
  end;

  Function FindAttribute(Configuration: p_scs_telemetry_configuration_t; const Name: TelemetryString; Index: scs_u32_t; ExpectedType: scs_value_type_t): p_scs_named_value_t;
  var
    TempAttr: p_scs_named_value_t;
  begin
    TempAttr := Configuration^.attributes;
    while Assigned(TempAttr^.name) do
      begin
        If (TempAttr^.index = Index) and TelemetrySameText(APIStringToTelemetryString(TempAttr^.name),Name) then
          begin
            If TempAttr^.value._type = ExpectedType then
              begin
                Result := TempAttr;
                Exit;
              end
            else fLog.AddLogNoTime('ERROR: Attribute ' + TelemetryStringDecode(Name) + ' has unexpected type ' + IntToStr(ExpectedType));
          end;
        Inc(TempAttr);
      end;
    Result := nil;      
  end;

begin
case Event of
  SCS_TELEMETRY_EVENT_frame_end:
    begin
      If fOutputPaused then Exit;
      HeadPosCabinSpace := Add(fTelemetry.HeadPosition,fTelemetry.HeadOffset.position);
      HeadPosVehicleSpace := Add(Add(fTelemetry.CabinPosition,fTelemetry.CabinOffset.position),Rotate(fTelemetry.CabinOffset.orientation,HeadPosCabinSpace));
      HeadPosWorldSpace := Add(fTelemetry.TruckPlacement.position,Rotate(fTelemetry.TruckPlacement.orientation,HeadPosVehicleSpace));
      fLog.AddLogNoTime(FloatToStrF(HeadPosWorldSpace.x,ffFixed,15,6,fFormatSettings) + ';' +
                        FloatToStrF(HeadPosWorldSpace.y,ffFixed,15,6,fFormatSettings) + ';' +
                        FloatToStrF(HeadPosWorldSpace.z,ffFixed,15,6,fFormatSettings));
    end;
  SCS_TELEMETRY_EVENT_paused,
  SCS_TELEMETRY_EVENT_started:
    begin
      fOutputPaused := Event = SCS_TELEMETRY_EVENT_paused;
    end;
  SCS_TELEMETRY_EVENT_configuration:
    begin
      If not TelemetrySameText(APIStringToTelemetryString(scs_telemetry_configuration_t(Data^).id),SCS_TELEMETRY_CONFIG_truck) then Exit;
      Config := FindAttribute(data,SCS_TELEMETRY_CONFIG_ATTRIBUTE_cabin_position,SCS_U32_NIL,SCS_VALUE_TYPE_fvector);
      If Assigned(Config) then fTelemetry.CabinPosition := Config^.value.value_fvector
      else
        begin
          fTelemetry.CabinPosition.x := 0.0;
          fTelemetry.CabinPosition.y := 0.0;
          fTelemetry.CabinPosition.z := 0.0;
        end;
      Config := FindAttribute(data,SCS_TELEMETRY_CONFIG_ATTRIBUTE_head_position,SCS_U32_NIL,SCS_VALUE_TYPE_fvector);
      If Assigned(Config) then fTelemetry.HeadPosition := Config^.value.value_fvector
        else
        begin
          fLog.AddLogNoTime('WARNING: Head position unavailable');
          fTelemetry.HeadPosition.x := 0.0;
          fTelemetry.HeadPosition.y := 0.0;
          fTelemetry.HeadPosition.z := 0.0;
        end;
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
begin
If Assigned(Value) and Assigned(UserData) then
  case scs_value_t(Value^)._type of
    SCS_VALUE_TYPE_dplacement:  scs_value_dplacement_t(UserData^) := scs_value_t(Value^).value_dplacement;
    SCS_VALUE_TYPE_fplacement:  scs_value_fplacement_t(UserData^) := scs_value_t(Value^).value_fplacement;
  end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryPosition.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
// nothing to do here
end;

end.
