{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
// todo: documentation
unit TelemetrySCS_Examples_telemetry_mem;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
{$ENDIF}
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
  MAX_SUPPORTED_WHEEL_COUNT = 8;

  def_MemoryMapName = 'SCSTelemetryExample';

type
  TSCSExm_TelemetryMemState = packed record
    Running:                  scs_u8_t;
    WSTruckPlacement:         scs_value_dplacement_t;
    SpeedometerSpeed:         scs_float_t;
    RPM:                      scs_float_t;
    Gear:                     scs_s32_t;
    Steering:                 scs_float_t;
    Throttle:                 scs_float_t;
    Brake:                    scs_float_t;
    Clutch:                   scs_float_t;
    LinearVelocity:           scs_value_fvector_t;
    AngularVelocity:          scs_value_fvector_t;
    LinearAcceleration:       scs_value_fvector_t;
    AngularAcceleration:      scs_value_fvector_t;
    CabinAngularVelocity:     scs_value_fvector_t;
    CabinAngularAcceleration: scs_value_fvector_t;
    WheelCount:               scs_u32_t;
    WheelDeflections:         Array[0..MAX_SUPPORTED_WHEEL_COUNT - 1] of scs_float_t;
  end;
  PSCSExm_TelemetryMemState = ^TSCSExm_TelemetryMemState;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TSCSExm_TelemetryMem                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem // Class declaration                                  }
{==============================================================================}
  TSCSExm_TelemetryMem = class(TTelemetryRecipientBinder)
  private
    fFormatSettings:  TFormatSettings;
    fMemoryMapName:   AnsiString;
    fMemoryMapping:   THandle;
    fSharedMemory:    PSCSExm_TelemetryMemState;
  protected
    procedure LogLine(LogType: scs_log_type_t; const Text: String); virtual;
    Function InitializeSharedMemory: Boolean; virtual;
    procedure DeinitializeSharedMemory; virtual;
  public
    constructor Create(Recipient: TTelemetryRecipient; const MemoryMapName: AnsiString = def_MemoryMapName);
    destructor Destroy; override;
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); override;
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TSCSExm_TelemetryMem_Reader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem_Reader // Class declaration                           }
{==============================================================================}
  TSCSExm_TelemetryMem_Reader = class(TObject)
  private
    fMemoryMapName: AnsiString;
    fMemoryMapping: THandle;
    fSharedMemory:  PSCSExm_TelemetryMemState;
    fStoredState:   TSCSExm_TelemetryMemState;
    Function GetInitialized: Boolean;
  protected
    procedure Deinitialize; virtual;
  public
    constructor Create(const MemoryMapName: AnsiString = def_MemoryMapName);
    destructor Destroy; override;
    Function Initialize: Boolean; virtual;
    Function RetrieveCurrentState: Boolean; virtual;
    property SharedMemory: PSCSExm_TelemetryMemState read fSharedMemory;
  published
    property Initialized: Boolean read GetInitialized;
    property MemoryMapName: AnsiString read fMemoryMapName;
    property MemoryMapping: THandle read fMemoryMapping;
    property StoredState: TSCSExm_TelemetryMemState read fStoredState;
  end;

implementation

uses
  Windows;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TSCSExm_TelemetryMem                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem // Class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryMem // Protected methods                                  }
{------------------------------------------------------------------------------}

procedure TSCSExm_TelemetryMem.LogLine(LogType: scs_log_type_t; const Text: String);
begin
Recipient.Log(LogType,Copy(Text,1,999{+ terminating null character}));
end;

//------------------------------------------------------------------------------

Function TSCSExm_TelemetryMem.InitializeSharedMemory: Boolean;
begin
fMemoryMapping := CreateFileMappingA(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE or SEC_COMMIT,0,SizeOf(TSCSExm_TelemetryMemState),PAnsiChar(fMemoryMapName));
If fMemoryMapping = 0 then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to create shared memory ' + IntToHex(GetLastError,8));
    DeinitializeSharedMemory;
    Result := False;
    Exit;
  end;
If GetLastError = ERROR_ALREADY_EXISTS then
  begin
    LogLine(SCS_LOG_TYPE_error,'Shared memory is already in use.');
    DeinitializeSharedMemory;
    Result := False;
    Exit;
  end;
fSharedMemory := MapViewOfFile(fMemoryMapping,FILE_MAP_ALL_ACCESS,0,0,0);
If not Assigned(fSharedMemory) then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to map the view ' + IntToHex(GetLastError,8));
    DeinitializeSharedMemory;
    Result := False;
    Exit;
  end;
ZeroMemory(fSharedMemory,SizeOf(TSCSExm_TelemetryMemState));
fSharedMemory.Running := 0;
fSharedMemory.WheelCount := 0;
Result := True;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.DeinitializeSharedMemory;
begin
If Assigned(fSharedMemory) then
  begin
    UnmapViewOfFile(fSharedMemory);
    fSharedMemory := nil;
  end;
If fMemoryMapping <> 0 then
  begin
    CloseHandle(fMemoryMapping);
    fMemoryMapping := 0;
  end;
end;

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryMem // Public methods                                     }
{------------------------------------------------------------------------------}

constructor TSCSExm_TelemetryMem.Create(Recipient: TTelemetryRecipient; const MemoryMapName: AnsiString = def_MemoryMapName);
begin
inherited Create(Recipient);
If not Assigned(Recipient) then
  raise Exception.Create('TSCSExm_TelemetryMem.Create: Recipient is not assigned.');
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
fFormatSettings.DecimalSeparator := '.';
Recipient.KeepUtilityEvents := False;
Recipient.StoreConfigurations := False;
Recipient.EventUnregisterAll;
fMemoryMapName := MemoryMapName;
LogLine(SCS_LOG_TYPE_message,'Game ''' + TelemetryStringDecode(Recipient.GameID) + ''' ' +
                             IntToStr(SCSGetMajorVersion(Recipient.GameVersion)) + '.' +
                             IntToStr(SCSGetMinorVersion(Recipient.GameVersion)));
If not TelemetrySameStrSwitch(Recipient.GameID, SCS_GAME_ID_EUT2) then
  begin
    LogLine(SCS_LOG_TYPE_warning,'Unsupported game, some features or values might behave incorrectly');
  end
else
  begin
    If Recipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_03 then
      begin
        LogLine(SCS_LOG_TYPE_error,'Too old version of the game');
        raise Exception.Create('TSCSExm_TelemetryMem.Create: Unsupported (too old) version of the game (' + SCSGetVersionAsString(Recipient.GameVersion) + ').');
      end;
    If Recipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_07 then
      LogLine(SCS_LOG_TYPE_warning,'This version of the game has less precise output of angular acceleration of the cabin');
    If SCSGetMajorVersion(Recipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT) then
      LogLine(SCS_LOG_TYPE_warning,'Too new major version of the game, some features might behave incorrectly');
  end;
If not (Recipient.EventRegister(SCS_TELEMETRY_EVENT_paused) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_started) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_configuration)) then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to register event callbacks');
    raise Exception.Create('TSCSExm_TelemetryMem.Create: Events registration failed.');
  end;
If not InitializeSharedMemory then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to initialize shared memory');
    raise Exception.Create('TSCSExm_TelemetryMem.Create: Shared memory initialization failed.');
  end;
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,            SCS_U32_NIL, SCS_VALUE_TYPE_dplacement,SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.WSTruckPlacement));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_speed,                      SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.SpeedometerSpeed));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,                 SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.RPM));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,                SCS_U32_NIL, SCS_VALUE_TYPE_s32,       SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Gear));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering,         SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Steering));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle,         SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Throttle));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake,            SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Brake));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch,           SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Clutch));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity,      SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.LinearVelocity));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity,     SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.AngularVelocity));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration,  SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.LinearAcceleration));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration, SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.AngularAcceleration));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity,     SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.CabinAngularVelocity));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration, SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.CabinAngularAcceleration));
LogLine(SCS_LOG_TYPE_message,'Memory telemetry example initialized');
end;

//------------------------------------------------------------------------------

destructor TSCSExm_TelemetryMem.Destroy;
begin
DeinitializeSharedMemory;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
var
  Config:     p_scs_named_value_t;
  WheelCount: LongWord;

  Function FindAttribute(Configuration: p_scs_telemetry_configuration_t; const Name: TelemetryString; Index: scs_u32_t; ExpectedType: scs_value_type_t): p_scs_named_value_t;
  var
    TempAttr: p_scs_named_value_t;
  begin
    TempAttr := Configuration^.attributes;
    while Assigned(TempAttr^.name) do
      begin
        If (TempAttr^.index = Index) and TelemetrySameTextSwitch(APIStringToTelemetryString(TempAttr^.name),Name) then
          begin
            If TempAttr^.value._type = ExpectedType then
              begin
                Result := TempAttr;
                Exit;
              end
            else LogLine(SCS_LOG_TYPE_error,'Attribute ' + TelemetryStringDecode(Name) + ' has unexpected type ' + IntToStr(ExpectedType));
          end;
        Inc(TempAttr);
      end;
    Result := nil;      
  end;

begin
case Event of
  SCS_TELEMETRY_EVENT_paused:
    begin
      fSharedMemory^.Running := 0;
    end;
  SCS_TELEMETRY_EVENT_started:
    begin
      fSharedMemory^.Running := 1;
    end;
  SCS_TELEMETRY_EVENT_configuration:
    begin
      If not TelemetrySameTextSwitch(APIStringToTelemetryString(scs_telemetry_configuration_t(Data^).id),SCS_TELEMETRY_CONFIG_truck) then Exit;
      Config := FindAttribute(data,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count,SCS_U32_NIL,SCS_VALUE_TYPE_u32);
      If Assigned(Config) then WheelCount := Config^.value.value_u32.value else WheelCount := 0;
      If WheelCount > MAX_SUPPORTED_WHEEL_COUNT then WheelCount := MAX_SUPPORTED_WHEEL_COUNT;
      while fSharedMemory^.WheelCount > WheelCount do
        begin
          Dec(fSharedMemory^.WheelCount);
          fSharedMemory^.WheelDeflections[fSharedMemory^.WheelCount] := 0.0;
          Recipient.ChannelUnregister(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection,fSharedMemory^.WheelCount,SCS_VALUE_TYPE_float);
        end;
      while fSharedMemory^.WheelCount < WheelCount do
        begin
          Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection,fSharedMemory^.WheelCount,SCS_VALUE_TYPE_float,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fSharedMemory^.WheelDeflections[fSharedMemory^.WheelCount]));
          Inc(fSharedMemory^.WheelCount);
        end;
    end;
end;

end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
const
  NullVector: scs_value_fvector_t = (x: 0.0; y: 0.0; z: 0.0);
begin
If ID = SCS_TELEMETRY_TRUCK_CHANNEL_ID_world_placement then
  begin
    If Assigned(Value) and (scs_value_t(Value^)._type = SCS_VALUE_TYPE_dplacement) then
      fSharedMemory^.WSTruckPlacement := scs_value_t(Value^).value_dplacement
    else
      begin
        fSharedMemory^.WSTruckPlacement.position.x := 0.0;
        fSharedMemory^.WSTruckPlacement.position.y := 0.0;
        fSharedMemory^.WSTruckPlacement.position.z := 0.0;
        fSharedMemory^.WSTruckPlacement.orientation.heading := 0.0;
        fSharedMemory^.WSTruckPlacement.orientation.pitch := 0.0;
        fSharedMemory^.WSTruckPlacement.orientation.roll := 0.0;
      end;
  end
else
  If Assigned(UserData) then
    case scs_value_t(Value^)._type of
      SCS_VALUE_TYPE_s32:     If Assigned(Value) then scs_s32_t(UserData^) := scs_value_t(Value^).value_s32.value
                                else scs_s32_t(UserData^) := 0;
      SCS_VALUE_TYPE_float:   If Assigned(Value) then scs_float_t(UserData^) := scs_value_t(Value^).value_float.value
                                else scs_float_t(UserData^) := 0.0;
      SCS_VALUE_TYPE_fvector: If Assigned(Value) then scs_value_fvector_t(UserData^) := scs_value_t(Value^).value_fvector
                                else scs_value_fvector_t(UserData^) := NullVector;
    end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_TelemetryMem.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
// nothing to do here
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TSCSExm_TelemetryMem_Reader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem_Reader // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryMem_Reader// Private methods                              }
{------------------------------------------------------------------------------}

Function TSCSExm_TelemetryMem_Reader.GetInitialized: Boolean;
begin
Result := (fMemoryMapping <> 0) and Assigned(fSharedMemory);
end;

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryMem_Reader// Protected methods                            }
{------------------------------------------------------------------------------}

procedure TSCSExm_TelemetryMem_Reader.Deinitialize;
begin
If Assigned(fSharedMemory) then
  begin
    UnmapViewOfFile(fSharedMemory);
    fSharedMemory := nil;
  end;
If fMemoryMapping <> 0 then
  begin
    CloseHandle(fMemoryMapping);
    fMemoryMapping := 0;
  end;
end;

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryMem_Reader// Public methods                               }
{------------------------------------------------------------------------------}

constructor TSCSExm_TelemetryMem_Reader.Create(const MemoryMapName: AnsiString = def_MemoryMapName);
begin
inherited Create;
fMemoryMapName := MemoryMapName;
ZeroMemory(@fStoredState,SizeOf(TSCSExm_TelemetryMemState));
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSCSExm_TelemetryMem_Reader.Destroy;
begin
Deinitialize;
inherited;
end;

//------------------------------------------------------------------------------

Function TSCSExm_TelemetryMem_Reader.Initialize: Boolean;
begin
fMemoryMapping := OpenFileMapping(FILE_MAP_READ,False,PAnsiChar(fMemoryMapName));
If fMemoryMapping <> 0 then fSharedMemory := MapViewOfFile(fMemoryMapping,FILE_MAP_READ,0,0,0);
Result := (fMemoryMapping <> 0) and Assigned(fSharedMemory);
If not Result then Deinitialize
  else RetrieveCurrentState;
end;

//------------------------------------------------------------------------------

Function TSCSExm_TelemetryMem_Reader.RetrieveCurrentState: Boolean;
begin
If Assigned(fSharedMemory) then
  begin
    fStoredState := fSharedMemory^;
    Result := True;
  end
else Result := False;
end;


end.
