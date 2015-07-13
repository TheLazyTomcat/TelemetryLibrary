{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Reimplementation of telemetry_mem example distributed with the SDK.)
@author(František Milt <fmilt@seznam.cz>)
@created(2015-07-13)
@lastmod(2015-07-13)

  @bold(@NoAutoLink(TelemetrySCSExample_telemetry_mem))

  ©2013-2015 František Milt, all rights reserved.

  This unit contains a class that is designed to imitate behavior of original
  @italic(telemetry_mem) example distributed with the Telemetry SDK. It also
  contains simple class designed to read from a shared memory created by a
  @italic(telemetry_mem) plugin.

  Last change: 2015-07-13

  Change List:@unorderedList(
    @item(2015-07-13 - First documentation added.))

@html(<hr>)}
unit TelemetrySCSExample_telemetry_mem;

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
  //:Maximum number of wheels supported by TSCSExm_TelemetryMem. For details,
  //:refer to the class implementation.
  MAX_SUPPORTED_WHEEL_COUNT = 8;

  //:Default name of the memory mapped file created by a TSCSExm_TelemetryMem
  //:class.
  def_MemoryMapName = 'SCSTelemetryExample';

type
{:
  @abstract(Structure used when working with shared memory. Directly mapped to
  the MMF location.)
  It is binary compatible with structure declared in original SDK example,
  meaning you can use plugin compiled from C++ code and access the shared memory
  using this structure, and vice-versa.

  @member(Running                  @True when the game is @noAutoLink(running),
                                   @false when it is not (paused, terminated,
                                   ...).)
  @member(WSTruckPlacement         World-space coordinates of vehicle position
                                   and orientation.)
  @member SpeedometerSpeed         Speed of the vehicle [m/s].
  @member RPM                      Engine revolutions [min@html(<sup>-1</sup>)].
  @member Gear                     Engaged @noAutoLink(gear).
  @member(Steering                 Effective @noAutoLink(steering) position
                                   <-1,1>.)
  @member Throttle                 Effective @noAutoLink(throttle) <0,1>.
  @member Brake                    Effective @noAutoLink(brake) <0,1>.
  @member Clutch                   Effective @noAutoLink(clutch) <0,1>.
  @member(LinearVelocity           Linear velocity (vector) of the vehicle
                                   [m/s].)
  @member(AngularVelocity          Angular velocity of the vehicle
                                   [s@html(<sup>-1</sup>)].)
  @member(LinearAcceleration       Linear acceleration of the vehicle
                                   [m.s@html(<sup>-2</sup>)].)
  @member(AngularAcceleration      Angular acceleration of the vehicle
                                   [s@html(<sup>-2</sup>)].)
  @member(CabinAngularVelocity     Angular velocity of the vehicle's cabin
                                   [s@html(<sup>-1</sup>)].)
  @member(CabinAngularAcceleration Angular acceleration of the vehicle's cabin
                                   [s@html(<sup>-2</sup>)].)
  @member(WheelCount               Number of wheels the vehicle have. Limited to
                                   MAX_SUPPORTED_WHEEL_COUNT.)
  @member(WheelDeflections         Deflection of the individual wheels. Only
                                   items up to WheelCount - 1 are valid.)
}
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
  //:Pointer to TSCSExm_TelemetryMemState structure.
  PSCSExm_TelemetryMemState = ^TSCSExm_TelemetryMemState;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TSCSExm_TelemetryMem                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem // Class declaration                                  }
{==============================================================================}
{:
  @abstract(Class designed to imitate behavior of @italic(telemetry_mem) example
  distributed with the Telemetry SDK.)
  It writes values obtained from the telemetry channels into the shared memory
  (memory mapped file object). Also manages automatic registration of wheel
  channels based on configuration data.@br
  Shared memory is binary compatible with that produced by original C++ example.

  @bold(WARNING) - creation of MMF object fails if it already exists. If you
                   want to access the shared memory for reading, you have to
                   first @noAutoLink(create) instance of this class and only
                   then you can open MMF in some other program for reading.
}
  TSCSExm_TelemetryMem = class(TTelemetryRecipientBinder)
  private
  {:
    Full name of the shared memory (MMF).
  }
    fMemoryMapName: AnsiString;
  {:
    Handle of the memory mapped file object.
  }
    fMemoryMapping: THandle;
  {:
    Pointer to the shared memory (MMF).
  }    
    fSharedMemory:  PSCSExm_TelemetryMemState;
  protected
  {:
    Writes a new entry to the game log using telemetry API.
  }
    procedure LogLine(LogType: scs_log_type_t; const Text: String); virtual;
  {:
    Initializes shared memory - creates MMF object, opens view of file and fills
    the shared memory with zeroes.

    @bold(WARNING) - this function will fail if the mapping already exists. This
                     behaviour is intentional as it is present in the original
                     example.

    @returns @True on successful creation of MMF object, @false when it fails
             or when the object already exists.
  }
    Function InitializeSharedMemory: Boolean; virtual;
  {:
    Deinitializes shared memory - unmaps view of file and closes the MMF object
    handle.
  }
    procedure DeinitializeSharedMemory; virtual;
  public
  {:
    Class constructor.

    Creates a shared memory (memory mapped file object) by calling
    InitializeSharedMemory method.@br
    @code(aRecipient) parameter must assigned, otherwise an exception is raised.
    @code(MemoryMapName) must not be empty and must not contain backslashes (\).

    @param(aRecipient    Must contain valid reference to a TTelemetryRecipient
                         instance.)
    @param(MemoryMapName Name of the memory mapped file object.)

    @raises ETLNilReference    When @code(aRecipient) is not assigned.
    @raises(ETLUnsupportedGame When game version for which this object is
                               prepared is lower than 0x00010003.)
    @raises(ETLInitFailed      When initialization fails (method
                               InitializeSharedMemory returns @false).)
    @raises(ETLRegFailed       When registration of any of the following
                               telemetry events fails:
    @preformatted(
    SCS_TELEMETRY_EVENT_paused
    SCS_TELEMETRY_EVENT_started
    SCS_TELEMETRY_EVENT_configuration))
  }
    constructor Create(aRecipient: TTelemetryRecipient; const MemoryMapName: AnsiString = def_MemoryMapName);
  {:
    Class destructor.@br

    Deinitializes memory.
  }
    destructor Destroy; override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }
    procedure LogHandler(Sender: TObject; {%H-}LogType: scs_log_type_t; const {%H-}LogText: String); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }
    procedure EventRegisterHandler(Sender: TObject; {%H-}Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }
    procedure EventUnregisterHandler(Sender: TObject; {%H-}Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Processes all events passed from the API. Automated (un)registration of the
    wheel channels is done within this method (number of registered wheel
    channels is given by the truck configuration parsed in this method).
    For details see method implementation.@br
    For details on parameters, see @inherited.
  }
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; {%H-}UserData: Pointer); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }
    procedure ChannelRegisterHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; {%H-}ValueType: scs_value_type_t; {%H-}Flags: scs_u32_t; {%H-}UserData: Pointer); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }
    procedure ChannelUnregisterHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; {%H-}ValueType: scs_value_type_t; {%H-}UserData: Pointer); override;
  {:
    Processes all channels passed from the API. Stores channel value to
    appropriate location in shared memory (see structure
    TSCSExm_TelemetryMemState).@br
    Actual location where to store the value is passed in parameter
    @code(UserData). When @code(Value) parameter is not assigned, zero is
    stored. @br
    For details on parameters, see @inherited.
  }     
    procedure ChannelHandler(Sender: TObject; const {%H-}Name: TelemetryString; ID: TChannelID; {%H-}Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }    
    procedure ConfigHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TConfigID; {%H-}Index: scs_u32_t; {%H-}Value: scs_value_localized_t); override;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TSCSExm_TelemetryMem_Reader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TSCSExm_TelemetryMem_Reader // Class declaration                           }
{==============================================================================}
{:
  @abstract(Simple class created as a reader of shared memory produced by
            @italic(telemetry_mem) example.)
  When using this class, do not read directly from the shared memory, as there
  is no synchronization and data consistency protection whatsoever. Instead,
  call RetrieveCurrentState method and then access StoredState property. But
  note that even this process does not guarantee data consistency.
}
  TSCSExm_TelemetryMem_Reader = class(TObject)
  private
  {:
    See MemoryMapName property.
  }
    fMemoryMapName: AnsiString;
  {:
    See MemoryMapping property.
  }
    fMemoryMapping: THandle;
  {:
    See SharedMemory property.
  }
    fSharedMemory:  PSCSExm_TelemetryMemState;
  {:
    See StoredState property.
  }
    fStoredState:   TSCSExm_TelemetryMemState;
  {:
    Getter for property Initialized.@br

    @returns(@True when fMemoryMapping is non null and fSharedMemory is
             assigned.)
  }
    Function GetInitialized: Boolean;
  public
  {:
    Class constructor.

    Calls Initialize method.@br
    @noAutoLink(@code(MemoryMapName)) must not be empty and must not contain
    backslashes (\), also, in order for this class to work as expected, it must
    be the same as name passes in the plugin (so the same MMF os opened).

    @param MemoryMapName Name of the memory mapped file object.
  }
    constructor Create(const MemoryMapName: AnsiString = def_MemoryMapName);
  {:
    Class destructor.

    Calls Deinitialize method.
  }
    destructor Destroy; override;
  {:
    Shared memory initialization.@br
    Calls RetrieveCurrentState when successful.@br
    Note that file mapping is opened only for reading.

    @returns(@True when MMF is opened and view of file is successfully mapped,
             @false otherwise.)
  }
    Function Initialize: Boolean; virtual;
  {:
    Shared memory deinitialization. Unmaps view of file and closes handle of the
    MMF object.
  }
    procedure Deinitialize; virtual;
  {:
    Retrieves data from the shared memory and stores them in fStoredState
    snapshot.

    @returns(@True when successful, false otherwise (eg. when file view is not
             mapped).)
  }
    Function RetrieveCurrentState: Boolean; virtual;
  {:
    Pointer to the shared memory (MMF).
  }
    property SharedMemory: PSCSExm_TelemetryMemState read fSharedMemory;
  {:
    Stored snapshot of the shared memory.
  }
    property StoredState: TSCSExm_TelemetryMemState read fStoredState;
  published
  {:
    @True when fMemoryMapping is non null and fSharedMemory is assigned, false
    otherwise.
  }
    property Initialized: Boolean read GetInitialized;
  {:
    Full name of the shared memory (memory mapped file object).
  }
    property MemoryMapName: AnsiString read fMemoryMapName;
  {:
    Handle of the memory mapped file object.
  }
    property MemoryMapping: THandle read fMemoryMapping;
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
fSharedMemory^.Running := 0;
fSharedMemory^.WheelCount := 0;
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

constructor TSCSExm_TelemetryMem.Create(aRecipient: TTelemetryRecipient; const MemoryMapName: AnsiString = def_MemoryMapName);
begin
inherited Create(aRecipient);
If not Assigned(aRecipient) then
  raise ETLNilReference.Create('TSCSExm_TelemetryMem.Create: Recipient is not assigned.');
aRecipient.KeepUtilityEvents := False;
aRecipient.StoreConfigurations := False;
aRecipient.EventUnregisterAll;
fMemoryMapName := MemoryMapName;
LogLine(SCS_LOG_TYPE_message,'Game ''' + TelemetryStringDecode(aRecipient.GameID) + ''' ' +
                             IntToStr(SCSGetMajorVersion(aRecipient.GameVersion)) + '.' +
                             IntToStr(SCSGetMinorVersion(aRecipient.GameVersion)));
If not TelemetrySameStr(aRecipient.GameID, SCS_GAME_ID_EUT2) then
  begin
    LogLine(SCS_LOG_TYPE_warning,'Unsupported game, some features or values might behave incorrectly');
  end
else
  begin
    If aRecipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_03 then
      begin
        LogLine(SCS_LOG_TYPE_error,'Too old version of the game');
        raise ETLUnsupportedGame.Create('TSCSExm_TelemetryMem.Create: Unsupported (too old) version of the game (' + SCSGetVersionAsString(aRecipient.GameVersion) + ').');
      end;
    If aRecipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_07 then
      LogLine(SCS_LOG_TYPE_warning,'This version of the game has less precise output of angular acceleration of the cabin');
    If SCSGetMajorVersion(aRecipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT) then
      LogLine(SCS_LOG_TYPE_warning,'Too new major version of the game, some features might behave incorrectly');
  end;
If not (aRecipient.EventRegister(SCS_TELEMETRY_EVENT_paused) and
        aRecipient.EventRegister(SCS_TELEMETRY_EVENT_started) and
        aRecipient.EventRegister(SCS_TELEMETRY_EVENT_configuration)) then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to register event callbacks');
    raise ETLRegFailed.Create('TSCSExm_TelemetryMem.Create: Events registration failed.');
  end;
If not InitializeSharedMemory then
  begin
    LogLine(SCS_LOG_TYPE_error,'Unable to initialize shared memory');
    raise ETLInitFailed.Create('TSCSExm_TelemetryMem.Create: Shared memory initialization failed.');
  end;
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,            SCS_U32_NIL, SCS_VALUE_TYPE_dplacement,SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.WSTruckPlacement));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_speed,                      SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.SpeedometerSpeed));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,                 SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.RPM));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,                SCS_U32_NIL, SCS_VALUE_TYPE_s32,       SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Gear));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering,         SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Steering));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle,         SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Throttle));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake,            SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Brake));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch,           SCS_U32_NIL, SCS_VALUE_TYPE_float,     SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.Clutch));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity,      SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.LinearVelocity));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity,     SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.AngularVelocity));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration,  SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.LinearAcceleration));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration, SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.AngularAcceleration));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity,     SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.CabinAngularVelocity));
aRecipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration, SCS_U32_NIL, SCS_VALUE_TYPE_fvector,   SCS_TELEMETRY_CHANNEL_FLAG_none, Addr(fSharedMemory^.CabinAngularAcceleration));
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
        If (TempAttr^.index = Index) and TelemetrySameText(APIStringToTelemetryString(TempAttr^.name),Name) then
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
      If not TelemetrySameText(APIStringToTelemetryString(scs_telemetry_configuration_t(Data^).id),SCS_TELEMETRY_CONFIG_truck) then Exit;
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
If fMemoryMapping <> 0 then
  fSharedMemory := MapViewOfFile(fMemoryMapping,FILE_MAP_READ,0,0,0);
Result := (fMemoryMapping <> 0) and Assigned(fSharedMemory);
If not Result then Deinitialize
  else RetrieveCurrentState;
end;

//------------------------------------------------------------------------------

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
