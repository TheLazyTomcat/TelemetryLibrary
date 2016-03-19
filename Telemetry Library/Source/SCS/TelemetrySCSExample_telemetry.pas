{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Reimplementation of telemetry example distributed with the SDK.)
@author(František Milt <fmilt@seznam.cz>)
@created(2015-07-12)
@lastmod(2015-07-12)

  @bold(@NoAutoLink(TelemetrySCSExample_telemetry))

  ©2013-2015 František Milt, all rights reserved.

  Last change: 2015-07-12  

  This unit contains a class that is designed to imitate behavior of original
  @italic(telemetry) example distributed with the Telemetry SDK. Output log file
  created by this reimplementation should be exactly the same as is produced by
  the original C++ implementation.

@html(<hr>)}
unit TelemetrySCSExample_telemetry;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
  SimpleLog,
{$ENDIF}  
  TelemetryCommon,
  TelemetryIDs,
  TelemetryStrings,
  TelemetryRecipient,
  TelemetryRecipientBinder,
{$IFDEF CondensedHeaders}
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
  //:Default name of the file used for log output in TSCSExm_Telemetry class.
  def_LogFileName = 'telemetry.log';

type
{:
  Structure used to hold data produced by the telemetry API in TSCSExm_Telemetry
  class.

  @member Timestamp                     Frame @noAutoLink(timestamp).
  @member RawRenderingTimestamp         Time controlling the visualization.
  @member(RawSimulationTimestamp        Time controlling the physical
                                        simulation.)
  @member(RawPausedSimulationTimestamp  Similar to simulation time however it
                                        stops when the physics simulation is
                                        paused.)
  @member(OrientationAvailable          @True when orientation fields (Heading,
                                        Pitch, Roll) contains valid data, @false
                                        otherwise.)
  @member(Heading                       @noAutoLink(Heading) (horizontal
                                        direction) of the vehicle in degrees (0
                                        = north, 90 = west, ...).)
  @member(Pitch                         @noAutoLink(Pitch) of the vehicle in
                                        degrees (90 = up, -90 = down).)
  @member(Roll                          @noAutoLink(Roll) of the vehicle in
                                        degrees (-180 -- +180).)
  @member Speed                         @noAutoLink(Speed) of the vehicle in m/s.
  @member RPM                           Engine revolutions per minute.
  @member Gear                          Actual @noAutoLink(gear) engaged.
}
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
{   TSCSExm_Telemetry // Class declaration                                     }
{==============================================================================}
{:
  @abstract(Class designed to imitate behavior of @italic(telemetry) example
  distributed with the Telemetry SDK.)
  It writes data to a textual log file the same way as mentioned example, so
  parsers intended to read such log should have no problem reading log produced
  by this class.@br
  Note that there may be slight behavioral differences as a consequence of use
  of different language and programming style (object instead of functions), but
  they should not pose any problem.

  It writes complete configurations data when they are received and also
  following values on per-frame basis:
  @unorderedList(
    @item(Timestamp [@html(&#181)s])
    @item(Raw rendering timestamp [@html(&#181)s])
    @item(Raw simulation timestamp [@html(&#181)s])
    @item(Raw paused simulation timestamp [@html(&#181)s])
    @item(Vehicle heading [degrees])
    @item(Vehicle pitch [degrees])
    @item(Vehicle roll [degrees])
    @item(Speed of the vehicle [m/s])
    @item(Engine revolutions [min@html(<sup>-1</sup>)])
    @item(Engaged gead)
  )

  Resulting file can then look like this (@code(<...>) means a part of the text
  is removed):
  @preformatted(
  Log opened
  Game 'eut2' 1.12

  <...>

  Configuration: truck
    brand_id : string = volvo
    brand : string = Volvo
    id : string = vehicle.volvo.fh16_2012
    name : string = FH
    fuel.capacity : float = 800.000000
    adblue.capacity : float = 80.000000
    rpm.limit : float = 2500.000000
    gears.forward : u32 = 12
    gears.reverse : u32 = 1
    differential.ratio : float = 3.080000
    retarder.steps : u32 = 3
    forward.ratio[0] : float = 11.730000
    forward.ratio[1] : float = 9.210000
    forward.ratio[2] : float = 7.090000
    forward.ratio[3] : float = 5.570000
    forward.ratio[4] : float = 4.350000
    forward.ratio[5] : float = 3.410000
    forward.ratio[6] : float = 2.700000
    forward.ratio[7] : float = 2.120000
    forward.ratio[8] : float = 1.630000
    forward.ratio[9] : float = 1.280000
    forward.ratio[10] : float = 1.000000
    forward.ratio[11] : float = 0.780000
    reverse.ratio[0] : float = -10.780000
    cabin.position : fvector = (0.000000,1.288755,-1.445675)
    head.position : fvector = (-0.717515,1.403714,-0.448380)
    hook.position : fvector = (0.000000,1.000000,1.504521)
    fuel.warning.factor : float = 0.150000
    adblue.warning.factor : float = 0.150000
    brake.air.pressure.warning : float = 65.000000
    brake.air.pressure.emergency : float = 30.000000
    oil.pressure.warning : float = 10.000000
    water.temperature.warning : float = 105.000000
    battery.voltage.warning : float = 22.000000
    wheels.count : u32 = 6
    wheel.position[0] : fvector = (-1.088030,0.501832,-1.757469)
    wheel.steerable[0] : bool = True
    wheel.simulated[0] : bool = True
    wheel.radius[0] : float = 0.512435
    wheel.powered[0] : bool = False
    wheel.position[1] : fvector = (1.088030,0.501832,-1.757469)
    wheel.steerable[1] : bool = True
    wheel.simulated[1] : bool = True
    wheel.radius[1] : float = 0.512435
    wheel.powered[1] : bool = False
    wheel.position[2] : fvector = (-0.951700,0.501832,2.504161)
    wheel.steerable[2] : bool = False
    wheel.simulated[2] : bool = True
    wheel.radius[2] : float = 0.512435
    wheel.powered[2] : bool = True
    wheel.position[3] : fvector = (0.951700,0.501832,2.504161)
    wheel.steerable[3] : bool = False
    wheel.simulated[3] : bool = True
    wheel.radius[3] : float = 0.512435
    wheel.powered[3] : bool = True
    wheel.position[4] : fvector = (-0.951700,0.501832,1.184254)
    wheel.steerable[4] : bool = False
    wheel.simulated[4] : bool = True
    wheel.radius[4] : float = 0.512435
    wheel.powered[4] : bool = True
    wheel.position[5] : fvector = (0.951700,0.501832,1.184254)
    wheel.steerable[5] : bool = False
    wheel.simulated[5] : bool = True
    wheel.radius[5] : float = 0.512435
    wheel.powered[5] : bool = True
  Configuration: trailer
  Telemetry unpaused
  timestamp[us];raw rendering timestamp[us];raw simulation timestamp[us];raw paused simulation timestamp[us];<...>
  16666;4130731;4149834;16666;23.735498;-0.012581;0.000137;0.000000;0.000000;0
  33332;4130731;4166500;33332;23.735533;-0.010737;0.000205;0.000053;0.000000;0
  49998;4130731;4183166;49998;23.735512;0.074886;0.000382;0.001186;0.000000;0
  66664;4130731;4199832;66664;23.735493;0.137273;0.000451;0.001307;0.000000;0
  83330;4130731;4216498;83330;23.735472;0.188103;0.000437;0.000425;0.000000;0
  99996;4130731;4233164;99996;23.735458;0.226516;0.000328;-0.000958;0.000000;0

  <...>

  14682746;18766034;18815914;14682746;333.018005;-0.048385;-0.038235;7.095694;1073.287476;7
  14699412;18766034;18832580;14699412;333.103394;-0.048453;-0.035927;7.079689;1070.869019;7
  Telemetry paused
  Telemetry unpaused
  timestamp[us];raw rendering timestamp[us];raw simulation timestamp[us];raw paused simulation timestamp[us];<...>
  14716078;21770552;21799128;14716078;333.188049;-0.048453;-0.034479;7.063697;1068.457886;7

  <...>

  19115902;26140004;26198952;19115902;36.438198;-0.054396;-0.056363;8.529539;1289.598755;7
  Telemetry paused
  Log ended)
}
  TSCSExm_Telemetry = class(TTelemetryRecipientBinder)
  private
  {:
    Holds format settings used when converting floating point values to text.
  }
    fFormatSettings:  TFormatSettings;
  {:
    Object doing actual write into the output file.@br
    Managed internally.
  }
    fLog:             TSimpleLog;
  {:
    Name of the output log file.
  }
    fLogFileName:     String;
  {:
    When @True, output is paused and nothing is written to it (eg. when game is
    paused).
  }
    fOutputPaused:    Boolean;
  {:
    When @True, a header should be written into the output in next
    @code(SCS_TELEMETRY_EVENT_frame_end) event.
  }
    fPrintHeader:     Boolean;
  {:
    Timestamp of the last frame (@code(SCS_TELEMETRY_EVENT_frame_start) event).@br
    Initialized to -1. 
  }
    fLastTimestamp:   scs_timestamp_t;
  {:
    Structure holding data obtained from the telemetry.
  }
    fTelemetry:       TSCSExm_TelemetryState;
  protected
  {:
    Initialializes log.@br
    Called at the class instantiation.

    @returns @True when the log was initialized sucessfully, @false otherwise.
  }
    Function InitLog: Boolean; virtual;
  {:
    Finalizes log.@br
    Called on object destruction.
  }
    procedure FinishLog; virtual;
  public
  {:
    Class constructor.

    Manages everything what is necesary at the start of logging (clears fields,
    initializes log, registers events and channels, creates internal objects,
    ...).@br
    @code(Recipient) parameter must be assigned, otherwise an exception is
    raised. @code(LogFileName) must not be empty.

    @param(Recipient   Must contain valid reference to a TTelemetryRecipient
                       instance.)
    @param(LogFileName Name of the output log file. File is stored in the same
                       folder where a module containing this code is placed.)

    @raises ETLNilReference When @code(aRecipient) is not assigned.
    @raises(ETLInitFailed   When log initialization fails (when method InitLog
                            returns @false).)
    @raises(ETLRegFailed    When registration of any of the following telemetry
                            events fails:
    @preformatted(
    SCS_TELEMETRY_EVENT_frame_start
    SCS_TELEMETRY_EVENT_frame_end
    SCS_TELEMETRY_EVENT_paused
    SCS_TELEMETRY_EVENT_started))
  }
    constructor Create(Recipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
  {:
    Class destructor.@br

    Frees all internal objects and other used resources.
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
    Processes all events passed from the API. All writing to the output log is
    done inside this method. For details see method implementation.@br
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
    appropriate field (see fTelemetry fields) for further processing.@br
    For details on parameters, see @inherited.
  }
    procedure ChannelHandler(Sender: TObject; const {%H-}Name: TelemetryString; ID: TChannelID; {%H-}Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
  {:
    Does nothing in this implementation.@br
    For details see @inherited.
  }    
    procedure ConfigHandler(Sender: TObject; {%H-}ConfigReference: TConfigReference; {%H-}Index: scs_u32_t; {%H-}Value: scs_value_localized_t); override;
  end;

implementation

uses
  Windows
{$IF Defined(FPC) and not Defined(Unicode)}
  (*
    If compiler throws error that LazUTF8 unit cannot be found, you have to
    add LazUtils to required packages (Project > Project Inspector).
  *)
  , LazUTF8
{$IFEND};

{==============================================================================}
{   TSCSExm_Telemetry // Class implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TSCSExm_Telemetry // Protected methods                                     }
{------------------------------------------------------------------------------}

Function TSCSExm_Telemetry.InitLog: Boolean;
begin
try
  fLog.InternalLog := False;
  fLog.StreamFileAccessRights := fmShareDenyNone;
{$IF Defined(FPC) and not Defined(Unicode)}
  fLog.StreamFileName := ExtractFilePath(WinCPToUTF8(GetModuleName(hInstance))) + fLogFileName;
{$ELSE}
  fLog.StreamFileName := ExtractFilePath(GetModuleName(hInstance)) + fLogFileName;
{$IFEND}
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
{   TSCSExm_Telemetry // Public methods                                        }
{------------------------------------------------------------------------------}

constructor TSCSExm_Telemetry.Create(Recipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
begin
inherited Create(Recipient);
If not Assigned(Recipient) then
  raise ETLNilReference.Create('TSCSExm_Telemetry.Create: Recipient is not assigned.');
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fFormatSettings.DecimalSeparator := '.';
Recipient.KeepUtilityEvents := False;
Recipient.StoreConfigurations := False;
Recipient.EventUnregisterAll;
fLogFileName := LogFileName;
fLog := TSimpleLog.Create;
If not InitLog then
  begin
    Recipient.Log(SCS_LOG_TYPE_error,'Unable to initialize the log file');
    raise ETLInitFailed.Create('TSCSExm_Telemetry.Create: Log initialization failed.');
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
    raise ETLRegFailed.Create('TSCSExm_Telemetry.Create: Events registration failed.');
  end;
Recipient.EventRegister(SCS_TELEMETRY_EVENT_configuration);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,SCS_U32_NIL,SCS_VALUE_TYPE_euler,SCS_TELEMETRY_CHANNEL_FLAG_no_value);
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_speed,SCS_U32_NIL,SCS_VALUE_TYPE_float,SCS_TELEMETRY_CHANNEL_FLAG_no_value,Addr(fTelemetry.Speed));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,SCS_U32_NIL,SCS_VALUE_TYPE_float,SCS_TELEMETRY_CHANNEL_FLAG_no_value,Addr(fTelemetry.RPM));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,SCS_U32_NIL,SCS_VALUE_TYPE_s32,SCS_TELEMETRY_CHANNEL_FLAG_no_value,Addr(fTelemetry.Gear));
Recipient.Log('Initializing telemetry log example');
FillChar(fTelemetry,SizeOf(TSCSExm_TelemetryState),0);
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

procedure TSCSExm_Telemetry.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);

  procedure WriteDataToLog;
  var
    TempStr:  String;
  begin
    TempStr := IntToStr(fTelemetry.Timestamp) + ';' + IntToStr(fTelemetry.RawRenderingTimestamp) + ';' +
               IntToStr(fTelemetry.RawSimulationTimestamp) + ';' + IntToStr(fTElemetry.RawPausedSimulationTimestamp);
    If fTelemetry.OrientationAvailable then
      TempStr := TempStr + ';' + FloatToStrF(fTelemetry.Heading,ffFixed,15,6,fFormatSettings) +
                           ';' + FloatToStrF(fTelemetry.Pitch,ffFixed,15,6,fFormatSettings) +
                           ';' + FloatToStrF(fTelemetry.Roll,ffFixed,15,6,fFormatSettings)
    else
      TempStr := TempStr + ';---;---;---';
    TempStr := TempStr + ';' + FloatToStrF(fTelemetry.Speed,ffFixed,15,6,fFormatSettings) +
                         ';' + FloatToStrF(fTelemetry.RPM,ffFixed,15,6,fFormatSettings) +
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
            TempStr := TempStr + 'float = ' + FloatToStrF(TempAttr^.value.value_float.value,ffFixed,15,6,fFormatSettings);
          SCS_VALUE_TYPE_double:
            TempStr := TempStr + 'double = ' + FloatToStrF(TempAttr^.value.value_double.value,ffFixed,15,6,fFormatSettings);
          SCS_VALUE_TYPE_fvector:
            TempStr := TempStr + 'fvector = (' + FloatToStrF(TempAttr^.value.value_fvector.x,ffFixed,15,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_fvector.y,ffFixed,15,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_fvector.z,ffFixed,15,6,fFormatSettings) + ')';
          SCS_VALUE_TYPE_dvector:
            TempStr := TempStr + 'dvector = (' + FloatToStrF(TempAttr^.value.value_dvector.x,ffFixed,15,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_dvector.y,ffFixed,15,6,fFormatSettings) + ',' +
                                                 FloatToStrF(TempAttr^.value.value_dvector.z,ffFixed,15,6,fFormatSettings) + ')';
          SCS_VALUE_TYPE_euler:
            TempStr := TempStr + 'euler = h:' + FloatToStrF(TempAttr^.value.value_euler.heading * 360,ffFixed,15,6,fFormatSettings) +
                                        ' p:' + FloatToStrF(TempAttr^.value.value_euler.pitch * 360,ffFixed,15,6,fFormatSettings) +
                                        ' r:' + FloatToStrF(TempAttr^.value.value_euler.roll * 360,ffFixed,15,6,fFormatSettings);
          SCS_VALUE_TYPE_fplacement:
            TempStr := TempStr + 'fplacement = (' + FloatToStrF(TempAttr^.value.value_fplacement.position.x,ffFixed,15,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_fplacement.position.y,ffFixed,15,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_fplacement.position.z,ffFixed,15,6,fFormatSettings) +
                                           ') h:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.heading * 360,ffFixed,15,6,fFormatSettings) +
                                            ' p:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.pitch * 360,ffFixed,15,6,fFormatSettings) +
                                            ' r:' + FloatToStrF(TempAttr^.value.value_fplacement.orientation.roll * 360,ffFixed,15,6,fFormatSettings);
          SCS_VALUE_TYPE_dplacement:
            TempStr := TempStr + 'dplacement = (' + FloatToStrF(TempAttr^.value.value_dplacement.position.x,ffFixed,15,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_dplacement.position.y,ffFixed,15,6,fFormatSettings) + ',' +
                                                    FloatToStrF(TempAttr^.value.value_dplacement.position.z,ffFixed,15,6,fFormatSettings) +
                                           ') h:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.heading * 360,ffFixed,15,6,fFormatSettings) +
                                            ' p:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.pitch * 360,ffFixed,15,6,fFormatSettings) +
                                            ' r:' + FloatToStrF(TempAttr^.value.value_dplacement.orientation.roll * 360,ffFixed,15,6,fFormatSettings);
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

procedure TSCSExm_Telemetry.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
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
else
  If Assigned(Value) and Assigned(UserData) then
    case scs_value_t(Value^)._type of
      SCS_VALUE_TYPE_float: Single(UserData^) := scs_value_t(Value^).value_float.value;
      SCS_VALUE_TYPE_s32:   Integer(UserData^) := scs_value_t(Value^).value_s32.value;
    end;
end;

//------------------------------------------------------------------------------

procedure TSCSExm_Telemetry.ConfigHandler(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t);
begin
// nothing to do here
end;     

end.
