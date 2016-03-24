{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Reimplementation of telemetry_position example distributed with the
          SDK.)
@author(František Milt <fmilt@seznam.cz>)
@created(2015-07-12)
@lastmod(2016-03-22)

  @bold(@NoAutoLink(TelemetrySCSExample_telemetry_position))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-22

  This unit contains a class that is designed to imitate behavior of original
  @italic(telemetry_position) example distributed with the Telemetry SDK. Output
  log file created by this reimplementation should be exactly the same as is
  produced by the original C++ implementation.

@html(<hr>)}
unit TelemetrySCSExample_telemetry_position;

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
  TelemetryRecipientBinder
{$IFNDEF Documentation},
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2,
  scssdk_ats,
  scssdk_telemetry_ats;
{$ENDIF}
{$ELSE};
{$ENDIF}

const
  //:Default name of the file used for log output in TSCSExm_TelemetryPosition
  //:class.
  def_LogFileName = 'telemetry_position.log';

type
{:
  Structure used to hold data produced by the telemetry API and some
  intermediate values in TSCSExm_TelemetryPosition class.

  @member(CabinPosition  Cabin position in the world space. Calculated.)
  @member(HeadPosition   Head position in the world space. Calculated.)
  @member(TruckPlacement World space position and orientation of the truck.
                         Returend by the telemetry, channel
                         @code(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement).)
  @member(CabinOffset    Vehicle space position and orientation delta of the
                         cabin from its default position. Returned by the
                         telemetry, channel
                         @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset).)
  @member(HeadOffset     Cabin space position and orientation delta of the
                         driver head from its default position. Returned by the
                         telemtry, channel
                         @code(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset).)
}
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
{:
  @abstract(Class designed to imitate behavior of @italic(telemetry_position)
  example distributed with the Telemetry SDK.)
  It writes data to a textual log file whe same way as mentioned example, so
  parsers intended to read such log should have no problem reading log produced
  by this class.@br
  Note that there may be slight behavioral differences as a consequence of use
  of different language and programming style (object instead of functions), but
  they should not pose any problem.

  It writes world-space position of driver's head on per-frame basis to the
  output.@br
  Resulting file can then look like this (@code(<...>) means a part of the text
  is removed):
  @preformatted(
  Log opened
  Game 'eut2' 1.12
  WARNING: Head position unavailable
  -59601.889437;51.311265;-7795.376735
  -59601.889422;51.308747;-7795.376725
  -59601.890405;51.312907;-7795.378027
  -59601.889879;51.317791;-7795.377263
  -59601.888888;51.322672;-7795.375585
  -59601.887792;51.327103;-7795.373351

  <...>

  -59734.282434;51.712648;-7890.447486
  -59734.282899;51.712515;-7890.447426
  -59734.283343;51.712392;-7890.447367
  Log ended)
}
  TSCSExm_TelemetryPosition = class(TTelemetryRecipientBinder)
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
    Structure holding data obtained from the telemetry and some calculated
    values.
  }
    fTelemetry:       TSCSExm_TelemetryPositionState;
  protected
  {:
    Initialializes log.@br
    Called from the class constructor.

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
    SCS_TELEMETRY_EVENT_frame_end
    SCS_TELEMETRY_EVENT_paused
    SCS_TELEMETRY_EVENT_started
    SCS_TELEMETRY_EVENT_configuration))
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
    done inside this method as are all vector calculations. For details see
    method implementation.@br
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
    appropriate field (see fTelemetry field) for further processing.@br
    For details on parameters, see @inherited.
  }    
    procedure ChannelHandler(Sender: TObject; const {%H-}Name: TelemetryString; {%H-}ID: TChannelID; {%H-}Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
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

procedure TSCSExm_TelemetryPosition.FinishLog;
begin
fLog.AddLogNoTime('Log ended');
end;

{------------------------------------------------------------------------------}
{   TSCSExm_TelemetryPosition // Public methods                                }
{------------------------------------------------------------------------------}

constructor TSCSExm_TelemetryPosition.Create(Recipient: TTelemetryRecipient; const LogFileName: String = def_LogFileName);
begin
inherited Create(Recipient);
If not Assigned(Recipient) then
  raise ETLNilReference.Create('TSCSExm_TelemetryPosition.Create: Recipient is not assigned.');
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
    raise ETLInitFailed.Create('TSCSExm_TelemetryPosition.Create: Log initialization failed.');
  end;
fLog.AddLogNoTime('Game ''' + TelemetryStringDecode(Recipient.GameID) + ''' '
                            + IntToStr(SCSGetMajorVersion(Recipient.GameVersion)) + '.'
                            + IntToStr(SCSGetMinorVersion(Recipient.GameVersion)));
If TelemetrySameStr(Recipient.GameID, SCS_GAME_ID_EUT2) then
  begin
    If Recipient.GameVersion < SCS_TELEMETRY_EUT2_GAME_VERSION_1_00 then
      fLog.AddLogNoTime('WARNING: Too old version of the game, some features might behave incorrectly');
    If SCSGetMajorVersion(Recipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT) then
      fLog.AddLogNoTime('WARNING: Too new major version of the game, some features might behave incorrectly');
  end
else If TelemetrySameStr(Recipient.GameID, SCS_GAME_ID_ATS) then
  begin
    If Recipient.GameVersion < SCS_TELEMETRY_ATS_GAME_VERSION_1_00 then
      fLog.AddLogNoTime('WARNING: Too old version of the game, some features might behave incorrectly');
    If SCSGetMajorVersion(Recipient.GameVersion) > SCSGetMajorVersion(SCS_TELEMETRY_ATS_GAME_VERSION_CURRENT) then
      fLog.AddLogNoTime('WARNING: Too new major version of the game, some features might behave incorrectly');
  end
else fLog.AddLogNoTime('WARNING: Unsupported game, some features or values might behave incorrectly');
If not (Recipient.EventRegister(SCS_TELEMETRY_EVENT_frame_end) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_paused) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_started) and
        Recipient.EventRegister(SCS_TELEMETRY_EVENT_configuration)) then
  begin
    Recipient.Log(SCS_LOG_TYPE_error,'Unable to register event callbacks');
    raise ETLRegFailed.Create('TSCSExm_TelemetryPosition.Create: Events registration failed.');
  end;
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,SCS_U32_NIL,SCS_VALUE_TYPE_dplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.TruckPlacement));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset,SCS_U32_NIL,SCS_VALUE_TYPE_fplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.CabinOffset));
Recipient.ChannelRegister(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset,SCS_U32_NIL,SCS_VALUE_TYPE_fplacement,SCS_TELEMETRY_CHANNEL_FLAG_none,Addr(fTelemetry.HeadOffset));
FillChar(fTelemetry,SizeOf(TSCSExm_TelemetryPositionState),0);
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

procedure TSCSExm_TelemetryPosition.ConfigHandler(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t);
begin
// nothing to do here
end;

end.
