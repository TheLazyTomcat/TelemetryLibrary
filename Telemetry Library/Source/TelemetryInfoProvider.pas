{@html(<hr>)
@abstract(Information provider class (known telemetry events, channels, etc.).)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-07)
@lastmod(2014-11-24)

  @bold(@NoAutoLink(TelemetryInfoProvider))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryInfoProvider class (see class declaration for
  details).

  Included files:@preformatted(
    .\Inc\TTelemetryInfoProvider.Prepare_Telemetry_1_0.pas
      Contains body of method TTelemetryInfoProvider.Prepare_Telemetry_1_0.)

  Last change:  2014-11-24

  Change List:@unorderedList(
    @item(2013-10-07 - First stable version.)
    @item(2014-04-15 - Type of parameter @code(Name) in method
                       TTelemetryInfoProvider.ChannelGetValueType changed to
                       @code(TelemetryString).)
    @item(2014-04-18 - Result type of method TTelemetryInfoProvider.EventGetName
                       changed to @code(TelemetryString).)
    @item(2014-04-27 - Added constructor mehod
                       TTelemetryInfoProvider.CreateCurrent.)
    @item(2014-05-04 - Following callback functions were added:@unorderedList(
                         @itemSpacing(Compact)
                         @item(InfoProviderGetChannelIDFromName)
                         @item(InfoProviderGetChannelNameFromID)
                         @item(InfoProviderGetConfigIDFromName)
                         @item(InfoProviderGetConfigNameFromID)))
    @item(2014-10-23 - Added support for eut2 1.9.)
    @item(2014-10-24 - Type of paramter @code(GameID) in first parametrized
                       constructor of class TTelemetryInfoProvider changed to
                       @code(TelemetryString).)
    @item(2014-11-07 - Added support for eut2 1.10.)
    @item(2014-11-07 - Implementation changes (new channels are not inserted to
                       the lists, they are only added).)
    @item(2014-11-24 - Changes due to a new system of storing and passing
                       secondary types of channel value. These changes include:
                       @unorderedList(
                         @itemSpacing(Compact)
                         @item(Reimplemented method
                               TTelemetryInfoProvider.ChannelGetValueType)
                         @item(Added new variant of method
                               TTelemetryInfoProvider.ChannelGetValueType)
                         @item(Reimplemented all additions of known channels to
                               the list))))

  ToDo:@unorderedList(
  @item(Add capability for loading information from file (text, ini, resources).))    

@html(<hr>)}
unit TelemetryInfoProvider;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
  TelemetryValueTypeUtils,
  TelemetryIDs,
  TelemetryLists,
  TelemetryVersionObjects,  
{$IFDEF Documentation}
  TelemetryCommon,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_telemetry_event,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2;
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryInfoProvider                            }
{------------------------------------------------------------------------------}
{==============================================================================}

type
{
  Used to distinguish which value type should method
  TTelemetryInfoProvider.ChannelGetValueType return for given channel.
  @value(cvtpPrimary   Basic value type.)
  @value(cvtpSecondary Second used type (e.g. double for float types, u64 for
                       u32, ...).)
  @value(cvtpTertiary  Third type (e.g. euler for [f/d]placement).)
}
  TChannelValueTypePriority = (cvtpPrimary, cvtpSecondary, cvtpTertiary);

{==============================================================================}
{   TTelemetryInfoProvider // Class declaration                                }
{==============================================================================}
{
  @abstract(@NoAutoLink(TTelemetryInfoProvider) class provide lists of all known
  game events, channels and configurations along with some methods operating on
  them.)

  It can be created in two ways, user managed or automatically managed.@br
  When created as user managed (using no-paramater constructor), the object is
  created with empty lists and it is up to the user to fill them (use methods of
  individual lists to do so).@br
  When automatically managed (object is created using parametrized constructor),
  the telemetry and game versions are passed to the constructor and it checks
  whether they are supported or not. If they are, the lists are filled
  accordingly to them, if they are not supported, the constructor raises an
  exception.@br


@member(fUserManaged
    Holds state indicating whether current instance is user managed (when not,
    it is managed automatically).@br
    This field is set automatically in constructor(s).)

@member(fKnownEvents See KnownEvents property.)

@member(fKnownChannels See KnownChannels property.)

@member(fKnownConfigs See KnownConfigs property.)


@member(Prepare_Telemetry_1_0 Preparation for telemetry 1.0.)

@member(Prepare_Game_eut2_1_0 Preparation for eut2 1.0.)

@member(Prepare_Game_eut2_1_1 Preparation for eut2 1.1.)

@member(Prepare_Game_eut2_1_2 Preparation for eut2 1.2.)

@member(Prepare_Game_eut2_1_4 Preparation for eut2 1.4.)

@member(Prepare_Game_eut2_1_9 Preparation for eut2 1.9.)

@member(Prepare_Game_eut2_1_10 Preparation for eut2 1.10.)


@member(Destroy
    Object destructor.@br
    Internal lists are automatically cleared in destructor, so it is unnecessary
    to @noAutoLink(clear) them explicitly.)

@member(Clear
    When current instance is created as user managed, calling this procedure
    will clear all internal lists. When it is called on automatically managed
    object, it does nothing.)

@member(EventGetName
    Returns internal (i.e. not defined by the API) name of passed event.

    @param Event Event whose name is requested.

    @returns(Name of given event or an empty string when no such event is
             known.))


@member(KnownEvents
    List containing informations about known telemetry events.)

@member(KnownChannels
    List containing informations about known telemetry channels.)

@member(KnownConfigs
    List containing informations about known telemetry configs.)

@member(UserManaged
    @True when current instance is user managed, @false when it is managed
    automatically.)
}
  TTelemetryInfoProvider = class(TTelemetryVersionPrepareObject)
  private
    fUserManaged:   Boolean;
    fKnownEvents:   TKnownEventsList;
    fKnownChannels: TKnownChannelsList;
    fKnownConfigs:  TKnownConfigsList;
  protected
    procedure Prepare_Telemetry_1_0; override;
    procedure Prepare_Game_eut2_1_0; override;
    procedure Prepare_Game_eut2_1_1; override;
    procedure Prepare_Game_eut2_1_2; override;
    procedure Prepare_Game_eut2_1_4; override;
    procedure Prepare_Game_eut2_1_9; override;
    procedure Prepare_Game_eut2_1_10; override;
  public
  {
    Basic object constructor.@br

    Call this no-parameter constructor when creating user managed info provider.
    Lists of known items are created empty.
  }
    constructor Create; overload;
  {
    Parameterized object constructor.@br

    Call this constructor when creating automatically managed info provider.@br
    Lists of known items are filled automatically in this constructor
    accordingly to passed telemetry and game versions.@br
    If passed telemetry/game versions are not supported then an exception is
    raised.

    @param TelemetryVersion Version of telemetry.
    @param GameID           Game identifier.
    @param GameVersion      Version of game.
  }
    constructor Create(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t); overload;
  {
    Parameterized object constructor.@br

    Works exactly the same as first parametrized constructor (actually calls
    it) - creates automatically managed instance.@br
    Lists of known items are filled automatically in this constructor
    accordingly to passed telemetry and game versions.@br
    If passed telemetry/game versions are not supported then an exception is
    raised.

    @param TelemetryVersion Version of telemetry.
    @param(Parameters       Structure containing other necessary game and
                            version informations.)
  }
    constructor Create(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t); overload;
  {
    Specialized object constructor.@br

    This constructor is designed to automatically fill lists with latest data
    available for passed game. It actually calls parametrized constructor with
    parameter @code(TelemetryVersion) set to value returned by function
    HighestSupportedTelemetryVersion, @code(GameID) set to passed game id and
    @code(GameVersion) set to value returned by function
    HighestSupportedGameVersion.

    @param GameID Game identifier.
  }
    constructor CreateCurrent(GameID: TelemetryString); virtual;
    destructor Destroy; override;
    procedure Clear;
    Function EventGetName(Event: scs_event_t): TelemetryString; virtual;
  {
    Returns type of value for given channel and selected priority.

    @param Name         Name of requested channel.
    @param TypePriority Priority of value type that should be returned.

    @returns(Type of value for selected channel and priority. When requested
             channel is not found, @code(SCS_VALUE_TYPE_INVALID) is returned.)
  }             
    Function ChannelGetValueType(const Name: TelemetryString; TypePriority: TChannelValueTypePriority = cvtpPrimary): scs_value_type_t; overload; virtual;
  {
    Returns type of value for given channel and selected priority.

    @param Name         Name of requested channel.
    @param TypePriority Priority of value type that should be returned. It must
                        be a number from interval <0,33), if it is not from this
                        interval, SCS_VALUE_TYPE_INVALID is returned.@br
                        0 corresponds to primary value type, 1 to first
                        secondary type, 2 to second secondary type and so on.
                        If selected type is beyond what a given channel can
                        support (ie. 5 for channel with only one secondary type),
                        SCS_VALUE_TYPE_INVALID is returned.

    @returns(Type of value for selected channel and priority. When requested
             channel is not found, @code(SCS_VALUE_TYPE_INVALID) is returned.)
  }
    Function ChannelGetValueType(const Name: TelemetryString; TypePriority: Integer): scs_value_type_t; overload; virtual;
  published
    property UserManaged: Boolean read fUserManaged;  
    property KnownEvents: TKnownEventsList read fKnownEvents;
    property KnownChannels: TKnownChannelsList read fKnownChannels;
    property KnownConfigs: TKnownConfigsList read fKnownConfigs;
  end;

{==============================================================================}
{   Unit Functions and procedures // Declaration                               }
{==============================================================================}

{
  @abstract(Function intended as callback for streaming functions, converting
            channel name to ID.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryInfoProvider object.

  @param Name                  Channel name to be converted to ID.
  @param(TelemetryInfoProvider TTelemetryInfoProvider object that will be used
                               for actual conversion.)

  @returns Channel ID obtained from passed name.
}
Function InfoProviderGetChannelIDFromName(const Name: TelemetryString; TelemetryInfoProvider: Pointer): TChannelID;

{
  @abstract(Function intended as callback for streaming functions, converting
            channel ID to name.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryInfoProvider object.

  @param ID                    Channel ID to be converted to name.
  @param(TelemetryInfoProvider TTelemetryInfoProvider object that will be used
                               for actual conversion.)

  @returns Channel name obtained from passed ID.
}
Function InfoProviderGetChannelNameFromID(ID: TChannelID; TelemetryInfoProvider: Pointer): TelemetryString;

{
  @abstract(Function intended as callback for streaming functions, converting
            config name to ID.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryInfoProvider object.

  @param Name                  Config name to be converted to ID.
  @param(TelemetryInfoProvider TTelemetryInfoProvider object that will be used
                               for actual conversion.)

  @returns Config ID obtained from passed name.
}
Function InfoProviderGetConfigIDFromName(const Name: TelemetryString; TelemetryInfoProvider: Pointer): TConfigID;

{
  @abstract(Function intended as callback for streaming functions, converting
            ID to config name.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryInfoProvider object.

  @param ID                    Config ID to be converted to name.
  @param(TelemetryInfoProvider TTelemetryInfoProvider object that will be used
                               for actual conversion.)

  @returns Config name obtained from passed ID.
}
Function InfoProviderGetConfigNameFromID(ID: TConfigID; TelemetryInfoProvider: Pointer): TelemetryString;


implementation

uses
  SysUtils,
  TelemetryCommon;

{==============================================================================}
{   Unit Functions and procedures // Implementation                            }
{==============================================================================}

Function InfoProviderGetChannelIDFromName(const Name: TelemetryString; TelemetryInfoProvider: Pointer): TChannelID;
begin
Result := TTelemetryInfoProvider(TelemetryInfoProvider).KnownChannels.ChannelNameToID(Name);
end;

//------------------------------------------------------------------------------

Function InfoProviderGetChannelNameFromID(ID: TChannelID; TelemetryInfoProvider: Pointer): TelemetryString;
begin
Result := TTelemetryInfoProvider(TelemetryInfoProvider).KnownChannels.ChannelIDToName(ID);
end;

//------------------------------------------------------------------------------

Function InfoProviderGetConfigIDFromName(const Name: TelemetryString; TelemetryInfoProvider: Pointer): TConfigID;
begin
Result := TTelemetryInfoProvider(TelemetryInfoProvider).KnownConfigs.ConfigNameToID(Name);
end;

//------------------------------------------------------------------------------

Function InfoProviderGetConfigNameFromID(ID: TConfigID; TelemetryInfoProvider: Pointer): TelemetryString;
begin
Result := TTelemetryInfoProvider(TelemetryInfoProvider).KnownConfigs.ConfigIDToName(ID);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryInfoProvider                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryInfoProvider // Class implementation                             }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryInfoProvider // Protected methods                                }
{------------------------------------------------------------------------------}

procedure TTelemetryInfoProvider.Prepare_Telemetry_1_0;
begin
inherited;
// As content of this function is rather monstrous, it is, for the sake of
// clarity, separated in its own file.
{$INCLUDE '.\Inc\TTelemetryInfoProvider.Prepare_Telemetry_1_0.pas'}
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_0;
begin
inherited;
fKnownChannels.Remove(SCS_TELEMETRY_TRUCK_CHANNEL_adblue);
fKnownChannels.Remove(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning);
fKnownChannels.Remove(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_1;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency,SCS_VALUE_TYPE_bool,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_air_pressure_emergency,SCS_VALUE_TYPE_float,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_2;
begin
inherited;
fKnownChannels.Replace('truck.cabin.orientation',SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset,SCS_VALUE_TYPE_fplacement,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_4;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker,SCS_VALUE_TYPE_bool,False);
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker,SCS_VALUE_TYPE_bool,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_9;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_CHANNEL_game_time,SCS_VALUE_TYPE_u32,False);
fKnownChannels.Add(SCS_TELEMETRY_CHANNEL_next_rest_stop,SCS_VALUE_TYPE_s32,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_cargo_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_cargo,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_cargo_mass,SCS_VALUE_TYPE_float,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_destination_city_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_destination_city,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_destination_company_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_destination_company,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_source_city_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_source_city,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_source_company_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_source_company,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_income,SCS_VALUE_TYPE_u64,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job_ATTRIBUTE_delivery_time,SCS_VALUE_TYPE_u32,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_10;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift,SCS_VALUE_TYPE_float,True,SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset,SCS_VALUE_TYPE_float,True,SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_liftable,SCS_VALUE_TYPE_bool,True);
end;

{------------------------------------------------------------------------------}
{   TTelemetryInfoProvider // Public methods                                   }
{------------------------------------------------------------------------------}

constructor TTelemetryInfoProvider.Create;
begin
inherited Create;
// User managed instance.
fUserManaged := True;
// Create lists.
fKnownEvents := TKnownEventsList.Create;
fKnownChannels := TKnownChannelsList.Create;
fKnownConfigs := TKnownConfigsList.Create;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryInfoProvider.Create(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t);
begin
// Call basic constructor to initialize lists.
Create;
// Automatically managed instance.
fUserManaged := False;
// Prepare for required telemetry/game version, raise exception on unsupported
// versions.
If not PrepareForTelemetryVersion(TelemetryVersion) then
  raise Exception.Create('TTelemetryInfoProvider.Create(...): Telemetry version (' +
    SCSGetVersionAsString(TelemetryVersion) + ') is not supported');
If not PrepareForGameVersion('',GameID,GameVersion) then
  raise Exception.Create('TTelemetryInfoProvider.Create(...): Game version (' +
    TelemetryStringDecode(GameID) + ' ' +
    SCSGetVersionAsString(GameVersion) + ') is not supported');
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryInfoProvider.Create(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t);
begin
Create(TelemetryVersion,APIStringToTelemetryString(Parameters.common.game_id),Parameters.common.game_version);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryInfoProvider.CreateCurrent(GameID: TelemetryString);
begin
Create(HighestSupportedTelemetryVersion,GameID,HighestSupportedGameVersion(GameID));
end;

//------------------------------------------------------------------------------

destructor TTelemetryInfoProvider.Destroy;
begin
fKnownConfigs.Free;
fKnownChannels.Free;
fKnownEvents.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Clear;
begin
If UserManaged then
  begin
    fKnownEvents.Clear;
    fKnownChannels.Clear;
    fKnownConfigs.Clear;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryInfoProvider.EventGetName(Event: scs_event_t): TelemetryString;
var
  Index: Integer;
begin
Index := fKnownEvents.IndexOf(Event);
If Index >= 0 then Result := fKnownEvents[Index].Name
  else Result := '';
end;

//------------------------------------------------------------------------------

Function TTelemetryInfoProvider.ChannelGetValueType(const Name: TelemetryString; TypePriority: TChannelValueTypePriority = cvtpPrimary): scs_value_type_t;
var
  Index:  Integer;
  Types:  TValueTypesArray;
begin
Index := fKnownChannels.IndexOf(Name);
If Index >= 0 then
  begin
    Types := BitmaskValueTypesAddPrimary(fKnownChannels[Index].SecondaryTypes,fKnownChannels[Index].PrimaryType);
    case TypePriority of
      cvtpPrimary:    Result := Types[0];
      cvtpSecondary:  Result := Types[1];
      cvtpTertiary:   Result := Types[2];
    else
      Result := SCS_VALUE_TYPE_invalid;
    end;
  end
else Result := SCS_VALUE_TYPE_invalid;
end;

//------------------------------------------------------------------------------

Function TTelemetryInfoProvider.ChannelGetValueType(const Name: TelemetryString; TypePriority: Integer): scs_value_type_t;
var
  Index:  Integer;
begin
Index := fKnownChannels.IndexOf(Name);
If (Index >= 0) and (TypePriority >= Low(TValueTypesArray)) and (TypePriority <= High(TValueTypesArray)) then
  Result := BitmaskValueTypesAddPrimary(fKnownChannels[Index].SecondaryTypes,fKnownChannels[Index].PrimaryType)[TypePriority]
else
  Result := SCS_VALUE_TYPE_invalid;
end;

end.
