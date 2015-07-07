{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Information provider class (known telemetry events, channels, etc.).)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-07)
@lastmod(2015-06-28)

  @bold(@NoAutoLink(TelemetryInfoProvider))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryInfoProvider class (see class declaration for
  details).

  Last change:  2015-06-28

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
                               the list)))
    @item(2015-06-28 - Type TChannelValueTypePriority and method
                       TTelemetryInfoProvider.ChannelGetValueType that is using
                       this type in one of its parameters are now both marked as
                       deprecated.)
    @item(2015-06-28 - Removed file inclusion, all content moved directly into
                       this unit.))

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
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels;
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryInfoProvider                            }
{------------------------------------------------------------------------------}
{==============================================================================}

type
{: @deprecated
  Used to distinguish which value type should method
  TTelemetryInfoProvider.ChannelGetValueType return for given channel.
  @value(cvtpPrimary   Basic value type.)
  @value(cvtpSecondary Second used type (e.g. double for float types, u64 for
                       u32, ...).)
  @value(cvtpTertiary  Third type (e.g. euler for [f/d]placement).)
}
  TChannelValueTypePriority = (cvtpPrimary, cvtpSecondary, cvtpTertiary){$IFDEF FPC}deprecated{$ENDIF}; 

{==============================================================================}
{   TTelemetryInfoProvider // Class declaration                                }
{==============================================================================}
{:
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
  exception.
}
  TTelemetryInfoProvider = class(TTelemetryVersionPrepareObject)
  private
  {:
    Holds state indicating whether current instance is user managed (when not,
    it is managed automatically).@br
    This field is set automatically in constructor(s).
  }
    fUserManaged:   Boolean;
  {:
    See KnownEvents property.
  }
    fKnownEvents:   TKnownEventsList;
  {:
    See KnownChannels property.
  }
    fKnownChannels: TKnownChannelsList;
  {:
    See KnownConfigs property.
  }
    fKnownConfigs:  TKnownConfigsList;
  protected
  {:
    Preparation for telemetry 1.0.
  }
    procedure Prepare_Telemetry_1_0; override;
  {:
    Preparation for eut2 1.0.
  }
    procedure Prepare_Game_eut2_1_0; override;
  {:
    Preparation for eut2 1.1.
  }
    procedure Prepare_Game_eut2_1_1; override;
  {:
    Preparation for eut2 1.2.
  }
    procedure Prepare_Game_eut2_1_2; override;
  {:
    Preparation for eut2 1.4.
  }
    procedure Prepare_Game_eut2_1_4; override;
  {:
    Preparation for eut2 1.9.
  }
    procedure Prepare_Game_eut2_1_9; override;
  {:
    Preparation for eut2 1.10.
  }
    procedure Prepare_Game_eut2_1_10; override;
  public
  {:
    Basic object constructor.@br

    Call this no-parameter constructor when creating user managed info provider.
    Lists of known items are created empty.
  }
    constructor Create; overload;
  {:
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
  {:
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
  {:
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
  {:
    Object destructor.@br
    Internal lists are automatically cleared in destructor, so it is unnecessary
    to @noAutoLink(clear) them explicitly.
  }
    destructor Destroy; override;
  {:
    When current instance is created as user managed, calling this procedure
    will clear all internal lists. When it is called on automatically managed
    object, it does nothing.)
  }
    procedure Clear;
  {:
    Returns internal (ie. not defined by the API) name of passed event.

    @param Event Event whose name is requested.

    @returns(Name of given event or an empty string when no such event is
             known.)
  }
    Function EventGetName(Event: scs_event_t): TelemetryString; virtual;
  {:@deprecated   
    Returns type of value for given channel and selected priority.

    @param Name         Name of requested channel.
    @param TypePriority Priority of value type that should be returned.

    @returns(Type of value for selected channel and priority. When requested
             channel is not found, @code(SCS_VALUE_TYPE_INVALID) is returned.)
  }             
    Function ChannelGetValueType(const Name: TelemetryString; TypePriority: TChannelValueTypePriority = cvtpPrimary): scs_value_type_t; overload; virtual;
      deprecated {$IFDEF DeprecatedMessage}'Please use other methods with the same name instead of this one.'{$ENDIF};
  {:
    Returns type of value for given channel and selected priority.

    @param Name         Name of requested channel.
    @param TypePriority Priority of value type that should be returned. It must
                        be a number from interval <0,33), if it is not from this
                        interval, SCS_VALUE_TYPE_INVALID is returned.@br
                        0 corresponds to primary value type, 1 to first
                        secondary type, 2 to second secondary type and so on.
                        If selected type is beyond what a given channel can
                        support (eg. 5 for channel with only one secondary type),
                        SCS_VALUE_TYPE_INVALID is returned.

    @returns(Type of value for selected channel and priority. When requested
             channel is not found, @code(SCS_VALUE_TYPE_INVALID) is returned.)
  }
    Function ChannelGetValueType(const Name: TelemetryString; TypePriority: Integer): scs_value_type_t; overload; virtual;
  published
  {:
    @True when current instance is user managed, @false when it is managed
    automatically.
  }
    property UserManaged: Boolean read fUserManaged;
  {:
    List containing informations about known telemetry events.
  }
    property KnownEvents: TKnownEventsList read fKnownEvents;
  {:
    List containing informations about known telemetry channels.
  }
    property KnownChannels: TKnownChannelsList read fKnownChannels;
  {:
    List containing informations about known telemetry configs.
  }
    property KnownConfigs: TKnownConfigsList read fKnownConfigs;
  end;

{==============================================================================}
{   Unit functions and procedures // Declaration                               }
{==============================================================================}

{:
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

{:
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

{:
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

{:
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
{   Unit functions and procedures // Implementation                            }
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
//=== Adding Events ============================================================
// Adding known events to internal list.

with fKnownEvents do
  begin
    Add(SCS_TELEMETRY_EVENT_invalid,       'Invalid',       False, False);
    Add(SCS_TELEMETRY_EVENT_frame_start,   'Frame start',   True,  False);
    Add(SCS_TELEMETRY_EVENT_frame_end,     'Frame end',     True,  True);
    Add(SCS_TELEMETRY_EVENT_paused,        'Paused',        True,  False);
    Add(SCS_TELEMETRY_EVENT_started,       'Started',       True,  False);
    Add(SCS_TELEMETRY_EVENT_configuration, 'Configuration', True,  False);
  end;

//=== Adding Channels ==========================================================
// Adding known channels to internal list.

with fKnownChannels do
  begin
    //--- Global ---------------------------------------------------------------
    Add(SCS_TELEMETRY_CHANNEL_local_scale,                        SCS_VALUE_TYPE_float,      False);

    //--- Trailer specific -----------------------------------------------------
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_connected,                  SCS_VALUE_TYPE_bool,       False);
    // Movement
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement,            SCS_VALUE_TYPE_dplacement, False);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity,      SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity,     SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration,  SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration, SCS_VALUE_TYPE_fvector,    False);
    // Damage
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis,               SCS_VALUE_TYPE_float,      False);
    // Wheels
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection,      SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground,            SCS_VALUE_TYPE_bool,       True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance,            SCS_VALUE_TYPE_u32,        True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity,             SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering,             SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation,             SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,7);

    //--- Truck specific -------------------------------------------------------
    // Movement
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,              SCS_VALUE_TYPE_dplacement, False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity,        SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity,       SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration,    SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration,   SCS_VALUE_TYPE_fvector,    False);
    Add('truck.cabin.orientation',                                SCS_VALUE_TYPE_fplacement, False); // later replaced with cabin_offset
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity,       SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration,   SCS_VALUE_TYPE_fvector,    False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset,                  SCS_VALUE_TYPE_fplacement, False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_speed,                        SCS_VALUE_TYPE_float,      False);
    // Engine
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,                   SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,                  SCS_VALUE_TYPE_s32,        False);
    // Driving
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering,               SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle,               SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake,                  SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch,                 SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering,           SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle,           SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake,              SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch,             SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control,               SCS_VALUE_TYPE_float,      False);
    // Gearbox
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot,                SCS_VALUE_TYPE_u32,        False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector,            SCS_VALUE_TYPE_bool,       True, SCS_TELEMETRY_CONFIG_hshifter_ATTRIBUTE_selector_count,1);
    // Brakes
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake,                SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake,                  SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level,               SCS_VALUE_TYPE_u32,        False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure,           SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning,   SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature,            SCS_VALUE_TYPE_float,      False);
    // Consumables
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel,                         SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning,                 SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption,     SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue,                       SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning,               SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption,   SCS_VALUE_TYPE_float,      False);
    // Oil
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure,                 SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning,         SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature,              SCS_VALUE_TYPE_float,      False);
    // Temperature
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature,            SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning,    SCS_VALUE_TYPE_bool,       False);
    // Battery
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage,              SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning,      SCS_VALUE_TYPE_bool,       False);
    // Enabled state of various elements
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled,             SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled,               SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker,                     SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker,                     SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking,                SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam,               SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam,              SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front,              SCS_VALUE_TYPE_u32,        False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof,               SCS_VALUE_TYPE_u32,        False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon,                 SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake,                  SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse,                SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wipers,                       SCS_VALUE_TYPE_bool,       False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight,          SCS_VALUE_TYPE_float,      False);
    // Wear
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine,                  SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission,            SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin,                   SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis,                 SCS_VALUE_TYPE_float,      False);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels,                  SCS_VALUE_TYPE_float,      False);
    // Odometer
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_odometer,                     SCS_VALUE_TYPE_float,      False);
    // Wheels
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection,        SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground,              SCS_VALUE_TYPE_bool,       True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance,              SCS_VALUE_TYPE_u32,        True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity,               SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering,               SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation,               SCS_VALUE_TYPE_float,      True, SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,7);
  end;

//=== Adding Configs ===========================================================
// Adding known configurations to internal list.

with fKnownConfigs do
  begin
    Add(SCS_TELEMETRY_CONFIG_substances_ATTRIBUTE_id,                   SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_controls_ATTRIBUTE_shifter_type,           SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_hshifter_ATTRIBUTE_selector_count,         SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_hshifter_ATTRIBUTE_slot_gear,              SCS_VALUE_TYPE_s32,     True);
    Add(SCS_TELEMETRY_CONFIG_hshifter_ATTRIBUTE_slot_handle_position,   SCS_VALUE_TYPE_u32,     True);
    Add(SCS_TELEMETRY_CONFIG_hshifter_ATTRIBUTE_slot_selectors,         SCS_VALUE_TYPE_u32,     True);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_brand_id,                  SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_brand,                     SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_id,                        SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_name,                      SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_fuel_capacity,             SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_fuel_warning_factor,       SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_adblue_capacity,           SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_air_pressure_warning,      SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_oil_pressure_warning,      SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_water_temperature_warning, SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_battery_voltage_warning,   SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_rpm_limit,                 SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_forward_gear_count,        SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_reverse_gear_count,        SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_retarder_step_count,       SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_cabin_position,            SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_head_position,             SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_hook_position,             SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_count,               SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_position,            SCS_VALUE_TYPE_fvector, True);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_steerable,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_simulated,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_truck_ATTRIBUTE_wheel_radius,              SCS_VALUE_TYPE_float,   True);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_id,                      SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_cargo_accessory_id,      SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_hook_position,           SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_count,             SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_position,          SCS_VALUE_TYPE_fvector, True);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_steerable,         SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_simulated,         SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_trailer_ATTRIBUTE_wheel_radius,            SCS_VALUE_TYPE_float,   True);
  end;
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
  raise Exception.CreateFmt('TTelemetryInfoProvider.Create: Telemetry version (%s) is not supported.',
                            [SCSGetVersionAsString(TelemetryVersion)]);
If not PrepareForGameVersion('',GameID,GameVersion) then
  raise Exception.CreateFmt('TTelemetryInfoProvider.Create: Game version (%s %s) is not supported.',
                            [TelemetryStringDecode(GameID),SCSGetVersionAsString(GameVersion)]);
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
