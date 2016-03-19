{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Information provider class (known telemetry events, channels, ...).)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-07)
@lastmod(2015-07-14)

  @bold(@NoAutoLink(TelemetryInfoProvider))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-19 

  This unit contains TTelemetryInfoProvider class (see class declaration for
  details).

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
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_telemetry_event,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels;
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryInfoProvider                            }
{------------------------------------------------------------------------------}
{==============================================================================}

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
type
  TTelemetryInfoProvider = class(TTelemetryVersionPrepareObject)
  private
  {:
    Holds state indicating whether current instance is user managed (when not,
    it is managed automatically).@br
    This field is set automatically in constructor(s).
  }
    fUserManaged:       Boolean;
  {:
    See KnownEvents property.
  }
    fKnownEvents:       TKnownEventsList;
  {:
    See KnownChannels property.
  }
    fKnownChannels:     TKnownChannelsList;
  {:
    See KnownConfigs property.
  }
    fKnownConfigs:      TKnownConfigsList;
  {:
    See MaxWheelCount property.
  }
    fMaxWheelCount:     scs_u32_t;
  {:
    See MaxSelectorCount property.
  }
    fMaxSelectorCount:  scs_u32_t;
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

    @raises(ETLUnsupportedAPI  When telemetry version is not supported by this
                               class or when preparation for it fails.)
    @raises(ETLUnsupportedGame When game and/or its version is not supported by
                               this class or when preparation for it fails.)
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
                            version information.)

    @raises(ETLUnsupportedAPI  When telemetry version is not supported by this
                               class or when preparation for it fails.)
    @raises(ETLUnsupportedGame When game and/or its version is not supported by
                               this class or when preparation for it fails.)                            
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
    object, it does nothing.
  }
    procedure Clear;
  {:
    Returns internal (ie. not defined by the API) name of passed event.

    @param Event Event whose name is requested.

    @returns(Name of given event or an empty string when no such event is
             known.)
  }
    Function EventGetName(Event: scs_event_t): TelemetryString; virtual;
  {:
    Returns type of value for given channel and selected priority.

    @param Name         Name of requested channel.
    @param(TypePriority Priority of value type that should be returned. It must
                        be a number from interval <0,33@), if it is not from this
                        interval, SCS_VALUE_TYPE_INVALID is returned.@br
                        0 corresponds to primary value type, 1 to first
                        secondary type, 2 to second secondary type and so on.
                        If selected type is beyond what a given channel can
                        support (eg. 5 for channel with only one secondary type),
                        SCS_VALUE_TYPE_INVALID is returned.)

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
    List containing information about known telemetry events.
  }
    property KnownEvents: TKnownEventsList read fKnownEvents;
  {:
    List containing information about known telemetry channels.
  }
    property KnownChannels: TKnownChannelsList read fKnownChannels;
  {:
    List containing information about known telemetry configs.
  }
    property KnownConfigs: TKnownConfigsList read fKnownConfigs;
  {:
    Maximum number of wheels for appropriate indexed channels.@br
    Default value 8. Should be set to maximum number of supported wheels
    in user-managed mode.
  }
    property MaxWheelCount: scs_u32_t read fMaxWheelCount write fMaxWheelCount;
  {:
    Maximum number of selectors for appropriate indexed channel.@br
    Default value 2. Should be set to maximum number of supported selectors
    in user-managed mode.
  }
    property MaxSelectorCount: scs_u32_t read fMaxSelectorCount write fMaxSelectorCount;
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
fMaxWheelCount := 8;
fMaxSelectorCount := 2;

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
    Add(SCS_TELEMETRY_CHANNEL_local_scale,                        SCS_VALUE_TYPE_float,      False, EmptyConfigReference);

    //--- Trailer specific -----------------------------------------------------
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_connected,                  SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    // Movement
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement,            SCS_VALUE_TYPE_dplacement, False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity,      SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity,     SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration,  SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration, SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    // Damage
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis,               SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Wheels
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection,      SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground,            SCS_VALUE_TYPE_bool,       True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance,            SCS_VALUE_TYPE_u32,        True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity,             SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering,             SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation,             SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_trailer,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);

    //--- Truck specific -------------------------------------------------------
    // Movement
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement,              SCS_VALUE_TYPE_dplacement, False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity,        SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity,       SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration,    SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration,   SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add('truck.cabin.orientation',                                SCS_VALUE_TYPE_fplacement, False, EmptyConfigReference); // later replaced with cabin_offset
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity,       SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration,   SCS_VALUE_TYPE_fvector,    False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset,                  SCS_VALUE_TYPE_fplacement, False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_speed,                        SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Engine
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm,                   SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear,                  SCS_VALUE_TYPE_s32,        False, EmptyConfigReference);
    // Driving
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering,               SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle,               SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake,                  SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch,                 SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering,           SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle,           SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake,              SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch,             SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control,               SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Gearbox
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot,                SCS_VALUE_TYPE_u32,        False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector,            SCS_VALUE_TYPE_bool,       True,  ConfigReference(SCS_TELEMETRY_CONFIG_hshifter,SCS_TELEMETRY_CONFIG_ATTRIBUTE_selector_count), MaxSelectorCount - 1);
    // Brakes
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake,                SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake,                  SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level,               SCS_VALUE_TYPE_u32,        False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure,           SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning,   SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature,            SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Consumables
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel,                         SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning,                 SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption,     SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue,                       SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning,               SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption,   SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Oil
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure,                 SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning,         SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature,              SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Temperature
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature,            SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning,    SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    // Battery
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage,              SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning,      SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    // Enabled state of various elements
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled,             SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled,               SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker,                     SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker,                     SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking,                SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam,               SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam,              SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front,              SCS_VALUE_TYPE_u32,        False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof,               SCS_VALUE_TYPE_u32,        False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon,                 SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake,                  SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse,                SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wipers,                       SCS_VALUE_TYPE_bool,       False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight,          SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Wear
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine,                  SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission,            SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin,                   SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis,                 SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels,                  SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Odometer
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_odometer,                     SCS_VALUE_TYPE_float,      False, EmptyConfigReference);
    // Wheels
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection,        SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground,              SCS_VALUE_TYPE_bool,       True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance,              SCS_VALUE_TYPE_u32,        True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity,               SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering,               SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
    Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation,               SCS_VALUE_TYPE_float,      True,  ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
  end;

//=== Adding Configs ===========================================================
// Adding known configurations to internal list.

with fKnownConfigs do
  begin
    Add(SCS_TELEMETRY_CONFIG_substances,SCS_TELEMETRY_CONFIG_ATTRIBUTE_id,                        SCS_VALUE_TYPE_string,  True);
    Add(SCS_TELEMETRY_CONFIG_controls,  SCS_TELEMETRY_CONFIG_ATTRIBUTE_shifter_type,              SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_hshifter,  SCS_TELEMETRY_CONFIG_ATTRIBUTE_selector_count,            SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_hshifter,  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_gear,                 SCS_VALUE_TYPE_s32,     True);
    Add(SCS_TELEMETRY_CONFIG_hshifter,  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_handle_position,      SCS_VALUE_TYPE_u32,     True);
    Add(SCS_TELEMETRY_CONFIG_hshifter,  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_selectors,            SCS_VALUE_TYPE_u32,     True);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_brand_id,                  SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_brand,                     SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_id,                        SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_name,                      SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_fuel_capacity,             SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_fuel_warning_factor,       SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_adblue_capacity,           SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_air_pressure_warning,      SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_oil_pressure_warning,      SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_water_temperature_warning, SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_battery_voltage_warning,   SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_rpm_limit,                 SCS_VALUE_TYPE_float,   False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_forward_gear_count,        SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_reverse_gear_count,        SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_retarder_step_count,       SCS_VALUE_TYPE_u32,     False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_cabin_position,            SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_head_position,             SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_hook_position,             SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count,               SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_position,            SCS_VALUE_TYPE_fvector, True);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_steerable,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_simulated,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_truck,     SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_radius,              SCS_VALUE_TYPE_float,   True);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_id,                        SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_accessory_id,        SCS_VALUE_TYPE_string,  False);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_hook_position,             SCS_VALUE_TYPE_fvector, False);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count,               SCS_VALUE_TYPE_u32,     False, True);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_position,            SCS_VALUE_TYPE_fvector, True);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_steerable,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_simulated,           SCS_VALUE_TYPE_bool,    True);
    Add(SCS_TELEMETRY_CONFIG_trailer,   SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_radius,              SCS_VALUE_TYPE_float,   True);
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
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency,SCS_VALUE_TYPE_bool,False,EmptyConfigReference);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_air_pressure_emergency,SCS_VALUE_TYPE_float,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_2;
begin
inherited;
fKnownChannels.Replace('truck.cabin.orientation',SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset,SCS_VALUE_TYPE_fplacement,False,EmptyConfigReference);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_4;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker,SCS_VALUE_TYPE_bool,False,EmptyConfigReference);
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker,SCS_VALUE_TYPE_bool,False,EmptyConfigReference);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_9;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_CHANNEL_game_time,SCS_VALUE_TYPE_u32,False,EmptyConfigReference);
fKnownChannels.Add(SCS_TELEMETRY_CHANNEL_next_rest_stop,SCS_VALUE_TYPE_s32,False,EmptyConfigReference);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_mass,SCS_VALUE_TYPE_float,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_city_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_city,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_company_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_company,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_city_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_city,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_company_id,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_company,SCS_VALUE_TYPE_string,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_income,SCS_VALUE_TYPE_u64,False);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_job,SCS_TELEMETRY_CONFIG_ATTRIBUTE_delivery_time,SCS_VALUE_TYPE_u32,False);
end;

//------------------------------------------------------------------------------

procedure TTelemetryInfoProvider.Prepare_Game_eut2_1_10;
begin
inherited;
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift,SCS_VALUE_TYPE_float,True,ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
fKnownChannels.Add(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset,SCS_VALUE_TYPE_float,True,ConfigReference(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count), MaxWheelCount - 1);
fKnownConfigs.Add(SCS_TELEMETRY_CONFIG_truck,SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_liftable,SCS_VALUE_TYPE_bool,True);
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
fMaxWheelCount := 8;
fMaxSelectorCount := 2;
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
  raise ETLUnsupportedAPI.CreateFmt('TTelemetryInfoProvider.Create: Telemetry version (%s) is not supported.',
                                    [SCSGetVersionAsString(TelemetryVersion)]);
If not PrepareForGameVersion('',GameID,GameVersion) then
  raise ETLUnsupportedGame.CreateFmt('TTelemetryInfoProvider.Create: Game version (%s %s) is not supported.',
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
