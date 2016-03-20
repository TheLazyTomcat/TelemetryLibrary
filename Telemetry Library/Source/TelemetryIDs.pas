{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Unit providing constans with precalculated channel IDs a others stuff
          around IDs generally.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-04-15)
@lastmod(2016-03-20)

  @bold(@NoAutoLink(TelemetryIDs))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-20 

  This unit contains definitions of identification number types, set of
  constants containing IDs for individual channels, as well as function used to
  obtain those IDs and more.

@html(<hr>)

  Table of precomputed IDs for channel names.@br
  IDs are stored in constans whose identifiers corresponds to indetifiers of
  constants containing string value. For example, for name stored in constant
  @code(SCS_TELEMETRY_CHANNEL_local_scale), the ID is stored in constant
  SCS_TELEMETRY_CHANNEL_ID_local_scale.
  @table(
  @rowHead(@cell(Channel name constant identifier)                            @cell(String value)                               @cell(Precomputed ID))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_local_scale))                        @cell(@code(local.scale))                         @cell(@code(0x33DE67E4)))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_game_time))                          @cell(@code(game.time))                           @cell(@code(0x40333D16)))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_next_rest_stop))                     @cell(@code(rest.stop))                           @cell(@code(0x0B878C8C)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_connected))                  @cell(@code(trailer.connected))                   @cell(@code(0x7007CCEE)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement))            @cell(@code(trailer.world.placement))             @cell(@code(0x3A729370)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity))      @cell(@code(trailer.velocity.linear))             @cell(@code(0xAE80C1D0)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity))     @cell(@code(trailer.velocity.angular))            @cell(@code(0xA2981BDF)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration))  @cell(@code(trailer.acceleration.linear))         @cell(@code(0x99A57FA9)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration)) @cell(@code(trailer.acceleration.angular))        @cell(@code(0x8B76F7F9)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis))               @cell(@code(trailer.wear.chassis))                @cell(@code(0xE071BE1A)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection))      @cell(@code(trailer.wheel.suspension.deflection)) @cell(@code(0x1E44DA91)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground))            @cell(@code(trailer.wheel.on_ground))             @cell(@code(0x9A68642F)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance))            @cell(@code(trailer.wheel.substance))             @cell(@code(0x1DFBFCF1)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity))             @cell(@code(trailer.wheel.angular_velocity))      @cell(@code(0xC387243E)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering))             @cell(@code(trailer.wheel.steering))              @cell(@code(0x3B417600)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation))             @cell(@code(trailer.wheel.rotation))              @cell(@code(0xEA941F6D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement))              @cell(@code(truck.world.placement))               @cell(@code(0x6B48D06B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity))        @cell(@code(truck.local.velocity.linear))         @cell(@code(0x5D9D7AB3)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity))       @cell(@code(truck.local.velocity.angular))        @cell(@code(0x76D03686)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration))    @cell(@code(truck.local.acceleration.linear))     @cell(@code(0xA2E5F90F)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration))   @cell(@code(truck.local.acceleration.angular))    @cell(@code(0xB4F8B1A2)))
  @row(@cell(@code(*SCS_TELEMETRY_TRUCK_CHANNEL_cabin_orientation))           @cell(@code(truck.cabin.orientation))             @cell(@code(0x36F3F15D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset))                 @cell(@code(truck.cabin.offset))                  @cell(@code(0x303DEF2A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity))       @cell(@code(truck.cabin.velocity.angular))        @cell(@code(0x10976F36)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration))   @cell(@code(truck.cabin.acceleration.angular))    @cell(@code(0xD10EF7A8)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset))                  @cell(@code(truck.head.offset))                   @cell(@code(0xEEE287E8)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_speed))                        @cell(@code(truck.speed))                         @cell(@code(0x4E839148)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm))                   @cell(@code(truck.engine.rpm))                    @cell(@code(0x160E7B38)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear))                  @cell(@code(truck.engine.gear))                   @cell(@code(0x9582C042)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_displayed_gear))               @cell(@code(truck.displayed.gear))                @cell(@code(0x1566ECE2)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering))               @cell(@code(truck.input.steering))                @cell(@code(0xDCFA7E3B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle))               @cell(@code(truck.input.throttle))                @cell(@code(0xCF8FC74B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake))                  @cell(@code(truck.input.brake))                   @cell(@code(0x5EEDB702)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch))                 @cell(@code(truck.input.clutch))                  @cell(@code(0xF5ECF339)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering))           @cell(@code(truck.effective.steering))            @cell(@code(0x94181EAB)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle))           @cell(@code(truck.effective.throttle))            @cell(@code(0x876DA7DB)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake))              @cell(@code(truck.effective.brake))               @cell(@code(0x47E5F7F0)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch))             @cell(@code(truck.effective.clutch))              @cell(@code(0xA6466849)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control))               @cell(@code(truck.cruise_control))                @cell(@code(0xC31E2094)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot))                @cell(@code(truck.hshifter.slot))                 @cell(@code(0x36C98B9D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector))            @cell(@code(truck.hshifter.select))               @cell(@code(0xE4A50350)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake))                @cell(@code(truck.brake.parking))                 @cell(@code(0x5664B035)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake))                  @cell(@code(truck.brake.motor))                   @cell(@code(0x8E0C8ABA)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level))               @cell(@code(truck.brake.retarder))                @cell(@code(0xA8D6B016)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure))           @cell(@code(truck.brake.air.pressure))            @cell(@code(0x9384DD05)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning))   @cell(@code(truck.brake.air.pressure.warning))    @cell(@code(0xC58F8B5A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency)) @cell(@code(truck.brake.air.pressure.emergency))  @cell(@code(0x78FAD40D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature))            @cell(@code(truck.brake.temperature))             @cell(@code(0xE1AE4E3F)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel))                         @cell(@code(truck.fuel.amount))                   @cell(@code(0xC298DD2D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning))                 @cell(@code(truck.fuel.warning))                  @cell(@code(0x9D0FD9A2)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption))     @cell(@code(truck.fuel.consumption.average))      @cell(@code(0x013149D4)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue))                     @cell(@code(truck.adblue))                        @cell(@code(0x8D32829D)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning))             @cell(@code(truck.adblue.warning))                @cell(@code(0xFF3464CB)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption)) @cell(@code(truck.adblue.consumption.average))    @cell(@code(0xC253FC24)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure))                 @cell(@code(truck.oil.pressure))                  @cell(@code(0xA368F9A6)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning))         @cell(@code(truck.oil.pressure.warning))          @cell(@code(0x1A1815C5)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature))              @cell(@code(truck.oil.temperature))               @cell(@code(0x405A67E9)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature))            @cell(@code(truck.water.temperature))             @cell(@code(0xB8B46564)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning))    @cell(@code(truck.water.temperature.warning))     @cell(@code(0x783F3300)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage))              @cell(@code(truck.battery.voltage))               @cell(@code(0x91BB0105)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning))      @cell(@code(truck.battery.voltage.warning))       @cell(@code(0x26000473)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled))             @cell(@code(truck.electric.enabled))              @cell(@code(0x9D4D7843)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled))               @cell(@code(truck.engine.enabled))                @cell(@code(0xFACA0BF9)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker))                     @cell(@code(truck.lblinker))                      @cell(@code(0xA7B8351B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker))                     @cell(@code(truck.rblinker))                      @cell(@code(0xCE891602)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker))               @cell(@code(truck.light.lblinker))                @cell(@code(0xECC0AC62)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker))               @cell(@code(truck.light.rblinker))                @cell(@code(0x85F18F7B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking))                @cell(@code(truck.light.parking))                 @cell(@code(0x6931D205)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam))               @cell(@code(truck.light.beam.low))                @cell(@code(0x612D677D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam))              @cell(@code(truck.light.beam.high))               @cell(@code(0x7E93DFB5)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front))              @cell(@code(truck.light.aux.front))               @cell(@code(0xD6464C43)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof))               @cell(@code(truck.light.aux.roof))                @cell(@code(0x5ADBA32B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon))                 @cell(@code(truck.light.beacon))                  @cell(@code(0x990180CD)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake))                  @cell(@code(truck.light.brake))                   @cell(@code(0xE2790B7B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse))                @cell(@code(truck.light.reverse))                 @cell(@code(0x71711168)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wipers))                       @cell(@code(truck.wipers))                        @cell(@code(0xEE7920A7)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight))          @cell(@code(truck.dashboard.backlight))           @cell(@code(0x91DA5D6D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine))                  @cell(@code(truck.wear.engine))                   @cell(@code(0xD89A5F14)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission))            @cell(@code(truck.wear.transmission))             @cell(@code(0xABB45C97)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin))                   @cell(@code(truck.wear.cabin))                    @cell(@code(0x49F699F6)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis))                 @cell(@code(truck.wear.chassis))                  @cell(@code(0xBC2A6A7A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels))                  @cell(@code(truck.wear.wheels))                   @cell(@code(0x7C35EF18)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_odometer))                     @cell(@code(truck.odometer))                      @cell(@code(0xF988B0E0)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_distance))          @cell(@code(truck.navigation.distance))           @cell(@code(0x0F2E1B05)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_time))              @cell(@code(truck.navigation.time))               @cell(@code(0x8F92E692)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_speed_limit))       @cell(@code(truck.navigation.speed.limit))        @cell(@code(0x89699C9D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection))        @cell(@code(truck.wheel.suspension.deflection))   @cell(@code(0x369CAB49)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground))              @cell(@code(truck.wheel.on_ground))               @cell(@code(0xCB522734)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance))              @cell(@code(truck.wheel.substance))               @cell(@code(0x4CC1BFEA)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocit))                @cell(@code(truck.wheel.angular_velocity))        @cell(@code(0xB7B25C28)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering))               @cell(@code(truck.wheel.steering))                @cell(@code(0xDF025731)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation))               @cell(@code(truck.wheel.rotation))                @cell(@code(0x0ED73E5C)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift))                   @cell(@code(truck.wheel.lift))                    @cell(@code(0xF8BC370D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset))            @cell(@code(truck.wheel.lift.offset))             @cell(@code(0x046B7B44)))
  )
  @code(*) - This constant does not actually exist (it was removed from
  telemetry SDK and replaced by @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset)).@br
  @code(**) - These channels are not working until version eut2 1.12.@br

@html(<hr>)}
unit TelemetryIDs;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  CRC32,
{$ENDIF}
  TelemetryCommon
{$IFNDEF Documentation},
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk
{$IFNDEF ID_TrueConstants},
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels;
{$ELSE};
{$ENDIF}
{$ENDIF}
{$ELSE};
{$ENDIF}

{==============================================================================}
{   Types and general constants                                                }
{==============================================================================}

const
  //:Character used as a separator for config + config_attribute conglomerate
  //:(full config name).
  ConfigFieldsSeparator = '.';

type
  //:General item identifier. All other item identifiers are of this type.
  TItemID = TCRC32;
  //:Pointer to a variable of type TItemID
  PItemID = ^TITemID;

  //:Channel identifier obtained from its name.
  TChannelID = TItemID;
  //:Pointer to a variable of type TChannelID.
  PChannelID = ^TChannelID;

const  
{$IFDEF ID_TrueConstants}
// True constants.

{==============================================================================}
{   Channel IDs                                                                }
{==============================================================================}

  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_local_scale) channel.
  SCS_TELEMETRY_CHANNEL_ID_local_scale                        = TChannelID($33DE67E4);
  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_game_time) channel.
  SCS_TELEMETRY_CHANNEL_ID_game_time                          = TChannelID($40333D16);
  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_next_rest_stop) channel.
  SCS_TELEMETRY_CHANNEL_ID_next_rest_stop                     = TChannelID($0B878C8C);

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_connected) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_connected                  = TChannelID($7007CCEE);

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_world_placement            = TChannelID($3A729370);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_velocity      = TChannelID($AE80C1D0);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_velocity     = TChannelID($A2981BDF);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_acceleration  = TChannelID($99A57FA9);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_acceleration = TChannelID($8B76F7F9);

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wear_chassis               = TChannelID($E071BE1A);

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_susp_deflection      = TChannelID($1E44DA91);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_on_ground            = TChannelID($9A68642F);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_substance            = TChannelID($1DFBFCF1);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_velocity             = TChannelID($C387243E);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_steering             = TChannelID($3B417600);
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_rotation             = TChannelID($EA941F6D);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_world_placement              = TChannelID($6B48D06B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_velocity        = TChannelID($5D9D7AB3);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_velocity       = TChannelID($76D03686);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_acceleration    = TChannelID($A2E5F90F);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_acceleration   = TChannelID($B4F8B1A2);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_orientation) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_orientation            = TChannelID($36F3F15D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_offset                 = TChannelID($303DEF2A);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_velocity       = TChannelID($10976F36);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_acceleration   = TChannelID($D10EF7A8);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_head_offset                  = TChannelID($EEE287E8);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_speed) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_speed                        = TChannelID($4E839148);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_rpm                   = TChannelID($160E7B38);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_gear                  = TChannelID($9582C042);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_displayed_gear) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_displayed_gear               = TChannelID($1566ECE2);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_steering               = TChannelID($DCFA7E3B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_throttle               = TChannelID($CF8FC74B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_brake                  = TChannelID($5EEDB702);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_clutch                 = TChannelID($F5ECF339);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_steering           = TChannelID($94181EAB);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_throttle           = TChannelID($876DA7DB);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_brake              = TChannelID($47E5F7F0);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_clutch             = TChannelID($A6466849);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cruise_control               = TChannelID($C31E2094);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_slot                = TChannelID($36C98B9D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_selector            = TChannelID($E4A50350);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_parking_brake                = TChannelID($5664B035);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_motor_brake                  = TChannelID($8E0C8ABA);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_retarder_level               = TChannelID($A8D6B016);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure           = TChannelID($9384DD05);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_warning   = TChannelID($C58F8B5A);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_temperature            = TChannelID($E1AE4E3F);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_emergency = TChannelID($78FAD40D);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel                         = TChannelID($C298DD2D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_warning                 = TChannelID($9D0FD9A2);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_average_consumption     = TChannelID($013149D4);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue                       = TChannelID($8D32829D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_warning               = TChannelID($FF3464CB);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_average_consumption   = TChannelID($C253FC24);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure                 = TChannelID($A368F9A6);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure_warning         = TChannelID($1A1815C5);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_temperature              = TChannelID($405A67E9);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature            = TChannelID($B8B46564);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature_warning    = TChannelID($783F3300);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage              = TChannelID($91BB0105);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage_warning      = TChannelID($26000473);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_electric_enabled             = TChannelID($9D4D7843);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_enabled               = TChannelID($FACA0BF9);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_lblinker                     = TChannelID($A7B8351B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_rblinker                     = TChannelID($CE891602);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_parking                = TChannelID($6931D205);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_rblinker               = TChannelID($85F18F7B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_lblinker               = TChannelID($ECC0AC62);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_low_beam               = TChannelID($612D677D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_high_beam              = TChannelID($7E93DFB5);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_front              = TChannelID($D6464C43);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_roof               = TChannelID($5ADBA32B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_beacon                 = TChannelID($990180CD);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_brake                  = TChannelID($E2790B7B);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_reverse                = TChannelID($71711168);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wipers) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wipers                       = TChannelID($EE7920A7);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_dashboard_backlight          = TChannelID($91DA5D6D);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_engine                  = TChannelID($D89A5F14);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_transmission            = TChannelID($ABB45C97);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_cabin                   = TChannelID($49F699F6);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_chassis                 = TChannelID($BC2A6A7A);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_wheels                  = TChannelID($7C35EF18);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_odometer) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_odometer                     = TChannelID($F988B0E0);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_distance) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_distance          = TChannelID($0F2E1B05);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_time) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_time              = TChannelID($8F92E692);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_speed_limit) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_speed_limit       = TChannelID($89699C9D);

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_susp_deflection        = TChannelID($369CAB49);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_on_ground              = TChannelID($CB522734);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_substance              = TChannelID($4CC1BFEA);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_velocity               = TChannelID($B7B25C28);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_steering               = TChannelID($DF025731);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_rotation               = TChannelID($0ED73E5C);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift                   = TChannelID($F8BC370D);
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift_offset            = TChannelID($046B7B44);

{$ELSE}
// Writeable constants (more-or-less initialized variables).

{$WRITEABLECONST ON}
{==============================================================================}
{   Channel IDs                                                                }
{==============================================================================}

  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_local_scale) channel.
  SCS_TELEMETRY_CHANNEL_ID_local_scale:                        TChannelID = $33DE67E4;
  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_game_time) channel.
  SCS_TELEMETRY_CHANNEL_ID_game_time:                          TChannelID = $40333D16;
  //:Identification number for @code(SCS_TELEMETRY_CHANNEL_next_rest_stop) channel.
  SCS_TELEMETRY_CHANNEL_ID_next_rest_stop:                     TChannelID = $0B878C8C;

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_connected) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_connected:                  TChannelID = $7007CCEE;

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_world_placement:            TChannelID = $3A729370;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_velocity:      TChannelID = $AE80C1D0;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_velocity:     TChannelID = $A2981BDF;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_acceleration:  TChannelID = $99A57FA9;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_acceleration: TChannelID = $8B76F7F9;

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wear_chassis:               TChannelID = $E071BE1A;

  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_susp_deflection:      TChannelID = $1E44DA91;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_on_ground:            TChannelID = $9A68642F;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_substance:            TChannelID = $1DFBFCF1;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_velocity:             TChannelID = $C387243E;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_steering:             TChannelID = $3B417600;
  //:Identification number for @code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation) channel.
  SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_rotation:             TChannelID = $EA941F6D;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_world_placement:              TChannelID = $6B48D06B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_velocity:        TChannelID = $5D9D7AB3;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_velocity:       TChannelID = $76D03686;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_acceleration:    TChannelID = $A2E5F90F;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_acceleration:   TChannelID = $B4F8B1A2;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_orientation) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_orientation:            TChannelID = $36F3F15D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_offset:                 TChannelID = $303DEF2A;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_velocity:       TChannelID = $10976F36;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_acceleration:   TChannelID = $D10EF7A8;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_head_offset:                  TChannelID = $EEE287E8;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_speed) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_speed:                        TChannelID = $4E839148;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_rpm:                   TChannelID = $160E7B38;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_gear:                  TChannelID = $9582C042;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_displayed_gear) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_displayed_gear:               TChannelID = $1566ECE2;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_steering:               TChannelID = $DCFA7E3B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_throttle:               TChannelID = $CF8FC74B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_brake:                  TChannelID = $5EEDB702;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_clutch:                 TChannelID = $F5ECF339;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_steering:           TChannelID = $94181EAB;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_throttle:           TChannelID = $876DA7DB;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_brake:              TChannelID = $47E5F7F0;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_clutch:             TChannelID = $A6466849;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_cruise_control:               TChannelID = $C31E2094;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_slot:                TChannelID = $36C98B9D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_selector:            TChannelID = $E4A50350;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_parking_brake:                TChannelID = $5664B035;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_motor_brake:                  TChannelID = $8E0C8ABA;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_retarder_level:               TChannelID = $A8D6B016;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure:           TChannelID = $9384DD05;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_warning:   TChannelID = $C58F8B5A;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_temperature:            TChannelID = $E1AE4E3F;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_emergency: TChannelID = $78FAD40D;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel:                         TChannelID = $C298DD2D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_warning:                 TChannelID = $9D0FD9A2;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_average_consumption:     TChannelID = $013149D4;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue:                       TChannelID = $8D32829D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_warning:               TChannelID = $FF3464CB;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_average_consumption:   TChannelID = $C253FC24;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure:                 TChannelID = $A368F9A6;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure_warning:         TChannelID = $1A1815C5;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_temperature:              TChannelID = $405A67E9;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature:            TChannelID = $B8B46564;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature_warning:    TChannelID = $783F3300;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage:              TChannelID = $91BB0105;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage_warning:      TChannelID = $26000473;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_electric_enabled:             TChannelID = $9D4D7843;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_enabled:               TChannelID = $FACA0BF9;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_lblinker:                     TChannelID = $A7B8351B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_rblinker:                     TChannelID = $CE891602;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_parking:                TChannelID = $6931D205;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_rblinker:               TChannelID = $85F18F7B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_lblinker:               TChannelID = $ECC0AC62;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_low_beam:               TChannelID = $612D677D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_high_beam:              TChannelID = $7E93DFB5;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_front:              TChannelID = $D6464C43;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_roof:               TChannelID = $5ADBA32B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_beacon:                 TChannelID = $990180CD;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_brake:                  TChannelID = $E2790B7B;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_reverse:                TChannelID = $71711168;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wipers) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wipers:                       TChannelID = $EE7920A7;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_dashboard_backlight:          TChannelID = $91DA5D6D;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_engine:                  TChannelID = $D89A5F14;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_transmission:            TChannelID = $ABB45C97;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_cabin:                   TChannelID = $49F699F6;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_chassis:                 TChannelID = $BC2A6A7A;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_wheels:                  TChannelID = $7C35EF18;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_odometer) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_odometer:                     TChannelID = $F988B0E0;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_distance) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_distance:          TChannelID = $0F2E1B05;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_time) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_time:              TChannelID = $8F92E692;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_speed_limit) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_speed_limit:       TChannelID = $89699C9D;

  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_susp_deflection:        TChannelID = $369CAB49;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_on_ground:              TChannelID = $CB522734;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_substance:              TChannelID = $4CC1BFEA;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_velocity:               TChannelID = $B7B25C28;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_steering:               TChannelID = $DF025731;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_rotation:               TChannelID = $0ED73E5C;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift:                   TChannelID = $F8BC370D;
  //:Identification number for @code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset) channel.
  SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift_offset:            TChannelID = $046B7B44;

{$WRITEABLECONST OFF}
{$ENDIF}

{==============================================================================}
{   Unit functions and procedures declarations                                 }
{==============================================================================}

{:
  @abstract(Function used to get identifier of passed item name.)
  At the moment, identifiers are implemented as CRC32 checksum of given item
  name (string).

  @param Item Item name.

  @returns Item identification number.
}
  Function GetItemID(const Item: TelemetryString): TItemID;

//------------------------------------------------------------------------------

{:
  @abstract(Returns textual representation of item ID.)

  @param ID Item ID to be converted to text.

  @returns Textual representation of passed ID.
}
  Function ItemIDToStr(ID: TItemID): String;

//------------------------------------------------------------------------------

{$IF not Defined(ID_TrueConstants) or Defined(Documentation)}
{:
  @abstract(Procedure calculating identification numbers for channels and
  configs.)
  When you call this routine, it recalculates all ID constants. Use it only when
  you truly need this recalculation (note that IDs are initialized).@br
  It is called automatically in initialization section of this unit when neither
  of switches @code(ID_TrueConstants) and @code(ID_ManualCompute) is defined.
}
  procedure InitializeItemsIDs;
{$IFEND}

//------------------------------------------------------------------------------

{:
  Returns full config name composed from config ID and attribute name separated
  by ConfigFieldsSeparator.

  @param ID        ID of configuration.
  @param Attribute Name of attribute.

  @returns Full config name.
}
  Function FullConfigName(const ID, Attribute: TelemetryString): TelemetryString; overload;

//------------------------------------------------------------------------------

{:
  Returns full config name composed from config ID and attribute name separated
  by ConfigFieldsSeparator.

  @param(ConfigReference Reference from which the ID and attribute names are
                         taken.)

  @returns Full config name.
}
  Function FullConfigName(ConfigReference: TConfigReference): TelemetryString; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Removes passed config ID from full config name.)
  @bold(Note) - function does not control whether passed name truly starts with
  given config ID. It simply removes number of characters corresponding to
  length of config ID from the start of config name.

  @param FullConfigName Full name of config from which ID should be removed.
  @param ID             ID that has be removed from passed config name.

  @returns Config attribute name.
}
  Function ExtractConfigAttributeName(const FullConfigName, ID: TelemetryString): TelemetryString;

implementation

{==============================================================================}
{   Unit functions and procedures implementation                               }
{==============================================================================}

Function GetItemID(const Item: TelemetryString): TItemID;
begin
Result := BufferCRC32(InitialCRC32,PUTF8Char(Item)^,Length(Item) * SizeOf(TUTF8Char));
end;

//------------------------------------------------------------------------------

Function ItemIDToStr(ID: TItemID): String;
begin
Result := CRC32ToStr(ID);
end;

//------------------------------------------------------------------------------

{$IFNDEF ID_TrueConstants}
procedure InitializeItemsIDs;
begin
//---   ---   ---   ---   ---   Channels IDs   ---   ---   ---   ---   ---   ---

SCS_TELEMETRY_CHANNEL_ID_local_scale                         := GetItemID(SCS_TELEMETRY_CHANNEL_local_scale);
SCS_TELEMETRY_CHANNEL_ID_game_time                           := GetItemID(SCS_TELEMETRY_CHANNEL_game_time);
SCS_TELEMETRY_CHANNEL_ID_next_rest_stop                      := GetItemID(SCS_TELEMETRY_CHANNEL_next_rest_stop);

SCS_TELEMETRY_TRAILER_CHANNEL_ID_connected                   := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_connected);

SCS_TELEMETRY_TRAILER_CHANNEL_ID_world_placement             := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_velocity       := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_velocity      := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_linear_acceleration   := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_local_angular_acceleration  := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration);

SCS_TELEMETRY_TRAILER_CHANNEL_ID_wear_chassis                := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis);

SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_susp_deflection       := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_on_ground             := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_substance             := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_velocity              := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_steering              := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering);
SCS_TELEMETRY_TRAILER_CHANNEL_ID_wheel_rotation              := GetItemID(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_world_placement               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_velocity         := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_velocity        := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_linear_acceleration     := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_local_angular_acceleration    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_orientation             := GetItemID('truck.cabin.orientation');

SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_offset                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_velocity        := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_cabin_angular_acceleration    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_head_offset                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_speed                         := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_speed);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_rpm                    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_gear                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_displayed_gear                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_displayed_gear);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_steering                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_throttle                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_brake                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_input_clutch                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_steering            := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_throttle            := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_brake               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_effective_clutch              := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_cruise_control                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_slot                 := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_hshifter_selector             := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_parking_brake                 := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_motor_brake                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_retarder_level                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure            := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_warning    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_temperature             := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_brake_air_pressure_emergency  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel                          := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_fuel);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_warning                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_fuel_average_consumption      := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue                        := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_adblue);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_warning                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_adblue_average_consumption    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_pressure_warning          := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_oil_temperature               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature             := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_water_temperature_warning     := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_battery_voltage_warning       := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_electric_enabled              := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_engine_enabled                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_lblinker                      := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_rblinker                      := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_parking                 := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_rblinker                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_lblinker                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_low_beam                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_high_beam               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_front               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_aux_roof                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_beacon                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_brake                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_light_reverse                 := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wipers                        := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wipers);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_dashboard_backlight           := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_engine                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_transmission             := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_cabin                    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_chassis                  := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wear_wheels                   := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_odometer                      := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_odometer);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_distance           := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_distance);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_time               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_time);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_navigation_speed_limit        := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_navigation_speed_limit);

SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_susp_deflection         := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_on_ground               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_substance               := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_velocity                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_steering                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_rotation                := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift                    := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift);
SCS_TELEMETRY_TRUCK_CHANNEL_ID_wheel_lift_offset             := GetItemID(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function FullConfigName(const ID, Attribute: TelemetryString): TelemetryString;
begin
Result := ID + ConfigFieldsSeparator + Attribute;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function FullConfigName(ConfigReference: TConfigReference): TelemetryString;
begin
Result := ConfigReference.ID + ConfigFieldsSeparator + ConfigReference.Attribute;
end;

//------------------------------------------------------------------------------

Function ExtractConfigAttributeName(const FullConfigName,ID: TelemetryString): TelemetryString;
begin
Result := Copy(FullConfigName,Length(ID) + Length(ConfigFieldsSeparator) + 1, Length(FullConfigName));
end;

{==============================================================================}
{   Initialization section                                                     }
{==============================================================================}

{$IF not Defined(ID_TrueConstants) and not Defined(ID_ManualCompute)}
initialization
  InitializeItemsIDs;
{$IFEND}

end.
