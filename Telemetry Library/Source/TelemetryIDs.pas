{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Unit providing constans with precalculated IDs a others stuff around
          IDs generally.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-04-15)
@lastmod(2015-04-20)

  @bold(@NoAutoLink(TelemetryIDs))

  ©2013-2015 František Milt, all rights reserved.

  This unit contains definitions of identification number types, set of
  constants containing full configuration names, constants with IDs for
  individual configs and channel, as well as function used to obtain those IDs.

  Last change: 2015-04-20

  Change List:@unorderedList(
    @item(2014-04-15 - First stable version.)
    @item(2014-04-15 - Function GetItemID was moved to this unit.)
    @item(2014-04-18 - Constant @code(cConfigFieldsSeparator) was moved to this
                       unit.)
    @item(2014-10-23 - Added support for eut2 1.9.)
    @item(2014-10-23 - Added true constants when precomputed IDs are used
                       (optional, can be reconfigured to use writeable
                       constans instead).)
    @item(2014-11-07 - Added support for eut2 1.10.)
    @item(2015-04-20 - Constant @code(cConfigFieldsSeparator) renamed to
                       ConfigFieldsSeparator.)
    @item(2015-04-20 - Slight implementation changes.)
    @item(2015-07-14 - Started rework of configs...)
    @item(2015-07-14 - Removed types @code(TConfigID) and @code(PConfigID).)
    @item(2015-07-14 - Removed full config names.)
    @item(2015-07-14 - Removed config ID constants.)
    @item(2015-07-14 - Function @code(ConfigMergeIDAndAttribute) renamed to
                       FullConfigName, renamed parameters.)
    @item(2015-07-14 - Function @code(ConfigRemoveIDFromName) renamed to
                       ExtractConfigAttributeName, renamed parameters.)
    @item(2015-07-14 - Added new variant of function FullConfigName returning
                       filled config reference.))
    
@html(<hr>)

  Table of precomputed IDs for channel names.@br
  IDs are stored in constans whose identifiers corresponds to indetifiers of
  constants containing string value. For example, for name stored in constant
  @code(SCS_TELEMETRY_CHANNEL_local_scale), the ID is stored in constant
  SCS_TELEMETRY_CHANNEL_ID_local_scale.
  @table(
  @rowHead(@cell(Channel name constant identifier)                            @cell(String value)                               @cell(Precomputed ID (hexadecimal)))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_local_scale))                        @cell(@code(local.scale))                         @cell(@code(33DE67E4)))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_game_time))                          @cell(@code(game.time))                           @cell(@code(40333D16)))
  @row(@cell(@code(SCS_TELEMETRY_CHANNEL_next_rest_stop))                     @cell(@code(rest.stop))                           @cell(@code(0B878C8C)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_connected))                  @cell(@code(trailer.connected))                   @cell(@code(7007CCEE)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_world_placement))            @cell(@code(trailer.world.placement))             @cell(@code(3A729370)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity))      @cell(@code(trailer.velocity.linear))             @cell(@code(AE80C1D0)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity))     @cell(@code(trailer.velocity.angular))            @cell(@code(A2981BDF)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration))  @cell(@code(trailer.acceleration.linear))         @cell(@code(99A57FA9)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration)) @cell(@code(trailer.acceleration.angular))        @cell(@code(8B76F7F9)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis))               @cell(@code(trailer.wear.chassis))                @cell(@code(E071BE1A)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection))      @cell(@code(trailer.wheel.suspension.deflection)) @cell(@code(1E44DA91)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground))            @cell(@code(trailer.wheel.on_ground))             @cell(@code(9A68642F)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance))            @cell(@code(trailer.wheel.substance))             @cell(@code(1DFBFCF1)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity))             @cell(@code(trailer.wheel.angular_velocity))      @cell(@code(C387243E)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering))             @cell(@code(trailer.wheel.steering))              @cell(@code(3B417600)))
  @row(@cell(@code(SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation))             @cell(@code(trailer.wheel.rotation))              @cell(@code(EA941F6D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_world_placement))              @cell(@code(truck.world.placement))               @cell(@code(6B48D06B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity))        @cell(@code(truck.local.velocity.linear))         @cell(@code(5D9D7AB3)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity))       @cell(@code(truck.local.velocity.angular))        @cell(@code(76D03686)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration))    @cell(@code(truck.local.acceleration.linear))     @cell(@code(A2E5F90F)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration))   @cell(@code(truck.local.acceleration.angular))    @cell(@code(B4F8B1A2)))
  @row(@cell(@code(*SCS_TELEMETRY_TRUCK_CHANNEL_cabin_orientation))           @cell(@code(truck.cabin.orientation))             @cell(@code(36F3F15D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset))                 @cell(@code(truck.cabin.offset))                  @cell(@code(303DEF2A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity))       @cell(@code(truck.cabin.velocity.angular))        @cell(@code(10976F36)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration))   @cell(@code(truck.cabin.acceleration.angular))    @cell(@code(D10EF7A8)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_head_offset))                  @cell(@code(truck.head.offset))                   @cell(@code(EEE287E8)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_speed))                        @cell(@code(truck.speed))                         @cell(@code(4E839148)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm))                   @cell(@code(truck.engine.rpm))                    @cell(@code(160E7B38)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear))                  @cell(@code(truck.engine.gear))                   @cell(@code(9582C042)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_steering))               @cell(@code(truck.input.steering))                @cell(@code(DCFA7E3B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle))               @cell(@code(truck.input.throttle))                @cell(@code(CF8FC74B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_brake))                  @cell(@code(truck.input.brake))                   @cell(@code(5EEDB702)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch))                 @cell(@code(truck.input.clutch))                  @cell(@code(F5ECF339)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering))           @cell(@code(truck.effective.steering))            @cell(@code(94181EAB)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle))           @cell(@code(truck.effective.throttle))            @cell(@code(876DA7DB)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake))              @cell(@code(truck.effective.brake))               @cell(@code(47E5F7F0)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch))             @cell(@code(truck.effective.clutch))              @cell(@code(A6466849)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control))               @cell(@code(truck.cruise_control))                @cell(@code(C31E2094)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot))                @cell(@code(truck.hshifter.slot))                 @cell(@code(36C98B9D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector))            @cell(@code(truck.hshifter.select))               @cell(@code(E4A50350)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake))                @cell(@code(truck.brake.parking))                 @cell(@code(5664B035)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake))                  @cell(@code(truck.brake.motor))                   @cell(@code(8E0C8ABA)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level))               @cell(@code(truck.brake.retarder))                @cell(@code(A8D6B016)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure))           @cell(@code(truck.brake.air.pressure))            @cell(@code(9384DD05)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning))   @cell(@code(truck.brake.air.pressure.warning))    @cell(@code(C58F8B5A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency)) @cell(@code(truck.brake.air.pressure.emergency))  @cell(@code(78FAD40D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature))            @cell(@code(truck.brake.temperature))             @cell(@code(E1AE4E3F)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel))                         @cell(@code(truck.fuel.amount))                   @cell(@code(C298DD2D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning))                 @cell(@code(truck.fuel.warning))                  @cell(@code(9D0FD9A2)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption))     @cell(@code(truck.fuel.consumption.average))      @cell(@code(013149D4)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue))                     @cell(@code(truck.adblue))                        @cell(@code(8D32829D)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning))             @cell(@code(truck.adblue.warning))                @cell(@code(FF3464CB)))
  @row(@cell(@code(**SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption)) @cell(@code(truck.adblue.consumption.average))    @cell(@code(C253FC24)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure))                 @cell(@code(truck.oil.pressure))                  @cell(@code(A368F9A6)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning))         @cell(@code(truck.oil.pressure.warning))          @cell(@code(1A1815C5)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature))              @cell(@code(truck.oil.temperature))               @cell(@code(405A67E9)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature))            @cell(@code(truck.water.temperature))             @cell(@code(B8B46564)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning))    @cell(@code(truck.water.temperature.warning))     @cell(@code(783F3300)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage))              @cell(@code(truck.battery.voltage))               @cell(@code(91BB0105)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning))      @cell(@code(truck.battery.voltage.warning))       @cell(@code(26000473)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled))             @cell(@code(truck.electric.enabled))              @cell(@code(9D4D7843)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled))               @cell(@code(truck.engine.enabled))                @cell(@code(FACA0BF9)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_lblinker))                     @cell(@code(truck.lblinker))                      @cell(@code(A7B8351B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_rblinker))                     @cell(@code(truck.rblinker))                      @cell(@code(CE891602)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker))               @cell(@code(truck.light.lblinker))                @cell(@code(ECC0AC62)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker))               @cell(@code(truck.light.rblinker))                @cell(@code(85F18F7B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_parking))                @cell(@code(truck.light.parking))                 @cell(@code(6931D205)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam))               @cell(@code(truck.light.beam.low))                @cell(@code(612D677D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam))              @cell(@code(truck.light.beam.high))               @cell(@code(7E93DFB5)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front))              @cell(@code(truck.light.aux.front))               @cell(@code(D6464C43)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof))               @cell(@code(truck.light.aux.roof))                @cell(@code(5ADBA32B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon))                 @cell(@code(truck.light.beacon))                  @cell(@code(990180CD)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_brake))                  @cell(@code(truck.light.brake))                   @cell(@code(E2790B7B)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse))                @cell(@code(truck.light.reverse))                 @cell(@code(71711168)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wipers))                       @cell(@code(truck.wipers))                        @cell(@code(EE7920A7)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight))          @cell(@code(truck.dashboard.backlight))           @cell(@code(91DA5D6D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine))                  @cell(@code(truck.wear.engine))                   @cell(@code(D89A5F14)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission))            @cell(@code(truck.wear.transmission))             @cell(@code(ABB45C97)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin))                   @cell(@code(truck.wear.cabin))                    @cell(@code(49F699F6)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis))                 @cell(@code(truck.wear.chassis))                  @cell(@code(BC2A6A7A)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels))                  @cell(@code(truck.wear.wheels))                   @cell(@code(7C35EF18)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_odometer))                     @cell(@code(truck.odometer))                      @cell(@code(F988B0E0)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection))        @cell(@code(truck.wheel.suspension.deflection))   @cell(@code(369CAB49)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground))              @cell(@code(truck.wheel.on_ground))               @cell(@code(CB522734)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance))              @cell(@code(truck.wheel.substance))               @cell(@code(4CC1BFEA)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocit))                @cell(@code(truck.wheel.angular_velocity))        @cell(@code(B7B25C28)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering))               @cell(@code(truck.wheel.steering))                @cell(@code(DF025731)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation))               @cell(@code(truck.wheel.rotation))                @cell(@code(0ED73E5C)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift))                   @cell(@code(truck.wheel.lift))                    @cell(@code(F8BC370D)))
  @row(@cell(@code(SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset))            @cell(@code(truck.wheel.lift.offset))             @cell(@code(046B7B44)))
  )
  @code(*) - This constant does not actually exist (it was removed from
  telemetry SDK and replaced by @code(SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset)).@br
  @code(**) - These channels does not work in current SDK.@br

@html(<hr>)}
unit TelemetryIDs;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
  CRC32,
  TelemetryCommon,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_telemetry_common_configs
{$IFNDEF PrecomputedItemID},
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels;
{$ELSE};
{$ENDIF}
{$ENDIF}

{==============================================================================}
{   Types and general constants                                                }
{==============================================================================}

const
  //:Character used as a separator for config + config_attribute conglomerate.
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
{$IFDEF TrueIDConstants}
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
// Writeable constants.

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

{$IFNDEF PrecomputedItemID}
{:
  @abstract(Procedure calculating identification numbers for channels and
  configs.)
  When you call this routine, it recalculates all ID constants. Use it only when
  you truly need this recalculation.@br
  It is called automatically in initialization section of this unit when neither
  of @code(PrecomputedItemID) and @code(ManualItemIDCompute) switches is
  defined.
}
  procedure InitializeItemsIDs;
{$ENDIF}

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
  Returns full config reference build from config ID and attribute name.

  @param ID        ID of configuration.
  @param Attribute Name of attribute.

  @returns Full config reference.
}
  Function FullConfigName(const ID, Attribute: TelemetryString): TConfigReference; overload;

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

{$IFNDEF PrecomputedItemID}
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

//------------------------------------------------------------------------------

Function FullConfigName(const ID, Attribute: TelemetryString): TConfigReference;
begin
Result.ID := ID;
Result.Attribute := Attribute;
end;

//------------------------------------------------------------------------------

Function ExtractConfigAttributeName(const FullConfigName,ID: TelemetryString): TelemetryString;
begin
Result := Copy(FullConfigName,Length(ID) + Length(ConfigFieldsSeparator) + 1, Length(FullConfigName));
end;

{==============================================================================}
{   Initialization section                                                     }
{==============================================================================}

{$IF not Defined(PrecomputedItemID) and not Defined(ManualItemIDCompute)}
initialization
  InitializeItemsIDs;
{$IFEND}

end.
