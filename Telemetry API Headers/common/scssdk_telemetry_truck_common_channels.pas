(**
 * @file scssdk_telemetry_truck_common_channels.h
 *
 * @brief Truck telemetry specific constants for channels.
 *
 * This file defines truck specific telemetry constants which
 * might be used by more than one SCS game. See game-specific
 * file to determine which constants are supported by specific
 * game.
 *
 * Unless state otherwise, following rules apply.
 * @li Whenever channel has float based type (float, fvector, fplacement)
 *     is can also provide double based values (double, dvector, dplacement)
 *     and vice versa. Note that using the non-native type might incur
 *     conversion costs or cause precision loss (double->float in
 *     world-space context).
 * @li Whenever channel has u32 type is can also provide u64 value.
 *     Note that using the non-native type might incur conversion costs.
 * @li Whenever channel uses placement based type (dplacement, fplacement),
 *     it also supports euler type containg just the rotational part and
 *     dvector/fvector type containing just the positional part.
 * @li Indexed entries are using zero-based indices.
 *)
(*<unit>*)  
unit scssdk_telemetry_truck_common_channels;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;

(*<interface>*)
// Movement.

const
(**
 * @brief Represents world space position and orientation of the truck.
 *
 * Type: dplacement
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_world_placement             = TelemetryString('truck.world.placement');

(**
 * @brief Represents vehicle space linear velocity of the truck measured
 * in meters per second.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_velocity       = TelemetryString('truck.local.velocity.linear');

(**
 * @brief Represents vehicle space angular velocity of the truck measured
 * in rotations per second.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_velocity      = TelemetryString('truck.local.velocity.angular');

(**
 * @brief Represents vehicle space linear acceleration of the truck measured
 * in meters per second^2
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_local_linear_acceleration   = TelemetryString('truck.local.acceleration.linear');

(**
 * @brief Represents vehicle space angular acceleration of the truck meassured
 * in rotations per second^2
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_local_angular_acceleration  = TelemetryString('truck.local.acceleration.angular');

(**
 * @brief Represents a vehicle space position and orientation delta
 * of the cabin from its default position.
 *
 * Type: fplacement
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_cabin_offset                = TelemetryString('truck.cabin.offset');

(**
 * @brief Represents cabin space angular velocity of the cabin measured
 * in rotations per second.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_velocity      = TelemetryString('truck.cabin.velocity.angular');

 (**
 * @brief Represents cabin space angular acceleration of the cabin
 * measured in rotations per second^2
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_cabin_angular_acceleration  = TelemetryString('truck.cabin.acceleration.angular');

(**
 * @brief Represents a cabin space position and orientation delta
 * of the driver head from its default position.
 *
 * Note that this value might change rapidly as result of
 * the user switching between cameras or camera presets.
 *
 * Type: fplacement
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_head_offset                 = TelemetryString('truck.head.offset');

(**
 * @brief Speedometer speed in meters per second.
 *
 * Uses negative value to represent reverse movement.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_speed                       = TelemetryString('truck.speed');

// Powertrain related

(**
 * @brief RPM of the engine.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_engine_rpm                  = TelemetryString('truck.engine.rpm');

(**
 * @brief Gear currently selected in the engine.
 *
 * @li >0 - Forwad gears
 * @li 0 - Neutral
 * @li <0 - Reverse gears
 *
 * Type: s32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_engine_gear                 = TelemetryString('truck.engine.gear');

(**
 * @brief Gear currently displayed on dashboard.
 *
 * @li >0 - Forwad gears
 * @li 0 - Neutral
 * @li <0 - Reverse gears
 *
 * Type: s32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_displayed_gear              = TelemetryString('truck.displayed.gear');

// Driving

(**
 * @brief Steering received from input <-1;1>.
 *
 * Note that it is interpreted counterclockwise.
 *
 * If the user presses the steer right button on digital input
 * (e.g. keyboard) this value goes immediatelly to -1.0
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_input_steering              = TelemetryString('truck.input.steering');

(**
 * @brief Throttle received from input <0;1>
 *
 * If the user presses the forward button on digital input
 * (e.g. keyboard) this value goes immediatelly to 1.0
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_input_throttle              = TelemetryString('truck.input.throttle');

(**
 * @brief Brake received from input <0;1>
 *
 * If the user presses the brake button on digital input
 * (e.g. keyboard) this value goes immediatelly to 1.0
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_input_brake                 = TelemetryString('truck.input.brake');

(**
 * @brief Clutch received from input <0;1>
 *
 * If the user presses the clutch button on digital input
 * (e.g. keyboard) this value goes immediatelly to 1.0
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_input_clutch                = TelemetryString('truck.input.clutch');

(**
 * @brief Steering as used by the simulation <-1;1>
 *
 * Note that it is interpreted counterclockwise.
 *
 * Accounts for interpolation speeds and simulated
 * counterfoces for digital inputs.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_effective_steering          = TelemetryString('truck.effective.steering');

(**
 * @brief Throttle pedal input as used by the simulation <0;1>
 *
 * Accounts for the press attack curve for digital inputs
 * or cruise-control input.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_effective_throttle          = TelemetryString('truck.effective.throttle');

(**
 * @brief Brake pedal input as used by the simulation <0;1>
 *
 * Accounts for the press attack curve for digital inputs. Does
 * not contain retarder, parking or motor brake.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_effective_brake             = TelemetryString('truck.effective.brake');

(**
 * @brief Clutch pedal input as used by the simulation <0;1>
 *
 * Accounts for the automatic shifting or interpolation of
 * player input.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_effective_clutch            = TelemetryString('truck.effective.clutch');

(**
 * @brief Speed selected for the cruise control in m/s
 *
 * Is zero if cruise control is disabled.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_cruise_control              = TelemetryString('truck.cruise_control');

// Gearbox related

(**
 * @brief Gearbox slot the h-shifter handle is currently in.
 *
 * 0 means that no slot is selected.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_slot               = TelemetryString('truck.hshifter.slot');

(**
 * @brief Enabled state of range/splitter selector toggles.
 *
 * Mapping between the range/splitter functionality and
 * selector index is described by HSHIFTER configuration.
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_hshifter_selector           = TelemetryString('truck.hshifter.select');

 // Brakes.

(**
 * @brief Is the parking brake enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_parking_brake               = TelemetryString('truck.brake.parking');

(**
 * @brief Is the motor brake enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_motor_brake                 = TelemetryString('truck.brake.motor');

(**
 * @brief Current level of the retarder.
 *
 * <0;max> where 0 is disabled retarder and max is maximal
 * value found in TRUCK configuration.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_retarder_level              = TelemetryString('truck.brake.retarder');

(**
 * @brief Pressure in the brake air tank in psi
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure          = TelemetryString('truck.brake.air.pressure');

(**
 * @brief Is the air pressure warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_warning  = TelemetryString('truck.brake.air.pressure.warning');

(**
 * @brief Are the emergency brakes active as result of low air pressure?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_brake_air_pressure_emergency = TelemetryString('truck.brake.air.pressure.emergency');

(**
 * @brief Temperature of the brakes in degrees celsius.
 *
 * Aproximated for entire truck, not at the wheel level.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_brake_temperature           = TelemetryString('truck.brake.temperature');

// Various = TelemetryString('consumables'

(**
 * @brief Amount of fuel in liters
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_fuel                        = TelemetryString('truck.fuel.amount');

(**
 * @brief Is the low fuel warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_fuel_warning                = TelemetryString('truck.fuel.warning');

(**
 * @brief Average consumption of the fuel in liters/km
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_fuel_average_consumption    = TelemetryString('truck.fuel.consumption.average');

(**
 * @brief Estimated range of truck with current amount of fuel in km
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_fuel_range                  = TelemetryString('truck.fuel.range');

(**
 * @brief Amount of AdBlue in liters
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_adblue                      = TelemetryString('truck.adblue');

(**
 * @brief Is the low adblue warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_adblue_warning              = TelemetryString('truck.adblue.warning');

(**
 * @brief Average consumption of the adblue in liters/km
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_adblue_average_consumption  = TelemetryString('truck.adblue.consumption.average');

// Oil

(**
 * @brief Pressure of the oil in psi
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure                = TelemetryString('truck.oil.pressure');

(**
 * @brief Is the oil pressure warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_oil_pressure_warning        = TelemetryString('truck.oil.pressure.warning');

(**
 * @brief Temperature of the oil in degrees celsius.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_oil_temperature             = TelemetryString('truck.oil.temperature');

// Temperature in various systems.

(**
 * @brief Temperature of the water in degrees celsius.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature           = TelemetryString('truck.water.temperature');

(**
 * @brief Is the water temperature warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_water_temperature_warning   = TelemetryString('truck.water.temperature.warning');

// Battery

(**
 * @brief Voltage of the battery in volts.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage             = TelemetryString('truck.battery.voltage');

(**
 * @brief Is the battery voltage/not charging warning active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_battery_voltage_warning     = TelemetryString('truck.battery.voltage.warning');

// Enabled state of various elements.

(**
 * @brief Is the electric enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_electric_enabled            = TelemetryString('truck.electric.enabled');

(**
 * @brief Is the engine enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_engine_enabled              = TelemetryString('truck.engine.enabled');

(**
 * @brief Is the left blinker enabled?
 *
 * This represents the logical enable state of the blinker. It
 * it is true as long the blinker is enabled regardless of the
 * physical enabled state of the light (i.e. it does not blink
 * and ignores enable state of electric).
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_lblinker                    = TelemetryString('truck.lblinker');

(**
 * @brief Is the right blinker enabled?
 *
 * This represents the logical enable state of the blinker. It
 * it is true as long the blinker is enabled regardless of the
 * physical enabled state of the light (i.e. it does not blink
 * and ignores enable state of electric).
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_rblinker                    = TelemetryString('truck.rblinker');

(**
 * @brief Is the light in the left blinker currently on?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_lblinker              = TelemetryString('truck.light.lblinker');

(**
 * @brief Is the light in the right blinker currently on?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_rblinker              = TelemetryString('truck.light.rblinker');

(**
 * @brief Are the parking lights enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_parking               = TelemetryString('truck.light.parking');

(**
 * @brief Are the low beam lights enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_low_beam              = TelemetryString('truck.light.beam.low');

(**
 * @brief Are the high beam lights enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_high_beam             = TelemetryString('truck.light.beam.high');

(**
 * @brief Are the auxiliary front lights active?
 *
 * Those lights have several intensity levels:
 * @li 1 - dimmed state
 * @li 2 - full state
 *
 * Type: u32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_front             = TelemetryString('truck.light.aux.front');

(**
 * @brief Are the auxiliary roof lights active?
 *
 * Those lights have several intensity levels:
 * @li 1 - dimmed state
 * @li 2 - full state
 *
 * Type: u32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_aux_roof              = TelemetryString('truck.light.aux.roof');

(**
 * @brief Are the beacon lights enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_beacon                = TelemetryString('truck.light.beacon');

(**
 * @brief Is the brake light active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_brake                 = TelemetryString('truck.light.brake');

(**
 * @brief Is the reverse light active?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_light_reverse               = TelemetryString('truck.light.reverse');

(**
 * @brief Are the wipers enabled?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wipers                      = TelemetryString('truck.wipers');

(**
 * @brief Intensity of the dashboard backlight as factor <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_dashboard_backlight         = TelemetryString('truck.dashboard.backlight');

// Wear info.

(**
 * @brief Wear of the engine accessory as <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wear_engine                 = TelemetryString('truck.wear.engine');

(**
 * @brief Wear of the transmission accessory as <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wear_transmission           = TelemetryString('truck.wear.transmission');

(**
 * @brief Wear of the cabin accessory as <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wear_cabin                  = TelemetryString('truck.wear.cabin');

(**
 * @brief Wear of the chassis accessory as <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wear_chassis                = TelemetryString('truck.wear.chassis');

(**
 * @brief Average wear across the wheel accessories as <0;1>
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wear_wheels                 = TelemetryString('truck.wear.wheels');

(**
 * @brief The value of the odometer in km.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_odometer                    = TelemetryString('truck.odometer');

(**
 * @brief The value of truck's navigation distance (in meters).
 *
 * This is the value used by the advisor.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_navigation_distance         = TelemetryString('truck.navigation.distance');

(**
 * @brief The value of truck's navigation eta (in second).
 *
 * This is the value used by the advisor.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_navigation_time             = TelemetryString('truck.navigation.time');

(**
 * @brief The value of truck's navigation speed limit (in m/s).
 *
 * This is the value used by the advisor and respects the
 * current state of the "Route Advisor speed limit" option.
 *
 * Type: float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_navigation_speed_limit      = TelemetryString('truck.navigation.speed.limit');

// Wheels.

(**
 * @brief Vertical displacement of the wheel from its
 * neutral position in meters.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_susp_deflection       = TelemetryString('truck.wheel.suspension.deflection');

(**
 * @brief Is the wheel in contact with ground?
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_on_ground             = TelemetryString('truck.wheel.on_ground');

(**
 * @brief Substance bellow the whell.
 *
 * Index of substance as delivered trough SUBSTANCE config.
 *
 * Type: indexed u32
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_substance             = TelemetryString('truck.wheel.substance');

(**
 * @brief Angular velocity of the wheel in rotations per
 * second.
 *
 * Positive velocity corresponds to forward movement.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_velocity              = TelemetryString('truck.wheel.angular_velocity');

(**
 * @brief Steering rotation of the wheel in rotations.
 *
 * Value is from <-0.25,0.25> range in counterclockwise direction
 * when looking from top (e.g. 0.25 corresponds to left and
 * -0.25 corresponds to right).
 *
 * Set to zero for non-steered wheels.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_steering              = TelemetryString('truck.wheel.steering');

(**
 * @brief Rolling rotation of the wheel in rotations.
 *
 * Value is from <0.0,1.0) range in which value
 * increase corresponds to forward movement.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_rotation              = TelemetryString('truck.wheel.rotation');

(**
 * @brief Lift state of the wheel <0;1>
 *
 * For use with simple lifted/non-lifted test or logical
 * visualization of the lifting progress.
 *
 * Value of 0 corresponds to non-lifted axle.
 * Value of 1 corresponds to fully lifted axle.
 *
 * Set to zero or not provided for non-liftable axles.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift                  = TelemetryString('truck.wheel.lift');

(**
 * @brief Vertical displacement of the wheel axle
 * from its normal position in meters as result of
 * lifting.
 *
 * Might have non-linear relation to lift ratio.
 *
 * Set to zero or not provided for non-liftable axles.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_TRUCK_CHANNEL_wheel_lift_offset           = TelemetryString('truck.wheel.lift.offset');

(*</interface>*)

implementation

(*</unit>*) 
end.