(**
 * @file scssdk_telemetry_common_configs.h
 *
 * @brief Telemetry specific constants for configs.
 *
 * This file defines truck specific telemetry constants which
 * might be used by more than one SCS game. See game-specific
 * file to determine which constants are supported by specific
 * game.
 *)
(*<unit>*) 
unit scssdk_telemetry_common_configs;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;

(*<interface>*)
const
(**
 * @brief Configuration of the substances.
 *
 * Attribute index is index of the substance.
 *
 * Supported attributes:
 * @li id
 * TODO: Whatever additional info necessary.
 *)
  SCS_TELEMETRY_CONFIG_substances  = TelemetryString('substances');

(**
 * @brief Static configuration of the controls.
 *
 * @li shifter_type
 *)
  SCS_TELEMETRY_CONFIG_controls = TelemetryString('controls');

(**
 * @brief Configuration of the h-shifter.
 *
 * When evaluating the selected gear, find slot which matches
 * the handle position and bitmask of on/off state of selectors.
 * If one is found, it contains the resulting gear. Otherwise
 * a neutral is assumed.
 *
 * Supported attributes:
 * @li selector_count
 * @li resulting gear index for each slot
 * @li handle position index for each slot
 * @li bitmask of selectors for each slot
 *)
  SCS_TELEMETRY_CONFIG_hshifter = TelemetryString('hshifter');

(**
 * @brief Static configuration of the truck.
 *
 * If empty set of attributes is returned, there is no configured truck.
 *
 * Supported attributes:
 * @li brand_id
 * @li brand
 * @li id
 * @li name
 * @li fuel_capacity
 * @li fuel_warning_factor
 * @li adblue_capacity
 * @li ablue_warning_factor 
 * @li air_pressure_warning
 * @li air_pressure_emergency
 * @li oil_pressure_warning
 * @li water_temperature_warning
 * @li battery_voltage_warning
 * @li rpm_limit
 * @li foward_gear_count
 * @li reverse_gear_count
 * @li retarder_step_count
 * @li cabin_position
 * @li head_position
 * @li hook_position
 * @li wheel_count
 * @li wheel positions for wheel_count wheels
 *)
  SCS_TELEMETRY_CONFIG_truck = TelemetryString('truck');

(**
 * @brief Static configuration of the trailer.
 *
 * If empty set of attributes is returned, there is no configured trailer.
 *
 * Supported attributes:
 * @li id
 * @li cargo_accessory_id
 * @li hook_position
 * @li wheel_count
 * @li wheel offsets for wheel_count wheels
 *)
  SCS_TELEMETRY_CONFIG_trailer = TelemetryString('trailer');

(**
 * @brief Static configuration of the job.
 *
 * If empty set of attributes is returned, there is no job.
 *
 * Supported attributes:
 * @li cargo_id
 * @li cargo
 * @li cargo_mass
 * @li destination_city_id
 * @li destination_city
 * @li source_city_id
 * @li source_city
 * @li destination_company_id
 * @li destination_company
 * @li source_company_id
 * @li source_company
 * @li income - represents expected income for the job without any penalties
 * @li delivery_time
 *)
  SCS_TELEMETRY_CONFIG_job = TelemetryString('job');

 // Attributes

(**
 * @brief Brand id for configuration purposes.
 *
 * Limited to C-identifier characters.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_brand_id = TelemetryString('brand_id');

(**
 * @brief Brand for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_brand = TelemetryString('brand');

(**
 * @brief Name for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_id = TelemetryString('id');

(**
 * @brief Name of cargo accessory for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_accessory_id = TelemetryString('cargo.accessory.id');

(**
 * @brief Name for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_name = TelemetryString('name');

(**
 * @brief  Fuel tank capacity in litres.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_fuel_capacity = TelemetryString('fuel.capacity');

(**
 * @brief Fraction of the fuel capacity bellow which
 * is activated the fuel warning.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_fuel_warning_factor = TelemetryString('fuel.warning.factor');

(**
 * @brief  AdBlue tank capacity in litres.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_adblue_capacity = TelemetryString('adblue.capacity');

(**
 * @brief Fraction of the adblue capacity bellow which
 * is activated the adblue warning.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_adblue_warning_factor = TelemetryString('adblue.warning.factor');

(**
 * @brief Pressure of the air in the tank bellow which
 * the warning activates.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_air_pressure_warning = TelemetryString('brake.air.pressure.warning');

(**
 * @brief Pressure of the air in the tank bellow which
 * the emergency brakes activate.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_air_pressure_emergency = TelemetryString('brake.air.pressure.emergency');

(**
 * @brief Pressure of the oil bellow which the warning activates.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_oil_pressure_warning = TelemetryString('oil.pressure.warning');

(**
 * @brief Temperature of the water above which the warning activates.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_water_temperature_warning = TelemetryString('water.temperature.warning');

(**
 * @brief Voltage of the battery bellow which the warning activates.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_battery_voltage_warning = TelemetryString('battery.voltage.warning');

(**
 * @brief Maximal rpm value.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_rpm_limit = TelemetryString('rpm.limit');

(**
 * @brief Number of forward gears on undamaged truck.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_forward_gear_count = TelemetryString('gears.forward');

(**
 * @brief Number of reversee gears on undamaged truck.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_reverse_gear_count = TelemetryString('gears.reverse');

(**
 * @brief Differential ratio of the truck.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_differential_ratio = TelemetryString('differential.ratio');

(**
 * @brief Number of steps in the retarder.
 *
 * Set to zero if retarder is not mounted to the truck.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_retarder_step_count = TelemetryString('retarder.steps');

(**
 * @brief Forward transmission ratios.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_forward_ratio = TelemetryString('forward.ratio');

(**
 * @brief Reverse transmission ratios.
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_reverse_ratio = TelemetryString('reverse.ratio');

(**
 * @brief Position of the cabin in the vehicle space.
 *
 * This is position of the joint around which the cabin rotates.
 * This attribute might be not present if the vehicle does not
 * have a separate cabin.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_cabin_position = TelemetryString('cabin.position');

(**
 * @brief Default position of the head in the cabin space.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_head_position = TelemetryString('head.position');

(**
 * @brief Position of the trailer connection hook in vehicle
 * space.
 *
 * Type: fvector
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_hook_position = TelemetryString('hook.position');

(**
 * @brief Number of wheels
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_count = TelemetryString('wheels.count');

(**
 * @brief Position of respective wheels in the vehicle space.
 *
 * Type: indexed fvector
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_position = TelemetryString('wheel.position');

(**
 * @brief Is the wheel steerable?
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_steerable = TelemetryString('wheel.steerable');

(**
 * @brief Is the wheel physicaly simulated?
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_simulated = TelemetryString('wheel.simulated');

(**
 * @brief Radius of the wheel
 *
 * Type: indexed float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_radius = TelemetryString('wheel.radius');

(**
 * @brief Is the wheel powered?
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_powered = TelemetryString('wheel.powered');

(**
 * @brief Is the wheel liftable?
 *
 * Type: indexed bool
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_wheel_liftable = TelemetryString('wheel.liftable');

(**
 * @brief Number of selectors (e.g. range/splitter toggles).
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_selector_count = TelemetryString('selector.count');

(**
 * @brief Gear selected when requirements for this h-shifter slot are meet.
 *
 * Type: indexed s32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_gear = TelemetryString('slot.gear');

(**
 * @brief Position of h-shifter handle.
 *
 * Zero corresponds to neutral position. Mapping to physical position of
 * the handle depends on input setup.
 *
 * Type: indexed u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_handle_position = TelemetryString('slot.handle.position');

(**
 * @brief Bitmask of required on/off state of selectors.
 *
 * Only first selector_count bits are relevant.
 *
 * Type: indexed u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_slot_selectors = TelemetryString('slot.selectors');

(**
 * @brief Type of the shifter.
 *
 * One from SCS_SHIFTER_TYPE_* values.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_shifter_type = TelemetryString('shifter.type');

  SCS_SHIFTER_TYPE_arcade     = TelemetryString('arcade');
  SCS_SHIFTER_TYPE_automatic  = TelemetryString('automatic');
  SCS_SHIFTER_TYPE_manual     = TelemetryString('manual');
  SCS_SHIFTER_TYPE_hshifter   = TelemetryString('hshifter');

 // Attributes

(**
 * @brief Id of the cargo for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_id = TelemetryString('cargo.id');

(**
 * @brief Name of the cargo for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo = TelemetryString('cargo');

(**
 * @brief Mass of the cargo in kilograms.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_cargo_mass = TelemetryString('cargo.mass');

(**
 * @brief Id of the destination city for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_city_id = TelemetryString('destination.city.id');

(**
 * @brief Name of the destination city for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_city = TelemetryString('destination.city');

(**
 * @brief Id of the destination company for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_company_id = TelemetryString('destination.company.id');

(**
 * @brief Name of the destination company for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_destination_company = TelemetryString('destination.company');

(**
 * @brief Id of the source city for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_city_id = TelemetryString('source.city.id');

(**
 * @brief Name of the source city for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_city = TelemetryString('source.city');

(**
 * @brief Id of the source company for internal use by code.
 *
 * Limited to C-identifier characters and dots.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_company_id = TelemetryString('source.company.id');

(**
 * @brief Name of the source company for display purposes.
 *
 * Localized using the current in-game language.
 *
 * Type: string
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_source_company = TelemetryString('source.company');

(**
 * @brief Reward in internal game-specific currency.
 *
 * For detailed information about the currency see "Game specific units"
 * documentation in scssdk_telemetry_<game_id>.h
 *
 * Type: u64
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_income = TelemetryString('income');

(**
 * @brief Absolute in-game time of end of job delivery window.
 *
 * Delivering the job after this time will cause it be late.
 *
 * See SCS_TELEMETRY_CHANNEL_game_time for more info about absolute time.
 * Time remaining for delivery can be obtained like (delivery_time - game_time).
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CONFIG_ATTRIBUTE_delivery_time = TelemetryString('delivery.time');
(*</interface>*)

implementation

(*</unit>*) 
end.