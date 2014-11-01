(**
 * @file scssdk_telemetry_eut2.h
 *
 * @brief ETS 2 telemetry specific constants.
 *)
(*<unit>*) 
unit scssdk_telemetry_eut2;

interface

{$INCLUDE ..\scssdk_defs.inc}

(*<interface>*)
(**
 * @name Value used in the scs_sdk_init_params_t::game_version
 *
 * Changes in the major version indicate incompatible changes (e.g. changed interpretation
 * of the channel value). Change of major version is highly discouraged, creation of
 * alternative channel is preferred solution if necessary.
 * Changes in the minor version indicate compatible changes (e.g. added channel, more supported
 * value types). Removal of channel is also compatible change however it is recommended
 * to keep the channel with some default value.
 *
 * Changes:
 * 1.01 - added brake_air_pressure_emergency channel and air_pressure_emergency config
 * 1.02 - replaced cabin_orientation channel with cabin_offset channel
 * 1.03 - fixed reporting of invalid index value for wheels.count attribute
 * 1.04 - added lblinker_light and rblinker_light channels
 * 1.05 - fixed content of brand_id and brand attributes
 * 1.06 - fixed index value for selector_count attribute. It is now SCS_U32_NIL as the
 *        attribute is not indexed. For backward compatibility additional copy with
 *        index 0 is also present however it will be removed in the future.
 * 1.07 - fixed calculation of cabin_angular_acceleration channel.
 * 1.08 - a empty truck/trailer configuration event is generated when truck is removed
 *        (e.g. after completion of quick job)
 * 1.09 - added time and job related info
 *)
//@{
const
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_00    = (1 shl 16) or 0 {0x00010000};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_01    = (1 shl 16) or 1 {0x00010001};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_02    = (1 shl 16) or 2 {0x00010002};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_03    = (1 shl 16) or 3 {0x00010003};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_04    = (1 shl 16) or 4 {0x00010004};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_05    = (1 shl 16) or 5 {0x00010005}; // Patch 1.4
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_06    = (1 shl 16) or 6 {0x00010006};
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_07    = (1 shl 16) or 7 {0x00010007};	// Patch 1.6
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_08    = (1 shl 16) or 8 {0x00010008};	// Patch 1.9
  SCS_TELEMETRY_EUT2_GAME_VERSION_1_09    = (1 shl 16) or 9 {0x00010009};	// Patch 1.14
  SCS_TELEMETRY_EUT2_GAME_VERSION_CURRENT = SCS_TELEMETRY_EUT2_GAME_VERSION_1_09;
//@}

// Game specific units.
//
// @li The game uses Euro as internal currency provided
//     by the telemetry unless documented otherwise.

// Channels defined in scssdk_telemetry_common_channels.h,
// scssdk_telemetry_truck_common_channels.h and
// scssdk_telemetry_trailer_common_channels.h are supported
// with following exceptions and limitations as of v1.00:
//
// @li Adblue related channels are not supported.
// @li The fuel_average_consumption is currently mostly static and depends
//     on presence of the trailer and skills of the driver instead
//     of the workload of the engine.
// @li Rolling rotation of trailer wheels is determined from linear
//     movement.
// @li The pressures, temperatures and voltages are not simulated.
//     They are very loosely approximated.

// Configurations defined in scssdk_telemetry_common_configs.h are
// supported with following exceptions and limitations as of v1.00:
//
// @li The localized strings are not updated when different in-game
//     language is selected.
(*</interface>*)

implementation

(*</unit>*) 
end.