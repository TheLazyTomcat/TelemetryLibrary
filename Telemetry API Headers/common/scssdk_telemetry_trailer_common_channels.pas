(**
 * @file scssdk_telemetry_trailer_common_channels.h
 *
 * @brief Trailer telemetry specific constants for channels.
 *
 * See scssdk_telemetry_truck_common_channels.h for more info.
 *)
(*<unit>*)  
unit scssdk_telemetry_trailer_common_channels;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;

(*<interface>*)
const
(**
 * @brief Is the trailer connected to the truck?
 *
 * Type: bool
 *)
  SCS_TELEMETRY_TRAILER_CHANNEL_connected                   = TelemetryString('trailer.connected');

(**
 * @name Channels similar to the truck ones
 *
 * See scssdk_telemetry_truck_common_channels.h for description of
 * corresponding truck channels
 *)
//@{
  SCS_TELEMETRY_TRAILER_CHANNEL_world_placement             = TelemetryString('trailer.world.placement');
  SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_velocity       = TelemetryString('trailer.velocity.linear');
  SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_velocity      = TelemetryString('trailer.velocity.angular');
  SCS_TELEMETRY_TRAILER_CHANNEL_local_linear_acceleration   = TelemetryString('trailer.acceleration.linear');
  SCS_TELEMETRY_TRAILER_CHANNEL_local_angular_acceleration  = TelemetryString('trailer.acceleration.angular');

// Damage.

  SCS_TELEMETRY_TRAILER_CHANNEL_wear_chassis                = TelemetryString('trailer.wear.chassis');

// Wheels.

  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_susp_deflection       = TelemetryString('trailer.wheel.suspension.deflection');
  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_on_ground             = TelemetryString('trailer.wheel.on_ground');
  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_substance             = TelemetryString('trailer.wheel.substance');
  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_velocity              = TelemetryString('trailer.wheel.angular_velocity');
  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_steering              = TelemetryString('trailer.wheel.steering');
  SCS_TELEMETRY_TRAILER_CHANNEL_wheel_rotation              = TelemetryString('trailer.wheel.rotation');
//@}
(*</interface>*)

implementation

(*</unit>*) 
end.