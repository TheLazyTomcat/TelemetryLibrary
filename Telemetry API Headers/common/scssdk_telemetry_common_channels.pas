(**
 * @file scssdk_telemetry_common_channels.h
 *
 * @brief Telemetry specific channels which might be used by more than one game.
 *)
(*<unit>*)  
unit scssdk_telemetry_common_channels;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;

(*<interface>*)
const
(**
 * @brief Scale applied to distance and time to compensate
 * for the scale of the map (e.g. 1s of real time corresponds to local_scale
 * seconds of simulated game time).
 *
 * Games which use real 1:1 maps will not provide this
 * channel.
 *
 * Type: float
 *)
  SCS_TELEMETRY_CHANNEL_local_scale = TelemetryString('local.scale');

(**
 * @brief Absolute in-game time.
 *
 * Represented in number of in-game minutes since beginning (i.e. 00:00)
 * of the first in-game day.
 *
 * Type: u32
 *)
  SCS_TELEMETRY_CHANNEL_game_time = TelemetryString('game.time');

(**
 * @brief Time until next rest stop.
 *
 * When the fatique simulation is disabled, the behavior of this channel
 * is implementation dependent. The game might provide the value which would
 * apply if it was enabled or provide no value at all.
 *
 * Represented in in-game minutes.
 *
 * Type: s32
 *)
  SCS_TELEMETRY_CHANNEL_next_rest_stop = TelemetryString('rest.stop');
(*</interface>*)

implementation

(*</unit>*) 
end.