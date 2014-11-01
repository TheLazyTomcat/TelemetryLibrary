(**
 * @file scssdk_eut2.h
 *
 * @brief ETS 2 specific constants.
 *)
(*<unit>*) 
unit scssdk_eut2;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;

(*<interface>*)
const
(**
 * @brief Value used in the scs_sdk_init_params_t::game_id to identify this game.
 *)
  SCS_GAME_ID_EUT2 = TelemetryString('eut2');
(*</interface>*)

implementation

(*</unit>*) 
end.