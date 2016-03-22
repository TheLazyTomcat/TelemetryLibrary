(**
 * @file scssdk_ats.h
 *
 * @brief ATS specific constants.
 *)
(*<unit>*)
unit scssdk_ats;

interface

{$INCLUDE ..\scssdk_defs.inc}

uses
  scssdk;
  
(*<interface>*)
const
(**
 * @brief Value used in the scs_sdk_init_params_t::game_id to identify this game.
 *)
  SCS_GAME_ID_ATS = TelemetryString('ats');
(*</interface>*)

implementation

(*</unit>*)
end.
