(**
 * @file scssdk_telemetry.h
 *
 * @brief Telemetry SDK.
 *)
(*<unit>*) 
unit scssdk_telemetry;

interface

{$INCLUDE scssdk_defs.inc}

uses
  scssdk,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;

(*<interface>*)
(**
 * @name Versions of the telemetry SDK
 *
 * Changes in the major version indicate incompatible changes in the API.
 * Changes in the minor version indicate additions (e.g. more events, defined
 * types as long layout of existing fields in scs_value_t does not change).
 *)
//@{
const
  SCS_TELEMETRY_VERSION_1_00    = (1 shl 16) or 0 {0x00010000};
  SCS_TELEMETRY_VERSION_CURRENT = SCS_TELEMETRY_VERSION_1_00;
//@}

// Structures used to pass additional data to the initialization function.

type
(**
 * @brief Common ancestor to all structures providing parameters to the telemetry
 * initialization.
 *)
//scs_telemetry_init_params_t = Record
//  procedure method_indicating_this_is_not_a_c_struct;
//end;
//see further

(**
 * @brief Initialization parameters for the 1.00 version of the telemetry API.
 *)
  scs_telemetry_init_params_v100_t = Record
    (**
     * @brief Common initialization parameters.
     *)
    common:                   scs_sdk_init_params_v100_t;
    (**
     * @name Functions used to handle registration of event callbacks.
     *)
    //@{
    register_for_event:       scs_telemetry_register_for_event_t;
    unregister_from_event:    scs_telemetry_unregister_from_event_t;
    //@}
    (**
     * @name Functions used to handle registration of telemetry callbacks.
     *)
    //@{
    register_for_channel:     scs_telemetry_register_for_channel_t;
    unregister_from_channel:  scs_telemetry_unregister_from_channel_t;
    //@}
  end;
  p_scs_telemetry_init_params_v100_t = ^scs_telemetry_init_params_v100_t;


  scs_telemetry_init_params_t = scs_telemetry_init_params_v100_t;
  p_scs_telemetry_init_params_t = ^scs_telemetry_init_params_t;

// Functions which should be exported by the dynamic library serving as
// recipient of the telemetry.

(**
 * @brief Initializes telemetry support.
 *
 * This function must be provided by the library if it wants to support telemetry API.
 *
 * The engine will call this function with API versions it supports starting from the latest
 * until the function returns SCS_RESULT_ok or error other than SCS_RESULT_unsupported or it
 * runs out of supported versions.
 *
 * At the time this function is called, the telemetry is in the paused state.
 *
 * @param version Version of the API to initialize.
 * @param params Structure with additional initialization data specific to the specified API version.
 * @return SCS_RESULT_ok if version is supported and library was initialized. Error code otherwise.
 *)
  scs_telemetry_init_t = Function(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;

(**
 * @brief Shuts down the telemetry support.
 *
 * The engine will call this function if available and if the scs_telemetry_init indicated
 * success.
 *)
  scs_telemetry_shutdown_t = procedure; stdcall;
(*</interface>*)

implementation

{$IFDEF AssertTypeSize}
initialization
(*<initialization>*)
  Assert(SCSCheckSize(SizeOf(scs_telemetry_init_params_v100_t),32,64));
(*</initialization>*)
{$ENDIF}

(*</unit>*) 
end.