(**
 * @file scssdk_telemetry_event.h
 *
 * @brief Telemetry SDK - events.
 *)
(*<unit>*) 
unit scssdk_telemetry_event;

interface

{$INCLUDE scssdk_defs.inc}

uses
  scssdk, scssdk_value;

(*<interface>*)
type
  scs_event_t = scs_u32_t;
  p_scs_event_t = ^scs_event_t;

(**
 * @name Telemetry event types.
 *)
//@{
const
(**
 * @brief Used to mark invalid value of event type.
 *)
  SCS_TELEMETRY_EVENT_invalid       = scs_event_t(0);

(**
 * @brief Generated before any telemetry data for current frame.
 *
 * The event_info parameter for this event points to
 * scs_telemetry_frame_start_t structure.
 *)
  SCS_TELEMETRY_EVENT_frame_start   = scs_event_t(1);

(**
 * @brief Generated after all telemetry data for current frame.
 *)
  SCS_TELEMETRY_EVENT_frame_end     = scs_event_t(2);

(**
 * @brief Indicates that the game entered paused state (e.g. menu)
 *
 * If the recipient generates some form of force feedback effects,
 * it should probably stop them until SCS_TELEMETRY_EVENT_started
 * event is received.
 *
 * After sending this event, the game stop sending telemetry data
 * unless specified otherwise in description of specific telemetry.
 * The frame start and event events are still generated.
 *)
  SCS_TELEMETRY_EVENT_paused        = scs_event_t(3);

(**
 * @brief Indicates that the player is now driving.
 *)
  SCS_TELEMETRY_EVENT_started       = scs_event_t(4);

(**
 * @brief Provides set of attributes which change only
 * in special situations (e.g. parameters of the vehicle).
 *
 * The event_info parameter for this event points to
 * scs_telemetry_configuration_t structure.
 *
 * The initial configuration info is delivered to the plugin
 * after its scs_telemetry_init() function succeeds and before
 * any other callback is called. If the the plugin is interested
 * in the configuration info, it must register for this event
 * during its initialization call to ensure that it does
 * not miss it. Future changes in configuration are
 * delivered as described in the event sequence bellow.
 *)
  SCS_TELEMETRY_EVENT_configuration = scs_event_t(5);

//@}

// Sequence of events during frame.
//
// @li Optionally one or more CONFIGURATION events if the configuration changed.
// @li Optionally one from PAUSED or STARTED if there was change since last frame.
// @li FRAME_START
// @li Cannel callbacks
// @li FRAME_END

(**
 * @brief Indicates that timers providing the frame timing info
 * were restarted since last frame.
 *
 * When timer is restarted, it will start counting from zero.
 *)
  SCS_TELEMETRY_FRAME_START_FLAG_timer_restart = scs_u32_t($00000001);

(**
 * @brief Parameters the for SCS_TELEMETRY_EVENT_frame_start event callback.
 *)
type
  scs_telemetry_frame_start_t = Record
    (**
     * @brief Additional information about this event.
     *
     * Combination of SCS_TELEMETRY_FRAME_START_FLAG_* values.
     *)
    flags:                  scs_u32_t;
    (**
     * @brief Explicit alignment for the 64 bit timestamps.
     *)
    _padding:               scs_u32_t;
    (**
     * @brief Time controlling the visualization.
     *
     * Its step changes depending on rendering FPS.
     *)
    render_time:            scs_timestamp_t;
    (**
     * @brief Time controlling the physical simulation.
     *
     * Usually changes with fixed size steps so it oscilates
     * around the render time. This value changes even if the
     * physics simulation is currently paused.
     *)
    simulation_time:        scs_timestamp_t;
    (**
     * @brief Similar to simulation time however it stops
     * when the physics simulation is paused.
     *)
    paused_simulation_time: scs_timestamp_t;
  end;
  p_scs_telemetry_frame_start_t = ^scs_telemetry_frame_start_t;

(**
 * @brief Parameters for the SCS_TELEMETRY_EVENT_configuration event callback.
 *)
  scs_telemetry_configuration_t = Record
    (**
     * @brief Set of logically grouped configuration parameters this
     * event describes (e.g. truck configuration, trailer configuration).
     *
     * See SCS_TELEMETRY_CONFIGURATION_ID_* constants for the game in question.
     *
     * This pointer will be never NULL.
     *)
    id:         scs_string_t;
    (**
     * @brief Array of individual attributes.
     *
     * The array is terminated by entry whose name pointer is set to NULL.
     *
     * Names of the attributes are the SCS_TELEMETRY_ATTRIBUTE_ID_* constants
     * for the game in question.
     *
     * This pointer will be never NULL.
     *)
    attributes: p_scs_named_value_t;
  end;
  p_scs_telemetry_configuration_t = ^scs_telemetry_configuration_t;

(**
 * @brief Type of function registered to be called for event.
 *
 * @param event Event in question. Allows use of single callback with  more than one event.
 * @param event_info Structure with additional event information about the event.
 * @param context Context information passed during callback registration.
 *)
  scs_telemetry_event_callback_t = procedure(event: scs_event_t; event_info: Pointer; context: scs_context_t); stdcall;

(**
 * @brief Registers callback to be called when specified event happens.
 *
 * At most one callback can be registered for each event.
 *
 * This funtion can be called from scs_telemetry_init or from within any
 * event callback other than the callback for the event itself.
 *
 * @param event Event to register for.
 * @param callback Callback to register.
 * @param context Context value passed to the callback.
 * @return SCS_RESULT_ok on successful registration. Error code otherwise.
 *)
  scs_telemetry_register_for_event_t = Function(event: scs_event_t; callback: scs_telemetry_event_callback_t; context: scs_context_t): scs_result_t; stdcall;

(**
 * @brief Unregisters callback registered for specified event.
 *
 * This function can be called from scs_telemetry_shutdown, scs_telemetry_init
 * or from within any event callback. Including callback of the event itself.
 * Any event left registered after scs_telemetry_shutdown ends will
 * be unregistered automatically.
 *
 * @param event Event to unregister from.
 * @return SCS_RESULT_ok on successful unregistration. Error code otherwise.
 *)
  scs_telemetry_unregister_from_event_t = Function(event: scs_event_t): scs_result_t; stdcall;
(*</interface>*)

implementation


{$IFDEF AssertTypeSize}
initialization
(*<initialization>*)
  Assert(SCSCheckSize(SizeOf(scs_telemetry_frame_start_t),32,32));
  Assert(SCSCheckSize(SizeOf(scs_telemetry_configuration_t),8,16));
(*</initialization>*)
{$ENDIF}

(*</unit>*) 
end.