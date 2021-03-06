{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Provides base for classes binding itself to telemetry recipient
          events.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-03)
@lastmod(2016-03-20)

  @bold(@NoAutoLink(TelemetryRecipientBinder))

  ©2014-2016 František Milt, all rights reserved.

  Last change: 2016-03-20

  This unit provides TTelemetryRecipientBinder class that is designed as a base
  for classes that are supposed to bind itself to telemetry recipient events
  (eg. loggers, servers, ...).

@html(<hr>)}
unit TelemetryRecipientBinder;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
  TelemetryCommon,
  TelemetryIDs,
  TelemetryRecipient
{$IFNDEF Documentation},
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}
{$ELSE};
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryRecipientBinder                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryRecipientBinder // Class declaration                             }
{==============================================================================}
{:
  @abstract(Class designed as a base for any class that is supposed to bind
            itself to telemetry @noAutoLink(recipient) events.)
  It has predefined abstract methods that can be assigned as handlers to
  @noAutoLink(recipient) events one by one (when you want full control over what
  and when is called), or at once using AssignHandlers method (Recipient must
  be assigned for this to work).
}
type
  TTelemetryRecipientBinder = class(TObject)
  private
  {:
    See Recipient property for details.
  }
    fRecipient:                 TTelemetryRecipient;
  {:
    See DeassignHandlersOnDestroy property for details.
  }
    fDeassignHandlersOnDestroy: Boolean;
  protected
  {:
    Method used to get valid telemetry @noAutoLink(recipient) for work.@br
    When Recipient property is assigned, then it is returned in output parameter
    @noAutoLink(@code(Recipient)), when not, check whether object passed in
    parameter @code(Sender) is of type TTelemetryRecipient (or its descendant)
    and when it is, then this object is returned, otherwise output parameter
    contains @nil.

    @param(Sender    Object suggested as working telemetry
                     @noAutoLink(recipient).)
    @param(Recipient Output variable containing either valid telemetry
                     @noAutoLink(recipient) or @nil.)

    @returns(@True when output parameter @noAutoLink(@code(Recipient)) contains
             valid telemetry @noAutoLink(recipient), @false otherwise.)
  }
    Function GetWorkingRecipient(Sender: TObject; out Recipient: TTelemetryRecipient): Boolean; virtual;
  public
  {:
    Class constructor.

    @param(Recipient Telemetry @noAutoLink(recipient) that operates on API. When
                     you assign this parameter, it will be assigned to Recipient
                     property and handlers will be assigned to its events using
                     AssignHandlers method.)
  }
    constructor Create(Recipient: TTelemetryRecipient = nil);
  {:
    Class destructor.

    Calls method DeassignHandlers when DeassignHandlersOnDestroy property is
    @true.
  }
    destructor Destroy; override;
  {:
    Assigns appropriate *Handler methods as handlers of Recipent events.@br
    When symbol @code(MulticastEvents) is defined, the handlers are assigned to
    multicast events, otherwise they are assigned to normal events.

    @returns(@True when Recipient is assigned and all handlers were assigned to
             its event, @false otherwise.)
  }
    Function AssignHandlers: Boolean; virtual;
  {:
    Deassigns handlers of Recipient events.

    @returns(@True when Recipient is assigned and all handlers were deassigned,
             @false otherwise.)
  }
    Function DeassignHandlers: Boolean; virtual;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnLog OnLog) event.

    @param Sender  Object that is calling this method.
    @param LogType Type of log written into game log.
    @param LogText Actual text written into game log.
  }
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnEventRegister OnEventRegister)
    event.

    @param Sender    Object that is calling this method.
    @param Event     Game event identification number.
    @param UserData  User defined data stored inside the event context.
  }
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnEventUnregister OnEventUnregister)
    event.

    @param Sender    Object that is calling this method.
    @param Event     Game event identification number.
    @param UserData  User defined data stored inside the event context.
  }
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnEvent OnEvent) event.

    @param Sender    Object that is calling this method.
    @param Event     Game event identification number.
    @param Data      Pointer to data accompanying the event. Can be @nil.
    @param UserData  User defined data stored inside the event context.
  }
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnChannelRegister OnChannelRegister)
    event.

    @param Sender    Object that is calling this method.
    @param Name      Name of registered channel.
    @param ID        ID of registered channel.
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.
    @param Flags     Registration flags.
    @param UserData  User defined data stored inside the channel context.
  }
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnChannelUnregister
    OnChannelUnregister)
    event.

    @param Sender    Object that is calling called this method.
    @param Name      Name of unregistered channel.
    @param ID        ID of unregistered channel.
    @param Index     Index of unregistered channel.
    @param ValueType Value type of unregistered channel.
    @param UserData  User defined data stored inside the channel context.
  }
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnChannel OnChannel) event.

    @param Sender Object that that is calling this method.
    @param Name   Name of the channel.
    @param ID     ID of the channel.
    @param Index  Index of the channel.
    @param Value  Actual value of the channel. Can be @nil.
    @param UserData  User defined data stored inside the channel context.
  }
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); virtual; abstract;
  {:
    Method that can be assigned to @noAutoLink(recipent) @link(
    TelemetryRecipient.TTelemetryRecipient.OnConfig OnConfig) event.

    @param Sender           Object that is calling called this method.
    @param ConfigReference  Full config reference (ID + Attribute).
    @param Index            Index of the config.
    @param Value            Actual value of the config.
  }
    procedure ConfigHandler(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t); virtual; abstract;
  published
  {:
    Reference to @noAutoLink(recipient) that operates on telemetry API. Not
    initialized, it is assigned in constructor. Can be @nil.
  }
    property Recipient: TTelemetryRecipient read fRecipient write fRecipient;
  {:
    When @true, the destructor calls method DeassignHandlers.@br
    Initialized to @true.
  }
    property DeassignHandlersOnDestroy: Boolean read fDeassignHandlersOnDestroy write fDeassignHandlersOnDestroy;
  end;

implementation

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryRecipientBinder                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryRecipientBinder // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryRecipientBinder // Constants, types, variables, etc...           }
{------------------------------------------------------------------------------}

const
  // Default (initial) values for TTelemetryRecipientBinder properties.
  def_DeassignHandlersOnDestroy = True;

{------------------------------------------------------------------------------}
{   TTelemetryRecipientBinder // Protected methods                             }
{------------------------------------------------------------------------------}

Function TTelemetryRecipientBinder.GetWorkingRecipient(Sender: TObject; out Recipient: TTelemetryRecipient): Boolean;
begin
If Assigned(fRecipient) then Recipient := fRecipient
  else If Sender is TTelemetryRecipient then Recipient := TTelemetryRecipient(Sender)
    else Recipient := nil;
Result := Assigned(Recipient);
end;

{------------------------------------------------------------------------------}
{   TTelemetryRecipientBinder // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryRecipientBinder.Create(Recipient: TTelemetryRecipient = nil);
begin
inherited Create;
fRecipient := Recipient;
If Assigned(Recipient) then AssignHandlers;
fDeassignHandlersOnDestroy := def_DeassignHandlersOnDestroy;
end;

//------------------------------------------------------------------------------

destructor TTelemetryRecipientBinder.Destroy;
begin
If DeassignHandlersOnDestroy then DeassignHandlers;
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipientBinder.AssignHandlers: Boolean;
begin
Result := False;
If Assigned(fRecipient) then
  begin
    {$IFDEF MulticastEvents}
    fRecipient.OnLogMulti.Add(LogHandler);
    fRecipient.OnEventRegisterMulti.Add(EventRegisterHandler);
    fRecipient.OnEventUnregisterMulti.Add(EventUnregisterHandler);
    fRecipient.OnEventMulti.Add(EventHandler);
    fRecipient.OnChannelRegisterMulti.Add(ChannelRegisterHandler);
    fRecipient.OnChannelUnregisterMulti.Add(ChannelUnregisterHandler);
    fRecipient.OnChannelMulti.Add(ChannelHandler);
    fRecipient.OnConfigMulti.Add(ConfigHandler);
    {$ELSE}
    fRecipient.OnLog := LogHandler;
    fRecipient.OnEventRegister := EventRegisterHandler;
    fRecipient.OnEventUnregister := EventUnregisterHandler;
    fRecipient.OnEvent := EventHandler;
    fRecipient.OnChannelRegister := ChannelRegisterHandler;
    fRecipient.OnChannelUnregister := ChannelUnregisterHandler;
    fRecipient.OnChannel := ChannelHandler;
    fRecipient.OnConfig := ConfigHandler;
    {$ENDIF}
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipientBinder.DeassignHandlers: Boolean;
begin
Result := False;
If Assigned(fRecipient) then
  begin
    {$IFDEF MulticastEvents}
    fRecipient.OnLogMulti.Remove(LogHandler);
    fRecipient.OnEventRegisterMulti.Remove(EventRegisterHandler);
    fRecipient.OnEventUnregisterMulti.Remove(EventUnregisterHandler);
    fRecipient.OnEventMulti.Remove(EventHandler);
    fRecipient.OnChannelRegisterMulti.Remove(ChannelRegisterHandler);
    fRecipient.OnChannelUnregisterMulti.Remove(ChannelUnregisterHandler);
    fRecipient.OnChannelMulti.Remove(ChannelHandler);
    fRecipient.OnConfigMulti.Remove(ConfigHandler);
    {$ELSE}
    fRecipient.OnLog := nil;
    fRecipient.OnEventRegister := nil;
    fRecipient.OnEventUnregister := nil;
    fRecipient.OnEvent := nil;
    fRecipient.OnChannelRegister := nil;
    fRecipient.OnChannelUnregister := nil;
    fRecipient.OnChannel := nil;
    fRecipient.OnConfig := nil;
    {$ENDIF}
    Result := True;
  end;
end;

end.
