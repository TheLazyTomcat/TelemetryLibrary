{==============================================================================}
{------------------------------------------------------------------------------}
{                           Muticast events managers                           }
{------------------------------------------------------------------------------}
{==============================================================================}
{$IFDEF DeclarationPart}

{$IFDEF TelemetryRecipient}
{==============================================================================}
{   TMulticastLogEvent // Class declaration                                    }
{==============================================================================}
{:
  Class used to manage multicast event called when recipient writes to game log.
}
  TMulticastLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; LogType: scs_log_type_t; const LogText: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventRegisterEvent // Class declaration                          }
{==============================================================================}
{:
  Class used to manage multicast event called when telemetry event is registered
  or unregistered.
}
  TMulticastEventRegisterEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TEventRegisterEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TEventRegisterEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TEventRegisterEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Event: scs_event_t; UserData: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventEvent // Class declaration                                  }
{==============================================================================}
{:
  Class used to manage multicast event called when telemetry when telemetery
  event occurs.
}
  TMulticastEventEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TEventEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TEventEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TEventEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelRegisterEvent // Class declaration                        }
{==============================================================================}
{:
  Class used to manage multicast event called when telemetry channel is
  registered.
}
  TMulticastChannelRegisterEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelRegisterEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TChannelRegisterEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelRegisterEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelUnregisterEvent // Class declaration                      }
{==============================================================================}
{:
  Class used to manage multicast event called when telemetry channel is
  unregistered.
}
  TMulticastChannelUnregisterEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelUnregisterEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)

  }
    Function Add(const Handler: TChannelUnregisterEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelUnregisterEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelEvent // Class declaration                                }
{==============================================================================}
{:
  Class used to manage multicast event called when telemetery channel callback
  occurs.
}
  TMulticastChannelEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TChannelEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastConfigEvent // Class declaration                                 }
{==============================================================================}
{:
  Class used to manage multicast event called when config is parsed from
  configuration telemetry event.
}
  TMulticastConfigEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TConfigEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TConfigEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TConfigEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); reintroduce;
  end;
{$ENDIF}

{$IFDEF TelemetryLogBinaryParser}
{==============================================================================}
{   TMulticastDataLogEvent // Class declaration                                }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes
  unprocessed log data.
}
  TMulticastDataLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TDataLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TDataLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
      Removes first or all occurrences, depending on @code(RemoveAll) value, of
      passed routine from internal list of handlers.

      @param Handler   Routine to be removed from the list.
      @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                       occurence of passed handler or all of them.)

      @returns(Index at which the removed item was stored in the list, -1 when
               passed routine was not found.)
  }
    Function Remove(const Handler: TDataLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Data: Pointer; Size: Integer); reintroduce;
  end;

{==============================================================================}
{   TMulticastTextLogEvent // Class declaration                                }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes text log
  data or any interpreted data.
}
  TMulticastTextLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TTextLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TTextLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TTextLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; const Text: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastDataTimeLogEvent // Class declaration                            }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes
  unprocessed log data with time.
}
  TMulticastDataTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TDataTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TDataTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TDataTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; Data: Pointer; Size: Integer); reintroduce;
  end;

{==============================================================================}
{   TMulticastTextTimeLogEvent // Class declaration                            }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes text log
  data or any interpreted data with time.
}
  TMulticastTextTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TTextTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TTextTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TTextTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; const Text: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastLogTimeLogEvent // Class declaration                             }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes
  information about a write to game log with time.
}
  TMulticastLogTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TLogTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TLogTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TLogTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventRegisterTimeLogEvent // Class declaration                   }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes event
  (un)registration log data with time.
}
  TMulticastEventRegisterTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TEventRegisterTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TEventRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TEventRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; Event: scs_event_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventTimeLogEvent // Class declaration                           }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes event
  log data with time.
}
  TMulticastEventTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TEventTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)

  }
    Function Add(const Handler: TEventTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TEventTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelRegisterTimeLogEvent // Class declaration                 }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes channel
  registration log data with time.
}
  TMulticastChannelRegisterTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelRegisterTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TChannelRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelUnregisterTimeLogEvent // Class declaration               }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes channel
  unregistration log data with time.
}
  TMulticastChannelUnregisterTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelUnregisterTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TChannelUnregisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelUnregisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelTimeLogEvent // Class declaration                         }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes channel
  log data with time.
}
  TMulticastChannelTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TChannelTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TChannelTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TChannelTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastConfigTimeLogEvent // Class declaration                          }
{==============================================================================}
{:
  Class used to manage multicast event called when reader/parser passes config
  log data with time.
}
  TMulticastConfigTimeLogEvent = class(TMulticastEvent)
  public
  {:
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found.
  }
    Function IndexOf(const Handler: TConfigTimeLogEvent): Integer; reintroduce;
  {:
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)

    @returns(Index at which the added routine is stored. -1 when storing has
             failed.)
  }
    Function Add(const Handler: TConfigTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
  {:
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.)
  }
    Function Remove(const Handler: TConfigTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
  {:
    Calls all stored handler routines with the same parameters as passed to this
    method.
  }
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); reintroduce;
  end;
{$ENDIF}

{$ENDIF}

{$IFDEF ImplementationPart}

{$IFDEF TelemetryRecipient}
{==============================================================================}
{   TMulticastLogEvent // Class implementation                                 }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastLogEvent // Public methods                                       }
{------------------------------------------------------------------------------}

Function TMulticastLogEvent.IndexOf(const Handler: TLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastLogEvent.Add(const Handler: TLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastLogEvent.Remove(const Handler: TLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastLogEvent.Call(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TLogEvent(Methods[i])(Sender,LogType,LogText);
end;

{==============================================================================}
{   TMulticastEventRegisterEvent // Class implementation                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastEventRegisterEvent // Public methods                             }
{------------------------------------------------------------------------------}

Function TMulticastEventRegisterEvent.IndexOf(const Handler: TEventRegisterEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastEventRegisterEvent.Add(const Handler: TEventRegisterEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastEventRegisterEvent.Remove(const Handler: TEventRegisterEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastEventRegisterEvent.Call(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TEventRegisterEvent(Methods[i])(Sender,Event,UserData);
end;

{==============================================================================}
{   TMulticastEventEvent // Class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastEventEvent // Public methods                                     }
{------------------------------------------------------------------------------}

Function TMulticastEventEvent.IndexOf(const Handler: TEventEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastEventEvent.Add(const Handler: TEventEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastEventEvent.Remove(const Handler: TEventEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastEventEvent.Call(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TEventEvent(Methods[i])(Sender,Event,Data,UserData);
end;

{==============================================================================}
{   TMulticastChannelRegisterEvent // Class implementation                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelRegisterEvent // Public methods                           }
{------------------------------------------------------------------------------}

Function TMulticastChannelRegisterEvent.IndexOf(const Handler: TChannelRegisterEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelRegisterEvent.Add(const Handler: TChannelRegisterEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelRegisterEvent.Remove(const Handler: TChannelRegisterEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelRegisterEvent.Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelRegisterEvent(Methods[i])(Sender,Name,ID,Index,ValueType,Flags,UserData);
end;

{==============================================================================}
{   TMulticastChannelUnregisterEvent // Class implementation                   }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelUnregisterEvent // Public methods                         }
{------------------------------------------------------------------------------}

Function TMulticastChannelUnregisterEvent.IndexOf(const Handler: TChannelUnregisterEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelUnregisterEvent.Add(const Handler: TChannelUnregisterEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelUnregisterEvent.Remove(const Handler: TChannelUnregisterEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelUnregisterEvent.Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelUnregisterEvent(Methods[i])(Sender,Name,ID,Index,ValueType,UserData);
end;

{==============================================================================}
{   TMulticastChannelEvent // Class implementation                             }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelEvent // Public methods                                   }
{------------------------------------------------------------------------------}

Function TMulticastChannelEvent.IndexOf(const Handler: TChannelEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelEvent.Add(const Handler: TChannelEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelEvent.Remove(const Handler: TChannelEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelEvent.Call(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelEvent(Methods[i])(Sender,Name,ID,Index,Value,UserData);
end;

{==============================================================================}
{   TMulticastConfigEvent // Class implementation                              }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastConfigEvent // Public methods                                    }
{------------------------------------------------------------------------------}

Function TMulticastConfigEvent.IndexOf(const Handler: TConfigEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastConfigEvent.Add(const Handler: TConfigEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastConfigEvent.Remove(const Handler: TConfigEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastConfigEvent.Call(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TConfigEvent(Methods[i])(Sender,Name,ID,Index,Value);
end;
{$ENDIF}

{$IFDEF TelemetryLogBinaryParser}
{==============================================================================}
{   TMulticastDataLogEvent // Class implementation                             }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastDataLogEvent // Public methods                                   }
{------------------------------------------------------------------------------}

Function TMulticastDataLogEvent.IndexOf(const Handler: TDataLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastDataLogEvent.Add(const Handler: TDataLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastDataLogEvent.Remove(const Handler: TDataLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastDataLogEvent.Call(Sender: TObject; Data: Pointer; Size: Integer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TDataLogEvent(Methods[i])(Sender,Data,Size);
end;

{==============================================================================}
{   TMulticastTextLogEvent // Class implementation                             }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastTextLogEvent // Public methods                                   }
{------------------------------------------------------------------------------}

Function TMulticastTextLogEvent.IndexOf(const Handler: TTextLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastTextLogEvent.Add(const Handler: TTextLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastTextLogEvent.Remove(const Handler: TTextLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastTextLogEvent.Call(Sender: TObject; const Text: String);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TTextLogEvent(Methods[i])(Sender,Text);
end;

{==============================================================================}
{   TMulticastDataTimeLogEvent // Class implementation                         }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastDataTimeLogEvent // Public methods                               }
{------------------------------------------------------------------------------}

Function TMulticastDataTimeLogEvent.IndexOf(const Handler: TDataTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastDataTimeLogEvent.Add(const Handler: TDataTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastDataTimeLogEvent.Remove(const Handler: TDataTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastDataTimeLogEvent.Call(Sender: TObject; Time: TDateTime; Data: Pointer; Size: Integer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TDataTimeLogEvent(Methods[i])(Sender,Time,Data,Size);
end;

{==============================================================================}
{   TMulticastTextTimeLogEvent // Class implementation                         }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastTextTimeLogEvent // Public methods                               }
{------------------------------------------------------------------------------}

Function TMulticastTextTimeLogEvent.IndexOf(const Handler: TTextTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastTextTimeLogEvent.Add(const Handler: TTextTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastTextTimeLogEvent.Remove(const Handler: TTextTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastTextTimeLogEvent.Call(Sender: TObject; Time: TDateTime; const Text: String);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TTextTimeLogEvent(Methods[i])(Sender,Time,Text);
end;

{==============================================================================}
{   TMulticastLogTimeLogEvent // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastLogTimeLogEvent // Public methods                                }
{------------------------------------------------------------------------------}

Function TMulticastLogTimeLogEvent.IndexOf(const Handler: TLogTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastLogTimeLogEvent.Add(const Handler: TLogTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastLogTimeLogEvent.Remove(const Handler: TLogTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastLogTimeLogEvent.Call(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TLogTimeLogEvent(Methods[i])(Sender,Time,LogType,LogText);
end;

{==============================================================================}
{   TMulticastEventRegisterTimeLogEvent // Class implementation                }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastEventRegisterTimeLogEvent // Public methods                      }
{------------------------------------------------------------------------------}

Function TMulticastEventRegisterTimeLogEvent.IndexOf(const Handler: TEventRegisterTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastEventRegisterTimeLogEvent.Add(const Handler: TEventRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastEventRegisterTimeLogEvent.Remove(const Handler: TEventRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastEventRegisterTimeLogEvent.Call(Sender: TObject; Time: TDateTime; Event: scs_event_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TEventRegisterTimeLogEvent(Methods[i])(Sender,Time,Event);
end;

{==============================================================================}
{   TMulticastEventTimeLogEvent // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastEventTimeLogEvent // Public methods                              }
{------------------------------------------------------------------------------}

Function TMulticastEventTimeLogEvent.IndexOf(const Handler: TEventTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastEventTimeLogEvent.Add(const Handler: TEventTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastEventTimeLogEvent.Remove(const Handler: TEventTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastEventTimeLogEvent.Call(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TEventTimeLogEvent(Methods[i])(Sender,Time,Event,Data);
end;

{==============================================================================}
{   TMulticastChannelRegisterTimeLogEvent // Class implementation              }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelRegisterTimeLogEvent // Public methods                    }
{------------------------------------------------------------------------------}

Function TMulticastChannelRegisterTimeLogEvent.IndexOf(const Handler: TChannelRegisterTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelRegisterTimeLogEvent.Add(const Handler: TChannelRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelRegisterTimeLogEvent.Remove(const Handler: TChannelRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelRegisterTimeLogEvent.Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelRegisterTimeLogEvent(Methods[i])(Sender,Time,Name,ID,Index,ValueType,Flags);
end;

{==============================================================================}
{   TMulticastChannelUnregisterTimeLogEvent // Class implementation            }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelUnregisterTimeLogEvent // Public methods                  }
{------------------------------------------------------------------------------}

Function TMulticastChannelUnregisterTimeLogEvent.IndexOf(const Handler: TChannelUnregisterTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelUnregisterTimeLogEvent.Add(const Handler: TChannelUnregisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelUnregisterTimeLogEvent.Remove(const Handler: TChannelUnregisterTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelUnregisterTimeLogEvent.Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelUnregisterTimeLogEvent(Methods[i])(Sender,Time,Name,ID,Index,ValueType);
end;

{==============================================================================}
{   TMulticastChannelTimeLogEvent // Class implementation                      }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastChannelTimeLogEvent // Public methods                            }
{------------------------------------------------------------------------------}

Function TMulticastChannelTimeLogEvent.IndexOf(const Handler: TChannelTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastChannelTimeLogEvent.Add(const Handler: TChannelTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastChannelTimeLogEvent.Remove(const Handler: TChannelTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastChannelTimeLogEvent.Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TChannelTimeLogEvent(Methods[i])(Sender,Time,Name,ID,Index,Value);
end;

{==============================================================================}
{   TMulticastConfigTimeLogEvent // Class implementation                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastConfigTimeLogEvent // Public methods                             }
{------------------------------------------------------------------------------}

Function TMulticastConfigTimeLogEvent.IndexOf(const Handler: TConfigTimeLogEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastConfigTimeLogEvent.Add(const Handler: TConfigTimeLogEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastConfigTimeLogEvent.Remove(const Handler: TConfigTimeLogEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastConfigTimeLogEvent.Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TConfigTimeLogEvent(Methods[i])(Sender,Time,Name,ID,Index,Value);
end;
{$ENDIF}

{$ENDIF}
