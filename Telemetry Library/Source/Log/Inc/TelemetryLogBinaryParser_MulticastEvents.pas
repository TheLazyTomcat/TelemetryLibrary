{$IFDEF DeclarationPart}
{==============================================================================}
{------------------------------------------------------------------------------}
{                           Muticast events managers                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMulticastDataLogEvent // Class declaration                                }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes unprocessed log data.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastDataLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TDataLogEvent): Integer; reintroduce;
    Function Add(const Handler: TDataLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TDataLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Data: Pointer; Size: Integer); reintroduce;
  end;

{==============================================================================}
{   TMulticastTextLogEvent // Class declaration                                }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes text log data or any interpreted data.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastTextLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TTextLogEvent): Integer; reintroduce;
    Function Add(const Handler: TTextLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TTextLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; const Text: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastDataTimeLogEvent // Class declaration                            }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes unprocessed log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastDataTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TDataTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TDataTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TDataTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; Data: Pointer; Size: Integer); reintroduce;
  end;

{==============================================================================}
{   TMulticastTextTimeLogEvent // Class declaration                            }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes text log data or any interpreted data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastTextTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TTextTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TTextTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TTextTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; const Text: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastLogTimeLogEvent // Class declaration                             }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes informations about a write to game log with time)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastLogTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TLogTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TLogTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TLogTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventRegisterTimeLogEvent // Class declaration                   }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes event (un)registration log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastEventRegisterTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TEventRegisterTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TEventRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TEventRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; Event: scs_event_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastEventTimeLogEvent // Class declaration                           }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes event log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastEventTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TEventTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TEventTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TEventTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelRegisterTimeLogEvent // Class declaration                 }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes channel registration log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastChannelRegisterTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TChannelRegisterTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TChannelRegisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TChannelRegisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelUnregisterTimeLogEvent // Class declaration               }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes channel unregistration log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastChannelUnregisterTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TChannelUnregisterTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TChannelUnregisterTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TChannelUnregisterTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastChannelTimeLogEvent // Class declaration                         }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes channel log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastChannelTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TChannelTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TChannelTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TChannelTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); reintroduce;
  end;

{==============================================================================}
{   TMulticastConfigTimeLogEvent // Class declaration                          }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called when reader/parser
            passes config log data with time.)

@member(IndexOf
    Returns index at which the handler is stored in internal list of handlers.

    @param Handler Handler routine whose index is requested.

    @returns Index of passed handler in the list, -1 when not found..)

@member(Add
    Adds passed handler to internal list of handlers. When @code(AllowDuplicity)
    is set to @true, the handler is added to the list even when it is already
    stored, when set to @false, no new list item is added and result contains
    index of  previous occurrence of passed handler routine in the list.

    @param Handler        New handler routine to be added.
    @param(AllowDuplicity Denotes whether passed routine can be stored in
                          internal list when already in it.)
                          
    @returns(Index at which the added routine is stored. -1 when storing has
             failed.))

@member(Remove
    Removes first or all occurrences, depending on @code(RemoveAll) value, of
    passed routine from internal list of handlers.

    @param Handler   Routine to be removed from the list.
    @param(RemoveAll Determines whether to @noAutoLink(remove) only first
                     occurence of passed handler or all of them.)

    @returns(Index at which the removed item was stored in the list, -1 when
             passed routine was not found.))

@member(Call
    Calls all stored handler routines with the same parameters as passed to this
    method.)
}
  TMulticastConfigTimeLogEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TConfigTimeLogEvent): Integer; reintroduce;
    Function Add(const Handler: TConfigTimeLogEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TConfigTimeLogEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); reintroduce;
  end;

{$ENDIF}

{$IFDEF ImplementationPart}
{==============================================================================}
{------------------------------------------------------------------------------}
{                           Muticast events managers                           }
{------------------------------------------------------------------------------}
{==============================================================================}

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
