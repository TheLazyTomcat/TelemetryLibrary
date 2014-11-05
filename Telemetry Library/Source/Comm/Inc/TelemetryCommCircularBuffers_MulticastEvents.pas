{$IFDEF DeclarationPart}
{==============================================================================}
{------------------------------------------------------------------------------}
{                           Muticast events managers                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMulticastCircularBufferItemNotifyEvent // Class declaration               }
{==============================================================================}
{
  @abstract(Class used to manage multicast event called by circular buffer on
            item operations.)

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
  TMulticastCircularBufferItemNotifyEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TCircularBufferItemNotifyEvent): Integer; reintroduce;
    Function Add(const Handler: TCircularBufferItemNotifyEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TCircularBufferItemNotifyEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Item: Pointer); reintroduce;
  end;
{$ENDIF}

{$IFDEF ImplementationPart}
{==============================================================================}
{------------------------------------------------------------------------------}
{                           Muticast events managers                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMulticastCircularBufferItemNotifyEvent // Class implementation            }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMulticastCircularBufferItemNotifyEvent // Public methods                  }
{------------------------------------------------------------------------------}

Function TMulticastCircularBufferItemNotifyEvent.IndexOf(const Handler: TCircularBufferItemNotifyEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastCircularBufferItemNotifyEvent.Add(const Handler: TCircularBufferItemNotifyEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastCircularBufferItemNotifyEvent.Remove(const Handler: TCircularBufferItemNotifyEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastCircularBufferItemNotifyEvent.Call(Sender: TObject; Item: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TCircularBufferItemNotifyEvent(Methods[i])(Sender,Item);
end;
{$ENDIF}
