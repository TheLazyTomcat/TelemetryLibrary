{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(List classes used in Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-04)
@lastmod(2015-07-09)

  @bold(@NoAutoLink(TelemetryLists))

  ©František Milt, all rights reserved.

  Last change: 2015-07-09

  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TCustomTelemetryList
   |- TKnownEventsList
   |- TKnownChannelsList
   |- TKnownConfigsList
   |- TRegisteredEventsList
   |- TRegisteredChannelsList
   |- TStoredConfigsList
   |- TStoredChannelsList
)

@html(<hr>)}
unit TelemetryLists;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  Classes,
  AuxTypes,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}   
{$ENDIF}
  TelemetryCommon,
  TelemetryValueTypeUtils,
  TelemetryIDs,
{$IFDEF Documentation}
  TelemetryConversions,
  TelemetryStrings,
{$ENDIF}
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}

type
  //:Event type used when user data stored in event or channel context should be
  //:freed.
  TUserDataFreeEvent = procedure(Sender: TObject; var UserData: Pointer) of object;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TCustomTelemetryList                             }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TCustomTelemetryList // Class declaration                                  }
{==============================================================================}
{: @abstract(Ancestor class for all other list classes in TelemetryLists unit.)

  TCustomTelemetryList serves as ancestor for all other list classes declared in
  this unit. It wrapps around TList class and adds methods used for easier list
  management in descendant classes.
}
type
  TCustomTelemetryList = class(TObject)
  private
  {:
    Internal list used to hold actual items. It is not publicly visible and is
    therefore managed automatically.
  }
    fMainList:      TList;
  {:
    When this field has value larger than 0, it indicates that the list is being
    updated from outside and OnChange event should not be called.@br
    It is set by methods BeginUpdate and EndUpdate.
  }
    fUpdateCount:   Integer;
  {:
    Holds reference to OnChange event handler.
  }
    fOnChange:      TNotifyEvent;
  {$IFDEF MulticastEvents}
  {:
    Object handling multicast OnChange event.
  }
    fOnChangeMulti: TMulticastNotifyEvent;
  {$ENDIF}
  protected
  {:
    Getter for property Count.@br
    Returns number of items in the list.
  }
    Function GetCount: Integer; virtual;
  {:
    Returns item (pointer) from internal list on position given by @code(Index)
    parameter.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item in list.

    @returns Item at position given by @code(Index).

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function PtrGetItem(Index: Integer): Pointer; virtual;
  {:
    Returns index at which the passed item (pointer) is located in the list.@br
    When passed pointer is not found in list, -1 is returned.

    @param Item Item (pointer) whose index is requested.

    @returns Index of passed item in the list, -1 when not found.
  }
    Function PtrIndexOf(Item: Pointer): Integer; virtual;
  {:
    Method used to add new item into list. Added item can be @nil.@br
    Calls method DoChange when addition is successful.

    @param Item New item to be added to the list.

    @returns Index at which the new item was put, -1 when the operation failed.
  }
    Function PtrAdd(Item: Pointer): Integer; virtual;
  {:
    Replaces item (change its value) at position given by @code(Index)
    parameter. Item can be @nil. When index falls out of allowed boundary
    (<0,Count - 1>), an exception is raised.@br
    Calls method DoChange when item is successfully replaced.

    @param Index Index of item that has to be replaced.
    @param Item  New walue of the replaced item.)

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@). 
  }
    procedure PtrReplace(Index: Integer; Item: Pointer); virtual;
  {:
    Inserts new item at position given by @code(Index). List Count is increased
    by one and all existing items from given position (included) up are moved
    higher. @code(Item) can be @nil. When index falls out of allowed boundary
    (<0,Count> - passed index can be higher than current highest index, if so,
    item is added at the end of the list), and exception is raised.@br
    Calls method DoChange when item is successfully inserted.

    @param Index Index at which the new item should be added.
    @param Item  Inserted item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure PtrInsert(Index: Integer; Item: Pointer); virtual;
  {:
    Removes item passed in @code(Item) parameter. @code(Item) should not be, but
    can be @nil. When given item is not found in list, nothing is removed and
    the function fails with no error (-1 is returned). If the list contains more
    than one copy of the removed item, only the first occurence is removed.@br
    Calls method DoChange when item is successfully removed.

    @param Item Item that has to be removed.

    @returns(Index at which the removed item was place before removal, -1 whem
             given item was not found in the list.)
  }
    Function PtrRemove(Item: Pointer): Integer; virtual;
  {:
    Deletes item at position given by @code(Index) parameter. When index falls
    out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    Calls method DoChange when item is successfully deleted.

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure PtrDelete(Index: Integer); virtual;
  public
  {:
    Object constructor.
  }
    constructor Create;
  {:
    ObjectDestructor.@br
    Method Clear is called within destructor, so it is not necessary to
    explicitly call it when class instance is freed.

    @bold(Note) - OnChange event is not called.
  }
    destructor Destroy; override;
  {:
    Increases fUpdateCount.
  }
    procedure BeginUpdate; virtual;
  {:
    Decreases fUpdateCount and calls DoChange when it reaches zero or lower.
  }
    procedure EndUpdate; virtual;
  {:
    Calls handler of OnChange event (if assigned) - but only if fUpdateCount is
    zero or lower, otherwise the handler is not called and method returns
    immediately.
  }
    procedure DoChange; virtual;
  {:
    Deletes all items in the list. Only deletes stored pointers, allocated
    memory they are pointing to is not freed. Override this method in
    descendants to free memory used by items (put inherited code at the end).@br
    Calls method DoChange after all items are deleted.
  }
    procedure Clear; virtual;
  published
  {:
    Number of items stored in the list.
  }
    property Count: Integer read GetCount;
  {:
    Event called whenever the list is changed. It can also be called manually by
    calling DoChange method. It is NOT called when fUpdateCount is higher than
    zero (but can be still called directly if handler is assigned).
  }
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  {$IFDEF MulticastEvents}
  {:
    Multicast event called whenever the list is changed. It can also be called
    manually by calling DoChange method. It is NOT called when fUpdateCount is
    higher than zero (but can be still called directly).@br
    Assigning handlers to this event does not interfere with normal OnChange
    event.@br
    Normal OnChange event is called first, handlers of this event are called
    afterwards.
  }
    property OnChangeMulti: TMulticastNotifyEvent read fOnChangeMulti;
  {$ENDIF}
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                               TKnownEventsList                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  Structure used as an item in TKnownEventsList class.

  @member(Event   @NoAutoLink(Event) identification number.)
  @member(Name    @NoAutoLink(Name) assigned to @NoAutoLink(event) (this
                  @NoAutoLink(name) is not defined by Telemetry SDK, do not
                  assume its value as it can change in the future).)
  @member(Valid   Denotes whether @NoAutoLink(event) is @NoAutoLink(valid)
                  (invalid events cannot be successfuly registered).)
  @member(Utility Denotes whether @NoAutoLink(event) is marked as
                  @NoAutoLink(utility) (such events cannot be unregistered
                  and/or are registered automatically at certain
                  circumstances).)
}
  TKnownEvent = record
    Event:    scs_event_t;
    Name:     TelemetryString;
    Valid:    Boolean;
    Utility:  Boolean;
  end;
  //:Pointer to TKnownEvent structure.
  PKnownEvent = ^TKnownEvent;

const
  //:Empty TKnownEvent structure.
  EmptyKnownEvent: TKnownEvent =
   (Event:    SCS_TELEMETRY_EVENT_invalid;
    Name:     '';
    Valid:    False;
    Utility:  False);

{==============================================================================}
{   TKnownEventsList // Class declaration                                      }
{==============================================================================}
{:
  List used to store information about known telemetry @noAutoLink(events).
}
type
  TKnownEventsList = class(TCustomTelemetryList)
  private
  {:
    Getter for Pointers property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetKnownEventPointer(Index: Integer): PKnownEvent;
  {:
    Getter for Events property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetKnownEvent(Index: Integer): TKnownEvent;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.
  }
    procedure Clear; override;
  {:
    Searches through list for given event. When the event is not found, -1 is
    returned.

    @param Event Event whose index is requested.

    @returns Index of requested event, -1 when not found.
  }
    Function IndexOf(Event: scs_event_t): Integer; overload; virtual;
  {:
    Searches through list for event with given name (case-insensitive). When
    event with that name is not found, -1 is returned.

    @param Name Name of the event whose index is requested.

    @returns Index of event with requested name, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString): Integer; overload; virtual;
  {:
    Adds new known game event into the list.@br
    OnChange event is called after successful addition.

    @param Event   Event identification number.
    @param Name    Name of added event.
    @param Valid   Flag denoting whether added event is marked as valid.
    @param Utility Flag denoting whether added event is marked as utility.

    @returns Index at which the new event was added, -1 when addition failed.
  }
    Function Add(Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer; virtual;
  {:
    Replaces event at position given by @code(Index) parameter. When index falls
    out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful replacement.

    @param Index   Index of item that has to be replaced.
    @param Event   Replacement event identification number.
    @param Name    Name of replacement event.
    @param Valid   Flag denoting whether replacement event is marked as valid.
    @param(Utility Flag denoting whether replacement event is marked as
                   utility.)

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).                
  }
    procedure ReplaceIndex(Index: Integer; Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False); virtual;
  {:
    Replaces event given by @code(OldEvent) parameter. When this old event is
    not found in the list, nothing happens and the method returns -1.@br
    OnChange event is called after successful replacement.

    @param OldEvent   Identification number of event that has to be replaced.
    @param Event      Replacement event identification number.
    @param Name       Name of replacement event.
    @param(Valid      Flag denoting whether replacement event is marked as
                      valid.)
    @param(Utility    Flag denoting whether replacement event is marked as
                      utility.)

    @returns(Index of event that was replaced, -1 when such event was not
             found.)
  }
    Function Replace(OldEvent, Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer; virtual;
  {:
    Inserts new event at position given by @code(Index) parameter. Count is
    increased by one and all existing items from given position (included) up
    are moved higher. When index falls out of allowed boundary (<0,Count> -
    passed index can be higher than current highest index, if so, item is added
    at the end of the list), and exception is raised.@br
    OnChange event is called after successful insertion.

    @param Index   Position where the new event should be inserted.
    @param Event   Event identification number.
    @param Name    Name of event.
    @param Valid   Flag denoting whether event is marked as valid.
    @param Utility Flag denoting whether event is marked as utility.

    @returns Actual position where the new event was inserted.
  }
    Function Insert(Index: Integer; Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer; virtual;
  {:
    Removes given event from the list. When this event is not found in the list,
    method returns -1 and nothing is removed.@br
    OnChange event is called after successful removal.

    @param(Event Identification number of event that has to be removed from the
                 list.)

    @returns(Index of item that was removed, -1 when requested event was not
             found.)
  }
    Function Remove(Event: scs_event_t): Integer; virtual;
  {:
    Deletes event at position given by @code(Index) parameter. When index falls
    out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful deletion.

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure Delete(Index: Integer); virtual;
  {:
    Returns @true when given event is valid, @false when it is not valid or when
    that event is not in list.

    @param Event Identification number of requested event.

    @returns Validity of requested event.
  }
    Function IsValid(Event: scs_event_t): Boolean; virtual;
  {:
    Returns @true when given event is marked as Utility, @false when it is not
    or when not found in the list.

    @param Event Identification number of requested event.

    @returns Utility state of requested event.
  }
    Function IsUtility(Event: scs_event_t): Boolean; virtual;
  {:
    Array property mapped directly to internal list. Use it for direct access to
    individual stored items.@br
    Unlike Events property, you can use returned pointer to change values of
    stored items.
  }
    property Pointers[Index: Integer]: PKnownEvent read GetKnownEventPointer;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.
  }
    property Events[Index: Integer]: TKnownEvent read GetKnownEvent; default;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                              TKnownChannelsList                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  Structure used as an item in TKnownChannelsList class.

  @member(Name           @NoAutoLink(Name) of the channel.)
  @member(ID             Identifier of the channel (calculated from
                         @noAutoLink(name)).)
  @member(PrimaryType    Primary value type of the channel.)
  @member(SecondaryTypes Bitmask storing all secondary types this channel can
                         provide for its value. For details about how they are
                         stored, refer to description of type TValueTypeBitmask.)
  @member(Indexed        Flag indicating whether the channel is
                         @NoAutoLink(indexed).)
  @member(IndexConfig    Full reference of a config that should store
                         count (that is, maxindex + 1) for this channel (has
                         meaning only for @NoAutoLink(indexed) channels).)
  @member(MaxIndex       Maximum index for @NoAutoLink(indexed) channels.)
}
  TKnownChannel = record
    Name:           TelemetryString;
    ID:             TChannelID;
    PrimaryType:    scs_value_type_t;
    SecondaryTypes: TValueTypeBitmask;
    Indexed:        Boolean;
    IndexConfig:    TConfigReference;
    MaxIndex:       scs_u32_t;
  end;
  //:Pointer to TKnownChannel structure.
  PKnownChannel = ^TKnownChannel;

const
  //:Empty TKnownChannel structure.
  EmptyKnownChannel: TKnownChannel =
   (Name:           '';
    ID:             0;
    PrimaryType:    SCS_VALUE_TYPE_INVALID;
    SecondaryTypes: NoValueType;
    Indexed:        False;
    IndexConfig: (
      ID:             '';
      Attribute:      '');
    MaxIndex:       SCS_U32_NIL);


{==============================================================================}
{   TKnownChannelsList // Class declaration                                    }
{==============================================================================}
{:
  List used to store information about known telemetry @noAutoLink(channels).
}
type
  TKnownChannelsList = class(TCustomTelemetryList)
  private
  {:
    Getter for Pointers property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetKnownChannelPointer(Index: Integer): PKnownChannel;
  {:
    Getter for Events property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetKnownChannel(Index: Integer): TKnownChannel;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.
  }
    procedure Clear; override;
  {:
    Searches through list for channel with given name (case-sensitive). When
    the channel is not found, -1 is returned.

    @param Name Name of the requested channel.

    @returns Index of channel with requested name, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString): Integer; overload; virtual;
  {:
    Searches through list for channel with given ID. When channel with that ID
    is not found, -1 is returned.

    @param ID ID of channel whose index is requested.

    @returns Index of channel with requested ID, -1 when not found.
  }
    Function IndexOf(ID: TChannelID): Integer; overload; virtual;
  {:
    Adds new known channel into the list.@br
    OnChange event is called after successful addition.

    @param Name           Name of added channel (ID is calculated from it).
    @param PrimaryType    Primary type of value for this channel.
    @param(SecondaryTypes Bitmask with secondary value types this channel can
                          provide.)
    @param Indexed        Flag denoting whether added channel is indexed.
    @param(IndexConfig    Full reference of the config containing
                          @noAutoLink(count) for channel indices. Has no meaning
                          when the channel is not indexed.)

    @returns Index at which the new channel was added, -1 when addition failed.
  }
    Function Add(const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Adds new known channel into the list.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    SecondaryValueTypesBitmask whose parameter @code(PrimaryValueType) is set
    from corresponding parameter of this method.@br
    OnChange event is called after successful addition.

    @param Name        Name of added channel (ID is calculated from it).
    @param PrimaryType Primary type of value for this channel.
    @param Indexed     Flag denoting whether added channel is indexed.
    @param(IndexConfig Full reference of the config containing
                       @noAutoLink(count) for channel indices. Has no meaning
                       when the channel is not indexed.)

    @returns Index at which the new channel was added, -1 when addition failed.
  }
    Function Add(const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Adds new known channel into the list.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    ValueTypesBitmask whose parameter @code(ValueTypes) consists of values
    passed in parameters @code(SecondaryType) and @code(TertiaryType).@br
    OnChange event is called after successful addition.

    @param Name          Name of added channel (ID is calculated from it).
    @param PrimaryType   Primary type of value for this channel.
    @param(SecondaryType Secondary (first secondary) type of value for this
                         channel.)
    @param(TertiaryType  Tertiary (second secondary) type of value for this
                         channel.)
    @param Indexed       Flag denoting whether added channel is indexed.
    @param(IndexConfig   Full reference of the config containing
                         @noAutoLink(count) for channel indices. Has no meaning
                         when the channel is not indexed.)

    @returns Index at which the new channel was added, -1 when addition failed.
  }
    Function Add(const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Replaces channel at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful replacement.

    @param Index          Index of item that has to be replaced.
    @param Name           Name of replacement channel (ID is calculated from it).
    @param PrimaryType    Primary type of value for this channel.
    @param(SecondaryTypes Bitmask with secondary value types this channel can
                          provide.)
    @param Indexed        Flag denoting whether channel is indexed.
    @param(IndexConfig    Full reference of the config containing
                          @noAutoLink(count) for channel indices. Has no meaning
                          when the channel is not indexed.)

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL); overload; virtual;
  {:
    Replaces channel at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    SecondaryValueTypesBitmask whose parameter @code(PrimaryValueType) is set
    from corresponding parameter of this method.@br
    OnChange event is called after successful replacement.

    @param Index       Index of item that has to be replaced.
    @param Name        Name of replacement channel (ID is calculated from it).
    @param PrimaryType Primary type of value for this channel.
    @param Indexed     Flag denoting whether channel is indexed.
    @param(IndexConfig Full reference of the config containing
                       @noAutoLink(count) for channel indices. Has no meaning
                       when the channel is not indexed.)
  }
    procedure ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL); overload; virtual;
  {:
    Replaces channel at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    ValueTypesBitmask whose parameter @code(ValueTypes) consists of values
    passed in parameters @code(SecondaryType) and @code(TertiaryType).@br
    OnChange event is called after successful replacement.

    @param Index         Index of item that has to be replaced.
    @param Name          Name of replacement channel (ID is calculated from it).
    @param PrimaryType   Primary type of value for this channel.
    @param(SecondaryType Secondary (first secondary) type of value for this
                         channel.)
    @param(TertiaryType  Tertiary (second secondary) type of value for this
                         channel.)
    @param Indexed       Flag denoting whether channel is indexed.
    @param(IndexConfig   Full reference of the config containing
                         @noAutoLink(count) for channel indices. Has no meaning
                         when the channel is not indexed.)
  }
    procedure ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL); overload; virtual;
  {:
    Replaces channel with name given by @code(OldChannel) parameter. When this
    channel is not found in the list, nothing happens and the method returns
    -1.@br
    OnChange event is called after successful replacement.

    @param(OldEvent       Identification number of channel that has to be
                          replaced.)
    @param Name           Name of replacement channel (ID is calculated from it).
    @param PrimaryType    Primary type of value for this channel.
    @param(SecondaryTypes Bitmask with secondary value types this channel can
                          provide.)
    @param Indexed        Flag denoting whether channel is indexed.
    @param(IndexConfig    Full reference of the config containing
                          @noAutoLink(count) for channel indices. Has no meaning
                          when the channel is not indexed.)

    @returns(Index of channel that was replaced, -1 when old channel was not
             found.)
  }
    Function Replace(const OldChannel, Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Replaces channel with name given by @code(OldChannel) parameter. When this
    channel is not found in the list, nothing happens and the method returns
    -1.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    SecondaryValueTypesBitmask whose parameter @code(PrimaryValueType) is set
    from corresponding parameter of this method.@br
    OnChange event is called after successful replacement.

    @param(OldEvent    Identification number of channel that has to be replaced.)
    @param Name        Name of replacement channel (ID is calculated from it).
    @param PrimaryType Primary type of value for this channel.
    @param Indexed     Flag denoting whether channel is indexed.
    @param(IndexConfig Full reference of the config containing
                       @noAutoLink(count) for channel indices. Has no meaning
                       when the channel is not indexed.)

    @returns(Index of channel that was replaced, -1 when old channel was not
             found.)
  }
    Function Replace(const OldChannel, Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Replaces channel with name given by @code(OldChannel) parameter. When this
    channel is not found in the list, nothing happens and the method returns
    -1.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    ValueTypesBitmask whose parameter @code(ValueTypes) consists of values
    passed in parameters @code(SecondaryType) and @code(TertiaryType).@br
    OnChange event is called after successful replacement.

    @param(OldEvent      Identification number of channel that has to be
                         replaced.)
    @param Name          Name of replacement channel (ID is calculated from it).
    @param PrimaryType   Primary type of value for this channel.
    @param(SecondaryType Secondary (first secondary) type of value for this
                         channel.)
    @param(TertiaryType  Tertiary (second secondary) type of value for this
                         channel.)
    @param Indexed       Flag denoting whether channel is indexed.
    @param(IndexConfig   Full reference of the config containing
                         @noAutoLink(count) for channel indices. Has no meaning
                         when the channel is not indexed.)

    @returns(Index of channel that was replaced, -1 when old channel was not
             found.)
  }
    Function Replace(const OldChannel, Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Inserts new channel at position given by @code(Index) parameter. Count is
    increased by one and all existing items from given position (included) up
    are moved higher. When index falls out of allowed boundary (<0,Count> -
    passed index can be higher than current highest index, if so, item is added
    at the end of the list), and exception is raised.@br
    OnChange event is called after successful insertion.

    @param Index          Position where the new channel should be inserted.
    @param Name           Name of inserted channel (ID is calculated from it).
    @param PrimaryType    Primary type of value for this channel.
    @param(SecondaryTypes Bitmask with secondary value types this channel can
                          provide.)
    @param Indexed        Flag denoting whether channel is indexed.
    @param(IndexConfig    Full reference of the config containing
                          @noAutoLink(count) for channel indices. Has no meaning
                          when the channel is not indexed.)

    @returns Actual position where the new channel was inserted.
  }
    Function Insert(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Inserts new channel at position given by @code(Index) parameter. Count is
    increased by one and all existing items from given position (included) up
    are moved higher. When index falls out of allowed boundary (<0,Count> -
    passed index can be higher than current highest index, if so, item is added
    at the end of the list), and exception is raised.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    SecondaryValueTypesBitmask whose parameter @code(PrimaryValueType) is set
    from corresponding parameter of this method.@br
    OnChange event is called after successful insertion.

    @param Index       Position where the new channel should be inserted.
    @param Name        Name of inserted channel (ID is calculated from it).
    @param PrimaryType Primary type of value for this channel.
    @param Indexed     Flag denoting whether channel is indexed.
    @param(IndexConfig Full reference of the config containing
                       @noAutoLink(count) for channel indices. Has no meaning
                       when the channel is not indexed.)

    @returns Actual position where the new channel was inserted.
  }
    Function Insert(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Inserts new channel at position given by @code(Index) parameter. Count is
    increased by one and all existing items from given position (included) up
    are moved higher. When index falls out of allowed boundary (<0,Count> -
    passed index can be higher than current highest index, if so, item is added
    at the end of the list), and exception is raised.@br
    Internally calls variant of this method that has parameter
    @code(SecondaryTypes) with this parameter filled by function
    ValueTypesBitmask whose parameter @code(ValueTypes) consists of values
    passed in parameters @code(SecondaryType) and @code(TertiaryType).@br    
    OnChange event is called after successful insertion.

    @param Index         Position where the new channel should be inserted.
    @param Name          Name of inserted channel (ID is calculated from it).
    @param PrimaryType   Primary type of value for this channel.
    @param(SecondaryType Secondary (first secondary) type of value for this
                         channel.)
    @param(TertiaryType  Tertiary (second secondary) type of value for this
                         channel.)
    @param Indexed       Flag denoting whether channel is indexed.
    @param(IndexConfig   Full reference of the config containing
                         @noAutoLink(count) for channel indices. Has no meaning
                         when the channel is not indexed.)

    @returns Actual position where the new channel was inserted.
  }
    Function Insert(Index: Integer; const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Removes channel with given name from the list. When this channel is not
    found in the list, method returns -1 and nothing is removed.@br
    OnChange event is called after successful removal.

    @param Name Name of the channel that has to be removed from the list.

    @returns(Index of item that was removed, -1 when requested channel was not
             found.)
  }
    Function Remove(const Name: TelemetryString): Integer; virtual;
  {:
    Deletes channel at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful deletion.

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure Delete(Index: Integer); virtual;
  {:
    Returns @code(IndexConfig) (config reference) for channel with name given in
    @code(Name) parameter. When such channel is not found in the list, an empty
    reference is returned.

    @param Name Name of requested channel.

    @returns Reference of index config for requested channel.
  }
    Function ChannelIndexConfigID(const Name: TelemetryString): TConfigReference; virtual;
  {:
    Returns ID for passed channel name.

    @param Name Name of chanel for which you want an ID.

    @returns ID of passed channel name.
  }
    Function ChannelNameToID(const Name: TelemetryString): TChannelID; virtual;
  {:
    Returns name of channel with the same ID as is passed in @code(ID) prameter.
    This method actually searches through the list for channel with appropriate
    ID. When such channel is not found, an empty string is returned.

    @param ID ID of requested channel.

    @returns Name of the channel with appropriate ID.
  }
    Function ChannelIDToName(ID: TChannelID): TelemetryString; virtual;
  {:
    Array property mapped directly to internal list. Use it for direct access to
    individual stored items.@br
    Unlike Channels property, you can use returned pointer to change values of
    stored items.
  }
    property Pointers[Index: Integer]: PKnownChannel read GetKnownChannelPointer;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.
  }
    property Channels[Index: Integer]: TKnownChannel read GetKnownChannel; default;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TKnownConfigsList                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  Structure used as a subitem of items in TKnownConfigsList class.

  @member(Name      @NoAutoLink(Name) of the config attribute.)
  @member(ValueType Type of value for current config.)
  @member(Indexed   Flag indicating whether the config is @NoAutoLink(indexed).)
  @member(Binded    Flag denoting whether this config is @NoAutoLink(binded) by
                    a channel (meaning that some @NoAutoLink(indexed) channel
                    uses this config to obtain maximum index).)
}
  TKnownAttribute = record
    Name:       TelemetryString;
    ValueType:  scs_value_type_t;
    Indexed:    Boolean;
    Binded:     Boolean;
  end;
  //:Pointer to TKnownAttribute structure.
  PKnownAttribute = ^TKnownAttribute;

{:
  Used as an item in TKnownConfigsList class.

  @member(ID         Identifier of the configuration.)
  @member(Attributes Array of @noAutoLink(attributes) belonging to a particular
                     configuration.)
}
  TKnownConfiguration = record
    ID:         TelemetryString;
    Attributes: array of TKnownAttribute;
  end;
  //:Pointer to TKnownConfiguration structure.
  PKnownConfiguration = ^TKnownConfiguration;

{:
  Used for streaming of individual known configs.

  @member(ID         Identifier of the configuration group to which the config
                     belongs.)
  @member(Attribute  @noAutoLink(Attribute) data of the known config.)
}
  TKnownConfig = record
    ID:         TelemetryString;
    Attribute:  TKnownAttribute;
  end;

const
  //:Empty TKnownAttribute structure.
  EmptyKnownAttribute: TKnownAttribute = (
    Name:       '';
    ValueType:  SCS_VALUE_TYPE_INVALID;
    Indexed:    False;
    Binded:     False);

  //:Empty TKnownConfiguration structure.
  EmptyKnownConfiguration: TKnownConfiguration =(
    ID:         '';
    Attributes: nil);

  //:Empty TKnownConfig structure;
  EmptyKnownConfig: TKnownConfig = (
    ID:         '';
    Attribute:  (
      Name:       '';
      ValueType:  SCS_VALUE_TYPE_INVALID;
      Indexed:    False;
      Binded:     False));

{==============================================================================}
{   TKnownConfigsList // Auxiliary functions                                   }
{==============================================================================}

{:
  Auxiliary function that can be used to create in-place variable of type
  TKnownConfig.

  @param ID        Configuration group identifier.
  @param Attribute Attribute data.

  @returns TKnownConfig structure build from passed data.
}
Function KnownConfig(const ID: TelemetryString; Attribute: TKnownAttribute): TKnownConfig;

{==============================================================================}
{   TKnownConfigsList // Class declaration                                     }
{==============================================================================}
{:
  @abstract(List used to store information about known telemetry
            @noAutoLink(configurations) values.)

  Known @noAutoLink(configs) (note that a word "config", as used in here, means
  a specific value, that is, a specific attribute in a specifin configuration
  group) in this list are stored as two-level (or two-dimensional) arrray. First
  level consists af configuration groups (e.g. Truck, Trailer, ...), second
  level is an array of attributes for each of the configuration group.@br
  It can be visually presented as such:
@preformatted(
    KnownConfigsList
     |
     |- configuration[0]
     |     |- attribute[0]
     |     |- attribute[1]
     |
     |- configuration[1]
     |
     |- configuration[2]
     |     |- attribute[0]
     |
     |- configuration[3]
           |- attribute[0]
           |- attribute[1]
           |- attribute[2]
)
}
type
  TKnownConfigsList = class(TCustomTelemetryList)
  private
  {:
    Getter for ConfigurationCount property.

    @returns Number of @noAutoLink(configurations) stored in the list.
  }
    Function GetConfigurationCount: Integer; virtual;
  {:
    Getter for AttributeCount property.@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.

    @param(Index Index of configuration for which number of attributes is
                 requested.)

    @returns(Number of attributes in configuration given by @code(Index)
             parameter.)                  

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
  }
    Function GetAttributeCount(Index: Integer): Integer; virtual;
  {:
    Getter for ConfigurationPointers property.@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.

    @param Index Index of requested configuration.

    @returns Pointer to requested configuration.

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
  }
    Function GetKnownConfigurationPointer(Index: Integer): PKnownConfiguration;
  {:
    Getter for Configurations property.@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.

    @param Index Index of requested configuration.

    @returns Requested configuration.

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
  }
    Function GetKnownConfiguration(Index: Integer): TKnownConfiguration;
  {:
    Getter for Pointers property.@br
    When any index falls out of allowed boundary, an exception is raised.

    @param(Indices Indices of requested attribute (index of configuration and
                   attribute within it).)

    @returns Pointer to requested config attribute.

    @raises(ETLIndexOfBounds When any index is out of allowed interval
                             (Index1 - <0,ConfigurationCount - 1>,
                             Index2 - <0,AttributeCount[Index1] - 1>).)
  }
    Function GetKnownConfigPointer(Indices: TDoubleIndex): PKnownAttribute;
  {:
    Getter for Configs property.@br
    When any index falls out of allowed boundary, an exception is raised.

    @param(Indices Indices of requested attribute (index of configuration and
                   attribute within it).)

    @returns Requested config attribute.

    @raises(ETLIndexOfBounds When any index is out of allowed interval
                             (Index1 - <0,ConfigurationCount - 1>,
                             Index2 - <0,AttributeCount[Index1] - 1>).)
  }
    Function GetKnownConfig(Indices: TDoubleIndex): TKnownAttribute;
  protected
  {:
    Getter for Count propery.

    @returns Total number of all attributes in all @noAutoLink(configurations).
  }
    Function GetCount: Integer; override;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.
  }
    procedure Clear; override;
  {:
    Deletes all atributes in configuration stored at position given by
    @code(Index) parameter.@br
    When index falls out of allowed boundary, the function does nothing.

    @param Index Index of configuration that should be cleared.
  }
    procedure ClearConfiguration(Index: Integer); virtual;
  {:
    Searches through list for configuration with given ID (case-sensitive).
    When the config is not found, -1 is returned.

    @param ID   ID of the requested configuration.

    @returns Index of configuration with requested ID, -1 when not found.
  }
    Function IndexOfConfiguration(ID: TelemetryString): Integer; virtual;
  {:
    Searches through list for configuration with given ID and attribute with
    given name within it (case-sensitive).
    When the config is not found, invalid double index is returned.

    @param(ID         ID of the configuration in which to search for requested
                      attribute.)
    @param Attribute  Attribute name of the requested config.

    @returns(Indices of config with requested name, invalid double index when
             not found.)
  }
    Function IndexOf(ID,Attribute: TelemetryString): TDoubleIndex; overload; virtual;
  {:
    Searches through list for config with given config reference
    (case-sensitive).
    When the config is not found, invalid double index is returned.

    @param Reference  Full reference of requested config (ID + Attribute).

    @returns(Indices of config with requested reference, invalid double index
             when  not found.)
  }
    Function IndexOf(ConfigReference: TConfigReference): TDoubleIndex; overload; virtual;
  {:
    Searches through configuration given by @code(Index) parameter for config
    with passed attribute name (case-sensitive).@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.@br
    If the config is not found, invalid double index is returned.


    @param Index     Index of configuration where to search for given attribute.
    @param Attribute Name of the requested attribute.

    @returns(Indices of requested config, invalid double index when not found.)

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
  }
    Function IndexOf(Index: Integer; const Attribute: TelemetryString): Integer; overload; virtual;
  {:
    Adds new empty configuration to the list.@br
    If configuration of the same name already exists, the function only returns
    index of existing item.@br
    OnChange event is called after successful addition.

    @param ID  Identifier of the added configuration.

    @returns Index at which the configuretation has been placed.
  }
    Function AddConfiguration(const ID: TelemetryString): Integer; virtual;
  {:
    Renames configuration stored at given index.@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.@br
    When there is already configuration with the same name as passed in
    parameter @code(NewID) and it is stored at different index than is
    requested, an exception is raised.@br
    OnChange event is called after successful renaming.

    @param Index  Index of the configuration that has to be renamed.
    @param NewID  New identifier of the requested configuration.

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
    @raises ETLAlreadyExists When requested configuration already exists.
  }
    procedure RenameConfiguration(Index: Integer; const NewID: TelemetryString); overload; virtual;
  {:
    Renames requested configuration.@br
    When there is already configuration with the same name as passed in
    parameter @code(NewID), an exception is raised.@br
    OnChange event is called after successful renaming.

    @param OldID  Identifier of the configuration to be renamed.
    @param NewID  New identifier of the requested configuration.

    @returns(Index of renamed configuration, -1 when configuration with
             @code(OldID) is not in list.)

    @raises(ETLAlreadyExists When configuration with @code(NewID) identifier
                             already exists.)
  }
    Function RenameConfiguration(const OldID, NewID: TelemetryString): Integer; overload; virtual;
  {:
    Inserts new configuration at position given by @code(Index) parameter.
    When index is lower than zero or higher than ConfigurationCount - 1, then
    new configuration is added instead of inserted.@br
    When there is already configuration with the same name as passed in
    parameter @code(ID), an exception is raised.@br
    OnChange event is called after successful insertion.

    @param Index  Position at which the new configuration should be inserted.
    @param ID     Identifier of the new configuration.

    @returns Index at which the new configuration has been inserted or added.

    @raises(ETLAlreadyExists When configuration with @code(NewID) identifier
                             already exists.)    
  }
    Function InsertConfiguration(Index: Integer; const ID: TelemetryString): Integer; virtual;
  {:
    Removes configuration of a given name form the list.@br
    OnChange event is called after successful removal.

    @param ID Identifier of the configuration to be removed.

    @returns(Index at which the configuration has been stored prior to its
             deletion, -1 when it was not found.)
  }
    Function RemoveConfiguration(const ID: TelemetryString): Integer; virtual;
  {:
    Deletes configuration stored at position given by @code(Index) parameter.@br
    When index falls out of allowed boundary (<0,ConfigurationCount - 1>),
    an exception is raised.@br
    OnChange event is called after successful deletion.

    @param Index Index of configuration to be deleted.

    @raises(ETLIndexOfBounds When index is out of the interval
                             <0,ConfigurationCount@).)
  }
    procedure DeleteConfiguration(Index: Integer); virtual;
  {:
    Adds new known config into the list.@br
    If configuration given by the @code(ID) parameter does not exists, it is
    automatically created.
    OnChange event is called after successful addition.

    @param(ID            Identifier of configuration to which to
                         @noAutoLink(add) the new attribute.)
    @param Attribute     Name of the new attribute.
    @param ValuetyType   Type of this @noAutoLink(configs) value.
    @param Indexed       Flag denoting whether added config is indexed.
    @param(Binded        Flag denoting whether added config is binded by some
                         channel (ie. some channel has this config as its
                         IndexConfig property).)

    @returns(Position at which the new config was added, invalid double index
             when the addition failed.)
  }
    Function Add(const ID, Attribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex; overload; virtual;
  {:
    Adds new known config into the list.@br
    If configuration given by the @code(ConfigReference.ID) field does not
    exists, it is automatically created.
    OnChange event is called after successful addition.

    @param(Reference     Full reference (ID + Attribute) of the newly created
                         config.)
    @param ValuetyType   Type of this @noAutoLink(configs) value.
    @param Indexed       Flag denoting whether added config is indexed.
    @param(Binded        Flag denoting whether added config is binded by some
                         channel.)

    @returns(Position at which the new config was added, invalid double index
             when the addition failed.)
  }
    Function Add(ConfigReference: TConfigReference; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex; overload; virtual;
  {:
    Replaces config stored at the position given by @code(Indices) parameter.@br
    When any index falls out of allowed boundary, an exception is raised.@br
    OnChange event is called after successful replacement.

    @param(Indices       Position of config that should be replaced (index of
                         configuration and index of attribute within this
                         configuration).)
    @param NewAttribute  Name of the replacement attribute.
    @param ValuetyType   Type of this @noAutoLink(configs) value.
    @param Indexed       Flag denoting whether the config is indexed.
    @param(Binded        Flag denoting whether the config is binded by some
                         channel.)

    @raises(ETLIndexOfBounds When Index1 is out of interval
                             <0,ConfigurationCount - 1> or Index2 is out of
                             interval <0,AttributeCount[Index1] - 1>.)
  }
    procedure ReplaceIndex(Indices: TDoubleIndex; const NewAttribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False); overload; virtual;
  {:
    Replaces config given by configuration ID and attribute name.@br
    When either of configuration or attribute does not exists, the function
    returns immediatelly, returning invalid double index.@br
    OnChange event is called after successful replacement.

    @param(ID            Identifier of configuration in which the attribute will
                         be replaced.)
    @param OldAttribute  Name of the attribute to be replaced.
    @param NewAttribute  Name of the replacement attribute.
    @param ValuetyType   Type of this @noAutoLink(configs) value.
    @param Indexed       Flag denoting whether the config is indexed.
    @param(Binded        Flag denoting whether the config is binded by some
                         channel.)

    @returns(Position of the replaced config, invalid double index when the
             replacement failed.)
  }
    Function Replace(const ID, OldAttribute, NewAttribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex; virtual;
  {:
    Inserts new config into configuration given by @code(ID) parameter at
    position given by @code(Index) parameter.@br
    When the requested configuration does not exists, the function fails and
    will return invalid double index. When the passed index is out of interval
    <0,AttributeCount - 1> (in given configuration), then it gets normally added
    at the end of array.@br
    OnChange event is called after successful insertion.

    @param(ID            Identifier of configuration to which the attribute will
                         be inserted)
    @param(Index         Position in the configuration where the new config
                         should be inserted.)
    @param Attribute     Name of the new attribute.
    @param ValuetyType   Type of this @noAutoLink(configs) value.
    @param Indexed       Flag denoting whether the config is indexed.
    @param(Binded        Flag denoting whether the config is binded by some
                         channel.)

    @returns(Position at which the config was placed, invalid double index on
             fail.)
  }
    Function Insert(const ID: TelemetryString; Index: Integer; const Attribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex; virtual;
  {:
    Removes config given by parameters @code(ID) and @code(Attribute).@br
    If the config is not found in the list, the function returns invalid double
    index.@br
    OnChange event is called after successful removal.

    @param(ID         Identifier of configuration from which to remove the
                      config.)
    @param Attribute  Name of the attribute to be removed.

    @returns(Position where the removed config were stored, invalid double index
             when it was not found in the list.)
  }
    Function Remove(const ID, Attribute: TelemetryString): TDoubleIndex; overload; virtual;
  {:
    Removes config given by a config reference.@br
    If the config is not found in the list, the function returns invalid double
    index.@br
    OnChange event is called after successful removal.

    @param(Reference Full reference (ID + Attribute) of the config to be
                     removed.)

    @returns(Position where the removed config were stored, invalid double index
             when it was not found in the list.)
  }
    Function Remove(ConfigReference: TConfigReference): TDoubleIndex; overload; virtual;
  {:
    Deletes config from configuration given by parameted @code(ID) at position
    given by @code(Index).@br
    When requested configuration does not exists, an exception is raised.@br
    OnChange event is called after successful deletion.

    @param ID     Identifier of configuration from which to delete the config.
    @param(Index  Position of attribute to be deleted within the configuration
                  group.)

    @raises(ETLNotFound      When configuration given by parameter @code(ID)
                             does not exist.)
    @raises(ETLIndexOfBounds When Index is out of interval
                             <0,AttributeCount - 1> (in given configuration).)
  }
    procedure Delete(const ID: TelemetryString; Index: Integer); overload; virtual;
  {:
    Deletes config stored at position given by @code(Indices) parameter.@br
    When any index is out of allowed boundary, the function raises an
    exception.@br
    OnChange event is called after successful deletion.

    @param(Indices  Position of config to be deleted (index of configuration and
                    index of attribute within this configuration).)

    @raises(ETLIndexOfBounds When Index1 is out of interval
                             <0,ConfigurationCount - 1> or Index2 is out of
                             interval <0,AttributeCount[Index1] - 1>.)
  }
    procedure Delete(Indices: TDoubleIndex); overload; virtual;
  {:
    Returns @true when given config is binded, @false when it is not binded or
    when not found in the list.

    @param ID         Identifier of configuration in which the config resides.
    @param Attribute  Attribute name of the checked config.

    @returns Binded state of requested config.
  }
    Function IsBinded(const ID, Attribute: TelemetryString): Boolean; overload; virtual;
  {:
    Returns @true when given config is binded, @false when it is not binded or
    when not found in the list.

    @param Reference Full reference of the config to be checked.

    @returns Binded state of requested config.
  }
    Function IsBinded(ConfigReference: TConfigReference): Boolean; overload; virtual;
  {:
    Returns @true when given config is Indexed, @false when it is not Indexed or
    when not found in list.

    @param ID         Identifier of configuration in which the config resides.
    @param Attribute  Attribute name of the checked config.

    @returns Indexed state of requested config.
  }
    Function IsIndexed(const ID, Attribute: TelemetryString): Boolean; overload; virtual;
  {:
    Returns @true when given config is Indexed, @false when it is not Indexed or
    when not found in list.

    @param Reference Full reference of the config to be checked.

    @returns Indexed state of requested config.
  }
    Function IsIndexed(ConfigReference: TConfigReference): Boolean; overload; virtual;
  {:
    Array property intended for direct access to individual stored configuration
    groups.@br
    Unlike with Configurations property, you can use returned pointer to change
    stored data.
  }
    property ConfigurationPointers[Index: Integer]: PKnownConfiguration read GetKnownConfigurationPointer;
  {:
    Array property giving access to stored configuration groups.
  }
    property Configurations[Index: Integer]: TKnownConfiguration read GetKnownConfiguration;
  {:
    Array property intended for direct access to individual stored
    @noAutoLink(configs).@br
    Unlike with Configs property, you can use returned pointer to change stored
    data.
  }
    property Pointers[Indices: TDoubleIndex]: PKnownAttribute read GetKnownConfigPointer;
  {:
    Array property giving access to stored @noAutoLink(configs) (specific
    attribute in a specific configuration group).
  }
    property Configs[Indices: TDoubleIndex]: TKnownAttribute read GetKnownConfig; default;
  {:
    Number of @noAutoLink(configurations) stored in the list.
  }
    property ConfigurationCount: Integer read GetConfigurationCount;
  {:
    Number of attributes within configuration group given by passed index.
  }
    property AttributeCount[Index: Integer]: Integer read GetAttributeCount;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                            TRegisteredEventsList                             }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  Structure holding information about registered @noAutoLink(event).

  @member(Event    @NoAutoLink(Event) identifier.)
  @member(Utility  Denotes whether current @NoAutoLink(event) is marked as
                   @NoAutoLink(utility).)
}
  TEventInfo = record
    Event:    scs_event_t;
    Utility:  Boolean;
  end;
  //:Pointer to TEventInfo structure.
  PEventInfo = ^TEventInfo;

const
  //:Empty TEventInfo structure.
  EmptyEventInfo: TEventInfo =
   (Event:    SCS_TELEMETRY_EVENT_invalid;
    Utility:  False);

{:
  @abstract(Structure used as item in TRegisteredEventsList.)
  Pointer to this structure is passed as @noAutoLink(context) in telemetry API
  calls when registering telemetry event.

  @member(Recipient Object that should receive event callbacks.)
  @member(EventInfo Information about registered event.)
  @member(UserData  User data stored in the context.)
}
type
  TEventContext = record
    Recipient:  TObject;
    EventInfo:  TEventInfo;
    UserData:   Pointer;
  end;
  //:Pointer to TEventContext structure.
  PEventContext = ^TEventContext;

{==============================================================================}
{   TRegisteredEventsList // Class declaration                                 }
{==============================================================================}
{:
  @abstract(List used to store @noAutoLink(contexts of registered events).)

  When new event is registered in the telemetry API, it is registered with
  context which is actually pointer to a variable of TEventContext structure.
  This variable is at the same time added as a new item into this list.@br
  When event is unregistered, context it is bound to is removed from this list.
}
  TRegisteredEventsList = class(TCustomTelemetryList)
  private
  {:
    Holds reference to OnUserDataFree event handler.
  }
    fOnUserDataFree:  TUserDataFreeEvent;
  {:
    Getter for Contexts property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetEventContext(Index: Integer): PEventContext;
  {:
    Getter for Events property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetEventInfo(Index: Integer): TEventInfo;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.@br
    Contexts are freed using method FreeContext (OnUserDataFree event is
    called).
  }
    procedure Clear; override;
  {:
    Searches through list for context with requested event. When the context is
    not found, -1 is returned.

    @param Event Identification number of requested event.

    @returns Index of context with requested event, -1 when not found.
  }
    Function IndexOf(Event: scs_event_t): Integer; overload; virtual;
  {:
    Searches through list for given context. When the context is not found,
    -1 is returned.

    @param EventContext Requested event context.

    @returns Index of requested context, -1 when not found.
  }
    Function IndexOf(EventContext: PEventContext): Integer; overload; virtual;
  {:
    Adds new context into the list. @code(EventContext) parameter must not be
    @nil, otherwise an exception is raised.@br
    OnChange event is called after successful addition.

    @param(EventContext Pointer to event context that has to be added to the
                        list.)

    @returns Index at which the new context was added, -1 when addition failed.

    @raises ETLNilReference When context pointer is not assigned.
  }
    Function Add(EventContext: PEventContext): Integer; overload; virtual;
  {:
    Creates and adds new context into the list.@br
    OnChange event is called after successful addition.

    @param Recipient Telemetry recipient registering this new context.
    @param Event     Identification number of registered event.
    @param(Utility   Flag indicating whether registered event is marked as
                     utility.)
    @param UserData  User defined data to be stored inside the event context.

    @returns Index at which the new context was added, -1 when addition failed.
  }
    Function Add(Recipient: TObject; Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): Integer; overload; virtual;
  {:
    Creates and adds new context into the list. Created context has
    @code(Recipient) field set to @nil.@br
    OnChange event is called after successful addition.

    @param Event     Identification number of registered event.
    @param(Utility   Flag indicating whether registered event is marked as
                     utility.)
    @param UserData  User defined data to be stored inside the event context.

    @returns Index at which the new context was added, -1 when addition failed.
  }
    Function Add(Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): Integer; overload; virtual;
  {:
    Removes context with given event from the list.@br
    OnChange event is called after successful removal.

    @param(Event Identification number of registered event whose context has to
                 be removed.)

    @returns(Index of item that was removed, -1 when context with requested
             event was not found.)
  }
    Function Remove(Event: scs_event_t): Integer; overload; virtual;
  {:
    Removes given context from the list.@br
    OnChange event is called after successful removal.

    @param EventContext Context to be removed.

    @returns(Index of item that was removed, -1 when requested context was not
             found.)
  }
    Function Remove(EventContext: PEventContext): Integer; overload; virtual;
  {:
    Deletes context at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful deletion.@br
    Contexts are freed using method FreeContext (OnUserDataFree event is
    called).

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure Delete(Index: Integer); virtual;
  {:
    This method creates new event context variable, fills it from parameters and
    returns pointer to this variable. It is used when registering new telemetry
    event, because actual context is needed for this registration, but cannot be
    added to this list before the registration is complete.@br
    Sequence of actions taken during registration is as follows:
    @unorderedlist(
      @itemSpacing(Compact)
      @item Context is created using this function.
      @item Registration in API is attempted.
      @item When successful, context is added to this list.
      @item When unsuccessful, context is freed using FreeContext method.
    )

    @param Recipient Telemetry recipient registering created context.
    @param Event     Identification number of registered event.
    @param(Utility   Flag indicating whether registered event is marked as
                     utility.)
    @param(UserData  User defined data to be stored inside the created event
                     context.)

    @returns Pointer to created event context.
  }
    Function CreateContext(Recipient: TObject; Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): PEventContext; virtual;
  {:
    Frees memory allocated for event context and sets this pointer to @nil.@br
    Calls OnUserDataFree event before the context is freed.

    @param EventContext Pointer to event context to be freed.
  }
    procedure FreeContext(var EventContext: PEventContext); virtual;
  {:
    Calls handler of OnUserDataFree event (if assigned).
  }
    procedure DoOnUserDataFree(Sender: TObject; var UserData: Pointer); virtual;
  {:
    Array property mapped directly to internal list. Use it for direct access to
    individual stored contexts.@br
    Unlike Events property, you can use returned pointer to change values of
    stored items.
  }
    property Contexts[Index: Integer]: PEventContext read GetEventContext;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.@br
    This property does not return whole context structure, only its "EventInfo"
    field.
  }
    property Events[Index: Integer]: TEventInfo read GetEventInfo; default;
  published
  {:
    Bind this event when you need to free user data stored inside a context when
    such context is destroyed.)
  }
    property OnUserDataFree: TUserDataFreeEvent read fOnUserDataFree write fOnUserDataFree;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                           TRegisteredChannelsList                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  Structure holding information about registered channel.

  @member(Name          @NoAutoLink(Name) of the channel.)
  @member(ID            Identifier of the channel.)
  @member(Index         @NoAutoLink(Index) of channel value.)
  @member(ValueType     Type of value for current channel.)
  @member(Flags         Registration flags.)
  @member(IndexConfig   Reference to config that stores count for this
                        channel.)
}
  TChannelInfo = record
    Name:         TelemetryString;
    ID:           TChannelID;
    Index:        scs_u32_t;
    ValueType:    scs_value_type_t;
    Flags:        scs_u32_t;
    IndexConfig:  TConfigReference;
  end;
  //:Pointer to TChannelInfo structure.
  PChannelInfo = ^TChannelInfo;

const
  //:Empty TEventInfo structure.
  EmptyChannelInfo: TChannelInfo =
   (Name:       '';
    ID:         0;
    Index:      SCS_U32_NIL;
    ValueType:  SCS_VALUE_TYPE_INVALID;
    Flags:      0;
    IndexConfig:(
      ID:         '';
      Attribute:  ''));

{:
  @abstract(Structure used as an item in TRegisteredEventsList.)
  Pointer to this structure is passed as @noAutoLink(context) in telemetry API
  calls when registering telemetry channel.

  @member(Recipient   Object that should receive channel callbacks.)
  @member(ChannelInfo Information about registered channel.)
  @member(UserData    User data stored in the context.)
}
type
  TChannelContext = record
    Recipient:    TObject;
    ChannelInfo:  TChannelInfo;
    UserData:     Pointer;
  end;
  //:Pointer to TChannelContext structure.
  PChannelContext = ^TChannelContext;

{==============================================================================}
{   TRegisteredChannelsList // Class declaration                               }
{==============================================================================}
{:
  @abstract(List used to store @noAutoLink(contexts of registered channels).)

  When new channel is registered in the telemetry API, it is registered with
  context which is actually pointer to a variable of TChannelContext structure.
  This variable is at the same time added as a new item into this list.@br
  When channel is unregistered, context it is bound to is removed from this
  list.
}
  TRegisteredChannelsList = class(TCustomTelemetryList)
  private
  {:
    Holds reference to OnUserDataFree event handler.
  }
    fOnUserDataFree:  TUserDataFreeEvent;
  {:
    Getter for Contexts property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetChannelContext(Index: Integer): PChannelContext;
  {:
    Getter for Channels property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetChannelInfo(Index: Integer): TChannelInfo;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.@br
    Contexts are freed using method FreeContext (OnUserDataFree event is
    called).
  }
    procedure Clear; override;
  {:
    Searches through list for context created for channel with appropriate name.
    When the context is not found, -1 is returned.

    @param Name  Name of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate ID.
    When the context is not found, -1 is returned.

    @param ID    ID of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(ID: TChannelID): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate name
    and index. When the context is not found, -1 is returned.

    @param Name  Name of the requested channel.
    @param Index Index of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString; Index: scs_u32_t): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate ID
    and index. When the context is not found, -1 is returned.

    @param ID    ID of the requested channel.
    @param Index Index of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(ID: TChannelID; Index: scs_u32_t): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate name,
    index and value type. When the context is not found, -1 is returned.

    @param Name      Name of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate ID,
    index and value type. When the context is not found, -1 is returned.

    @param ID        ID of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate name,
    index, value type and flags. When the context is not found, -1 is returned.

    @param Name      Name of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.
    @param Flags     Registering flags of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t): Integer; overload; virtual;
  {:
    Searches through list for context created for channel with appropriate ID,
    index, value type and flags. When the context is not found, -1 is returned.

    @param ID        ID of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.
    @param Flags     Registering flags of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t): Integer; overload; virtual;
  {:
    Searches through list given context. When the context is not found, -1 is returned.

    @param ChannelContext Requested channel context.

    @returns Index of requested context, -1 when not found.
  }
    Function IndexOf(ChannelContext: PChannelContext): Integer; overload; virtual;
  {:
    Adds new context into the list. @code(ChannelContext) parameter must not be
    @nil, otherwise an exception is raised.@br
    OnChange event is called after successful addition.

    @param(ChannelContext Pointer to channel context that has to be added to the
                          list.)

    @returns Index at which the new context was added, -1 when addition failed.

    @raises ETLNilReference When context pointer is not assigned.
  }
    Function Add(ChannelContext: PChannelContext): Integer; overload; virtual;
  {:
    Creates and adds new context into the list.@br
    OnChange event is called after successful addition.

    @param Recipient     Telemetry recipient registering this new context.
    @param(Name          Name of the registered channel (ID is calculated from
                         it).)
    @param Index         Index of registered channel.
    @param ValueType     Value type of registered channel.
    @param Flags         Registering flags.
    @param(IndexConfig   Reference to index configuration to which the
                         registered channel is bound.)
    @param(UserData      User defined data to be stored inside the channel
                         context.)

    @returns Index at which the new context was added, -1 when addition failed.
  }
    Function Add(Recipient: TObject; const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfig: TConfigReference; UserData: Pointer = nil): Integer; overload; virtual;
  {:
    Creates and adds new context into the list. Created context has
    @code(Recipient) field set to @nil.@br
    OnChange event is called after successful addition.

    @param(Name          Name of the registered channel (ID is calculated from
                         it).)
    @param Index         Index of registered channel.
    @param ValueType     Value type of registered channel.
    @param Flags         Registering flags.
    @param(IndexConfig   Reference to index configuration to which the
                         registered channel is bound.)
    @param(UserData      User defined data to be stored inside the channel
                         context.)

    @returns Index at which the new context was added, -1 when addition failed.
  }
    Function Add(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfigID: TConfigReference; UserData: Pointer = nil): Integer; overload; virtual;
  {:
    Removes context created for given channel from the list.@br
    OnChange event is called after successful removal.

    @param(Name      Name of the registered channel whose context has to be
                     removed.)
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.

    @returns(Index of item that was removed, -1 when context with requested
             channel was not found.)
  }
    Function Remove(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Removes context created for given channel from the list.@br
    OnChange event is called after successful removal.

    @param(ID        ID of the registered channel whose context has to be
                     removed.)
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.

    @returns(Index of item that was removed, -1 when context with requested
             channel was not found.)
  }
    Function Remove(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Removes given context from the list.@br
    OnChange event is called after successful removal.

    @param ChannelContext Context to be removed.

    @returns(Index of item that was removed, -1 when requested context was not
             found.)
  }    
    Function Remove(ChannelContext: PChannelContext): Integer; overload; virtual;
  {:
    Deletes context at position given by @code(Index) parameter. When index
    falls out of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful deletion.@br
    Contexts are freed using method FreeContext (OnUserDataFree event is
    called).

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure Delete(Index: Integer); virtual;
  {:
    This method creates new channel context variable, fills it from parameters
    and returns pointer to this variable. It is used when registering new
    telemetry channel, because actual context is needed for this registration,
    but cannot be added to this list before the registration is complete.@br
    Sequence of actions taken during registration is as follows:
    @unorderedlist(
      @itemSpacing(Compact)
      @item Context is created using this function.
      @item Registration in API is attempted.
      @item When successful, context is added to this list.
      @item When unsuccessful, context is freed using FreeContext method.
    )

    @param Recipient     Telemetry recipient registering created context.
    @param(Name          Name of the registered channel (ID is calculated from
                         it).)
    @param Index         Index of registered channel.
    @param ValueType     Value type of registered channel.
    @param Flags         Registering flags.
    @param(IndexConfig   Reference to index configuration to which the
                         registered channel is bound.)
    @param(UserData      User defined data to be stored inside the created
                         channel context.)

    @returns Pointer to created channel context.
  }
    Function CreateContext(Recipient: TObject; const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfig: TConfigReference; UserData: Pointer = nil): PChannelContext; virtual;
  {:
    Frees memory allocated for channel context and sets this pointer to @nil.@br
    Calls OnUserDataFree event before the context is freed.

    @param ChannelContext Pointer to channel context to be freed.
  }
    procedure FreeContext(var ChannelContext: PChannelContext); virtual;
  {:
    Calls handler of OnUserDataFree event (if assigned).
  }
    procedure DoOnUserDataFree(Sender: TObject; var UserData: Pointer); virtual;
  {:
    Array property mapped directly to internal list. Use it for direct access to
    individual stored contexts.@br
    Unlike Channels property, you can use returned pointer to change values of
    stored items.
  }
    property Contexts[Index: Integer]: PChannelContext read GetChannelContext;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.@br
    This property does not return whole context structure, only its
    "ChannelInfo" field.
  }
    property Channels[Index: Integer]: TChannelInfo read GetChannelInfo; default;
  published
  {:
    Bind this event when you need to free user data stored inside a context when
    such context is destroyed.
  }
    property OnUserDataFree: TUserDataFreeEvent read fOnUserDataFree write fOnUserDataFree;    
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                              TStoredConfigsList                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{:
  @abstract(Structure used as item in TStoredConfigsList.)
  This structure is used to store configuration @noAutoLink(value) obtained from
  the API call.

  @member(Reference Full @noAutoLink(reference) of specific config
                    (configuration ID and Attribute name).)
  @member(Index     @NoAutoLink(Index) of configs @NoAutoLink(value).)
  @member(Value     Actual @NoAutoLink(value) of the config.)
  @member(Binded    Flag denoting whether this config is @NoAutoLink(binded).)
}
  TStoredConfig = record
    Reference:  TConfigReference;
    Index:      scs_u32_t;
    Value:      scs_value_localized_t;
    Binded:     Boolean;
  end;
  //:Pointer to TStoredConfig structure.
  PStoredConfig = ^TStoredConfig;

const
  //:Empty TStoredConfig structure.
  EmptyStoredConfig: TStoredConfig =
   (Reference:  (
      ID:         '';
      Attribute:  '');
    Index:      SCS_U32_NIL;
    Value:  (
      ValueType:  SCS_VALUE_TYPE_INVALID;
      BinaryData: (
        _type:            SCS_VALUE_TYPE_INVALID;
        _padding:         $00000000;
        value_dplacement: (
          position:         (x: 0.0; y: 0.0; z: 0.0);
          orientation:      (heading: 0.0; pitch: 0.0; roll:0.0);
          _padding:         $00000000));
      StringData: '');
    Binded: True);

{==============================================================================}
{   TStoredConfigsList // Class declaration                                    }
{==============================================================================}
{:
  List used to store configuration values obtained from API calls.
}
type
  TStoredConfigsList = class(TCustomTelemetryList)
  private
  {:
    Getter for Pointers property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetStoredConfigPointer(Index: Integer): PStoredConfig;
  {:
    Getter for Configs property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetStoredConfig(Index: Integer): TStoredConfig;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.
  }
    procedure Clear; override;
  {:
    Searches through list for stored config with appropriate identification.
    When matching config is not found, -1 is returned.

    @param ID         Configuration ID of the requested config.
    @param Attribute  Attribute name of the requested config.

    @returns Index of requested config, -1 when not found.
  }
    Function IndexOf(const ID, Attribute: TelemetryString): Integer; overload; virtual;
  {:
    Searches through list for stored config with appropriate reference.
    When matching config is not found, -1 is returned.

    @param(ConfigReference  Full reference (ID + Attribute) of the requested
                            config.)

    @returns Index of requested config, -1 when not found.
  }
    Function IndexOf(ConfigReference: TConfigReference): Integer; overload; virtual;
  {:
    Searches through list for stored config with appropriate identification and
    index.
    When matching config is not found, -1 is returned.

    @param ID         Configuration ID of the requested config.
    @param Attribute  Attribute name of the requested config.
    @param Index      Index of the requested config.

    @returns Index of requested config, -1 when not found.
  }
    Function IndexOf(const ID, Attribute: TelemetryString; Index: scs_u32_t): Integer; overload; virtual;
  {:
    Searches through list for stored config with appropriate reference and
    index.
    When matching config is not found, -1 is returned.

    @param(ConfigReference  Full reference (ID + Attribute) of the requested
                            config.)
    @param Index            Index of the requested config.

    @returns Index of requested config, -1 when not found.
  }
    Function IndexOf(ConfigReference: TConfigReference; Index: scs_u32_t): Integer; overload; virtual;
  {:
    Stores new config with its value into the list.@br
    OnChange event is called after successful addition.

    @param ID         Configuration ID of the added config.
    @param Attribute  Attribute name of the added config.
    @param Index      Index of stored config.
    @param Value      Localized value (data) this config contains.
    @param(Binded     Flag denoting whether  this config is binded by some
                      channel (i.e. some channel has this config as its
                      IndexConfig property).)

    @returns Index at which the new config was stored, -1 when addition failed.
  }
    Function Add(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: scs_value_localized_t; Binded: Boolean = False): Integer; overload; virtual;
  {:
    Stores new config with its value into the list.@br
    OnChange event is called after successful addition.

    @param ID         Configuration ID of the added config.
    @param Attribute  Attribute name of the added config.
    @param Index      Index of stored config.
    @param Value      Value (data) this config contains.
    @param(Binded     Flag denoting whether  this config is binded by some
                      channel (i.e. some channel has this config as its
                      IndexConfig property).)

    @returns Index at which the new config was stored, -1 when addition failed.
  }
    Function Add(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: p_scs_value_t; Binded: Boolean = False): Integer; overload; virtual;
  {:
    Stores new config with its value into the list.@br
    OnChange event is called after successful addition.

    @param(ConfigReference  Full reference (ID + Attribute) of the added
                            config.)
    @param Index            Index of stored config.
    @param Value            Localized value (data) this config contains.
    @param(Binded           Flag denoting whether  this config is binded by some
                            channel (i.e. some channel has this config as its
                            IndexConfig property).)

    @returns Index at which the new config was stored, -1 when addition failed.
  }
    Function Add(ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t; Binded: Boolean = False): Integer; overload; virtual;
  {:
    Stores new config with its value into the list.@br
    OnChange event is called after successful addition.

    @param(ConfigReference  Full reference (ID + Attribute) of the added
                            config.)
    @param Index            Index of stored config.
    @param Value            Value (data) this config contains.
    @param(Binded           Flag denoting whether  this config is binded by some
                            channel (i.e. some channel has this config as its
                            IndexConfig property).)

    @returns Index at which the new config was stored, -1 when addition failed.
  }
    Function Add(ConfigReference: TConfigReference; Index: scs_u32_t; Value: p_scs_value_t; Binded: Boolean = False): Integer; overload; virtual;
  {:
    Removes stored config from the list.@br
    OnChange event is called after successful removal.

    @param ID         Configuration ID of the config that has to be removed.
    @param Attribute  Attribute name of the config that has to be removed.
    @param Index      Index of stored config.

    @returns(Index of item that was removed, -1 when requested config was not
             found.)
  }
    Function Remove(const ID, Attribute: TelemetryString; Index: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Removes stored config from the list.@br
    OnChange event is called after successful removal.

    @param(ConfigReference  Full reference (ID + Attribute) of stored config
                            that has to be removed.)
    @param Index            Index of stored config.

    @returns(Index of item that was removed, -1 when requested config was not
             found.)
  }
    Function Remove(ConfigReference: TConfigReference; Index: scs_u32_t = SCS_U32_NIL): Integer; overload; virtual;
  {:
    Deletes stored config at position given by @code(Index) parameter. When
    index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.@br
    OnChange event is called after successful deletion.

    @param Index Index of item that has to be deleted.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    procedure Delete(Index: Integer); virtual;
  {:
    Changes stored value in config defined by identification and index.
    OnChange event is called after successful removal.

    @param(ID         Configuration ID of the config whose value should be
                      changed.)
    @param(Attribute  Attribute name of the config whose value should be
                      changed.)
    @param Index      Index of stored config.

    @returns(Index of item whose value has been changed, -1 when requested
             config was not found.)
  }
    Function ChangeConfigValue(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: scs_value_localized_t): Integer; overload; virtual;
  {:
    Changes stored value in config defined by reference and index.
    OnChange event is called after successful removal.

    @param(ConfigReference  Full reference (ID + Attribute) of config whose
                            value should be changed.)
    @param Index            Index of stored config.

    @returns(Index of item whose value has been changed, -1 when requested
             config was not found.)
  }
    Function ChangeConfigValue(ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t): Integer; overload; virtual;
  {:
    Array property mapped directly to internal list. Use it for direct access to
    individual stored items.@br
    Unlike Configs property, you can use returned pointer to change values of
    stored items.
  }
    property Pointers[Index: Integer]: PStoredConfig read GetStoredConfigPointer;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.
  }
    property Configs[Index: Integer]: TStoredConfig read GetStoredConfig; default;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                              TStoredChannelsList                             }
{------------------------------------------------------------------------------}
{==============================================================================}

  //:Identification number used in TStoredChannelsList for faster searching.
  TMasterID = UInt32;

{:
  @abstract(Function used to calculate master identification number which is
  used in TStoredChannelsList.)@br
  This number is used in searching and sorting algorithms and is calculated from
  channel ID, Index and ValueType.@br
  At the moment, it is calculated by the formula:@br
  @longcode( MasterID := (ID xor Index) xor not ValueType; )

  @param ID        Channel ID.
  @param Index     Channel index.
  @param ValueType Channel ValueType.

  @returns Master identification number.
}
  Function GetMasterID(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): TMasterID; register;

type
{:
  @abstract(Structure used to store channel @noAutoLink(value) obtained form 
            API.)

  @member(Name      @NoAutoLink(Name) of the channel.)
  @member(ID        Identifier of the channel.)
  @member(Index     @NoAutoLink(Index) of channels @NoAutoLink(value).)
  @member(Value     Actual @NoAutoLink(value) of the channel.)
}
  TStoredChannel = record
    Name:       TelemetryString;
    ID:         TChannelID;
    Index:      scs_u32_t;
    Value:      scs_value_localized_t;
  end;
  //:Pointer to TStoredChannel structure.
  PStoredChannel = ^TStoredChannel;

{:
  @abstract(Structure used as item in TStoredChannelsList.)
  This structure is used to store channels @noAutoLink(values) obtained from API
  calls in an list sorted by items master ID.

  @member(MasterID      Identifier of stored channel @NoAutoLink(value).)
  @member(StoredChannel Stored channel with its value.)
}
  TStoredChannelMasterID = record
    MasterID:       TMasterID;
    StoredChannel:  TStoredChannel;
  end;
  //:Pointer to TStoredChannelSorted structure.
  PStoredChannelMasterID = ^TStoredChannelMasterID;

{==============================================================================}
{   TStoredChannelsList // Class declaration                                   }
{==============================================================================}
{:
  @abstract(List used to store channels values obtained from API calls.)

  Items in this list are sorted by their master ID.
}
  TStoredChannelsList = class(TCustomTelemetryList)
  private
  {:
    Getter for StoredChannelValues property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.

    @raises ETLIndexOfBounds When index is out of the interval <0,Count@).
  }
    Function GetStoredChannelValue(Index: Integer): TStoredChannel;
  protected
  {:
    This function is used to get position at which should the new item with
    given master ID be put in order for the list to stay sorted.

    @param MasterID Master ID of added item.

    @returns Position at which newly added item should be inserted.
  }
    Function GetInsertIndex(MasterID: TMasterID): Integer; virtual;
  public
  {:
    Deletes all items in the list.@br
    OnChange event is called after items deletion.
  }
    procedure Clear; override;
  {:
    Searches through list for stored channel with appropriate master index.
    When matching config is not found, -1 is returned.

    @param MasterID Master ID of requested channel.

    @returns Index of requested channel, -1 when not found.
  }
    Function IndexOf(MasterID: TMasterID): Integer; overload; virtual;
  {:
    Searches through list for stored channel with appropriate ID, index and
    value type. When the context is not found, -1 is returned.

    @param ID        ID of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Searches through list for stored channel with appropriate name, index and
    value type. When the context is not found, -1 is returned.

    @param Name      Name of the requested channel.
    @param Index     Index of the requested channel.
    @param ValueType Type of value of the requested channel.

    @returns Index of context with requested channel, -1 when not found.
  }
    Function IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; overload; virtual;
  {:
    Method used to store new value for already present channel. If such channel
    is not yet stored in the list, it is added as new item. Function can, in
    extreme cases, return -1 (when the channel is not already stored and
    addition fails for some reason).@br
    When @code(Value) parameter is not assigned, then the function returns
    immediatelly, the list stays unchanged and the funtion will return -1.@br
    OnChange event is called after successful addition or value change.

    @param Name  Name of the channel.
    @param ID    ID of the channel.
    @param Index Index of the channel.
    @param Value New value of the channel.

    @returns(Index (position) at which the channel is inserted or where it was
             found, -1 on failure.)
  }
    Function StoreChannelValue(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): Integer; virtual;
  {:
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.
  }
    property StoredChannelValues[Index: Integer]: TStoredChannel read GetStoredChannelValue; default;
  end;

{==============================================================================}
{   Unit functions and procedures // Declaration                               }
{==============================================================================}

{:
  @abstract(Function intended as callback for streaming functions, converting
            channel name to ID.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TKnownChannelsList object.

  @param Name          Channel name to be converted to ID.
  @param(KnownChannels TKnownChannelsList object that will be used for actual
                       conversion.)

  @returns Channel ID obtained from passed name.
}
Function GetChannelIDFromName(const Name: TelemetryString; KnownChannels: Pointer): TChannelID;

{:
  @abstract(Function intended as callback for streaming functions, converting
            channel ID to name.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TKnownChannelsList object.

  @param ID            Channel ID to be converted to name.
  @param(KnownChannels TKnownChannelsList object that will be used for actual
                       conversion.)

  @returns Channel name obtained from passed ID.
}
Function GetChannelNameFromID(ID: TChannelID; KnownChannels: Pointer): TelemetryString;

implementation

uses
  SysUtils,
  TelemetryConversions, TelemetryStrings;

{==============================================================================}
{   Unit functions and procedures // Implementation                            }
{==============================================================================}

Function GetMasterID(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): TMasterID; register; {$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
{******************************************************************************}
{     Register    Content                                                      }
{     RCX         ID                                                           }
{     RDX         Index                                                        }
{     R8          ValueType                                                    }
{                                                                              }
{     Registers used in routine:                                               }
{     RAX (Result), RCX, RDX, R8                                               }
{******************************************************************************}
      MOV   RAX,  RCX
      XOR   RAX,  RDX
      NOT   R8
      XOR   RAX,  R8
{$ELSE}
{******************************************************************************}
{     Register    Content                                                      }
{     EAX         ID, Result                                                   }
{     EDX         Index                                                        }
{     ECX         ValueType                                                    }
{                                                                              }
{     Registers used in routine:                                               }
{     EAX (contains result), ECX, EDX                                          }
{******************************************************************************}
      XOR   EAX,  ECX
      NOT   EDX
      XOR   EAX,  EDX
{$ENDIF}
end;
{$ELSE PurePascal}
begin
Result := (ID xor Index) xor not ValueType;
end;
{$ENDIF PurePascal}

//------------------------------------------------------------------------------

Function GetChannelIDFromName(const Name: TelemetryString; KnownChannels: Pointer): TChannelID;
begin
Result := TKnownChannelsList(KnownChannels).ChannelNameToID(Name);
end;

//------------------------------------------------------------------------------

Function GetChannelNameFromID(ID: TChannelID; KnownChannels: Pointer): TelemetryString;
begin
Result := TKnownChannelsList(KnownChannels).ChannelIDToName(ID);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TCustomTelemetryList                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TCustomTelemetryList // Implementation                                     }
{==============================================================================}
{------------------------------------------------------------------------------}
{   TCustomTelemetryList // Protected methods                                  }
{------------------------------------------------------------------------------}

Function TCustomTelemetryList.GetCount: Integer;
begin
Result := fMainList.Count;
end;

//------------------------------------------------------------------------------

Function TCustomTelemetryList.PtrGetItem(Index: Integer): Pointer;
begin
If (Index >= 0) and (Index < fMainList.Count) then
  Result := fMainList[Index]
else
  raise ETLIndexOfBounds.CreateFmt('TCustomTelemetryList.PtrGetItem: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TCustomTelemetryList.PtrIndexOf(Item: Pointer): Integer;
begin
Result := fMainList.IndexOf(Item);
end;

//------------------------------------------------------------------------------

Function TCustomTelemetryList.PtrAdd(Item: Pointer): Integer;
begin
Result := -1;
If Assigned(Item) then Result := fMainList.Add(Item);
If Result >= 0 then DoChange;
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.PtrReplace(Index: Integer; Item: Pointer);
begin
If (Index >= 0) and (Index < fMainList.Count) then
  begin
    fMainList[Index] := Item;
    DoChange;
  end
else
  raise ETLIndexOfBounds.CreateFmt('TCustomTelemetryList.PtrReplace: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.PtrInsert(Index: Integer; Item: Pointer);
begin
If (Index >= 0) and (Index <= fMainList.Count) then
  begin
    fMainList.Insert(Index,Item);
    DoChange;
  end
else
  raise ETLIndexOfBounds.CreateFmt('TCustomTelemetryList.PtrInsert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TCustomTelemetryList.PtrRemove(Item: Pointer): Integer;
begin
Result := fMainList.Remove(Item);
If Result >= 0 then DoChange;
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.PtrDelete(Index: Integer);
begin
If (Index >= 0) and (Index < fMainList.Count) then
  begin
    fMainList.Delete(Index);
    DoChange;
  end
else
  raise ETLIndexOfBounds.CreateFmt('TCustomTelemetryList.PtrDelete: Index (%d) out of bounds.',[Index]);
end;

{------------------------------------------------------------------------------}
{   TCustomTelemetryList // Public methods                                     }
{------------------------------------------------------------------------------}

constructor TCustomTelemetryList.Create;
begin
inherited;
fMainList := TList.Create;
fUpdateCount := 0;
{$IFDEF MulticastEvents}
fOnChangeMulti := TMulticastNotifyEvent.Create(Self);
{$ENDIF}
end;

//------------------------------------------------------------------------------

destructor TCustomTelemetryList.Destroy;
begin
// Prevent OnChange event call.
BeginUpdate;
{$IFDEF MulticastEvents}
fOnChangeMulti.Free;
{$ENDIF}
Clear;
fMainList.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.BeginUpdate;
begin
Inc(fUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.EndUpdate;
begin
Dec(fUpdateCount);
DoChange;
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.DoChange;
begin
If fUpdateCount <= 0 then
  begin
    If Assigned(fOnChange) then fOnChange(Self);
    {$IFDEF MulticastEvents}
    fOnChangeMulti.Call(Self);
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTelemetryList.Clear;
begin
fMainList.Clear;
DoChange;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TKnownEventsList                                }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TKnownEventsList // Implementation                                         }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TKnownEventsList // Private methods                                        }
{------------------------------------------------------------------------------}

Function TKnownEventsList.GetKnownEventPointer(Index: Integer): PKnownEvent;
begin
If (Index >= 0) and (Index < Count) then
  Result := PKnownEvent(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TKnownEventsList.GetKnownEventPointer: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.GetKnownEvent(Index: Integer): TKnownEvent;
begin
Result := GetKnownEventPointer(Index)^;
end;

{------------------------------------------------------------------------------}
{   TKnownEventsList // Public methods                                         }
{------------------------------------------------------------------------------}

procedure TKnownEventsList.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PKnownEvent(PtrGetItem(i)));
inherited;
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.IndexOf(Event: scs_event_t): Integer;
begin
For Result := 0 to (Count - 1) do
  If PKnownEvent(PtrGetItem(Result))^.Event = Event then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TKnownEventsList.IndexOf(const Name: TelemetryString): Integer;
begin
For Result := 0 to (Count - 1) do
  If TelemetrySameText(PKnownEvent(PtrGetItem(Result))^.Name,Name) then Exit;
Result := -1;
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.Add(Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer;
var
  NewEvent: PKnownEvent;
begin
New(NewEvent);
NewEvent^.Event := Event;
NewEvent^.Name := Name;
NewEvent^.Valid := Valid;
NewEvent^.Utility := Utility;
Result := PtrAdd(NewEvent);
If Result < 0 then Dispose(NewEvent);
end;

//------------------------------------------------------------------------------

procedure TKnownEventsList.ReplaceIndex(Index: Integer; Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False);
var
  KnownEvent:  PKnownEvent;
begin
If (Index >= 0) and (Index < Count) then
  begin
    KnownEvent := PKnownEvent(PtrGetItem(Index));
    KnownEvent^.Event := Event;
    KnownEvent^.Name := Name;
    KnownEvent^.Valid := Valid;
    KnownEvent^.Utility := Utility;
    PtrReplace(Index,KnownEvent);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownEventsList.ReplaceIndex: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.Replace(OldEvent, Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer;
begin
Result := IndexOf(OldEvent);
If Result >= 0 then ReplaceIndex(Result,Event,Name,Valid,Utility);
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.Insert(Index: Integer; Event: scs_event_t; const Name: TelemetryString; Valid: Boolean = True; Utility: Boolean = False): Integer;
var
  NewEvent: PKnownEvent;
begin
If (Index < 0) or (Index >= Count) then
  Result := Add(Event,Name,Valid,Utility)
else
  begin
    New(NewEvent);
    NewEvent^.Event := Event;
    NewEvent^.Name := Name;
    NewEvent^.Valid := Valid;
    NewEvent^.Utility := Utility;
    Result := Index;
    PtrInsert(Index,NewEvent);
  end;
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.Remove(Event: scs_event_t): Integer;
begin
Result := IndexOf(Event);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TKnownEventsList.Delete(Index: Integer);
begin
If (Index >= 0) and (Index < Count) then
  begin
    Dispose(PKnownEvent(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownEventsList.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.IsValid(Event: scs_event_t): Boolean;
var
  Index:  Integer;
begin
Result := False;
Index := IndexOf(Event);
If Index >= 0 then
  Result := PKnownEvent(PtrGetItem(Index))^.Valid;
end;

//------------------------------------------------------------------------------

Function TKnownEventsList.IsUtility(Event: scs_event_t): Boolean;
var
  Index:  Integer;
begin
Result := False;
Index := IndexOf(Event);
If Index >= 0 then
  Result := PKnownEvent(PtrGetItem(Index))^.Utility;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TKnownChannelsList                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TKnownChannelsList // Implementation                                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TKnownChannelsList // Private methods                                      }
{------------------------------------------------------------------------------}

Function TKnownChannelsList.GetKnownChannelPointer(Index: Integer): PKnownChannel;
begin
If (Index >= 0) and (Index < Count) then
  Result := PKnownChannel(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TKnownChannelsList.GetKnownChannelPointer: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.GetKnownChannel(Index: Integer): TKnownChannel;
begin
Result := GetKnownChannelPointer(Index)^;
end;

{------------------------------------------------------------------------------}
{   TKnownChannelsList // Public methods                                       }
{------------------------------------------------------------------------------}

procedure TKnownChannelsList.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PKnownChannel(PtrGetItem(i)));
inherited;
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.IndexOf(const Name: TelemetryString): Integer;
begin
For Result := 0 to (Count - 1) do
  If TelemetrySameStr(PKnownChannel(PtrGetItem(Result))^.Name,Name) then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TKnownChannelsList.IndexOf(ID: TChannelID): Integer;
begin
For Result := 0 to (Count - 1) do
  If PKnownChannel(PtrGetItem(Result))^.ID = ID then Exit;
Result := -1;
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Add(const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
var
  NewChannel: PKnownChannel;
begin
New(NewChannel);
NewChannel^.Name := Name;
NewChannel^.ID := ChannelNameToID(Name);
NewChannel^.PrimaryType := PrimaryType;
NewChannel^.SecondaryTypes := SecondaryTypes;
NewChannel^.Indexed := Indexed;
NewChannel^.IndexConfig := IndexConfig;
NewChannel^.MaxIndex := MaxIndex;
Result := PtrAdd(NewChannel);
If Result < 0 then Dispose(NewChannel);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Add(const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := Add(Name,PrimaryType,SecondaryValueTypesBitmask(PrimaryType),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Add(const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := Add(Name,PrimaryType,ValueTypesBitmask([SecondaryType,TertiaryType]),Indexed,IndexConfig,MaxIndex)
end;

//------------------------------------------------------------------------------

procedure TKnownChannelsList.ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL);
var
  KnownChannel: PKnownChannel;
begin
If (Index >= 0) and (Index < Count) then
  begin
    KnownChannel := PKnownChannel(PtrGetItem(Index));
    KnownChannel^.Name := Name;
    KnownChannel^.ID := ChannelNameToID(Name);
    KnownChannel^.PrimaryType := PrimaryType;
    KnownChannel^.SecondaryTypes := SecondaryTypes;
    KnownChannel^.Indexed := Indexed;
    KnownChannel^.IndexConfig := IndexConfig;
    KnownChannel^.MaxIndex := MaxIndex;
    PtrReplace(Index,KnownChannel);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownChannelsList.ReplaceIndex: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TKnownChannelsList.ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL);
begin
ReplaceIndex(Index,Name,PrimaryType,SecondaryValueTypesBitmask(PrimaryType),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

procedure TKnownChannelsList.ReplaceIndex(Index: Integer; const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL);
begin
ReplaceIndex(Index,Name,PrimaryType,ValueTypesBitmask([SecondaryType,TertiaryType]),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Replace(const OldChannel, Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := IndexOf(OldChannel);
If Result >= 0 then
  ReplaceIndex(Result,Name,PrimaryType,SecondaryTypes,Indexed,Indexconfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Replace(const OldChannel, Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer; 
begin
Result := Replace(OldChannel,Name,PrimaryType,SecondaryValueTypesBitmask(PrimaryType),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Replace(const OldChannel, Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := Replace(OldChannel,Name,PrimaryType,ValueTypesBitmask([SecondaryType,TertiaryType]),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Insert(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; SecondaryTypes: TValueTypeBitmask; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
var
  NewChannel: PKnownChannel;
begin
If (Index < 0) or (Index >= Count) then
  Result := Add(Name,PrimaryType,SecondaryTypes,Indexed,Indexconfig,MaxIndex)
else
  begin
    New(NewChannel);
    NewChannel^.Name := Name;
    NewChannel^.ID := ChannelNameToID(Name);
    NewChannel^.PrimaryType := PrimaryType;
    NewChannel^.SecondaryTypes := SecondaryTypes;
    NewChannel^.Indexed := Indexed;
    NewChannel^.IndexConfig := IndexConfig;
    NewChannel^.MaxIndex := MaxIndex;
    Result := Index;
    PtrInsert(Index,NewChannel);
  end;
end;
//------------------------------------------------------------------------------

Function TKnownChannelsList.Insert(Index: Integer; const Name: TelemetryString; PrimaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := Insert(Index,Name,PrimaryType,SecondaryValueTypesBitmask(PrimaryType),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Insert(Index: Integer; const Name: TelemetryString; PrimaryType, SecondaryType, TertiaryType: scs_value_type_t; Indexed: Boolean; IndexConfig: TConfigReference; MaxIndex: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := Insert(Index,Name,PrimaryType,ValueTypesBitmask([SecondaryType,TertiaryType]),Indexed,IndexConfig,MaxIndex);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.Remove(const Name: TelemetryString): Integer;
begin
Result := IndexOf(Name);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TKnownChannelsList.Delete(Index: Integer);
begin
If (Index >= 0) and (Index < Count) then
  begin
    Dispose(PKnownChannel(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownChannelsList.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.ChannelIndexConfigID(const Name: TelemetryString): TConfigReference;
var
  Index:  Integer;
begin
Index := IndexOf(Name);
If Index >= 0 then
  Result := PKnownChannel(PtrGetItem(Index))^.IndexConfig;
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.ChannelNameToID(const Name: TelemetryString): TChannelID;
begin
Result := GetItemID(Name);
end;

//------------------------------------------------------------------------------

Function TKnownChannelsList.ChannelIDToName(ID: TChannelID): TelemetryString;
var
  Index:  Integer;
begin
Index := IndexOf(ID);
If Index >= 0 then Result := PKnownChannel(PtrGetItem(Index))^.Name
  else Result := '';
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TKnownConfigsList                                }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TKnownConfigsList // Auxiliary functions                                   }
{==============================================================================}

Function KnownConfig(const ID: TelemetryString; Attribute: TKnownAttribute): TKnownConfig;
begin
Result.ID := ID;
Result.Attribute := Attribute;
end;

{==============================================================================}
{   TKnownConfigsList // Implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TKnownConfigsList // Private methods                                       }
{------------------------------------------------------------------------------}

Function TKnownConfigsList.GetConfigurationCount: Integer;
begin
Result := inherited GetCount;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.GetAttributeCount(Index: Integer): Integer;
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  Result := Length(PKnownConfiguration(PtrGetItem(Index))^.Attributes)
else
  raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.GetAttributeCount: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.GetKnownConfigurationPointer(Index: Integer): PKnownConfiguration;
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  Result := PKnownConfiguration(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.GetKnownConfigurationPointer: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.GetKnownConfiguration(Index: Integer): TKnownConfiguration;
begin
Result := GetKnownConfigurationPointer(Index)^;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.GetKnownConfigPointer(Indices: TDoubleIndex): PKnownAttribute;
var
  Configuration:  PKnownConfiguration;
begin
If (Indices.Index1 >= 0) and (Indices.Index1 < ConfigurationCount) then
  begin
    Configuration := GetKnownConfigurationPointer(Indices.Index1);
    If (Indices.Index2 >= Low(Configuration^.Attributes)) and (Indices.Index2 <= High(Configuration^.Attributes)) then
      Result := Addr(Configuration^.Attributes[Indices.Index2])
    else
      raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.GetKnownConfigPointer: Second-level index (%d) out of bounds.',[Indices.Index2]);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.GetKnownConfigPointer: First-level index (%d) out of bounds.',[Indices.Index1]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.GetKnownConfig(Indices: TDoubleIndex): TKnownAttribute;
begin
Result := GetKnownConfigPointer(Indices)^;
end;

{------------------------------------------------------------------------------}
{   TKnownConfigsList // Protected methods                                     }
{------------------------------------------------------------------------------}

Function TKnownConfigsList.GetCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := 0 to Pred(ConfigurationCount) do
  Inc(Result,Length(Configurations[i].Attributes));
end;

{------------------------------------------------------------------------------}
{   TKnownConfigsList // Public methods                                        }
{------------------------------------------------------------------------------}

procedure TKnownConfigsList.Clear;
var
  i:  Integer;
begin
For i := (ConfigurationCount - 1) downto 0 do
  begin
    SetLength(PKnownConfiguration(PtrGetItem(i))^.Attributes,0);
    Dispose(PKnownConfiguration(PtrGetItem(i)));
  end;
inherited;
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.ClearConfiguration(Index: Integer);
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  SetLength(PKnownConfiguration(PtrGetItem(Index))^.Attributes,0);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IndexOfConfiguration(ID: TelemetryString): Integer;
begin
For Result := 0 to Pred(ConfigurationCount) do
  If TelemetrySameStr(PKnownConfiguration(PtrGetItem(Result))^.ID,ID) then Exit;
Result := -1;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IndexOf(ID,Attribute: TelemetryString): TDoubleIndex;
begin
Result.Index1 := IndexOfConfiguration(ID);
If Result.Index1 >= 0 then
  begin
    Result.Index2 := IndexOf(Result.Index1,Attribute);
    If Result.Index2 < 0 then
      Result := InvalidDoubleIndex;
  end
else Result := InvalidDoubleIndex;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IndexOf(ConfigReference: TConfigReference): TDoubleIndex;
begin
Result := IndexOf(ConfigReference.ID,ConfigReference.Attribute);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IndexOf(Index: Integer; const Attribute: TelemetryString): Integer;
var
  Configuration:  PKnownConfiguration;
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  begin
    Configuration := GetKnownConfigurationPointer(Index);
    For Result := Low(Configuration^.Attributes) to High(Configuration^.Attributes) do
      If TelemetrySameStr(Configuration^.Attributes[Result].Name,Attribute) then Exit;
    Result := -1;
  end
else raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.IndexOf: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.AddConfiguration(const ID: TelemetryString): Integer;
var
  NewConfiguration: PKnownConfiguration;
begin
Result := IndexOfConfiguration(ID);
If Result < 0 then
  begin
    New(NewConfiguration);
    NewConfiguration^.ID := ID;
    SetLength(NewConfiguration^.Attributes,0);
    Result := PtrAdd(NewConfiguration);
    If Result < 0 then Dispose(NewConfiguration);
  end;
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.RenameConfiguration(Index: Integer; const NewID: TelemetryString);
var
  TempIdx:  Integer;
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  begin
    TempIdx := IndexOfConfiguration(NewID);
    If (TempIdx < 0) and (TempIdx <> Index) then
      begin
        PKnownConfiguration(PtrGetItem(Index))^.ID := NewID;
        DoChange;
      end
    else
      raise ETLAlreadyExists.CreateFmt('TKnownConfigsList.RenameConfiguration: Configuration (%s) already exists in the list.',[NewID]);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.RenameConfiguration: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.RenameConfiguration(const OldID, NewID: TelemetryString): Integer;
var
  TempIdx:  Integer;
begin
Result := IndexOfConfiguration(OldID);
If Result >= 0 then
  begin
    TempIdx := IndexOfConfiguration(NewID);
    If (TempIdx < 0) and (TempIdx <> Result) then
      begin
        PKnownConfiguration(PtrGetItem(Result))^.ID := NewID;
        DoChange;
      end
    else
      raise ETLAlreadyExists.CreateFmt('TKnownConfigsList.RenameConfiguration: Configuration (%s) already exists in the list.',[NewID]);
  end;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.InsertConfiguration(Index: Integer; const ID: TelemetryString): Integer;
var
  TempIdx:          Integer;
  NewConfiguration: PKnownConfiguration;
begin
TempIdx := IndexOfConfiguration(ID);
If TempIdx < 0 then
  begin
    If (Index < 0) or (Index >= ConfigurationCount) then
      Result := AddConfiguration(ID)
    else
      begin
        New(NewConfiguration);
        NewConfiguration^.ID := ID;
        SetLength(NewConfiguration^.Attributes,0);
        Result := Index;
        PtrInsert(Index,NewConfiguration);
      end;
  end
else ETLAlreadyExists.CreateFmt('TKnownConfigsList.InsertConfiguration: Configuration (%s) already exists in the list.',[ID]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.RemoveConfiguration(const ID: TelemetryString): Integer;
begin
Result := IndexOfConfiguration(ID);
If Result >= 0 then DeleteConfiguration(Result); 
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.DeleteConfiguration(Index: Integer);
begin
If (Index >= 0) and (Index < ConfigurationCount) then
  begin
    SetLength(PKnownConfiguration(PtrGetItem(Index))^.Attributes,0);
    Dispose(PKnownConfiguration(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.DeleteConfiguration: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Add(const ID, Attribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex;
var
  Configuration:  PKnownConfiguration;
begin
Result.Index1 := AddConfiguration(ID);
If Result.Index1 >= 0 then
  begin
    Configuration := GetKnownConfigurationPointer(Result.Index1);
    Result.Index2 := IndexOf(Result.Index1,Attribute);
    If Result.Index2 < 0 then
      begin
        SetLength(Configuration^.Attributes,Length(Configuration^.Attributes) + 1);
        Result.Index2 := High(Configuration^.Attributes);
      end;
    Configuration^.Attributes[Result.Index2].Name := Attribute;
    Configuration^.Attributes[Result.Index2].ValueType := ValueType;
    Configuration^.Attributes[Result.Index2].Indexed := Indexed;
    Configuration^.Attributes[Result.Index2].Binded := Binded;
    DoChange;
  end
else Result := InvalidDoubleIndex;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Add(ConfigReference: TConfigReference; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex;
begin
Result := Add(ConfigReference.ID,ConfigReference.Attribute,ValueType,Indexed,Binded);
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.ReplaceIndex(Indices: TDoubleIndex; const NewAttribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False);
var
  Configuration:  PKnownConfiguration;
begin
If (Indices.Index1 >= 0) and (Indices.Index1 < ConfigurationCount) then
  begin
    Configuration := GetKnownConfigurationPointer(Indices.Index1);
    If (Indices.Index2 >= Low(Configuration^.Attributes)) and (Indices.Index2 <= High(Configuration^.Attributes)) then
      with PKnownConfiguration(PtrGetItem(Indices.Index1))^ do
        begin
          Attributes[Indices.Index2].Name := NewAttribute;
          Attributes[Indices.Index2].ValueType := ValueType;
          Attributes[Indices.Index2].Indexed := Indexed;
          Attributes[Indices.Index2].Binded := Binded;
          DoChange;
        end
    else raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.ReplaceIndex: Second-level index (%d) out of bounds.',[Indices.Index2]);
  end
else raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.ReplaceIndex: First-level index (%d) out of bounds.',[Indices.Index1]);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Replace(const ID, OldAttribute, NewAttribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex;
begin
Result := IndexOf(ID,OldAttribute);
If ValidDoubleIndex(Result) then
  ReplaceIndex(Result,NewAttribute,ValueType,Indexed,Binded);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Insert(const ID: TelemetryString; Index: Integer; const Attribute: TelemetryString; ValueType: scs_value_type_t; Indexed: Boolean; Binded: Boolean = False): TDoubleIndex;
var
  Configuration:  PKnownConfiguration;
  i:              Integer;
begin
Result.Index1 := IndexOfConfiguration(ID);
If Result.Index1 >= 0 then
  begin
    Configuration := GetKnownConfigurationPointer(Result.Index1);
    If (Index >= Low(Configuration^.Attributes)) and (Index <= High(Configuration^.Attributes)) then
      begin
        SetLength(Configuration^.Attributes,Length(Configuration^.Attributes) + 1);
        For i := High(Configuration^.Attributes) downto (Index + 1) do
          Configuration^.Attributes[i] := Configuration^.Attributes[i - 1];
        Configuration^.Attributes[Index].Name := Attribute;
        Configuration^.Attributes[Index].ValueType := ValueType;
        Configuration^.Attributes[Index].Indexed := Indexed;
        Configuration^.Attributes[Index].Binded := Binded;
        Result.Index2 := Index;
        DoChange;
      end
    else Result := Add(ID,Attribute,ValueType,Indexed,Binded);
  end
else Result := InvalidDoubleIndex;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Remove(const ID, Attribute: TelemetryString): TDoubleIndex;
begin
Result := IndexOf(ID,Attribute);
If ValidDoubleIndex(Result) then Delete(Result);
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.Remove(ConfigReference: TConfigReference): TDoubleIndex;
begin
Result := IndexOf(ConfigReference);
If ValidDoubleIndex(Result) then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.Delete(const ID: TelemetryString; Index: Integer);
var
  ConfigurationIdx: Integer;
begin
ConfigurationIdx := IndexOfConfiguration(ID);
If (ConfigurationIdx >= 0) and (ConfigurationIdx < ConfigurationCount) then
  Delete(DoubleIndex(ConfigurationIdx,Index))
else
  raise ETLNotFound.CreateFmt('TKnownConfigsList.Delete: Configuration (%s) not found.',[ID])
end;

//------------------------------------------------------------------------------

procedure TKnownConfigsList.Delete(Indices: TDoubleIndex);
var
  Configuration:  PKnownConfiguration;
  i:              Integer;
begin
If (Indices.Index1 >= 0) and (Indices.Index1 < ConfigurationCount) then
  begin
    Configuration := GetKnownConfigurationPointer(Indices.Index1);
    If (Indices.Index2 >= Low(Configuration^.Attributes)) and (Indices.Index2 <= High(Configuration^.Attributes)) then
      begin
        For i := Indices.Index2 to Pred(High(Configuration^.Attributes)) do
          Configuration^.Attributes[i] := Configuration^.Attributes[i + 1];
        SetLength(Configuration^.Attributes,Length(Configuration^.Attributes) - 1);
        DoChange;
      end
    else raise ETLIndexOfBounds.CreateFmt('TKnownConfigsList.Delete: Second-level index (%d) out of bounds.',[Indices.Index2]);
  end
else raise ETLNotFound.CreateFmt('TKnownConfigsList.Delete: First-level index (%d) out of bounds.',[Indices.Index1])
end;
 
//------------------------------------------------------------------------------

Function TKnownConfigsList.IsBinded(const ID, Attribute: TelemetryString): Boolean;
var
  Indices:  TDoubleIndex;
begin
Result := False;
Indices := IndexOf(ID,Attribute);
If ValidDoubleIndex(Indices) then
  Result := Configs[Indices].Binded;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IsBinded(ConfigReference: TConfigReference): Boolean;
var
  Indices:  TDoubleIndex;
begin
Result := False;
Indices := IndexOf(ConfigReference);
If ValidDoubleIndex(Indices) then
  Result := Configs[Indices].Binded;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IsIndexed(const ID, Attribute: TelemetryString): Boolean;
var
  Indices:  TDoubleIndex;
begin
Result := False;
Indices := IndexOf(ID,Attribute);
If ValidDoubleIndex(Indices) then
  Result := Configs[Indices].Indexed;
end;

//------------------------------------------------------------------------------

Function TKnownConfigsList.IsIndexed(ConfigReference: TConfigReference): Boolean;
var
  Indices:  TDoubleIndex;
begin
Result := False;
Indices := IndexOf(ConfigReference);
If ValidDoubleIndex(Indices) then
  Result := Configs[Indices].Indexed;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                           TRegisteredEventsList                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRegisteredEventsList // Implementation                                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRegisteredEventsList // Private methods                                   }
{------------------------------------------------------------------------------}

Function TRegisteredEventsList.GetEventContext(Index: Integer): PEventContext;
begin
If (Index >= 0) and (Index < Count) then
  Result := PEventContext(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TRegisteredEventsList.GetEventContext: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TRegisteredEventsList.GetEventInfo(Index: Integer): TEventInfo;
begin
Result := GetEventContext(Index)^.EventInfo;
end;

{------------------------------------------------------------------------------}
{   TRegisteredEventsList // Public methods                                    }
{------------------------------------------------------------------------------}

procedure TRegisteredEventsList.Clear;
var
  i:        Integer;
  TempPtr:  PEventContext;
begin
For i := (Count - 1) downto 0 do
  begin
    TempPtr := PEventContext(PtrGetItem(i));
    FreeContext(TempPtr);
  end;
inherited;
end;

//------------------------------------------------------------------------------

Function TRegisteredEventsList.IndexOf(Event: scs_event_t): Integer;
begin
For Result := 0 to (Count - 1) do
  If PEventContext(PtrGetItem(Result))^.EventInfo.Event = Event then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredEventsList.IndexOf(EventContext: PEventContext): Integer;
begin
Result := PtrIndexOf(EventContext);
end;

//------------------------------------------------------------------------------

Function TRegisteredEventsList.Add(EventContext: PEventContext): Integer;
begin
If Assigned(EventContext) then Result := PtrAdd(EventContext)
else
  raise ETLNilReference.Create('TRegisteredEventsList.Add: Context must not be nil.');
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredEventsList.Add(Recipient: TObject; Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): Integer;
var
  NewEvent: PEventContext;
begin
NewEvent := CreateContext(Recipient,Event,Utility,UserData);
Result := Add(NewEvent);
If Result < 0 then FreeContext(NewEvent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredEventsList.Add(Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): Integer;
begin
Result := Add(nil,Event,Utility,UserData);
end;

//------------------------------------------------------------------------------

Function TRegisteredEventsList.Remove(Event: scs_event_t): Integer;
begin
Result := IndexOf(Event);
If Result >= 0 then Delete(Result);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredEventsList.Remove(EventContext: PEventContext): Integer;
begin
Result := IndexOf(EventContext);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TRegisteredEventsList.Delete(Index: Integer);
var
  TempPtr:  PEventContext;
begin
If (Index >= 0) and (Index < Count) then
  begin
    TempPtr := PEventContext(PtrGetItem(Index));
    FreeContext(TempPtr);
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TRegisteredEventsList.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TRegisteredEventsList.CreateContext(Recipient: TObject; Event: scs_event_t; Utility: Boolean = False; UserData: Pointer = nil): PEventContext;
begin
New(Result);
Result^.Recipient := Recipient;
Result^.EventInfo.Event := Event;
Result^.EventInfo.Utility := Utility;
Result^.UserData := UserData;
end;

//------------------------------------------------------------------------------

procedure TRegisteredEventsList.FreeContext(var EventContext: PEventContext);
begin
DoOnUserDataFree(Self,EventContext^.UserData);
Dispose(EventContext);
EventContext := nil;
end;

//------------------------------------------------------------------------------

procedure TRegisteredEventsList.DoOnUserDataFree(Sender: TObject; var UserData: Pointer);
begin
If Assigned(fOnUserDataFree) then fOnUserDataFree(Sender,UserData);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                          TRegisteredChannelsList                             }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRegisteredChannelsList // Implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRegisteredChannelsList // Private methods                                 }
{------------------------------------------------------------------------------}

Function TRegisteredChannelsList.GetChannelContext(Index: Integer): PChannelContext;
begin
If (Index >= 0) and (Index < Count) then
  Result := PChannelContext(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TRegisteredChannelsList.GetChannelContext: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TRegisteredChannelsList.GetChannelInfo(Index: Integer): TChannelInfo;
begin
Result := GetChannelContext(Index)^.ChannelInfo;
end;

{------------------------------------------------------------------------------}
{   TRegisteredChannelsList // Public methods                                  }
{------------------------------------------------------------------------------}

procedure TRegisteredChannelsList.Clear;
var
  i:        Integer;
  TempPtr:  PChannelContext;
begin
For i := (Count - 1) downto 0 do
  begin
    TempPtr := PChannelContext(PtrGetItem(i));
    FreeContext(TempPtr);
  end;
inherited;
end;

//------------------------------------------------------------------------------

Function TRegisteredChannelsList.IndexOf(const Name: TelemetryString): Integer;
begin
For Result := 0 to (Count - 1) do
  If TelemetrySameStr(PChannelContext(PtrGetItem(Result))^.ChannelInfo.Name,Name) then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(ID: TChannelID): Integer;
begin
For Result := 0 to (Count - 1) do
  If (PChannelContext(PtrGetItem(Result))^.ChannelInfo.ID = ID) then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(const Name: TelemetryString; Index: scs_u32_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If TelemetrySameStr(TempItem^.ChannelInfo.Name,Name) and
      (TempItem^.ChannelInfo.Index = Index) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(ID: TChannelID; Index: scs_u32_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If (TempItem^.ChannelInfo.ID = ID) and (TempItem^.ChannelInfo.Index = Index) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If TelemetrySameStr(TempItem^.ChannelInfo.Name,Name) and
      (TempItem^.ChannelInfo.Index = Index) and
      (TempItem^.ChannelInfo.ValueType = ValueType) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If (TempItem^.ChannelInfo.ID = ID) and (TempItem^.ChannelInfo.Index = Index) and
      (TempItem^.ChannelInfo.ValueType = ValueType) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If TelemetrySameStr(TempItem^.ChannelInfo.Name,Name) and
      (TempItem^.ChannelInfo.Index = Index) and
      (TempItem^.ChannelInfo.ValueType = ValueType) and
      (TempItem^.ChannelInfo.Flags = Flags) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t): Integer;
var
  TempItem: PChannelContext;
begin
For Result := 0 to (Count - 1) do
  begin
    TempItem := PChannelContext(PtrGetItem(Result));
    If (TempItem^.ChannelInfo.ID = ID) and (TempItem^.ChannelInfo.Index = Index) and
      (TempItem^.ChannelInfo.ValueType = ValueType) and (TempItem^.ChannelInfo.Flags = Flags) then Exit;
  end;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.IndexOf(ChannelContext: PChannelContext): Integer;
begin
Result := PtrIndexOf(ChannelContext);
end;

//------------------------------------------------------------------------------

Function TRegisteredChannelsList.Add(ChannelContext: PChannelContext): Integer;
begin
If Assigned(ChannelContext) then Result := PtrAdd(ChannelContext)
else
  raise ETLNilReference.Create('TRegisteredChannelsList.Add: Context must not be nil.');
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.Add(Recipient: TObject; const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfig: TConfigReference; UserData: Pointer = nil): Integer;
var
  NewChannel: PChannelContext;
begin
NewChannel := CreateContext(Recipient,Name,Index,ValueType,Flags,IndexConfig,UserData);
Result := Add(NewChannel);
If Result < 0 then FreeContext(NewChannel);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.Add(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfigID: TConfigReference; UserData: Pointer = nil): Integer;
begin
Result := Add(nil,Name,Index,ValueType,Flags,IndexConfigID,UserData);
end;

//------------------------------------------------------------------------------

Function TRegisteredChannelsList.Remove(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
begin
Result := IndexOf(Name,Index,ValueType);
If Result >= 0 then Delete(Result);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.Remove(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
begin
Result := IndexOf(ID,Index,ValueType);
If Result >= 0 then Delete(Result);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TRegisteredChannelsList.Remove(ChannelContext: PChannelContext): Integer;
begin
Result := IndexOf(ChannelContext);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TRegisteredChannelsList.Delete(Index: Integer);
var
  TempPtr:  PChannelContext;
begin
If (Index >= 0) and (Index < Count) then
  begin
    TempPtr := PChannelContext(PtrGetItem(Index));
    FreeContext(TempPtr);
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TRegisteredChannelsList.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TRegisteredChannelsList.CreateContext(Recipient: TObject; const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; IndexConfig: TConfigReference; UserData: Pointer = nil): PChannelContext;
begin
New(Result);
Result^.Recipient := Recipient;
Result^.ChannelInfo.Name := Name;
Result^.ChannelInfo.ID := GetItemID(Name);
Result^.ChannelInfo.Index := Index;
Result^.ChannelInfo.ValueType := ValueType;
Result^.ChannelInfo.Flags := Flags;
Result^.ChannelInfo.IndexConfig := IndexConfig;
Result^.UserData := UserData;
end;

//------------------------------------------------------------------------------

procedure TRegisteredChannelsList.FreeContext(var ChannelContext: PChannelContext);
begin
DoOnUserDataFree(Self,ChannelContext^.UserData);
Dispose(ChannelContext);
ChannelContext := nil;
end;

//------------------------------------------------------------------------------

procedure TRegisteredChannelsList.DoOnUserDataFree(Sender: TObject; var UserData: Pointer);
begin
If Assigned(fOnUserDataFree) then fOnUserDataFree(Sender,UserData);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                             TStoredConfigsList                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TStoredConfigsList // Implementation                                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TStoredConfigsList // Private methods                                      }
{------------------------------------------------------------------------------}

Function TStoredConfigsList.GetStoredConfigPointer(Index: Integer): PStoredConfig;
begin
If (Index >= 0) and (Index < Count) then
  Result := PStoredConfig(PtrGetItem(Index))
else
  raise ETLIndexOfBounds.CreateFmt('TStoredConfigsList.GetStoredConfigPointer: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TStoredConfigsList.GetStoredConfig(Index: Integer): TStoredConfig;
begin
Result := GetStoredConfigPointer(Index)^;
end;

{------------------------------------------------------------------------------}
{   TStoredConfigsList // Public methods                                       }
{------------------------------------------------------------------------------}

procedure TStoredConfigsList.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PStoredConfig(PtrGetItem(i)));
inherited;
end;

//------------------------------------------------------------------------------

Function TStoredConfigsList.IndexOf(const ID, Attribute: TelemetryString): Integer;
begin
For Result := 0 to (Count - 1) do
  If TelemetrySameStr(PStoredConfig(PtrGetItem(Result))^.Reference.ID,ID) and
     TelemetrySameStr(PStoredConfig(PtrGetItem(Result))^.Reference.Attribute,Attribute) then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.IndexOf(ConfigReference: TConfigReference): Integer;
begin
Result := IndexOf(ConfigReference.ID,ConfigReference.Attribute);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.IndexOf(const ID, Attribute: TelemetryString; Index: scs_u32_t): Integer;
begin
For Result := 0 to (Count - 1) do
  If TelemetrySameStr(PStoredConfig(PtrGetItem(Result))^.Reference.ID,ID) and
     TelemetrySameStr(PStoredConfig(PtrGetItem(Result))^.Reference.Attribute,Attribute) and
    (PStoredConfig(PtrGetItem(Result))^.Index = Index) then Exit;
Result := -1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.IndexOf(ConfigReference: TConfigReference; Index: scs_u32_t): Integer;
begin
Result := IndexOf(ConfigReference.ID,ConfigReference.Attribute,Index);
end;

//------------------------------------------------------------------------------

Function TStoredConfigsList.Add(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: scs_value_localized_t; Binded: Boolean = False): Integer;
var
  NewConfig: PStoredConfig;
begin
New(NewConfig);
NewConfig^.Reference := ConfigReference(ID,Attribute);
NewConfig^.Index := Index;
NewConfig^.Value := Value;
NewConfig^.Binded := Binded;
Result := PtrAdd(NewConfig);
If Result < 0 then Dispose(NewConfig);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.Add(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: p_scs_value_t; Binded: Boolean = False): Integer;
begin
If Assigned(Value) then Result := Add(ID,Attribute,Index,scs_value_localized(Value^),Binded)
  else Result := Add(ID,Attribute,Index,scs_value_localized_empty,Binded);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.Add(ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t; Binded: Boolean = False): Integer;
var
  NewConfig: PStoredConfig;
begin
New(NewConfig);
NewConfig^.Reference := ConfigReference;
NewConfig^.Index := Index;
NewConfig^.Value := Value;
NewConfig^.Binded := Binded;
Result := PtrAdd(NewConfig);
If Result < 0 then Dispose(NewConfig);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.Add(ConfigReference: TConfigReference; Index: scs_u32_t; Value: p_scs_value_t; Binded: Boolean = False): Integer;
begin
If Assigned(Value) then Result := Add(ConfigReference,Index,scs_value_localized(Value^),Binded)
  else Result := Add(ConfigReference,Index,scs_value_localized_empty,Binded);
end;

//------------------------------------------------------------------------------

Function TStoredConfigsList.Remove(const ID, Attribute: TelemetryString; Index: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := IndexOf(ID,Attribute,Index);
If Result >= 0 then Delete(Result);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.Remove(ConfigReference: TConfigReference; Index: scs_u32_t = SCS_U32_NIL): Integer;
begin
Result := IndexOf(ConfigReference,Index);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TStoredConfigsList.Delete(Index: Integer);
begin
If (Index >= 0) and (Index < Count) then
  begin
    Dispose(PStoredConfig(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise ETLIndexOfBounds.CreateFmt('TStoredConfigsList.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TStoredConfigsList.ChangeConfigValue(const ID, Attribute: TelemetryString; Index: scs_u32_t; Value: scs_value_localized_t): Integer;
begin
Result := IndexOf(ID,Attribute,Index);
If Result >= 0 then
  begin
    PStoredConfig(PtrGetItem(Result))^.Value := Value;
    DoChange;
  end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredConfigsList.ChangeConfigValue(ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t): Integer;
begin
Result := IndexOf(ConfigReference,Index);
If Result >= 0 then
  begin
    PStoredConfig(PtrGetItem(Result))^.Value := Value;
    DoChange;
  end;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                             TStoredChannelsList                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TStoredChannelsList // Implementation                                      }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TStoredChannelsList // Private methods                                     }
{------------------------------------------------------------------------------}

Function TStoredChannelsList.GetStoredChannelValue(Index: Integer): TStoredChannel;
begin
If (Index >= 0) and (Index < Count) then
  Result := PStoredChannelMasterID(PtrGetItem(Index))^.StoredChannel
else
  raise ETLIndexOfBounds.CreateFmt('TStoredChannelsList.GetStoredChannelValue: Index (%d) out of bounds.',[Index]);
end;

{------------------------------------------------------------------------------}
{   TStoredChannelsList // Protected methods                                   }
{------------------------------------------------------------------------------}

Function TStoredChannelsList.GetInsertIndex(MasterID: TMasterID): Integer;
var
  Index:    Integer;
  MinIndex: Integer;
  MaxIndex: Integer;
begin
Result := -1;
If Count > 0 then
  begin
    Index := -1;
    MinIndex := 0;
    MaxIndex := Count - 1;
    while MaxIndex >= MinIndex do
      begin
        Index := ((MaxIndex - MinIndex) shr 1) + MinIndex;
        If PStoredChannelMasterID(PtrGetItem(Index))^.MasterID < MasterID then MinIndex := Index + 1
          else If PStoredChannelMasterID(PtrGetItem(Index))^.MasterID > MasterID then MaxIndex := Index - 1;
      end;
    If Index >= 0 then
      If PStoredChannelMasterID(PtrGetItem(Index))^.MasterID > MasterID then Result := Index
        else Result := Index + 1;
  end;
end;

{------------------------------------------------------------------------------}
{   TStoredChannelsList // Public methods                                      }
{------------------------------------------------------------------------------}

procedure TStoredChannelsList.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PStoredChannelMasterID(PtrGetItem(i)));
inherited;
end;

//------------------------------------------------------------------------------

Function TStoredChannelsList.IndexOf(MasterID: TMasterID): Integer;
var
  Index:    Integer;
  MinIndex: Integer;
  MaxIndex: Integer;
begin
Result := -1;
MinIndex := 0;
MaxIndex := Count - 1;
while MaxIndex >= MinIndex do
  begin
    Index := ((MaxIndex - MinIndex) shr 1) + MinIndex;
    If PStoredChannelMasterID(PtrGetItem(Index))^.MasterID < MasterID then MinIndex := Index + 1
      else If PStoredChannelMasterID(PtrGetItem(Index))^.MasterID > MasterID then MaxIndex := Index - 1
        else
          begin
            Result := Index;
            Break;
          end;
   end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredChannelsList.IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
begin
Result := IndexOf(GetMasterID(ID,Index,ValueType));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TStoredChannelsList.IndexOf(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
begin
Result := IndexOf(GetItemID(Name),Index,ValueType);
end;

//------------------------------------------------------------------------------

Function TStoredChannelsList.StoreChannelValue(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): Integer;
var
  MasterID:       TMasterID;
  StoredChannel:  PStoredChannelMasterID;
begin
Result := -1;
If Assigned(Value) then
  begin
    MasterID := GetMasterID(ID,Index,Value^._type);
    Result := IndexOf(MasterID);
    If Result >= 0 then
      begin
        StoredChannel := PStoredChannelMasterID(PtrGetItem(Result));
        StoredChannel^.MasterID := MasterID;
        StoredChannel^.StoredChannel.Name := Name;
        StoredChannel^.StoredChannel.ID := ID;
        StoredChannel^.StoredChannel.Index := Index;
        StoredChannel^.StoredChannel.Value := scs_value_localized(Value^);
        DoChange;
      end
    else
      begin
        New(StoredChannel);
        StoredChannel^.MasterID := MasterID;
        StoredChannel^.StoredChannel.Name := Name;
        StoredChannel^.StoredChannel.ID := ID;
        StoredChannel^.StoredChannel.Index := Index;
        StoredChannel^.StoredChannel.Value := scs_value_localized(Value^);
        Result := GetInsertIndex(MasterID);
        If Result >= 0 then
          PtrInsert(Result,StoredChannel)
        else
          Result := PtrAdd(StoredChannel);
        If Result < 0 then Dispose(StoredChannel);
      end;
  end;
end;

end.
