{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{@html(<hr>)
@abstract(Provides classes implemented as one way circular buffer.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-16)
@lastmod(2014-11-25)

  @bold(@NoAutoLink(TelemetryCommCircularBuffers))

  ©František Milt, all rights reserved.

  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TCircularItemsBuffer
   |- TCircularChannelsBuffer
   |- TDefferedOperationsBuffer
)
  Last change:  2014-11-25

  Change List:@unorderedList(
    @item(2014-05-16 - First stable version.)
    @item(2014-11-25 - Changes due to a new system of storing and passing
                       secondary types of channel value. These changes include:
                       @unorderedList(
                         @itemSpacing(Compact)
                         @item(Fields @code(RegisterSecondaryTypes) and
                               @code(RegisterTertiaryTypes) in structure
                               TChannelRegisterAllOperationInfo replaced by field
                               @link(TKnownChannel.SecondaryTypesSelectionMask
                               SecondaryTypesSelectionMask))
                         @item(Method TDefferedOperationsBuffer.AddOperation
                               (variant that stores information about registering
                               all channels) reiwritten))))

@html(<hr>)}
unit TelemetryCommCircularBuffers;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryCommon,
  TelemetryIDs,
  TelemetryConversions,
  TelemetryLists,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}

type
{
  Event type used in circular buffers on item operations (drop, addition,
   etc.). @code(Item) parameter contains pointer to data field of affected item.
}
  TCircularBufferItemNotifyEvent = procedure(Sender: TObject; Item: Pointer) of object;

{$IFDEF MulticastEvents}
  {$DEFINE DeclarationPart}
    {$INCLUDE '.\Inc\TelemetryCommCircularBuffers_MulticastEvents.pas'}
  {$UNDEF DeclarationPart}
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TCircularItemsBuffer                             }
{------------------------------------------------------------------------------}
{==============================================================================}

{
  @abstract(Structure used as array item in all circular buffers.)
  Field Data contains actual item payload.

  @member(Filled When set to @true, item is considefer to be full (
                 assigned/stored/...).)
  @member(Data   Pointer to data (structure) used in individual circular buffers
                 (descendants).)
}
  TCircularBufferItem = record
    Filled: Boolean;
    Data:   Pointer;
  end;

{
  Structure used when traversing items in circular buffers.

  @member(Item  Contains pointer to items @code(Data) field, @nil when
                traversing function failed.)
  @member(Index @noAutoLink(Index) of current @noAutoLink(item). Only for
                internal purposes, do not use or change this value!)
}
  TCircularBufferTraversalInfo = record
    Item:  Pointer;
    Index: Integer;
  end;

{==============================================================================}
{   TCircularItemsBuffer // Class declaration                                  }
{==============================================================================}
{
  @abstract(Ancestor class for other circular buffer classes.)
  It is implemented as one way circular buffer, meaning it is created with fixed
  @noAutoLink(size), and is used where there is need for buffer that prevents
  uncontrolled growth of memory use. When the buffer becames full, newly added
  items simply overwrites the oldest ones (they are dropped).@br
  Buffer internally uses TCircularBufferItem structures to store individual
  buffered items, but only field @code(Data) of this structure can be accessed
  by user. So when adding, peeking, removing, etc. any item, only this pointer
  is accessed, everything else is managed automatically.


  @member(fMainArray
    Array used internally as actual storage for items. Its @noAutoLink(size) is
    initialized during creation and cannot be changed.)

  @member(fItemsDropped See ItemsDropped property.)

  @member(fCurrentPosition
    Index pointing to current position in fMainArray.)

  @member(fOldestPosition
    Index in fMainArray pointing to oldest stored item.)

  @member(fOnItemDrop Holds reference to OnItemDrop event handler.)

  @member(fOnItemAdd Holds reference to OnItemAdd event handler.)

  @member(fOnItemRemove Holds reference to OnItemRemove event handler.)

  @member(fOnItemDropMulti Object managing multicast OnItemDropMulti event.)

  @member(fOnItemAddMulti Object managing multicast OnItemAddMulti event.)

  @member(fOnItemRemoveMulti Object managing multicast OnItemRemoveMulti event.)

  @member(GetSize
    Getter for Size property.

    @returns @noAutoLink(Size) of the buffer.)

  @member(GetCount
    Getter for Count property.

    @returns Number of filled array items.)



  @member(DoOnItemDrop
    Calls hanler(s) of OnItemDrop event.)

  @member(DoOnItemAdd
    Calls hanler(s) of OnItemAdd event.)

    @member(DoOnItemRemove
    Calls hanler(s) of OnItemRemove event.)

  @member(DropItem
    Called when item at position given by index has to be dropped.@br
    Item memory (field @code(Data)) is not freed! Override this method if you
    want to free item memory (put inherited code at the end).@br
    OnItemDrop event is called before dropping the item.

    @param Index Index of item that has to be dropped.)

  @member(InitializeItem
    Override this method to initialize @code(Data) field of individual items.
    It is called on object creation for each item in internal array.

    @param Item Item to be initialized.)

  @member(IndexInc
    Performs circular addition (i.e. when the value exceeds maximum, it is set
    to minimum) on @code(Index) parameter.

    @param Index Value to be processed.)

  @member(IndexSucc
    Returns next index in array from @code(Index) parameter. As this array
    should behave as one-way circular buffer, the next value after maximum is
    mininum.

    @param Index Actual index from whom next index should be returned.

    @returns Next index after value given in @code(Index) parameter.)



  @member(Create
    Object constructor.

    @code(Size) parameter determines @noAutoLink(size) the created buffer will
    have. If @code(Size) is smaller than 1, then buffer of size 1 is created.

    @param Size Requiered @noAutoLink(size) of created buffer.)

  @member(Destroy
    Object destructor.

    Clears entire array, so explicit call to Clear before object destruction is
    unnecessary.)

  @member(Clear
    Sets @code(Filled) field of all items to @false and resets positions.@br
    Items memory is not freed! Override this method if you want to free items
    memory (put inherited code at the end).)

  @member(AddItem
    Use this method to add new item to the buffer.@br
    If buffer is full, then item at current position is dropped (DropItem method
    is called) and replaced by the passed one.@br
    Item parameter must not be nil, otherwise nothing is added.@br
    OnItemAdd event is called after successful addition.

    @param Item Pointer that should be added to buffer. Must not be @nil.)

  @member(PeekItem
    Returns oldest stored item pointer.@br
    If no item is stored in the buffer, then an exception is raised (use method
    ContainsItem to check whether there is any item stored).

    @returns Oldest stored pointer.)

  @member(RemoveItem
    Removes (sets @code(Filled) field of array items to @false) oldest stored
    pointer.@br
    OnItemRemove event is called before removal.

    @returns @True when item was succesfully removed, otherwise @false (e.g.
             when buffer contains no item).)

  @member(ContainsItem
    Checks @code(Filled) field of array item at position fOldestPosition.

    @returns @True when the buffer contains at least one item, othrewise @false.)

  @member(IsFull
    Buffer is considered full when fCurrentPosition equals fOldestPosition and
    item at fOldestPosition is filled.

    @returns @True when buffer is considered full, otherwise @false.)

  @member(IsEmpty
    Buffer is considered full when fCurrentPosition equals fOldestPosition and
    item at fOldestPosition is not filled.

    @returns @True when buffer is considered empty, otherwise @false.)

  @member(TraverseFirst
    Initializes @code(TraversalInfo) parameter and, when there is any, returns
    first traversable item.@br
    Traversing is done from the oldest item to newest.

    @param TraversalInfo Structure holding information for items traversal.

    @returns(@True when an item is returned in @code(TraversalInfo) parameter,
             @false otherwise (you must stop traversal in that case).))

  @member(TraverseNext
    Returns next traversable item. You must call TraverseFirst before calling
    this method.

    @param TraverseInfo Structure holding information for items traversal.

    @returns(@True when an item is returned in @code(TraversalInfo) parameter,
             @false otherwise (you must stop traversal in that case).))

  @member(TraverseClose
    Closes (finalizes) items traversal. You must call this method after every
    call to TraverseFirst (after you finishes traversing items).

    @param TraversalInfo Structure to be finalized.)
    


  @member(Size
    @noAutoLink(Size) of the buffer (number of items it can store).)

  @member(Count
    Number of filled array items (those with field @code(Filled) set to @true).)

  @member(ItemsDropped
    Number of items dropped since creation of current instance.@br
    Intialized to 0.)

  @member(OnItemDrop
    Event called just before item is droped.)

  @member(OnItemAdd
    Event called afted addition of new item, but before positions are
    recalculated, so do not use methods working with items inside this event.)

  @member(OnItemRemove
    Event called before item removal. As descendant will propably free items
    memory before this event can be caled, it is recommended to handle calls to
    this event manually or replace it with different event.)

  @member(OnItemDropMulti
    Multicast event called just before item is droped.)

  @member(OnItemAddMulti
    Multicast event called afted addition of new item.)

  @member(OnItemRemoveMulti
    Multicast event called before item removal.)
}
  TCircularItemsBuffer = class(TObject)
  private
    fMainArray:         Array of TCircularBufferItem;
    fItemsDropped:      Int64;
    fCurrentPosition:   Integer;
    fOldestPosition:    Integer;
    fOnItemDrop:        TCircularBufferItemNotifyEvent;
    fOnItemAdd:         TCircularBufferItemNotifyEvent;
    fOnItemRemove:      TCircularBufferItemNotifyEvent;
  {$IFDEF MulticastEvents}
    fOnItemDropMulti:   TMulticastCircularBufferItemNotifyEvent;
    fOnItemAddMulti:    TMulticastCircularBufferItemNotifyEvent;
    fOnItemRemoveMulti: TMulticastCircularBufferItemNotifyEvent;
  {$ENDIF}
    Function GetSize:   Integer;
    Function GetCount:  Integer;
  protected
    procedure DoOnItemDrop(Item: Pointer); virtual;
    procedure DoOnItemAdd(Item: Pointer); virtual;
    procedure DoOnItemRemove(Item: Pointer); virtual;
    procedure DropItem(Index: Integer); virtual;
    procedure InitializeItem(var Item: TCircularBufferItem); virtual;
    procedure IndexInc(var Index: Integer); virtual;
    Function IndexSucc(Index: Integer): Integer; virtual;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddItem(Item: Pointer); virtual;
    Function PeekItem: Pointer; virtual;
    Function RemoveItem: Boolean; virtual;
    Function ContainsItem: Boolean; virtual;
    Function IsFull: Boolean; virtual;
    Function IsEmpty: Boolean; virtual;
    Function TraverseFirst(var TraversalInfo: TCircularBufferTraversalInfo): Boolean; virtual;
    Function TraverseNext(var TraversalInfo: TCircularBufferTraversalInfo): Boolean; virtual;
    procedure TraverseClose(var TraversalInfo: TCircularBufferTraversalInfo); virtual;
  published
    property Size: Integer read GetSize;
    property Count: Integer read GetCount;
    property ItemsDropped: Int64 read fItemsDropped; 
    property OnItemDrop: TCircularBufferItemNotifyEvent read fOnItemDrop write fOnItemDrop;
    property OnItemAdd: TCircularBufferItemNotifyEvent read fOnItemAdd write fOnItemAdd;
    property OnItemRemove: TCircularBufferItemNotifyEvent read fOnItemRemove write fOnItemRemove;
  {$IFDEF MulticastEvents}
    property OnItemDropMulti: TMulticastCircularBufferItemNotifyEvent read fOnItemDropMulti;
    property OnItemAddMulti: TMulticastCircularBufferItemNotifyEvent read fOnItemAddMulti;
    property OnItemRemoveMulti: TMulticastCircularBufferItemNotifyEvent read fOnItemRemoveMulti;
  {$ENDIF}
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                            TCircularChannelsBuffer                           }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  // Default size of the channels buffer (number of channels it can store).
  def_CircularChannelsBuffer_BufferSize = 500;

{==============================================================================}
{   TCircularItemsBuffer // Class declaration                                  }
{==============================================================================}
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Complete documentation (reference to BufferChannels in communicator.'}
{$ENDIF}
{
  @abstract(Used to store buffered channels.)

  @member(InitializeItem
    Initializes items @code(Data) field to TStoredChannel structure.

    See @inherited for further info.)


  @member(Create
    Object constructor.

    @param aSize Requiered @noAutoLink(size) of created buffer.)

  @member(Destroy
    Object destructor.

    Frees memory of all items.)

  @member(PeekChannel
    Returns oldest stored channel.@br
    If no channel is stored in the buffer, then an exception is raised (use
    method ContainsChannel to check whether there is any channel stored).@br
    Calls PeekItem method.

    @returns Oldest stored channel.)

  @member(RemoveChannel
    Removes (deletes, frees used memory) oldest stored channel.@br
    Calls RemoveItem method.

    @returns @True when channel was succesfully removed, otherwise @false.)

  @member(ContainsChannel
    Calls ContainsItem method.

    @returns(@True when the buffer contains at least one channel, otherwise
             @false.))

}
type
  TCircularChannelsBuffer = class(TCircularItemsBuffer)
  protected
    procedure InitializeItem(var Item: TCircularBufferItem); override;
  public
    constructor Create(aSize: Integer = def_CircularChannelsBuffer_BufferSize);
    destructor Destroy; override;
  {
    Use this method to add new channel to the buffer.@br
    If buffer is full, then channel at current position is dropped and replaced
    by the passed one.@br
    Calls AddItem method.

    @param Name  Name of added channel.
    @param ID    Identificator of added channel.
    @param Index Index of added channel.
    @param Value Localized channel value.
  }
    procedure AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t); overload; virtual;
  {
    Use this method to add new channel to the buffer.@br
    If buffer is full, then channel at current position is dropped and replaced
    by the passed one.@br
    Calls AddItem method.

    @param Name  Name of added channel.
    @param ID    Identificator of added channel.
    @param Index Index of added channel.
    @param Value Channel value.
  }
    procedure AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: scs_value_t); overload; virtual;
  {
    Use this method to add new channel to the buffer.@br
    If buffer is full, then channel at current position is dropped and replaced
    by the passed one.@br
    When "value" parameter is not assigned, cEmptySCSValueLocalized is added.@br
    Calls AddItem method.

    @param Name  Name of added channel.
    @param ID    Identificator of added channel.
    @param Index Index of added channel.
    @param Value Pointer to channel value.
  }
    procedure AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); overload; virtual;
    Function PeekChannel: TStoredChannel; virtual;
    Function RemoveChannel: Boolean; virtual;
    Function ContainsChannel: Boolean; virtual;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                           TDefferedOperationsBuffer                          }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  // Default size of the deffered operations buffer (number of operations it can
  // hold).
  def_DefferedOperationsBuffer_BufferSize = 500;

type
{
  Type used in TDefferedOperationsBuffer denoting what type of operation was
  deffered and has to be executed later.

  @value(dotNone                No operation is deffered.)
  @value(dotEventReg            Event registration is deffered.)
  @value(dotEventUnreg          Event unregistration is deffered.)
  @value(dotEventRegByIndex     Event registration by index is deffered.)
  @value(dotEventUnregIndex     Event unregistration on index is deffered.)
  @value(dotEventUnregByIndex   Event unregistration by index is deffered.)
  @value(dotEventRegAll         Registration of all known events is deffered.)
  @value(dotEventUnregAll       Unregistration of all registered events is
                                deffered.)
  @value(dotChannelReg          Channel registration is deffered.)
  @value(dotChannelUnreg        Channel unregistration is deffered.)
  @value(dotChannelRegByIndex   Channel registration by index is deffered.)
  @value(dotChannelUnregIndex   Channel unregistration on index is deffered.)  
  @value(dotChannelUnregByIndex Channel unregistration by index is deffered.)
  @value(dotChannelRegByName    Channel registration by name is deffered.)
  @value(dotChannelUnregByName  Channel unregistration by name is deffered.)
  @value(dotChannelRegAll       Registration of all known channels is deffered.)
  @value(dotChannelUnregAll     Unregistration of all registered channels is
                                deffered.)
}
  TDefferedOperationType = (dotNone, dotEventReg, dotEventUnreg,
    dotEventRegByIndex, dotEventUnregIndex, dotEventUnregByIndex,
    dotEventRegAll, dotEventUnregAll, dotChannelReg, dotChannelUnreg,
    dotChannelRegAll, dotChannelUnregAll, dotChannelRegByIndex,
    dotChannelUnregIndex, dotChannelUnregByIndex, dotChannelRegByName,
    dotChannelUnregByName);

{
  Structure used in TDefferedOperationsBuffer to hold parameters for
  @code(dotChannelRegisterAll) operation type.

  @member(RegPrimaryType         Primary value types should be registered.)
  @member(SecondarySelectionMask Bitmask denoting what secondary value types
                                 should be registered. See description of
                                 function SelectSupportedValueTypes (parameter
                                 @code(SecondarySelectionMask)) for details
                                 about how the individual types are selected
                                 based on this mask.)
}
  TChannelRegisterAllOperationInfo = record
    RegPrimaryType:         Boolean;
    SecondarySelectionMask: LongWord;
  end;

{
  @abstract(Used as item in TDefferedOperationsBuffer to store operations and
            their parameters.)
  It is also returned when peeking deffered operations buffer.

  @member(ConnectionData       Contains data about connection endpoint that
                               added deffered operation to the buffer (eg.
                               socket, pipe, ...).)
  @member(OperationType        Type of deffered operation.)
  @member(EventParams          Parameters for events-based operations.)
  @member(ChannelParams        Parameters for channels-based operations.)
  @member(ChannelsRegAllParams Parameters for registration of all channels.)
}
  TDefferedOperation = record
    ConnectionData:       Pointer;
    OperationType:        TDefferedOperationType;
    EventParams:          TEventInfo;
    ChannelParams:        TChannelInfo;
    ChannelsRegAllParams: TChannelRegisterAllOperationInfo;
  end;
  // Pointer to TDefferedOperation structure.
  PDefferedOperation = ^TDefferedOperation;

{==============================================================================}
{   TDefferedOperationsBuffer // Class declaration                             }
{==============================================================================}
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Complete documentation (reference to DefferedOperations in communicator.'}
{$ENDIF}
{
  @abstract(Used to store deferred operations and their parameters.)


  @member(InitializeItem
    Initializes items @code(Data) field to TDefferedOperation structure.

    See @inherited for further info.)



  @member(Create
    Object constructor.

    @param Size Requiered @noAutoLink(size) of created buffer.)

  @member(Destroy
    Object destructor.

    Frees memory of all items.)

  @member(PeekOperation
    Returns oldest deffered operation.@br
    If no Operation is stored in the buffer, then an exception is raised (use
    method ContainsOperation to check whether there is any operation stored).@br
    Calls PeekItem method.

    @returns Oldest deffered operation.)

  @member(RemoveOperation
    Removes (deletes, frees used memory) oldest deffered operation.@br
    Calls RemoveItem method.

    @returns @True when operation was succesfully removed, otherwise @false.)

  @member(Containsoperation
    Calls ContainsItem method.

    @returns(@True when the buffer contains at least one deffered operation,
             othrewise @false.))
}
  TDefferedOperationsBuffer = class(TCircularItemsBuffer)
  protected
    procedure InitializeItem(var Item: TCircularBufferItem); override;
  public
    constructor Create(aSize: Integer = def_DefferedOperationsBuffer_BufferSize);
    destructor Destroy; override;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for operations with no parameters (@code(dotNone,
    dotEventRegisterAll, dotEventUnregisterAll, dotChannelUnregisterAll)).

    @param(ConnectionData Data about connection endpoint that is adding this
                          operation.)
    @param OperationType  Type of deffered operation.
  }
    procedure AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for event registration management operations
    (@code(dotEventRegister,dotEventUnregister)). Item @code(EventParams) field
    is filled.

    @param(ConnectionData Data about connection endpoint that is adding this
                          operation.)
    @param OperationType  Type of deffered operation.
    @param Event          Operation parameter (event identifier).
  }
    procedure AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType; Event: scs_event_t); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for basic channel registration management operations
    (@code(dotChannelRegister,dotChannelUnregister)). Item @code(ChannelParams)
    field is filled.

    @param(ConnectionData Data about connection endpoint that is adding this
                          operation.)
    @param OperationType  Type of deffered operation.
    @param Name           Operation parameter (channel name).
    @param Index          Operation parameter (channel index).
    @param ValueType      Operation parameter (channel value type).
    @param(Flags          Operation parameter (registration flags, valid only
                          for @code(dotChannelRegister)).)
  }
    procedure AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType;
                           Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t;
                           Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for operation that registers all known channels
    (@code(dotChannelRegisterAll)). Item @code(ChannelsRegAllParams) field is
    filled.

    @param(ConnectionData         Data about connection endpoint that is adding
                                  this operation.)
    @param OperationType          Type of deffered operation.
    @param RegPrimaryType         Operation parameter (primary value types).
    @param(SecondarySelectionMask Operation parameter (secondary value types
                                  selection bitmask).)

  }
    procedure AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType;
                           RegPrimaryType: Boolean; SecondarySelectionMask: LongWord); overload; virtual;
    Function PeekOperation: TDefferedOperation; virtual;
    Function RemoveOperation: Boolean; virtual;
    Function ContainsOperation: Boolean; virtual;
  end;

implementation

uses
  SysUtils;

{$IFDEF MulticastEvents}
  {$DEFINE ImplementationPart}
    {$INCLUDE '.\Inc\TelemetryCommCircularBuffers_MulticastEvents.pas'}
  {$UNDEF ImplementationPart}
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TCircularItemsBuffer                             }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TCircularItemsBuffer // Implementation                                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TCircularItemsBuffer // Private methods                                    }
{------------------------------------------------------------------------------}

Function TCircularItemsBuffer.GetSize: Integer;
begin
Result := Length(fMainArray);
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.GetCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Low(fMainArray) to High(fMainArray) do
  If fMainArray[i].Filled then Inc(Result);
end;

{------------------------------------------------------------------------------}
{   TCircularItemsBuffer // Protected methods                                  }
{------------------------------------------------------------------------------}

procedure TCircularItemsBuffer.DoOnItemDrop(Item: Pointer);
begin
If Assigned(OnItemDrop) then OnItemDrop(Self,Item);
{$IFDEF MulticastEvents}
OnItemDropMulti.Call(Self,Item);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.DoOnItemAdd(Item: Pointer);
begin
If Assigned(OnItemAdd) then OnItemAdd(Self,Item);
{$IFDEF MulticastEvents}
OnItemAddMulti.Call(Self,Item);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.DoOnItemRemove(Item: Pointer);
begin
If Assigned(OnItemRemove) then OnItemRemove(Self,Item);
{$IFDEF MulticastEvents}
OnItemRemoveMulti.Call(Self,Item);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.DropItem(Index: Integer);
begin
DoOnItemDrop(fMainArray[Index].Data);
fMainArray[Index].Filled := False;
Inc(fItemsDropped);
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.InitializeItem(var Item: TCircularBufferItem);
begin
Item.Filled := False;
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.IndexInc(var Index: Integer);
begin
Inc(Index);
If Index > High(fMainArray) then Index := Low(fMainArray);
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.IndexSucc(Index: Integer): Integer;
begin
Result := Index;
IndexInc(Result);
end;

{------------------------------------------------------------------------------}
{   TCircularItemsBuffer // Public methods                                     }
{------------------------------------------------------------------------------}

constructor TCircularItemsBuffer.Create(Size: Integer);
var
  i:  Integer;
begin
inherited Create;
If Size < 1 then Size := 1;
SetLength(fMainArray,Size);
fItemsDropped := 0;
// Initialize array.
For i := Low(fMainArray) to High(fMainArray) do InitializeItem(fMainArray[i]);
{$IFDEF MulticastEvents}
fOnItemDropMulti := TMulticastCircularBufferItemNotifyEvent.Create(Self);
fOnItemAddMulti := TMulticastCircularBufferItemNotifyEvent.Create(Self);
fOnItemRemoveMulti := TMulticastCircularBufferItemNotifyEvent.Create(Self);
{$ENDIF}
end;

//------------------------------------------------------------------------------

destructor TCircularItemsBuffer.Destroy;
begin
{$IFDEF MulticastEvents}
fOnItemDropMulti.Free;
fOnItemAddMulti.Free;
fOnItemRemoveMulti.Free;
{$ENDIF}
Clear;
SetLength(fMainArray,0);
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.Clear;
var
  i: Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do fMainArray[i].Filled := False;
fCurrentPosition := Low(fMainArray);
fOldestPosition := fCurrentPosition;
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.AddItem(Item: Pointer);
begin
If Assigned(Item) then
  begin
    If fMainArray[fCurrentPosition].Filled then DropItem(fCurrentPosition);
    fMainArray[fCurrentPosition].Filled := True;
    fMainArray[fCurrentPosition].Data := Item;
    DoOnItemAdd(fMainArray[fCurrentPosition].Data);
    If (fOldestPosition = fCurrentPosition) and fMainArray[IndexSucc(fOldestPosition)].Filled then
      IndexInc(fOldestPosition);
    IndexInc(fCurrentPosition);
  end;
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.PeekItem: Pointer;
begin
If fMainArray[fOldestPosition].Filled then
  Result := fMainArray[fOldestPosition].Data
else
  raise Exception.Create('TCircularItemsBuffer.PeekItem: No item to peek.');
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.RemoveItem: Boolean;
begin
If fMainArray[fOldestPosition].Filled then
  begin
    DoOnItemRemove(fMainArray[fOldestPosition].Data);
    fMainArray[fOldestPosition].Filled := False;
    IndexInc(fOldestPosition);
    Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.ContainsItem: Boolean;
begin
Result := fMainArray[fOldestPosition].Filled;
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.IsFull: Boolean;
begin
Result := (fCurrentPosition = fOldestPosition) and fMainArray[fOldestPosition].Filled;
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.IsEmpty: Boolean;
begin
Result := (fCurrentPosition = fOldestPosition) and not fMainArray[fOldestPosition].Filled;
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.TraverseFirst(var TraversalInfo: TCircularBufferTraversalInfo): Boolean;
begin
If fMainArray[fOldestPosition].Filled then
  begin
    TraversalInfo.Item := fMainArray[fOldestPosition].Data;
    TraversalInfo.Index := fOldestPosition;
  end
else
  begin
    TraversalInfo.Item := nil;
    TraversalInfo.Index := -1;
  end;
Result := Assigned(TraversalInfo.Item);
end;

//------------------------------------------------------------------------------

Function TCircularItemsBuffer.TraverseNext(var TraversalInfo: TCircularBufferTraversalInfo): Boolean;
begin
If TraversalInfo.Index >= 0 then
  begin
    IndexInc(TraversalInfo.Index);
    If (TraversalInfo.Index <> fOldestPosition) and fMainArray[TraversalInfo.Index].Filled then
      begin
        TraversalInfo.Item := fMainArray[TraversalInfo.Index].Data;
      end
    else
      begin
        TraversalInfo.Item := nil;
        TraversalInfo.Index := -1;
      end;
    Result := Assigned(TraversalInfo.Item);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.TraverseClose(var TraversalInfo: TCircularBufferTraversalInfo);
begin
TraversalInfo.Item := nil;
TraversalInfo.Index := -1;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TCircularChannelsBuffer                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TCircularChannelsBuffer // Implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TCircularChannelsBuffer // Protected methods                               }
{------------------------------------------------------------------------------}

procedure TCircularChannelsBuffer.InitializeItem(var Item: TCircularBufferItem);
begin
New(PStoredChannel(Item.Data));
inherited;
end;

{------------------------------------------------------------------------------}
{   TCircularChannelsBuffer // Public methods                                  }
{------------------------------------------------------------------------------}

constructor TCircularChannelsBuffer.Create(aSize: Integer = def_CircularChannelsBuffer_BufferSize);
begin
inherited Create(aSize);
end;

//------------------------------------------------------------------------------

destructor TCircularChannelsBuffer.Destroy;
var
  i:  Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do
  Dispose(PStoredChannel(fMainArray[i].Data));
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularChannelsBuffer.AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  BufferedChannel:  PStoredChannel;
begin
BufferedChannel := fMainArray[fCurrentPosition].Data;
BufferedChannel^.Name := Name;
BufferedChannel^.ID := ID;
BufferedChannel^.Index := Index;
BufferedChannel^.Value := Value;
AddItem(BufferedChannel);
end;

procedure TCircularChannelsBuffer.AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: scs_value_t);
begin
AddChannel(Name,ID,Index,scs_value_localized(Value));
end;

procedure TCircularChannelsBuffer.AddChannel(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If Assigned(Value) then
  AddChannel(Name,ID,Index,scs_value_localized(Value^))
else
  AddChannel(Name,ID,Index,EmptySCSValueLocalized);
end;

//------------------------------------------------------------------------------

Function TCircularChannelsBuffer.PeekChannel: TStoredChannel;
begin
Result := PStoredChannel(PeekItem)^;
end;

//------------------------------------------------------------------------------

Function TCircularChannelsBuffer.RemoveChannel: Boolean;
begin
Result := RemoveItem;
end;

//------------------------------------------------------------------------------

Function TCircularChannelsBuffer.ContainsChannel: Boolean;
begin
Result := ContainsItem;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                           TDefferedOperationsBuffer                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TDefferedOperationsBuffer // Implementation                                }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TDefferedOperationsBuffer // Protected methods                             }
{------------------------------------------------------------------------------}

procedure TDefferedOperationsBuffer.InitializeItem(var Item: TCircularBufferItem);
begin
New(PDefferedOperation(Item.Data));
inherited;
end;

{------------------------------------------------------------------------------}
{   TDefferedOperationsBuffer // Public methods                                }
{------------------------------------------------------------------------------}

constructor TDefferedOperationsBuffer.Create(aSize: Integer = def_DefferedOperationsBuffer_BufferSize);
begin
inherited Create(aSize);
end;

//------------------------------------------------------------------------------

destructor TDefferedOperationsBuffer.Destroy;
var
  i:  Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do
  Dispose(PDefferedOperation(fMainArray[i].Data));
inherited;
end;

//------------------------------------------------------------------------------

procedure TDefferedOperationsBuffer.AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotNone,dotEventRegAll,dotEventUnregAll,dotChannelUnregAll] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.ConnectionData := ConnectionData;
    DefferedOperation^.OperationType := OperationType;
    AddItem(DefferedOperation);
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation: Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

procedure TDefferedOperationsBuffer.AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType; Event: scs_event_t);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotEventReg,dotEventUnreg,dotEventRegByIndex,dotEventUnregByIndex] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.ConnectionData := ConnectionData;
    DefferedOperation^.OperationType := OperationType;
    DefferedOperation^.EventParams.Event := Event;
    AddItem(DefferedOperation);
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation(<Event>): Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

procedure TDefferedOperationsBuffer.AddOperation(ConnectionData: Pointer;
                                                 OperationType: TDefferedOperationType;
                                                 Name: TelemetryString;
                                                 Index: scs_u32_t;
                                                 ValueType: scs_value_type_t;
                                                 Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotChannelReg,dotChannelUnreg,
                     dotChannelRegByIndex,dotChannelUnregByIndex,
                     dotChannelRegByName,dotChannelUnregByName] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.ConnectionData := ConnectionData;
    DefferedOperation^.OperationType := OperationType;
    DefferedOperation^.ChannelParams.Name := Name;
    DefferedOperation^.ChannelParams.Index := Index;
    DefferedOperation^.ChannelParams.ValueType := ValueType;
    DefferedOperation^.ChannelParams.Flags := Flags;
    AddItem(DefferedOperation)
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation(<Channel>): Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

procedure TDefferedOperationsBuffer.AddOperation(ConnectionData: Pointer; OperationType: TDefferedOperationType;
                                                 RegPrimaryType: Boolean; SecondarySelectionMask: LongWord);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotChannelRegAll] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.ConnectionData := ConnectionData;
    DefferedOperation^.OperationType := OperationType;
    DefferedOperation^.ChannelsRegAllParams.RegPrimaryType := RegPrimaryType;
    DefferedOperation^.ChannelsRegAllParams.SecondarySelectionMask := SecondarySelectionMask;
    AddItem(DefferedOperation)
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation(<AllChannels>): Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

//------------------------------------------------------------------------------

Function TDefferedOperationsBuffer.PeekOperation: TDefferedOperation;
begin
Result := PDefferedOperation(PeekItem)^;
end;

//------------------------------------------------------------------------------

Function TDefferedOperationsBuffer.RemoveOperation: Boolean;
begin
Result := RemoveItem;
end;

//------------------------------------------------------------------------------

Function TDefferedOperationsBuffer.ContainsOperation: Boolean;
begin
Result := ContainsItem;
end;

end.

