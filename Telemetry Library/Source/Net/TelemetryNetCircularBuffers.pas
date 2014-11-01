{*******************************************************************************
@abstract(Circular item buffers.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-10)
@lastmod(2013-10-10)

  TelemetryNetCircularBuffers

  ©František Milt, all rights reserved.

  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TCircularItemsBuffer
   |- TCircularPacketsBuffer
   |- TCircularChannelsBuffer
   |- TDefferedOperationsBuffer
)
  Last change:  2013-10-10

  Change List:@unorderedList(
    @item(2013-10-10  - First stable version.))

*******************************************************************************}
unit TelemetryNetCircularBuffers;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  ScktComp,
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
  TelemetryNetPackets,  
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TCircularItemsBuffer                             }
{------------------------------------------------------------------------------}
{==============================================================================}

type
  // Structure used as array item in all circular buffers. Field Data contains
  // actual item payload.
  TArrayItem = Record
    // When set to @true, item is considefer to be full (assigned/stored/...).
    Filled: Boolean;
    // Pointer to data (structure) used in individual circular buffers
    // (descendants).
    Data:   Pointer;
  end;

  // Event type used in circular buffers on item operations (drop, addition,
  // etc.). "Item" parameter contains pointer to data of affected item. 
  TItemNotifyEvent = procedure(Sender: TObject; Item: Pointer) of object;

{==============================================================================}
{    TCircularItemsBuffer // Class declaration                                 }
{==============================================================================}
{
  @abstract(Ancestor class for other circular buffer classes.)
  It is implemented as one way circular buffer, meaning it is created with fixed
  @noAutoLink(size), and is used where there is need for buffer that prevents
  uncontrolled growth of memory use. When the buffer becames full, newly added
  items simply overwrites the oldest ones (they are dropped).@br
  Buffer internally uses TArrayItem structures to store individual buffered
  items, but only field "Data" of this structure can be accessed by user. So
  when adding, peeking, removing, etc. any item, only this pointer is accessed,
  everything else is managed automatically.


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

@member(GetSize
    Getter for Size property.

    @returns @noAutoLink(Size) of the buffer.)

@member(GetCount
    Getter for Count property.

    @returns Number of filled array items.)

@member(DropItem
    Called when item at position given by index has to be dropped.@br
    Item's memory (field "Data") is not freed! Override this method if you want
    to free item's memory (put inherited code at the end).@br
    OnItemDrop event is called before dropping the item.

    @param Index Index of item that has to be dropped.)

@member(InitializeItem
    Override this method to initialize "Data" field of individual items.
    It is called on object creation for each item in internal array.

    @param Item Item to be initialized.)

@member(IndexInc
    Performs circular addition (i.e. when the value exceeds maximum, it is set
    to minimum) on "Index" parameter.

    @param Index Value to be processed.)

@member(IndexSucc
    Returns next index in array from "Index" parameter. As this array should
    behave as one-way circular buffer, the next value after maximum is mininum.

    @param Index Actual index from whom next index should be returned.

    @returns Next index after value given in "Index" parameter.)



@member(Create
    Object constructor.@br
    "aSize" parameter determines @noAutoLink(size) the created buffer will have.
    If "aSize" is smaller than 1, then buffer of size 1 is created.

    @param aSize Requiered @noAutoLink(size) of created buffer.)

@member(Destroy
    Object destructor.@br
    Clears entire array, so explicit call to Clear before object destruction is
    unnecessary.)

@member(Clear
    Sets "Filled" field of all items to @false and resets positions.@br
    Item's memory is not freed! Override this method if you want to free item's
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
    Removes (sets "Filled" field of array items to @false) oldest stored
    pointer.@br
    OnItemRemove event is called before removal.

    @returns @True when item was succesfully removed, otherwise @false (e.g.
             when buffer contains no item).)

@member(ContainsItem
  Checks "Filled" field of array item at position fOldestPosition.

  @returns @True when the buffer contains at least one item, othrewise @false.)

@member(IsFull
  Buffer is considered full when fCurrentPosition equals fOldestPosition and
  item at fOldestPosition is filled.

  @returns @True when buffer is considered full, otherwise @false.)

@member(IsEmpty
  Buffer is considered full when fCurrentPosition equals fOldestPosition and
  item at fOldestPosition is not filled.

  @returns @True when buffer is considered empty, otherwise @false.)



@member(Size
    @noAutoLink(Size) of the buffer (number of items it can store).)

@member(Count
    Number of filled array items (those with field "Filled" set to @true).)

@member(ItemsDropped
    Number of items dropped since creation of current instance.@br
    Intialized to 0.)

@member(OnItemDrop
    Event called just before item is droped.)

@member(OnItemAdd
    Event called afted addition of new item, but before positions are
    recalculated, so do not use methods working with items inside this event.)

@member(OnItemRemove
    Event called before item removal. As descendant will propably free item's
    memory before this event can be caled, it is recommended to handle calls to
    this event manually or replace it with different event.)
}
  TCircularItemsBuffer = class(TObject)
  private
    fMainArray:         Array of TArrayItem;
    fItemsDropped:      Int64;
    fCurrentPosition:   Integer;
    fOldestPosition:    Integer;
    fOnItemDrop:        TItemNotifyEvent;
    fOnItemAdd:         TItemNotifyEvent;
    fOnItemRemove:      TItemNotifyEvent;
    Function GetSize:   Integer;
    Function GetCount:  Integer;
  protected
    procedure DropItem(Index: Integer); virtual;
    procedure InitializeItem(var Item: TArrayItem); virtual;
    procedure IndexInc(var Index: Integer); virtual;
    Function IndexSucc(Index: Integer): Integer; virtual;
  public
    constructor Create(aSize: Integer);
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddItem(Item: Pointer); virtual;
    Function PeekItem: Pointer; virtual;
    Function RemoveItem: Boolean; virtual;
    Function ContainsItem: Boolean; virtual;
    Function IsFull: Boolean; virtual;
    Function IsEmpty: Boolean; virtual;
  published
    property Size: Integer read GetSize;
    property Count: Integer read GetCount;
    property ItemsDropped: Int64 read fItemsDropped; 
    property OnItemDrop: TItemNotifyEvent read fOnItemDrop write fOnItemDrop;
    property OnItemAdd: TItemNotifyEvent read fOnItemAdd write fOnItemAdd;
    property OnItemRemove: TItemNotifyEvent read fOnItemRemove write fOnItemRemove;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                            TCircularPacketsBuffer                            }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  // Default size of the packets buffer (number of packets it can store).
  def_TCircularPacketsBuffer_BufferSize = 500;

{==============================================================================}
{    TCircularItemsBuffer // Class declaration                                 }
{==============================================================================}
{
  @abstract(TCircularPacketsBuffer serves as per-connection outgoing buffer.)
  Server-client communication is realized trought variable @noAutoLink(size)
  packets, when one has to be send, it is passed to connection
  @noAutoLink(socket). But these sockets use their own internal buffers, and
  when this @noAutoLink(socket) buffer is full, the @noAutoLink(socket) rejects
  to accept the packet. So the packet must be stored and sent a little later.
  TCircularPacketsBuffer is designed for this purpose.@br
  When a packet is rejected, it is passed to this buffer and stored. Later,
  when the @noAutoLink(socket) is ready to send more data, stored packet is
  loaded and sent.

@member(fSocket See Socket property.)

@member(fOnPacketDrop Holds reference to OnPacketDrop event handler.)

@member(GetItemsDropped
    Getter for PacketsDropped property.

    @returns Number of dropped items since creation of instance.)

@member(DropItem
    Overrided method for memory freeing of dropped packet.@br
    The OnPacketDrop event is called inside this method (before actual drop).

    See @inherited for further info.)

@member(InitializeItem
    Initializes items "Data" field to TPacketBuffer structure.

    See @inherited for further info.)



@member(Create
    Object constructor.

    @param(aSocket @noAutoLink(Socket) to which current instance is assigned
                   to.)
    @param aSize   Requiered @noAutoLink(size) of created buffer.)

@member(Destroy
    Object destructor.
    Explicitly frees memory of all items.)

@member(Clear
    Free memory of all packets and sets all items to empty state.)

@member(AddPacket
    Use this method to add new packet to the buffer.@br
    If CreateCopy is set to @true (dafault), then data are copied into new
    memory location and passed buffer can be then freed. If set to @false,
    only pointer is stored in the item, so the passed buffer must not be freed
    (it will be freed automatically on item drop or when the instance is
    destroyed).@br
    If buffer is full, then packet at current position is dropped (DropItem
    method is called) and replaced by the passed one.@br
    Calls AddItem method.

    @param Packet     Packet buffer to be added.

    @param(CreateCopy Determines whether packet copy or reference should be
                      added to the buffer.))

@member(PeekPacket
    Returns oldest stored packet.@br
    If no packet is stored in the buffer, then an exception is raised (use
    method ContainsPacket to check whether there is any packet stored).@br
    Calls PeekItem method.

    @returns Oldest stored packet buffer.)

@member(RemovePacket
    Removes (deletes, frees used memory) oldest stored packet.@br
    Calls RemoveItem method.

    @returns @True when packet was succesfully removed, otherwise @false.)

@member(ContainsPacket
    Calls ContainsItem method.

    @returns(@True when the buffer contains at least one packet, othrewise
             @false.))



@member(Socket
    @noAutoLink(Socket) to which currend instance is bound.@br
    Value is set during creation and thus is not initialized. Can be @nil.@br
    This object is passed as @noAutoLink("Socket") parameter in event calls.)

@member(PacketsDropped
    Number of packets dropped since instance creation.)

@member(OnPacketDrop
    Called when stored packet is about to be dropped (deleted). Use this event
    if you want to save or otherwise process stored data.)
}
type
  TCircularPacketsBuffer = class(TCircularItemsBuffer)
  private
    fSocket:          TCustomWinSocket;
    fOnPacketDrop:    TPacketNotifyEvent;
    Function GetItemsDropped: Int64;
  protected
    procedure DropItem(Index: Integer); override;
    procedure InitializeItem(var Item: TArrayItem); override;
  public
    constructor Create(aSocket: TCustomWinSocket = nil; aSize: Integer = def_TCircularPacketsBuffer_BufferSize);
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddPacket(Packet: TPacketBuffer; CreateCopy: Boolean = True); virtual;
    Function PeekPacket: TPacketBuffer; virtual;
    Function RemovePacket: Boolean; virtual;
    Function ContainsPacket: Boolean; virtual;
  published
    property Socket: TCustomWinsocket read fSocket;
    property PacketsDropped: Int64 read GetItemsDropped;
    property OnPacketDrop: TPacketNotifyEvent read fOnPacketDrop write fOnPacketDrop;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                            TCircularChannelsBuffer                           }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  // Default size of the channels buffer (number of channels it can store).
  def_TCircularChannelsBuffer_BufferSize = 500;

type
  // Used as item in TCircularChannelsBuffer to store passed channels data.
  // Is is also returned when peeking this buffer.
  TBufferedChannel = Record
    Name:   String;
    ID:     TChannelID;
    Index:  scs_u32_t;
    Value:  scs_value_localized_t;
  end;
  PBufferedChannel = ^TBufferedChannel;


{==============================================================================}
{    TCircularItemsBuffer // Class declaration                                 }
{==============================================================================}
{
  @abstract(Used to store buffered channels.)
  Refer to TTelemetryNetServer.BufferChannels property for details.

@member(InitializeItem
    Initializes items "Data" field to TBufferedChannel structure.

    See @inherited for further info.)



@member(Create
    Object constructor.

    @param aSize Requiered @noAutoLink(size) of created buffer.)

@member(Destroy
    Object destructor.@br
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

    @returns(@True when the buffer contains at least one channel, othrewise
             @false.))

}
  TCircularChannelsBuffer = class(TCircularItemsBuffer)
  protected
    procedure InitializeItem(var Item: TArrayItem); override;
  public
    constructor Create(aSize: Integer = def_TCircularChannelsBuffer_BufferSize);
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
    procedure AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t); overload; virtual;
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
    procedure AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_t); overload; virtual;
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
    procedure AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); overload; virtual;
    Function PeekChannel: TBufferedChannel; virtual;
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
  def_TDefferedOperationsBuffer_BufferSize = 100;

type
  // Type used in TDefferedOperationsBuffer denoting what type of operation was
  // deffered and has to be executed later.
  TDefferedOperationType = (dotNone, dotEventRegister, dotEventUnregister,
                            dotEventRegisterAll, dotEventUnregisterAll,
                            dotChannelRegister, dotChannelUnregister,
                            dotChannelRegisterAll, dotChannelUnregisterAll);

  // Structure used in TDefferedOperationsBuffer to hold parameters for
  //  @code(dotChannelUnregisterAll) operation type.
  TChannelRegisterAllOperationInfo = Record
    RegisterPrimaryTypes:   Boolean;
    RegisterSecondaryTypes: Boolean;
    RegisterTertiaryTypes:  Boolean;
  end;

  // Used as item in TDefferedOperationsBuffer to store operations and their
  // parameters.
  // Is is also returned when peeking this buffer.
  // *Params fields contain paramaters for given operation.
  TDefferedOperation = Record
    FromSocket:           TCustomWinSocket;
    OperationType:        TDefferedOperationType;
    EventParams:          TEventInfo;
    ChannelParams:        TChannelInfo;
    ChannelsRegAllParams: TChannelRegisterAllOperationInfo;
  end;
  PDefferedOperation = ^TDefferedOperation;

{==============================================================================}
{    TDefferedOperationsBuffer // Class declaration                            }
{==============================================================================}
{
  @abstract(Used to store deferred operations and their parametters.)
  Refer to TTelemetryNetServer.fDefferedOperations field for more information.


@member(InitializeItem
    Initializes items "Data" field to TDefferedOperation structure.

    See @inherited for further info.)



@member(Create
    Object constructor.

    @param aSize Requiered @noAutoLink(size) of created buffer.)

@member(Destroy
    Object destructor.@br
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
    procedure InitializeItem(var Item: TArrayItem); override;
  public
    constructor Create(aSize: Integer = def_TDefferedOperationsBuffer_BufferSize);
    destructor Destroy; override;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for operations with no parameters (@code(dotNone,
    dotEventRegisterAll, dotEventUnregisterAll, dotChannelUnregisterAll)). No
    items *Params field contains valid information.

    @param FromSocket    Socket which requested this operation.
    @param OperationType Type of deffered operation.
  }
    procedure AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for event registration management operations
    (@code(dotEventRegister,dotEventUnregister)). Items EventParams field is
    filled.

    @param FromSocket    Socket which requested this operation.
    @param OperationType Type of deffered operation.
    @param Event         Operation parameter (event identificator).
  }
    procedure AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType; Event: scs_event_t); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for basic channel registration management operations
    (@code(dotChannelRegister,dotChannelUnregister)). Items ChannelParams field
    is filled.

    @param FromSocket    Socket which requested this operation.
    @param OperationType Type of deffered operation.
    @param Name          Operation parameter (channel name).
    @param Index         Operation parameter (channel index).
    @param ValueType     Operation parameter (channel value type).
    @param(Flags         Operation parameter (registration flags, valid only for
                         @code(dotChannelRegister)).)
  }
    procedure AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType;
                           Name: String; Index: scs_u32_t; ValueType: scs_value_type_t;
                           Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none); overload; virtual;
  {
    Adds new deffered operation to the buffer.@br
    If buffer is full, then operation at current position is dropped and
    replaced by the passed one.@br
    Calls AddItem method.

    @bold(Warning!) - Use appropriate method for different operations.@br
    This method is intended for all channel registration operation
    (@code(dotChannelRegisterAll)). Items ChannelsRegAllParams field is filled.

    @param FromSocket         Socket which requested this operation.
    @param OperationType      Type of deffered operation.
    @param(PrimaryValueType   Operation parameter (see
                              TTelemetryRecipient.ChannelRegisterAll for
                              details).)
    @param(SecondaryValueType Operation parameter (see
                              TTelemetryRecipient.ChannelRegisterAll for
                              details).)
    @param(TertiaryValueType  Operation parameter (see
                              TTelemetryRecipient.ChannelRegisterAll for
                              details).)

  }
    procedure AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType;
                           PrimaryValueType,SecondaryValueType,TertiaryValueType: Boolean); overload; virtual;
    Function PeekOperation: TDefferedOperation; virtual;
    Function RemoveOperation: Boolean; virtual;
    Function ContainsOperation: Boolean; virtual;
  end;


implementation

uses
  Windows, SysUtils,
  TelemetryConversions;

{==============================================================================}
{    TCircularItemsBuffer // Implementation                                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TCircularItemsBuffer // Private methods                                   }
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
{    TCircularItemsBuffer // Protected methods                                 }
{------------------------------------------------------------------------------}

procedure TCircularItemsBuffer.DropItem(Index: Integer);
begin
If Assigned(fOnItemDrop) then fOnItemDrop(Self,fMainArray[Index].Data);
fMainArray[Index].Filled := False;
Inc(fItemsDropped);
end;

//------------------------------------------------------------------------------

procedure TCircularItemsBuffer.InitializeItem(var Item: TArrayItem);
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
{    TCircularItemsBuffer // Public methods                                    }
{------------------------------------------------------------------------------}

constructor TCircularItemsBuffer.Create(aSize: Integer);
var
  i:  Integer;
begin
inherited Create;
If aSize < 1 then aSize := 1;
SetLength(fMainArray,aSize);
fItemsDropped := 0;
// Initialize array.
For i := Low(fMainArray) to High(fMainArray) do
  InitializeItem(fMainArray[i]);
end;

//------------------------------------------------------------------------------

destructor TCircularItemsBuffer.Destroy;
begin
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
    If Assigned(fOnItemAdd) then fOnItemAdd(Self,fMainArray[fCurrentPosition].Data);
    If (fOldestPosition = fCurrentPosition) and
      fMainArray[IndexSucc(fOldestPosition)].Filled then IndexInc(fOldestPosition);
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
    If Assigned(fOnItemRemove) then fOnItemRemove(Self,fMainArray[fOldestPosition].Data);
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



{==============================================================================}
{    TCircularPacketsBuffer // Implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TCircularPacketsBuffer // Private methods                                 }
{------------------------------------------------------------------------------}

Function TCircularPacketsBuffer.GetItemsDropped: Int64;
begin
Result := ItemsDropped;
end;

{------------------------------------------------------------------------------}
{    TCircularPacketsBuffer // Protected methods                               }
{------------------------------------------------------------------------------}

procedure TCircularPacketsBuffer.DropItem(Index: Integer);
begin
If Assigned(fOnPacketDrop) then fOnPacketDrop(Self,Socket,PPacketBuffer(fMainArray[Index].Data)^);
FreeMem(PPacketBuffer(fMainArray[Index].Data)^.Data,PPacketBuffer(fMainArray[Index].Data)^.Size);
PPacketBuffer(fMainArray[Index].Data)^.Size := 0;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularPacketsBuffer.InitializeItem(var Item: TArrayItem);
begin
inherited;
New(PPacketBuffer(Item.Data));
PPacketBuffer(Item.Data).Data := nil;
PPacketBuffer(Item.Data).Size := 0;
end;

{------------------------------------------------------------------------------}
{    TCircularPacketsBuffer // Public methods                                  }
{------------------------------------------------------------------------------}

constructor TCircularPacketsBuffer.Create(aSocket: TCustomWinSocket = nil; aSize: Integer = def_TCircularPacketsBuffer_BufferSize);
begin
inherited Create(aSize);
fSocket := aSocket;
end;

destructor TCircularPacketsBuffer.Destroy;
var
  i: Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do
  begin
    If PPacketBuffer(fMainArray[i].Data)^.Size > 0 then
      FreeMem(PPacketBuffer(fMainArray[i].Data)^.Data,
              PPacketBuffer(fMainArray[i].Data)^.Size);
    Dispose(PPacketBuffer(fMainArray[i].Data));
  end;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularPacketsBuffer.Clear;
var
  i: Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do
  If fMainArray[i].Filled then
    begin
      If PPacketBuffer(fMainArray[i].Data)^.Size > 0 then
        FreeMem(PPacketBuffer(fMainArray[i].Data)^.Data,
                PPacketBuffer(fMainArray[i].Data)^.Size);
      PPacketBuffer(fMainArray[i].Data)^.Size := 0;
      fMainArray[i].Filled := False;
    end;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularPacketsBuffer.AddPacket(Packet: TPacketBuffer; CreateCopy: Boolean = True);
var
  PacketBuffer: PPacketBuffer;
begin
PacketBuffer := fMainArray[fCurrentPosition].Data;
AddItem(PacketBuffer);
PacketBuffer^.Size := Packet.Size;
If CreateCopy then
  begin
    GetMem(PacketBuffer^.Data,PacketBuffer^.Size);
    CopyMemory(PacketBuffer^.Data,Packet.Data,PacketBuffer^.Size);
  end
else
  PacketBuffer^.Data := Packet.Data;
end;

//------------------------------------------------------------------------------

Function TCircularPacketsBuffer.PeekPacket: TPacketBuffer;
begin
Result := PPacketBuffer(PeekItem)^;
end;

//------------------------------------------------------------------------------

Function TCircularPacketsBuffer.RemovePacket: Boolean;
begin
If fMainArray[fOldestPosition].Filled then
  begin
    FreeMem(PPacketBuffer(fMainArray[fOldestPosition].Data)^.Data,
            PPacketBuffer(fMainArray[fOldestPosition].Data)^.Size);
    PPacketBuffer(fMainArray[fOldestPosition].Data)^.Size := 0;
    Result := RemoveItem;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TCircularPacketsBuffer.ContainsPacket: Boolean;
begin
Result := ContainsItem;
end;


{==============================================================================}
{    TCircularChannelsBuffer // Implementation                                 }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TCircularChannelsBuffer // Protected methods                              }
{------------------------------------------------------------------------------}

procedure TCircularChannelsBuffer.InitializeItem(var Item: TArrayItem);
begin
New(PBufferedChannel(Item.Data));
inherited;
end;

{------------------------------------------------------------------------------}
{    TCircularChannelsBuffer // Public methods                                 }
{------------------------------------------------------------------------------}

constructor TCircularChannelsBuffer.Create(aSize: Integer = def_TCircularChannelsBuffer_BufferSize);
begin
inherited Create(aSize);
end;

//------------------------------------------------------------------------------

destructor TCircularChannelsBuffer.Destroy;
var
  i:  Integer;
begin
For i := Low(fMainArray) to High(fMainArray) do
  Dispose(PBufferedChannel(fMainArray[i].Data));
inherited;
end;

//------------------------------------------------------------------------------

procedure TCircularChannelsBuffer.AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  BufferedChannel:  PBufferedChannel;
begin
BufferedChannel := fMainArray[fCurrentPosition].Data;
BufferedChannel^.Name := Name;
BufferedChannel^.ID := ID;
BufferedChannel^.Index := Index;
BufferedChannel^.Value := Value;
AddItem(BufferedChannel);
end;

procedure TCircularChannelsBuffer.AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_t);
begin
AddChannel(Name,ID,Index,scs_value_localized(Value));
end;

procedure TCircularChannelsBuffer.AddChannel(Name: String; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If Assigned(Value) then
  AddChannel(Name,ID,Index,scs_value_localized(Value^))
else
  AddChannel(Name,ID,Index,cEmptySCSValueLocalized);
end;

//------------------------------------------------------------------------------

Function TCircularChannelsBuffer.PeekChannel: TBufferedChannel;
begin
Result := PBufferedChannel(PeekItem)^;
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
{    TDefferedOperationsBuffer // Implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TDefferedOperationsBuffer // Protected methods                            }
{------------------------------------------------------------------------------}

procedure TDefferedOperationsBuffer.InitializeItem(var Item: TArrayItem);
begin
New(PDefferedOperation(Item.Data));
inherited;
end;

{------------------------------------------------------------------------------}
{    TDefferedOperationsBuffer // Public methods                               }
{------------------------------------------------------------------------------}

constructor TDefferedOperationsBuffer.Create(aSize: Integer = def_TDefferedOperationsBuffer_BufferSize);
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

procedure TDefferedOperationsBuffer.AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotNone,dotEventRegisterAll,dotEventUnregisterAll,
                     dotChannelUnregisterAll] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.FromSocket := FromSocket;
    DefferedOperation^.OperationType := OperationType;
    AddItem(DefferedOperation);
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation: Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

procedure TDefferedOperationsBuffer.AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType; Event: scs_event_t);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotEventRegister,dotEventUnregister] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.FromSocket := FromSocket;
    DefferedOperation^.OperationType := OperationType;
    DefferedOperation^.EventParams.Event := Event;
    AddItem(DefferedOperation);
  end
else
  raise Exception.Create('TDefferedOperationsBuffer.AddOperation(<Event>): Operation (' +
    IntToStr(Integer(OperationType)) + ') not supported by this version of method AddOperation');
end;

procedure TDefferedOperationsBuffer.AddOperation(FromSocket: TCustomWinSocket;
                                                 OperationType: TDefferedOperationType;
                                                 Name: String;
                                                 Index: scs_u32_t;
                                                 ValueType: scs_value_type_t;
                                                 Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotChannelRegister,dotChannelUnregister] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.FromSocket := FromSocket;
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

procedure TDefferedOperationsBuffer.AddOperation(FromSocket: TCustomWinSocket; OperationType: TDefferedOperationType;
                                                 PrimaryValueType,SecondaryValueType,TertiaryValueType: Boolean);
var
  DefferedOperation:  PDefferedOperation;
begin
If OperationType in [dotChannelRegisterAll] then
  begin
    DefferedOperation := fMainArray[fCurrentPosition].Data;
    DefferedOperation^.FromSocket := FromSocket;
    DefferedOperation^.OperationType := OperationType;
    DefferedOperation^.ChannelsRegAllParams.RegisterPrimaryTypes := PrimaryValueType;
    DefferedOperation^.ChannelsRegAllParams.RegisterSecondaryTypes := SecondaryValueType;
    DefferedOperation^.ChannelsRegAllParams.RegisterTertiaryTypes := TertiaryValueType;
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
