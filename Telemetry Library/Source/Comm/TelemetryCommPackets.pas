{@html(<hr>)
@abstract(Types definitions, constants and routines for manipulation with
          packets.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-15)
@lastmod(2014-11-25)

  @bold(@NoAutoLink(TelemetryCommPackets))

  ©František Milt, all rights reserved.

  This file provides types, routines and mainly constants used when working with
  packets. Main goal of this unit is to provide set of constants containing
  identifier numbers of individual defined packets along with documentation
  about their structure.

  Last change:  2014-11-25

  Change List:@unorderedList(
    @item(2014-05-15 - First stable version.)
    @item(2014-11-02 - Small implementation changes.)
    @item(2014-11-05 - Type of fields TPacketBuffer.Size and
                       TPacketHeader.PayloadSize changed from signed to unsigned
                       integer.)
    @item(2014-11-05 - Added type TPacketID.)
    @item(2014-11-25 - Changed structure of packet TC_PACKET_CHANNEL_REGISTER_ALL
                       due to a new system of storing and passing secondary
                       types of channel value.))

@html(<hr>)
  Communication between server and client(s) is realized using variable length
  messages, called packets.

  All packets starts with a header of this structure:
@preformatted(
      value       |   size    |   value type

    Signature       4 bytes      32b uint (must be equal to TC_PACKET_SIGNATURE)
    Packet ID       4 bytes      32b uint
    Time stamp      8 bytes      TTimeStamp
    Payload size    4 bytes      32b int
)
  Signature is used to check validity of incoming packet or to search start of
  packet in incoming stream.@br
  Packet ID is general indetificator of the packet, it determines how the packet
  should be processed.@br
  Time stamp contains time when the packet was created. It is declared as
  following structure:
@preformatted(
    value |   size   | value type

    Time    4 bytes     32b int
    Date    4 bytes     32b int
)
  ...where Time is number of milliseconds that have elapsed since midnight, and
  Date indicates the number of calendar days since the start of the calendar
  (the number of days since 1/1/0001 plus one).
  Payload size is the size of payload of the packet in bytes. It can be zero.

  After this header, an optional payload data follows.@br
  Depending on payload presence, packets are divided into two groups, basic and
  extended.@br
  Basic packets contains no data (field "Payload size" is set to 0), only
  header, and are used only as notifications or no-parameter commands.@br
  Extended packets are used mainly for data transfer or parametrized
  commands.@br
  Packet payload can contain any data and its size is limited only by available
  memory (technical maximum 2GiB).

  Structure of packet payload (see individual packet ID definitions for
  appropriate structure) is only hint for processing implemetation, not actual
  structured type. For example, any basic packet can actually contain data,
  but they are ignored and discarded during processing.@br
  All basic binary types (integers, floats, ...) are stored as little-endian.@br
  Strings in packet payload are stored using function Ptr_WriteString or its
  alternatives (refer to it for rdetails).@br
  @bold(Note) - all strings stored in packet payload are UTF8 encoded, if you
  want to store string of different encoding, use conversion functions.

  Actual packet ID number is composed from two fields, high word is ID prefix
  and low word is packet number in the prefix/usage group.@br
  Prefixes are used to distinguish packet usage group. They are also used for
  better branching of packet processing.

  @bold(Note) - this unit contains definitions only for telemetry-specific
                prefix groups and packets. Meaning anything used for example in
                network communication is declared elsewhere.

  List of defined prefixes:
@preformatted(
    TC_PREFIX_COMMON
    TC_PREFIX_KNOWN_EVENTS
    TC_PREFIX_KNOWN_CHANNELS
    TC_PREFIX_KNOWN_CONFIGS
    TC_PREFIX_EVENT
    TC_PREFIX_CHANNEL
    TC_PREFIX_CONFIG
    TC_PREFIX_LOG
)
  List of defined packet ID identifiers:
@preformatted(
    - TC_PREFIX_COMMON prefix packets -
    (b|a) TC_PACKET_PING
    (e|a) TC_PACKET_PING_RESPONSE
    (b|a) TC_PACKET_PROTOCOL_VERSION_GET
    (e|a) TC_PACKET_PROTOCOL_VERSION
    (b|c) TC_PACKET_TELEMETRY_INFO_GET
    (e|s) TC_PACKET_TELEMETRY_INFO
    (e|a) TC_PACKET_PACKET
    (e|a) TC_PACKET_ERROR
    (e|s) TC_PACKET_DEFFERED

    - TC_PREFIX_KNOWN_EVENTS prefix packets -
    (b|c) TC_PACKET_KNOWN_EVENTS_COUNT_GET
    (e|s) TC_PACKET_KNOWN_EVENTS_COUNT
    (e|c) TC_PACKET_KNOWN_EVENTS_INDEX_GET
    (e|s) TC_PACKET_KNOWN_EVENTS_INDEX
    (b|c) TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET
    (b|c) TC_PACKET_KNOWN_EVENTS_ALL_GET
    (e|s) TC_PACKET_KNOWN_EVENTS_ALL

    - TC_PREFIX_KNOWN_CHANNELS prefix packets -
    (b|c) TC_PACKET_KNOWN_CHANNELS_COUNT_GET
    (e|s) TC_PACKET_KNOWN_CHANNELS_COUNT
    (e|c) TC_PACKET_KNOWN_CHANNELS_INDEX_GET
    (e|s) TC_PACKET_KNOWN_CHANNELS_INDEX
    (b|c) TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET
    (b|c) TC_PACKET_KNOWN_CHANNELS_ALL_GET
    (e|s) TC_PACKET_KNOWN_CHANNELS_ALL

    - TC_PREFIX_KNOWN_CONFIGS prefix packets -
    (b|c) TC_PACKET_KNOWN_CONFIGS_COUNT_GET
    (e|s) TC_PACKET_KNOWN_CONFIGS_COUNT
    (e|c) TC_PACKET_KNOWN_CONFIGS_INDEX_GET
    (e|s) TC_PACKET_KNOWN_CONFIGS_INDEX
    (b|c) TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET
    (b|c) TC_PACKET_KNOWN_CONFIGS_ALL_GET
    (e|s) TC_PACKET_KNOWN_CONFIGS_ALL

    - TC_PREFIX_EVENT prefix packets -
    (e|o) TC_PACKET_EVENT_REGISTER
    (e|c) TC_PACKET_EVENT_REGISTER_BY_INDEX
    (b|c) TC_PACKET_EVENT_REGISTER_ALL
    (e|o) TC_PACKET_EVENT_UNREGISTER
    (e|c) TC_PACKET_EVENT_UNREGISTER_INDEX
    (e|c) TC_PACKET_EVENT_UNREGISTER_BY_INDEX
    (b|c) TC_PACKET_EVENT_UNREGISTER_ALL
    (e|s) TC_PACKET_EVENT_EVENT
    (e|o) TC_PACKET_EVENT_REGISTERED
    (b|c) TC_PACKET_EVENT_REGISTERED_COUNT_GET
    (e|s) TC_PACKET_EVENT_REGISTERED_COUNT
    (e|c) TC_PACKET_EVENT_REGISTERED_INDEX_GET
    (e|s) TC_PACKET_EVENT_REGISTERED_INDEX
    (b|c) TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET
    (b|c) TC_PACKET_EVENT_REGISTERED_ALL_GET
    (e|s) TC_PACKET_EVENT_REGISTERED_ALL

    - TC_PREFIX_CHANNEL prefix packets -
    (e|o) TC_PACKET_CHANNEL_REGISTER
    (e|c) TC_PACKET_CHANNEL_REGISTER_BY_INDEX
    (e|c) TC_PACKET_CHANNEL_REGISTER_BY_NAME
    (e|c) TC_PACKET_CHANNEL_REGISTER_ALL
    (e|o) TC_PACKET_CHANNEL_UNREGISTER
    (e|c) TC_PACKET_CHANNEL_UNREGISTER_INDEX    
    (e|c) TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX
    (e|c) TC_PACKET_CHANNEL_UNREGISTER_BY_NAME
    (b|c) TC_PACKET_CHANNEL_UNREGISTER_ALL
    (e|s) TC_PACKET_CHANNEL_CHANNEL
    (e|s) TC_PACKET_CHANNEL_CHANNEL_BUFFERED
    (e|o) TC_PACKET_CHANNEL_REGISTERED
    (b|c) TC_PACKET_CHANNEL_REGISTERED_COUNT_GET
    (e|s) TC_PACKET_CHANNEL_REGISTERED_COUNT
    (e|c) TC_PACKET_CHANNEL_REGISTERED_INDEX_GET
    (e|s) TC_PACKET_CHANNEL_REGISTERED_INDEX
    (b|c) TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET
    (b|c) TC_PACKET_CHANNEL_REGISTERED_ALL_GET
    (e|s) TC_PACKET_CHANNEL_REGISTERED_ALL
    (e|c) TC_PACKET_CHANNEL_STORED_SEND_ALL

    - TC_PREFIX_CONFIG prefix packets -
    (e|s) TC_PACKET_CONFIG_CONFIG
    (e|o) TC_PACKET_CONFIG_STORED
    (b|c) TC_PACKET_CONFIG_STORED_COUNT_GET
    (e|s) TC_PACKET_CONFIG_STORED_COUNT
    (e|c) TC_PACKET_CONFIG_STORED_INDEX_GET
    (e|s) TC_PACKET_CONFIG_STORED_INDEX
    (b|c) TC_PACKET_CONFIG_STORED_INDEX_ALL_GET
    (b|c) TC_PACKET_CONFIG_STORED_ALL_GET
    (e|s) TC_PACKET_CONFIG_STORED_ALL

    - TC_PREFIX_LOG prefix packets -
    (e|o) TC_PACKET_LOG_LOG
)
  Left item in brackets is denoting whether particulat packet is basic or
  extended. Right item describes which side of connection can send that packet.
@preformatted(
    b = basic packet          s = sent by server
    e = extended packet       c = sent by client
                              o = can be sent by both client and server
                              a = can be sent by any connection endpoint
)
@html(<hr>)}
unit TelemetryCommPackets;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  SysUtils,
  TelemetryCommon{$IFDEF DevelopmentHints}{$MESSAGE 'Development hint: Should be in implementation section'}{$ENDIF};

{==============================================================================}
{   Constants, types, variables, etc.                                          }
{==============================================================================}

const
  // @abstract(Signature of packet.)
  // Every packet starts with this 4 byte value. It is used to check validity of
  // incoming data and to find packet start in general data stream.
  TC_PACKET_SIGNATURE = $654E6554;

type
  // Type used for packet identifier number.
  TPacketID = LongWord;

{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: petUnknownClient and petUnknownParameter are valid only for networking.'}
{$ENDIF}
{
  Used in TC_PACKET_ERROR to distinguish nature of reported error.
  Any of these errors can be sent with additional error code, but at the
  moment, only petErrorReadingPacket is sent with it.

  @value(petUnknown            An unknown error occured.)
  @value(petGeneric            Generic error (user for errors with known cause
                               but no defined type).)
  @value(petPacketTooSmall     Received packet resolving failed because it was
                               smaller than should be by its definition.)
  @value(petErrorReadingPacket Reading on received packet failed for some
                               reason (most often returned when received packet
                               was damaged or badly build). Error code sent with
                               this error contains number of error obtained from
                               GetLastError function.)
  @value(petUnexpectedPacket   Packet that should not be received have arrived
                               (e.g. packet that has no meaning for receiver).)
  @value(petUndefinedBehaviour No action is defined for received packet. This is
                               rather warning than error.)
  @value(petUnknownPacket      Packet with ID unknown to recipient was
                               received.)
  @value(petUnknownClient      Administrator requested operation on client that
                               is not connected to the server. @exclude)
  @value(petUnknownParameter   Request for unknown parameter have been
                               received. @exclude)
}
  TPacketErrorType = (petUnknown,petGeneric,petPacketTooSmall,
                      petErrorReadingPacket,petUnexpectedPacket,
                      petUndefinedBehaviour,petUnknownPacket,
                      petUnknownClient,petUnknownParameter,
                      petWrongData);

{
  Record used in TPacketBuffer structure containing information about memory
  allocation.

  @member(CanBeFreed Indicates whether this packet's memory can be freed
                     momentarily.)
}
  TPacketBufferAllocInfo = record
    CanBeFreed: Boolean;
  end;

{
  Record used for manipulation with packets (passing, saving, ...), or more
  precisely with their memory images.

  @member(Data      Pointer to actual packet memory image.)
  @member(Size      @noAutoLink(Size) of packet memory image in bytes.)
  @member(AllocInfo Memory allocation information. Do not access this field.)
}
  TPacketBuffer = record
    Data:       Pointer;
    Size:       LongWord;
    AllocInfo:  TPacketBufferAllocInfo;
  end;
  // Pointer to TPacketBuffer structure.
  PPacketBuffer = ^TPacketBuffer;

{
  Event type used where there is possibility or need to pass packet for further
  processing.
}
  TPacketNotifyEvent = procedure(Sender: TObject; var Packet: TPacketBuffer; ConnectionData: Pointer) of object;

{
  Type used as a timestamp in packets headers.
}
  TPacketHeaderTime = TTimeStamp;

{
  @abstract(Packet header structure.)
  Used mainly to help gettig packet header from and to general buffers and in
  packets building and resolving.

  @member(Signature   Must be equal to TC_PACKET_SIGNATURE.)
  @member(PacketID    Identification number of the packet.)
  @member(TimeStamp   Local time when the packet was created.)
  @member(PayloadSize Size of following payload in bytes. Can be 0.)
}
  TPacketHeader = packed record
    Signature:    LongWord;
    PacketID:     TPacketID;
    TimeStamp:    TPacketHeaderTime;
    PayloadSize:  LongWord;
  end;
  // Pointer to TPacketHeader structure.
  PPacketHeader = ^TPacketHeader;

const
{
  Minimum size of packet.@br
  Used to check if incoming data are valid. Size of every packet must be
  equal to or greater than this value.
}
  TC_MIN_PACKET_SIZE = SizeOf(TPacketHeader); {20 bytes}


{------------------------------------------------------------------------------}
{   Packets ID prefixes                                                        }
{------------------------------------------------------------------------------}

  // General communication packet group.
  TC_PREFIX_COMMON         = $00000000;

  // Packets used to get known events from server.
  TC_PREFIX_KNOWN_EVENTS   = $00010000;

  // Packets used to get known channels from server.
  TC_PREFIX_KNOWN_CHANNELS = $00020000;

  // Packets used to get known configs from server.
  TC_PREFIX_KNOWN_CONFIGS  = $00030000;

  // Packets used to get list of registered events, send events, (un)register
  // events, etc..
  TC_PREFIX_EVENT          = $00040000;

  // Packets used to get list of registered channels, send channels,
  // (un)register channels, etc..
  TC_PREFIX_CHANNEL        = $00050000;

  // Packets used to inform about change in stored config values and managing
  // this list.
  TC_PREFIX_CONFIG         = $00060000;

  // Packets used to inform about or request game log writes.  
  TC_PREFIX_LOG            = $00070000;


{==============================================================================}
{   Definintion of packet ID numbers and their structures.                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   General communication packets                                              }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_PING)) packet ID.)

  Sender: Any@br
  Packet: Basic

  Can be used to check connectivity or responsiveness of other side of the
  connection.
}
  TC_PACKET_PING = TC_PREFIX_COMMON or $0000;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_PING_RESPONSE)) packet ID.)

  Sender: Any@br
  Packet: Extended

  Payload structure:
@preformatted(
      value   |   size   |     value type    |  meaning

    Timestamp   8 bytes    TPacketHeaderTime    Time stamp of received TC_PACKET_PING packet
)

  Sent as immediate response to TC_PACKET_PING packet. Payload contains
  timestamp of TC_PACKET_PING that caused response with this packet.
}
  TC_PACKET_PING_RESPONSE = TC_PREFIX_COMMON or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_PROTOCOL_VERSION_GET)) packet ID.)

  Sender: Any@br
  Packet: Basic

  Instructs other side to send version of communication protocol it is running.
}
  TC_PACKET_PROTOCOL_VERSION_GET = TC_PREFIX_COMMON or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_PROTOCOL_VERSION)) packet ID.)

  Sender: Any@br
  Packet: Extended

  Payload structure:
@preformatted(
          value       |   size   |    value type    |  meaning

    Protocol version    4 bytes    TProtocolVersion   Version of protocol the server runs on
)

  Sent as immediate response to TC_PACKET_PROTOCOL_VERSION_GET packet.@br
  Contains version of protocol the peer is using. When client does not support
  given protocol version, it must immediately disconnect. When server receives
  this packet from client and it contains version that server cannot support,
  then it must force-disconnect such client.
}
  TC_PACKET_PROTOCOL_VERSION = TC_PREFIX_COMMON or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_TELEMETRY_INFO_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send information about running telemetry recipient.@br
  It can be sent at any time, but is usually sent as immediate response to
  TC_PACKET_PROTOCOL_VERSION packet (if client stays connected, i.e. supports
  protocol version from TC_PACKET_PROTOCOL_VERSION).
}
  TC_PACKET_TELEMETRY_INFO_GET = TC_PREFIX_COMMON or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_TELEMETRY_INFO)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
          value       |   size   |  value type  |  meaning

    Telemetry version   4 bytes     scs_u32_t     Version of telemetry API
    Game ID             variable    String        ID of running game
    Game version        4 bytes     scs_u32_t     API-specific version of game
    Game name           variable    String        Name of the running game
)

  Sent as immediate response to TC_PACKET_TELEMETRY_INFO_GET.@br
  Contains information about running telemetry recipient.@br
  Client can react to these values (e.g. when it cannot work with given
  telemetry version and disconnects itself), but it is not mandatory.
}
  TC_PACKET_TELEMETRY_INFO = TC_PREFIX_COMMON or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_PACKET)) packet ID.)

  Sender: Any@br
  Packet: Extended

  Payload structure:
@preformatted(
        value    |   size   |   value type   |  meaning

    Payload data   variable    not specified   Data carried by packet, can be empty
)

  Used to send unspecified data. Size of payload data can be obtained from
  @link(TPacketHeader.PayloadSize PayloadSize) field of packet header.
}
  TC_PACKET_PACKET = TC_PREFIX_COMMON or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_ERROR)) packet ID.)

  Sender: Any@br
  Packet: Extended

  Payload structure:
@preformatted(
          value      |   size   |     value type     |  meaning

    Packet ID          4 bytes    32b uint             ID of erroneous packet
    Packet timestamp   8 bytes    TPacketHeaderTime    Timestamp of erroneous packet
    Error type         4 bytes    TPacketErrorType     Error type
    Error code         4 bytes    32b uint             System error code
)

  Sent back when recipient receives erroneous packet. Fields @code(Packet ID)
  and @code(Packet timestamp) contains ID and timestamp of packet that was
  evaluated as broken. @code(Error type) contains identifier denoting the nature
  of error. Field @code(Error code) contains system error code for last error -
  it is set only for selected error types, for other types, it is set to 0.
}
  TC_PACKET_ERROR = TC_PREFIX_COMMON or $0007;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_DEFFERED)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
          value      |   size   |        value type        |  meaning

    Packet ID          4 bytes    32b uint                  ID of commanding packet
    Packet timestamp   8 bytes    TPacketHeaderTime         Timestamp of commanding packet
    Operation type     4 bytes    TDefferedOperationType    Kind of deffered operation (eg. chanel registration)
)

  Sent back when server must deffer received command.@br
  Because some commands cannot be executed immediately when they are received,
  server can deffer them for later execution. When this situation arises, this
  packet is sent back as a response to a packet that ordered such command. It
  serves only as a notification obout this deffering.@br
  @code(Packet ID) and @code(Packet tiemstamp) are taken from packet that
  ordered command that got deffered. Operation type indicates type of operation
  that got deffered. See TDefferedOperationType type for possible values.
}
  TC_PACKET_DEFFERED = TC_PREFIX_COMMON or $0008;



{------------------------------------------------------------------------------}
{   Known events packets                                                       }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_COUNT_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known events.
}
  TC_PACKET_KNOWN_EVENTS_COUNT_GET = TC_PREFIX_KNOWN_EVENTS or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of known events
)

  Immediate response to TC_PACKET_KNOWN_EVENTS_COUNT_GET packet.@br
  Contains number of known events.
}
  TC_PACKET_KNOWN_EVENTS_COUNT = TC_PREFIX_KNOWN_EVENTS or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_INDEX_GET)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested known event
)

  Instructs server to send known event on position given by @code(Index).
}
  TC_PACKET_KNOWN_EVENTS_INDEX_GET = TC_PREFIX_KNOWN_EVENTS or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value     |   size   |  value type  |  meaning

    Index       4 bytes    32b int        Index of requested known event
    Event       4 bytes    scs_event_t
    Name        variable   String
    Valid       1 byte     Boolean
    Utility     1 byte     Boolean
)

  Immediate response to TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.@br
  Contains informations about known event on position given by @code(Index).@br
  If @code(Index) in TC_PACKET_KNOWN_EVENTS_INDEX_GET packet pointed to invalid
  position, then @code(Index) in this packet is set to -1 and other fields
  (@code(Event), @code(Name), ...) are set to 0/@false/empty string.@br
  Fields after @code(Index) are saved using function Ptr_Write_KnownEvent.
}
  TC_PACKET_KNOWN_EVENTS_INDEX = TC_PREFIX_KNOWN_EVENTS or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known events. They are immediately sent as
  a stream of TC_PACKET_KNOWN_EVENTS_INDEX packets.
}
  TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET = TC_PREFIX_KNOWN_EVENTS or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known events. They are all sent in one
  TC_PACKET_KNOWN_EVENTS_ALL packet (see below).
}
  TC_PACKET_KNOWN_EVENTS_ALL_GET = TC_PREFIX_KNOWN_EVENTS or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_EVENTS_ALL)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |         value type         |  meaning

    Count     4 bytes    32b int                      Number of Info substructures
    Info[]    vyriable   array of Info substructures

  Info substructure:
  
    Event     4 bytes    scs_event_t
    Name      variable   String
    Valid     1 byte     Boolean
    Utility   1 byte     Boolean
)

  Immediate response to TC_PACKET_KNOWN_EVENTS_ALL packet.@br
  Contains array of known events informations.@br
  Info substructures are saved using function Ptr_Write_KnownEvent.
}
  TC_PACKET_KNOWN_EVENTS_ALL = TC_PREFIX_KNOWN_EVENTS or $0007;


{------------------------------------------------------------------------------}
{   Known channels packets                                                     }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_COUNT_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known channels.
}
  TC_PACKET_KNOWN_CHANNELS_COUNT_GET = TC_PREFIX_KNOWN_CHANNELS or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of known channels
)

  Immediate response to TC_PACKET_KNOWN_CHANNELS_COUNT_GET packet.@br
  Contains number of known channels.
}
  TC_PACKET_KNOWN_CHANNELS_COUNT = TC_PREFIX_KNOWN_CHANNELS or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_INDEX_GET)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested known channel
)

  Instructs server to send known channel on position given by @code(Index).
}
  TC_PACKET_KNOWN_CHANNELS_INDEX_GET = TC_PREFIX_KNOWN_CHANNELS or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
        value       |   size   |    value type    |  meaning

    Index             4 bytes    32b int            Index of requested known channel
    Name              variable   String
    ID                4 bytes    TChannelID
    Primary type      4 bytes    scs_value_type_t
    Secondary type    4 bytes    scs_value_type_t
    Tertiary type     4 bytes    scs_value_type_t
    Indexed           1 byte     Boolean
    Index config      variable   String
    Index config id   4 bytes    TItemID
    Maximum index     4 bytes    scs_u32_t
)

  Immediate response to TC_PACKET_KNOWN_CHANNELS_INDEX_GET packet.@br
  Contains informations about known channel on position given by
  @code(Index).@br
  If @code(Index) in TC_PACKET_KNOWN_CHANNELS_INDEX_GET pointed to invalid
  position, then @code(Index) in this packet is set to -1 and other fields
  (@code(Name), etc.) are set to 0/@false/empty string.@br
  Fields after @code(Index) are saved using function Ptr_Write_KnownChannel.
}
  TC_PACKET_KNOWN_CHANNELS_INDEX = TC_PREFIX_KNOWN_CHANNELS or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET)) packet
            ID.)
  
  Sender: Client@br
  Packet: Basic

  Instructs server to send all known channels. They are immediately sent as
  a stream of TC_PACKET_KNOWN_CHANNELS_INDEX packets.
}
  TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET = TC_PREFIX_KNOWN_CHANNELS or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known channels. They are all sent in one
  TC_PACKET_KNOWN_CHANNELS_ALL packet (see below).
}
  TC_PACKET_KNOWN_CHANNELS_ALL_GET = TC_PREFIX_KNOWN_CHANNELS or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CHANNELS_ALL)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
         value      |   size   |         value type         |  meaning

    Count             4 bytes    32b int                      Number of Info substructures
    Info[]            variable   array of Info substructures

  Info substructure:
  
    Name              variable   String
    ID                4 bytes    TChannelID
    Primary type      4 bytes    scs_value_type_t
    Secondary type    4 bytes    scs_value_type_t
    Tertiary type     4 bytes    scs_value_type_t
    Indexed:          1 byte     Boolean
    Index config      variable   String
    Index config id   4 bytes    TItemID
    Maximum index     4 bytes    scs_u32_t
)

  Immediate response to TC_PACKET_KNOWN_CHANNELS_ALL packet.@br
  Contains array of known channels informations.@br
  Info substructures are saved using function Ptr_Write_KnownChannel.
}
  TC_PACKET_KNOWN_CHANNELS_ALL = TC_PREFIX_KNOWN_CHANNELS or $0007;


{------------------------------------------------------------------------------}
{   Known configs packets                                                      }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_COUNT_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known configs.
}
  TC_PACKET_KNOWN_CONFIGS_COUNT_GET = TC_PREFIX_KNOWN_CONFIGS or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of known configs
)

  Immediate response to TC_PACKET_KNOWN_CONFIGS_COUNT_GET packet.@br
  Contains number of known configs.
}
  TC_PACKET_KNOWN_CONFIGS_COUNT = TC_PREFIX_KNOWN_CONFIGS or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_INDEX_GET)) packet ID.)

  Sender: Client@br
  Packet: Extended
  
  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested known channel
)

  Instructs server to send known config on position given by @code(Index).
}
  TC_PACKET_KNOWN_CONFIGS_INDEX_GET = TC_PREFIX_KNOWN_CONFIGS or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
       value    |   size   |    value type    |  meaning

    Index         4 bytes    32b int            Index of requested known channel
    Name          variable   String
    ID            4 bytes    TConfigID
    Value type    4 bytes    scs_value_type_t
    Indexed       1 byte     Boolean
    Binded        1 byte     Boolean
)

  Immediate response to TC_PACKET_KNOWN_CONFIGS_INDEX_GET packet.@br
  Contains informations about known config on position given by @code(Index).@br
  If @code(Index) in TC_PACKET_KNOWN_CONFIGS_INDEX_GET pointed to invalid
  position, then @code(Index) in this packet is set to -1 and other fields
  (@code(Name), etc.) are set to 0/@false/empty string.@br
  Fields after @code(Index) are saved using function Ptr_Write_KnownConfig.
}
  TC_PACKET_KNOWN_CONFIGS_INDEX = TC_PREFIX_KNOWN_CONFIGS or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET)) packet
            ID.)
  
  Sender: Client@br
  Packet: Basic

  Instructs server to send all known configs. They are immediately sent as
  a stream of TC_PACKET_KNOWN_CONFIGS_INDEX packets.
}
  TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET = TC_PREFIX_KNOWN_CONFIGS or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known configs. They are all sent in one
  TC_PACKET_KNOWN_CONFIGS_ALL packet (see below).
}
  TC_PACKET_KNOWN_CONFIGS_ALL_GET = TC_PREFIX_KNOWN_CONFIGS or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_KNOWN_CONFIGS_ALL)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
       value    |   size   |         value type         |  meaning

    Count         4 bytes    32b int                      Number of Info substructures
    Info[]        variable   array of Info substructures

  Info substructure:

    Name          variable   String
    ID            4 bytes    TConfigID
    Value type    4 bytes    scs_value_type_t
    Indexed       1 byte     Boolean
    Binded        1 byte     Boolean
)

  Immediate response to TC_PACKET_KNOWN_CONFIGS_ALL packet.@br
  Contains array of known configs informations.@br
  Info substructures are saved using function Ptr_Write_KnownConfig.
}
  TC_PACKET_KNOWN_CONFIGS_ALL = TC_PREFIX_KNOWN_CONFIGS or $0007;


{------------------------------------------------------------------------------}
{   Events packets                                                             }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTER)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   |  value type  |  meaning

    Event     4 bytes    scs_event_t    Event identifier number
    Result    4 bytes    scs_result_t   Registration result
)

  Server:@br
  Sent to all clients when given event is succesfuly registered (@code(Result)
  field is set to @code(SCS_RESULT_ok)).@br
  If registration of requested event fails, server send this packet back only to
  registering client with @code(Result) set to result code from registration.

  Client:@br
  Instructs server to register a particular event.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_EVENT_REGISTER = TC_PREFIX_EVENT or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTER_BY_INDEX)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   | value type |  meaning

    Index     4 bytes     32b int     Index of requested event in list of known events
)

  Instructs server to register known event stored in list of known events on
  position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_EVENT_REGISTER_BY_INDEX = TC_PREFIX_EVENT or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTER_ALL)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to register all known events.
}
  TC_PACKET_EVENT_REGISTER_ALL = TC_PREFIX_EVENT or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_UNREGISTER)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   |  value type  |  meaning

    Event     4 bytes    scs_event_t    Event identifier number
    Result    4 bytes    scs_result_t   Unregistration result
)

  Server:@br
  Sent to all clients when given event is succesfuly unregistered (@code(Result)
  field set to @code(SCS_RESULT_ok)).@br
  If unregistration of event fails, server send this packet back only to
  unregistering client with @code(Result) field set to result code from
  unregistration.

  Client:@br
  Instructs server to unregister a particular event.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_EVENT_UNREGISTER = TC_PREFIX_EVENT or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_UNREGISTER_INDEX)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   | value type |  meaning

    Index     4 bytes     32b int     Index of requested event in list of registered events
)

  Instructs server to unregister event stored in list of registered events on
  position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_EVENT_UNREGISTER_INDEX = TC_PREFIX_EVENT or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_UNREGISTER_BY_INDEX)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   | value type |  meaning

    Index     4 bytes     32b int     Index of requested event in list of known events
)

  Instructs server to unregister known event stored in list of known events on
  position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_EVENT_UNREGISTER_BY_INDEX = TC_PREFIX_EVENT or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_UNREGISTER_ALL)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to unregister all known events.
}
  TC_PACKET_EVENT_UNREGISTER_ALL = TC_PREFIX_EVENT or $0007;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_EVENT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   |  value type  |  meaning

    Event   4 bytes    scs_event_t    Event identifier number
    Data    variable                  Event data, can be empty
)

  Sent to all clients on all game avents occurences. @code(Data) field content
  depends on event type, and can be empty.

  Content of @code(Data) for individual events:
  @unorderedList(
    @item(@code(SCS_TELEMETRY_EVENT_frame_start) - @code(Data) contains one
          @code(scs_telemetry_frame_start_t) structure.)
    @item(@code(SCS_TELEMETRY_EVENT_configuration) - @code(Data) are stored
          using function Ptr_Write_scs_telemetry_configuration, refer to it for
          details (parameter @code(Minimize) is set to @true and parameter
          @code(ItemIDOnly) is set to @false).)
    @item(At the moment, all other events should have no data attached.))
}
  TC_PACKET_EVENT_EVENT = TC_PREFIX_EVENT or $0008;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
      value    |   size   |  value type  |  meaning

    Event        4 bytes    scs_event_t    Event identifier number
    Registered   1 bytes    Boolean        Result of check
)

  Server:@br
  Sent back to instructing client. Field @code(Registered) contains @true when
  given event is registered, otherwise @false.

  Client:@br
  Instructs server to check whether given event is registered.@br
  @code(Registered) field can have any value.
}
  TC_PACKET_EVENT_REGISTERED = TC_PREFIX_EVENT or $0009;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_COUNT_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of registered events.
}
  TC_PACKET_EVENT_REGISTERED_COUNT_GET = TC_PREFIX_EVENT or $000A;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of registered events
)

  Immediate response to TC_PACKET_EVENT_REGISTERED_COUNT_GET packet.@br
  Contains number of registered events.
}
  TC_PACKET_EVENT_REGISTERED_COUNT = TC_PREFIX_EVENT or $000B;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_INDEX_GET)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested registered event
)

  Instructs server to send informations about registered event on list position
  given by @code(Index).
}
  TC_PACKET_EVENT_REGISTERED_INDEX_GET = TC_PREFIX_EVENT or $000C;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value    |   size   |  value type  |  meaning

    Index      4 bytes    32b int        Index of registered event
    Event      4 bytes    scs_event_t
    Utility    1 byte     Boolean
)

  Immediate response to TC_PACKET_EVENT_REGISTERED_INDEX_GET packet.@br
  Contains information about registered event on position given by
  @code(Index).@br
  If @code(Index) in TC_PACKET_EVENT_REGISTERED_INDEX_GET pointed to invalid
  position, then @code(Index) in this packet is set to -1, field @code(Event) is
  set to @code(SCS_TELEMETRY_EVENT_invalid) (0) and @code(Utility) to @false.@br
  Fields after @code(Index) are saved using function Ptr_Write_EventInfo.
}
  TC_PACKET_EVENT_REGISTERED_INDEX = TC_PREFIX_EVENT or $000D;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET)) packet
            ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send informations about all registered events. They are
  immediately sent as a stream of TC_PACKET_EVENT_REGISTERED_INDEX packets.
}
  TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET = TC_PREFIX_EVENT or $000E;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send informations about all registered events. They are
  all sent in one TC_PACKET_EVENT_REGISTERED_ALL packet (see below).
}
  TC_PACKET_EVENT_REGISTERED_ALL_GET = TC_PREFIX_EVENT or $000F;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_EVENT_REGISTERED_ALL)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   |         value type         |  meaning

    Count     4 bytes    32b int                      Number of Info substructures
    Info[]    variable   array of Info substructures

  Info substructure:

    Event     4 bytes    scs_event_t
    Utility   1 byte     Boolean
)

  Immediate response to TC_PACKET_EVENT_REGISTERED_ALL_GET packet.@br
  Contains array of registered events.@br
  Info substructures are saved using function Ptr_Write_EventInfo.
}
  TC_PACKET_EVENT_REGISTERED_ALL = TC_PREFIX_EVENT or $0010;


{------------------------------------------------------------------------------}
{   Channels packets                                                           }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTER)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
       value   |   size   |    value type     |  meaning

    Name         variable    String             Channel name
    Index        4 bytes     scs_u32_t          Channel index
    Value type   4 bytes     scs_value_type_t   Channel value type
    Flags        4 bytes     scs_u32_t          Registration flags
    Result       4 bytes     scs_result_t       Registration result
)

  Server:@br
  Sent to all clients when given channel is succesfuly registered (@code(Result)
  field set to @code(SCS_RESULT_ok)).@br
  If registration of channel fails, server send this packet back only to
  registering client with @code(Result) field set to result code from
  registration.

  Client:@br
  Instructs server to register a particular channel.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_REGISTER = TC_PREFIX_CHANNEL or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTER_BY_INDEX)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |  value type  |  meaning

    Index     4 bytes      32b int      Index of requested channel in list of known channels
)

  Instructs server to register known channel stored in list of known channels on
  position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_REGISTER_BY_INDEX = TC_PREFIX_CHANNEL or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTER_BY_NAME)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |  value type  |  meaning

    Name      variable      String      Name of requested channel
)

  Instructs server to register channel of given name.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_REGISTER_BY_NAME = TC_PREFIX_CHANNEL or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTER_ALL)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
        value       |   size   |  value type  |  meaning

    Primary type      1 byte     Boolean
    Secondary types   4 byte     32b uint
)

  Instructs server to register all known channels.@br
  For parameters explanation, refer to TTelemetryRecipient.ChannelRegisterAll
  method.
}
  TC_PACKET_CHANNEL_REGISTER_ALL = TC_PREFIX_CHANNEL or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_UNREGISTER)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
       value   |   size   |    value type     |  meaning

    Name         variable    String             Channel name
    Index        4 bytes     scs_u32_t          Channel index
    Value type   4 bytes     scs_value_type_t   Channel value type
    Result       4 bytes     scs_result_t       Unregistration result
)

  Server:@br
  Sent to all clients when given channel is succesfuly unregistered
  (@code(Result) field set to @code(SCS_RESULT_ok)).@br
  If unregistration of channel fails, server send this packet back only to
  unregistering client with @code(Result) field set to result code from
  unregistration.

  Client:@br
  Instructs server to unregister a particular channel.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_UNREGISTER = TC_PREFIX_CHANNEL or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_UNREGISTER_INDEX)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |  value type  |  meaning

    Index     4 bytes      32b int      Index of requested channel in list of registered channels
)

  Instructs server to unregister channel stored in list of registered channels
  on position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_UNREGISTER_INDEX = TC_PREFIX_CHANNEL or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX)) packet
            ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |  value type  |  meaning

    Index     4 bytes      32b int      Index of requested channel in list of known channels
)

  Instructs server to unregister known channel stored in list of known channels
  on position given by @code(Index).@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX = TC_PREFIX_CHANNEL or $0007;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_UNREGISTER_BY_NAME)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value   |   size   |  value type  |  meaning

    Name      variable      String       Name of requested channel
)

  Instructs server to unregister channel of given name.@br
  @code(Result) field should be set to @code(SCS_RESULT_ok).
}
  TC_PACKET_CHANNEL_UNREGISTER_BY_NAME = TC_PREFIX_CHANNEL or $0008;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_UNREGISTER_ALL)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to unregister all known channels.
}
  TC_PACKET_CHANNEL_UNREGISTER_ALL = TC_PREFIX_CHANNEL or $0009;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_CHANNEL)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value  |    size    |  value type  |  meaning
    Name      variable    String         Name of occuring channel
    ID        4 bytes     TChannelID     ID of occuring channel
    Index     4 bytes     scs_u32_t      Index of occuring channel
    Value     variable                   Actual value
)

  Sent to all clients on every game channel callback. @code(Value) field is
  saved using function Ptr_Write_scs_value or its alternatives and is never
  empty.
}
  TC_PACKET_CHANNEL_CHANNEL = TC_PREFIX_CHANNEL or $000A;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_CHANNEL_BUFFERED)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
      value   |   size   |         value type         |  meaning

    Count       4 bytes    32b int                      Number of Info substructures
    Info[]      variable   array of Info substructures

  Info substructure:
    Name        variable   String
    ID          4 bytes    TChannelID
    Index       4 bytes    scs_u32_t
    Value       variable
)

  Sent to all clients on @code(SCS_TELEMETRY_EVENT_frame_end) event. Contains
  all values of all buffered channels.@br
  This packet is sent only when server is set to buffer channels. And if so,
  @code(TC_PACKET_CHANNEL_CHANNEL) packets are not sent.@br
  @code(Value) fields are saved using function Ptr_Write_scs_value or its
  alternatives and is never empty.
}
  TC_PACKET_CHANNEL_CHANNEL_BUFFERED = TC_PREFIX_CHANNEL or $000B;

//------------------------------------------------------------------------------

{         
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
      value     |   size   |    value type    |  meaning

    Name          variable   String             Name of the channel
    Index         4 bytes    scs_u32_t          Index of the channel
    Value type    4 bytes    scs_value_type_t   Value type of the channel
    Registered    1 byte     Boolean            Result of check
)

  Server:@br
  Sent back to instructing client. Field @code(Registered) contains @true when
  given channel is registered, @false otherwise.

  Client:@br
  Instructs server to check whether given channel is registered.@br
  @code(Registered) field can have any value.
}
  TC_PACKET_CHANNEL_REGISTERED = TC_PREFIX_CHANNEL or $000C;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_COUNT_GET)) packet
            ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of registered channels.
}
  TC_PACKET_CHANNEL_REGISTERED_COUNT_GET = TC_PREFIX_CHANNEL or $000D;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of registered channels
)

  Immediate response to TC_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.@br
  Contains number of registered channels.
}
  TC_PACKET_CHANNEL_REGISTERED_COUNT = TC_PREFIX_CHANNEL or $000E;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_INDEX_GET)) packet
            ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested registered channel
)

  Instructs server to send registered channel on position given by @code(Index).
}
  TC_PACKET_CHANNEL_REGISTERED_INDEX_GET = TC_PREFIX_CHANNEL or $000F;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
        value       |   size   |    value type     |  meaning

    List index        4 bytes    32b int             Index of channel in list of registered channels
    Name              variable   String
    ID                4 bytes    TChannelID
    Index             4 bytes    scs_u32_t
    Value type        4 bytes    scs_value_type_t
    Flags             4 bytes    scs_u32_t
    Index config ID   4 bytes    TItemID
)

  Immediate response to TC_PACKET_CHANNEL_REGISTERED_INDEX_GET packet .@br
  Contains informations about registered channel on position given by
  @code(Index).@br
  If @code(Index) in TC_PACKET_CHANNEL_REGISTERED_INDEX_GET pointed to invalid
  position, then @code(List index) in this packet is set to -1, field
  @code(Name) contains an empty string, @code(ID) is set to 0, @code(Index) is
  set to SCS_U32_NIL, @code(Value type) is set to @code(SCS_VALUE_TYPE_INVALID),
  @code(Flags) is set to @code(SCS_TELEMETRY_CHANNEL_FLAG_none) and
  @code(Index config ID) to 0.@br
  Fields after @code(List index) are saved using function Ptr_Write_ChannelInfo.
}
  TC_PACKET_CHANNEL_REGISTERED_INDEX = TC_PREFIX_CHANNEL or $0010;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET))
            packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all registered events. They are immediately sent as
  a stream of TC_PACKET_CHANNEL_REGISTERED_INDEX packets.
}
  TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET = TC_PREFIX_CHANNEL or $0011;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all registered channels. They are all sent in one
  TC_PACKET_CHANNEL_REGISTERED_ALL packet (see below).
}
  TC_PACKET_CHANNEL_REGISTERED_ALL_GET = TC_PREFIX_CHANNEL or $0012;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_REGISTERED_ALL)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
         value      |   size   |         value type         |  meaning

    Count             4 bytes    32b int                      Number of Info substructures
    Info[]            variable   array of Info substructures

  Info substructure:
    Name              variable   String
    ID                4 bytes    TChannelID
    Index             4 bytes    scs_u32_t
    Value type        4 bytes    scs_value_type_t
    Flags             4 bytes    scs_u32_t
    Index config ID   4 bytes    TItemID
)

  Immediate response to TC_PACKET_CHANNEL_REGISTERED_ALL_GET packet.@br
  Contains array of registered channels informations.@br
  Info substructures are saved using function Ptr_Write_ChannelInfo.
}
  TC_PACKET_CHANNEL_REGISTERED_ALL = TC_PREFIX_CHANNEL or $0013;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CHANNEL_STORED_SEND_ALL)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all channel values stored in recipient. They are sent
  as a stream of TC_PACKET_CHANNEL_CHANNEL packets.
}
  TC_PACKET_CHANNEL_STORED_SEND_ALL = TC_PREFIX_CHANNEL or $0014;


{------------------------------------------------------------------------------}
{   Configs packets                                                            }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_CONFIG)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value  |   size   |    value type    |  meaning

    Name     variable   String
    ID       4 bytes    TConfigID
    Index    4 bytes    scs_u32_t
    Value    variable
)

  Sent to all clients when value of stored config changes or a new config is
  stored. @code(Value) field is stored using function
  Ptr_Write_scs_value_localized and is never empty.
}
  TC_PACKET_CONFIG_CONFIG = TC_PREFIX_CONFIG or $0001;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value  |   size   |  value type  |  meaning
     
    Name      variable   String         Name of the requested config
    Index     4 bytes    scs_u32_t      Index of the requested config
    Stored    1 byte     Boolean        Result of check
)

  Server:@br
  Sent back to instructing client. Field @code(Stored) contains @true when given
  config is stored, @false otherwise.

  Client:@br
  Instructs server to check whether given config is stored.@br
  @code(Stored) field can have any value.
}
  TC_PACKET_CONFIG_STORED = TC_PREFIX_CONFIG or $0002;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_COUNT_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of stored configs.
}
  TC_PACKET_CONFIG_STORED_COUNT_GET = TC_PREFIX_CONFIG or $0003;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_COUNT)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Count   4 bytes     32b int     Number of stored configs
)

  Immediate response to TC_PACKET_CONFIG_STORED_COUNT_GET packet.@br
  Contains number of stored configs.
}
  TC_PACKET_CONFIG_STORED_COUNT = TC_PREFIX_CONFIG or $0004;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_INDEX_GET)) packet ID.)

  Sender: Client@br
  Packet: Extended

  Payload structure:
@preformatted(
    value |   size   | value type |  meaning

    Index   4 bytes     32b int     Index of requested stored config
)

  Instructs server to send config stored on position given by @code(Index).
}
  TC_PACKET_CONFIG_STORED_INDEX_GET = TC_PREFIX_CONFIG or $0005;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_INDEX)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
       value    |   size   |    value type    |  meaning

    List index    4 bytes    32b int            Index of config in list of stored config values
    Name          variable   String
    ID            4 bytes    TConfigID
    Index         4 bytes    scs_u32_t
    Value         variable
    Binded        1 byte     Boolean
)

  Immediate response to TC_PACKET_CONFIG_STORED_INDEX_GET packet.@br
  Contains informations about and actual value of stored config on position
  given by @code(Index) (in TC_PACKET_CONFIG_STORED_INDEX_GET packet).@br
  If @code(Index) in TC_PACKET_CONFIG_STORED_INDEX_GET pointed to invalid
  position, then @code(List index) in this packet is set to -1, field
  @code(Name) contains an empty string, @code(ID) is set to 0, @code(Index) is
  set to @code(SCS_U32_NIL), @code(Value) contains cEmptyStoredConfig and
  @code(Binded) is @false.
  Fields after @code(List index) are saved using function
  Ptr_Write_StoredConfig.
}
  TC_PACKET_CONFIG_STORED_INDEX = TC_PREFIX_CONFIG or $0006;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_INDEX_ALL_GET)) packet
            ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all stored configs. They are immediately sent as
  a stream of TC_PACKET_CONFIG_STORED_INDEX packets.
}
  TC_PACKET_CONFIG_STORED_INDEX_ALL_GET = TC_PREFIX_CONFIG or $0007;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_ALL_GET)) packet ID.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all stored configs. They are all sent in one
  TC_PACKET_CONFIG_STORED_ALL packet (see below).
}
  TC_PACKET_CONFIG_STORED_ALL_GET = TC_PREFIX_CONFIG or $0008;

//------------------------------------------------------------------------------

{
  @abstract(@noAutoLink(@code(TC_PACKET_CONFIG_STORED_ALL)) packet ID.)

  Sender: Server@br
  Packet: Extended

  Payload structure:
@preformatted(
       value    |   size   |         value type         |  meaning

    Count         4 bytes    32b int                      Number of Info substructures
    Info[]        variable   array of Info substructures

  Info substructure:

    Name          variable   String
    ID            4 bytes    TConfigID
    Index         4 bytes    scs_u32_t
    Value         variable
    Binded        1 byte     Boolean
)

  Immediate response to TC_PACKET_CONFIG_STORED_ALL_GET packet.@br
  Contains array of stored configs values.@br
  Info substructures are saved using function Ptr_Write_StoredChannel.
}
  TC_PACKET_CONFIG_STORED_ALL = TC_PREFIX_CONFIG or $0009;


{------------------------------------------------------------------------------}
{    Game log packets                                                          }
{------------------------------------------------------------------------------}

{
  @abstract(@noAutoLink(@code(TC_PACKET_LOG_LOG)) packet ID.)

  Sender: Server, Client@br
  Packet: Extended

  Payload structure:
@preformatted(
     value   |   size   |   value type   |  meaning

    Log type   4 bytes    scs_log_type_t   Type of log
    Log text   variable   String           Logged text
)

  Server:@br
  Sent to all clients when recipient tries to write to game log.

  Client: @br
  Instructs server to write to game log using telemetry recipient.
}
  TC_PACKET_LOG_LOG = TC_PREFIX_LOG or $0001;

{==============================================================================}
{   Functions declarations                                                     }
{==============================================================================}

{
  Function extracting prefix number from packet ID number.

  @param PacketID Packet ID from which the prefix will be extracted.

  @returns Prefix number of packet ID.
}
Function GetPacketIDPrefix(PacketID: LongWord): LongWord;

//------------------------------------------------------------------------------

{
  @abstract(Function used to obtain packet header from passed packet.)
  If packet is too small to contain actual valid header, then an exception
  is raised.

  @param PacketBuffer Packet buffer from which the header will be extracted.

  @returns Header of given packet.
}
Function GetPacketHeader(PacketBuffer: TPacketBuffer): TPacketHeader;

//------------------------------------------------------------------------------

{
  @abstract(Returns address of payload in memory image of given packet.)
  @bold(Note) - the packet might not contain any payload, but this function
  still returns valid address (pointing to the end of packet memory image). 

  @param Packet Packet from which the payload address is requested.

  @returns Address of payload of given packet.
}
Function GetPayloadAddress(Packet: TPacketBuffer): Pointer;


implementation

{==============================================================================}
{   Functions implementations                                                  }
{==============================================================================}

Function GetPacketIDPrefix(PacketID: LongWord): LongWord;
begin
Result := PacketID and $FFFF0000;
end;

//-----------------------------------------------------------------------------

Function GetPacketHeader(PacketBuffer: TPacketBuffer): TPacketHeader;
begin
If PacketBuffer.Size >= SizeOf(TPacketHeader) then
  Result := PPacketHeader(PacketBuffer.Data)^
else
  raise Exception.Create('GetPacketHeader: Packet size is too small.');
end;

//------------------------------------------------------------------------------

Function GetPayloadAddress(Packet: TPacketBuffer): Pointer;
begin
Result := Pointer(PtrUInt(Packet.Data) + SizeOf(TPacketHeader));
end;

end.
