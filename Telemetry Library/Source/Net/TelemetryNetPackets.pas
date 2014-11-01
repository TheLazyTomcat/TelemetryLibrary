{*******************************************************************************
@abstract(Types definitions, constants and routines for manipulation with
          packets.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-11)
@lastmod(2013-10-11)

  TelemetryNetPackets

  ©František Milt, all rights reserved.

  Communication between server and client(s) is realized using variable length
  messages, called packets.

  All packets starts with header of this structure:
@preformatted(
  begin
    signature     4bytes  (TN_PACKET_SIGNATURE)
    packet ID     4bytes  (identification number of the packet)
    time stamp    8bytes  (local time when the packet was created)
    payload size  4bytes  (signed 32bit integer, size of following payload in bytes, can be 0)
  end;)

  After this header an optional payload data follows.@br
  Depending on payload presence, packets are divided into two groups, basic and
  extended.@br
  Basic packets contains no data (field "payload size" is set to 0), only
  header, and are used only as notifications or non-parameter commands.@br
  Extended packets are used mainly for data transfer or parametrized
  commands.@br
  Packet payload can contain any data and its size is limited only by available
  memory (technical maximum 2GiB).

  Structure of packet payload (see individual packet ID definitions for
  appropriate structure) is only hint for processing implemetation, not actual
  structured type. For example, any basic packet can actually contain data,
  but they are ignored and discarded during processing.@br
  All basic binary types (integers, floats, ...) are stored as little-endian.@br
  Strings in packet payload are stored as following substructure:
@preformatted(
  begin
    length  4bytes  (signed 32bit integer, length of "data" field in AnsiChars)
    data    []      (AnsiChars)
  end;)


  Actual packet ID number is composed from two fields, high word is ID prefix
  and low word is packet number in the prefix/usage group.@br
  Prefixes are used to distinguish packet usage group.
  They are also used for better branching of packet processing.

  List of defined prefixes:
  @preformatted(
    TN_PREFIX_COMMON
    TN_PREFIX_ADMIN
    TN_PREFIX_SUPERADMIN
    TN_PREFIX_EVENT_KNOWN
    TN_PREFIX_CHANNEL_KNOWN
    TN_PREFIX_CONFIG_KNOWN
    TN_PREFIX_EVENT
    TN_PREFIX_CHANNEL
    TN_PREFIX_CONFIG
    TN_PREFIX_LOG)


  List of defined packet identificators (b = basic packet, e = extended packet,
  s = sent by server, c = sent by client, o = sent by both client and server,
  a = sent by any connection endpoint):
  @preformatted(
    - TN_PREFIX_COMMON prefix packets -
    (b/a) TN_PACKET_PING
    (b/a) TN_PACKET_PING_RESPONSE
    (e/c) TN_PACKET_HI
    (e/s) TN_PACKET_VERSION
    (b/c) TN_PACKET_TELEMETRY_VERSION_GET
    (e/s) TN_PACKET_TELEMETRY_VERSION
    (b/c) TN_PACKET_READY
    (e/c) TN_PACKET_PARAM_GET
    (e/s) TN_PACKET_PARAM
    (e/o) TN_PACKET_BYE
    (e/s) TN_PACKET_MESSAGE
    (e/a) TN_PACKET_PACKET
    (e/c) TN_PACKET_RIGHTS_ADMIN_REQUEST
    (e/o) TN_PACKET_RIGHTS_ADMIN
    (e/a) TN_PACKET_ERROR
    (e/c) TN_PACKET_RIGHTS_SUPERADMIN_REQUEST
    (e/o) TN_PACKET_RIGHTS_SUPERADMIN

    - TN_PREFIX_ADMIN prefix packets -
    (e/s) TN_PACKET_ADMIN_CLIENT_CONNECTING
    (e/s) TN_PACKET_ADMIN_CLIENT_REJECTED
    (e/s) TN_PACKET_ADMIN_CLIENT_CONNECTED
    (e/s) TN_PACKET_ADMIN_CLIENT_DISCONNECTED
    (e/s) TN_PACKET_ADMIN_CLIENT_CHANGE
    (e/c) TN_PACKET_ADMIN_CLIENT_OPERATION
    (b/c) TN_PACKET_ADMIN_CLIENT_COUNT_GET
    (e/s) TN_PACKET_ADMIN_CLIENT_COUNT
    (e/c) TN_PACKET_ADMIN_CLIENT_INDEX_GET
    (e/s) TN_PACKET_ADMIN_CLIENT_INDEX
    (b/c) TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET
    (b/c) TN_PACKET_ADMIN_CLIENT_ALL_GET
    (e/s) TN_PACKET_ADMIN_CLIENT_ALL
    (b/c) TN_PACKET_ADMIN_LIST_COUNT_GET
    (e/s) TN_PACKET_ADMIN_LIST_COUNT
    (e/c) TN_PACKET_ADMIN_LIST_INDEX_GET
    (e/s) TN_PACKET_ADMIN_LIST_INDEX
    (b/c) TN_PACKET_ADMIN_LIST_INDEX_ALL_GET
    (b/c) TN_PACKET_ADMIN_LIST_ALL_GET
    (e/s) TN_PACKET_ADMIN_LIST_ALL
    (b/c) TN_PACKET_ADMIN_LIST_CLEAR
    (e/c) TN_PACKET_ADMIN_LIST_ADD
    (e/c) TN_PACKET_ADMIN_LIST_REMOVE
    (e/c) TN_PACKET_ADMIN_LIST_DELETE
    (b/c) TN_PACKET_ADMIN_LIST_RELOAD
    (b/c) TN_PACKET_ADMIN_LIST_SAVE
    (b/s) TN_PACKET_ADMIN_LIST_CHANGE
    (e/c) TN_PACKET_ADMIN_SEND_MESSAGE
    (e/o) TN_PACKET_ADMIN_PASSWORD
    (e/c) TN_PACKET_ADMIN_CHANGE_PASSWORD
    (e/c) TN_PACKET_ADMIN_PARAM_SET

    - TN_PREFIX_SUPERADMIN prefix packets -
    (e/o) TN_PACKET_SUPERADMIN_ADMIN_PASSWORD
    (e/c) TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD

    - TN_PREFIX_EVENT_KNOWN prefix packets -
    (b/c) TN_PACKET_EVENT_KNOWN_COUNT_GET
    (e/s) TN_PACKET_EVENT_KNOWN_COUNT
    (e/c) TN_PACKET_EVENT_KNOWN_INDEX_GET
    (e/s) TN_PACKET_EVENT_KNOWN_INDEX
    (b/c) TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET
    (b/c) TN_PACKET_EVENT_KNOWN_ALL_GET
    (e/s) TN_PACKET_EVENT_KNOWN_ALL

    - TN_PREFIX_CHANNEL_KNOWN prefix packets -
    (b/c) TN_PACKET_CHANNEL_KNOWN_COUNT_GET
    (e/s) TN_PACKET_CHANNEL_KNOWN_COUNT
    (e/c) TN_PACKET_CHANNEL_KNOWN_INDEX_GET
    (e/s) TN_PACKET_CHANNEL_KNOWN_INDEX
    (b/c) TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET
    (b/c) TN_PACKET_CHANNEL_KNOWN_ALL_GET
    (e/s) TN_PACKET_CHANNEL_KNOWN_ALL

    - TN_PREFIX_CONFIG_KNOWN prefix packets -
    (b/c) TN_PACKET_CONFIG_KNOWN_COUNT_GET
    (e/s) TN_PACKET_CONFIG_KNOWN_COUNT
    (e/c) TN_PACKET_CONFIG_KNOWN_INDEX_GET
    (e/s) TN_PACKET_CONFIG_KNOWN_INDEX
    (b/c) TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET
    (b/c) TN_PACKET_CONFIG_KNOWN_ALL_GET
    (e/s) TN_PACKET_CONFIG_KNOWN_ALL

    - TN_PREFIX_EVENT prefix packets -
    (e/o) TN_PACKET_EVENT_REG
    (b/c) TN_PACKET_EVENT_REG_ALL
    (e/o) TN_PACKET_EVENT_UNREG
    (b/c) TN_PACKET_EVENT_UNREG_ALL
    (e/s) TN_PACKET_EVENT_EVENT
    (e/o) TN_PACKET_EVENT_REGISTERED
    (b/c) TN_PACKET_EVENT_REGISTERED_COUNT_GET
    (e/s) TN_PACKET_EVENT_REGISTERED_COUNT
    (e/c) TN_PACKET_EVENT_REGISTERED_INDEX_GET
    (e/s) TN_PACKET_EVENT_REGISTERED_INDEX
    (b/c) TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET
    (b/c) TN_PACKET_EVENT_REGISTERED_ALL_GET
    (e/s) TN_PACKET_EVENT_REGISTERED_ALL

    - TN_PREFIX_CHANNEL prefix packets -
    (e/o) TN_PACKET_CHANNEL_REG
    (e/c) TN_PACKET_CHANNEL_REG_ALL
    (e/o) TN_PACKET_CHANNEL_UNREG
    (b/c) TN_PACKET_CHANNEL_UNREG_ALL
    (e/s) TN_PACKET_CHANNEL_CHANNEL
    (e/s) TN_PACKET_CHANNEL_CHANNEL_BUFFERED
    (e/o) TN_PACKET_CHANNEL_REGISTERED
    (b/c) TN_PACKET_CHANNEL_REGISTERED_COUNT_GET
    (e/s) TN_PACKET_CHANNEL_REGISTERED_COUNT
    (e/c) TN_PACKET_CHANNEL_REGISTERED_INDEX_GET
    (e/s) TN_PACKET_CHANNEL_REGISTERED_INDEX
    (b/c) TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET
    (b/c) TN_PACKET_CHANNEL_REGISTERED_ALL_GET
    (e/s) TN_PACKET_CHANNEL_REGISTERED_ALL
    (e/c) TN_PACKET_CHANNEL_STORED_SEND_ALL

    - TN_PREFIX_CONFIG prefix packets -
    (e/s) TN_PACKET_CONFIG_CONFIG
    (e/o) TN_PACKET_CONFIG_STORED
    (b/c) TN_PACKET_CONFIG_STORED_COUNT_GET
    (e/s) TN_PACKET_CONFIG_STORED_COUNT
    (e/c) TN_PACKET_CONFIG_STORED_INDEX_GET
    (e/s) TN_PACKET_CONFIG_STORED_INDEX
    (b/c) TN_PACKET_CONFIG_STORED_INDEX_ALL_GET
    (b/c) TN_PACKET_CONFIG_STORED_ALL_GET
    (e/s) TN_PACKET_CONFIG_STORED_ALL

    - TN_PREFIX_LOG prefix packets -
    (e/o) TN_PACKET_LOG_LOG)

  Last change:  2013-10-11

  Change List:@unorderedList(
    @item(2013-10-10  - First stable version.))

  todo:@unorderedList(
  @item(Add new packet - (e)TN_PACKET_DEFFERED - sent when received operation
        gets deffered.))

*******************************************************************************}
unit TelemetryNetPackets;

interface

{$INCLUDE '..\Telemetry_defs.inc'}
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Implement TN_PACKET_DEFFERED packet.'}
{$ENDIF}

uses
  ScktComp;

{==============================================================================}
{    Constants, types, variables, etc.                                         }
{==============================================================================}

const
  // @abstract(Signature of packet.)
  // Every packet starts with this 4 byte value. It is used to check validity of
  // incoming data and to find packet start in general data stream.
  TN_PACKET_SIGNATURE = $654E6554;

type
{
  Used in TN_PACKET_ERROR to distinguish nature of reported error.
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
  @value(petUnknownPacket      Packet with id unknown to recipient was
                               received.)
  @value(petUnknownClient      Administrator requested operation on client that
                               is not connected to the server.)
  @value(petUnknownParameter   Request for unknown parameter have been
                               received.)
}
  TPacketErrorType = (petUnknown,petGeneric,petPacketTooSmall,
                      petErrorReadingPacket,petUnexpectedPacket,
                      petUndefinedBehaviour,petUnknownPacket,
                      petUnknownClient,petUnknownParameter);

  // Record used for manipulation with packets (passing, saving, ...).
  // @member(Data Pointer to actual packet memory image.)
  // @member(Size @noAutoLink(Size) of packet memory image in bytes.)
  TPacketBuffer = Record
    Data: Pointer;
    Size: Integer;
  end;
  PPacketBuffer = ^TPacketBuffer;

  // Event type used where there is possibility or need to pass packet for
  // further processing.
  TPacketNotifyEvent = procedure(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer) of object;

  // @abstract(Packet header structure.)
  // Used mainly to help gettig packet header from and to general buffers and in
  // packets building and resolving.
  // @member(Signature Must be equal to TN_PACKET_SIGNATURE.)
  // @member(PacketID Identification number of the packet.)
  // @member(TimeStamp Local time when the packet was created.)
  // @member(PayloadSize Size of following payload in bytes. Can be 0.)
  TPacketHeader = packed Record
    Signature:    Integer;
    PacketID:     Integer;
    TimeStamp:    TDateTime;    
    PayloadSize:  Integer;
  end;
  PPacketHeader = ^TPacketHeader;

const
  // Minimum size of packet.@br
  // Used to check if incoming data are valid. Size of every packet must be
  // equal to or greater than this value.
  TN_MIN_PACKET_SIZE = SizeOf(TPacketHeader); {20 bytes}

{--- Packets ID prefixes ------------------------------------------------------}

  // General communication packet group.
  TN_PREFIX_COMMON        = $00000000;

  // Admin level communication. Client must have admin rights to send and
  // receive these packets, otherwise they are ignored.
  TN_PREFIX_ADMIN         = $00010000;

  // SuperAdmin level communication. Client must have superadmin rights to
  // send and receive these packets, otherwise they are ignored.
  TN_PREFIX_SUPERADMIN    = $00020000;

  // Packets used to get known events from server.
  TN_PREFIX_EVENT_KNOWN   = $00050000;

  // Packets used to get known channels from server.
  TN_PREFIX_CHANNEL_KNOWN = $000A0000;

  // Packets used to get known configs from server.
  TN_PREFIX_CONFIG_KNOWN  = $000B0000;

  // Packets used to get list of registered events, send events, (un)register
  // events, etc..
  TN_PREFIX_EVENT         = $000F0000;

  // Packets used to get list of registered channels, send channels,
  // (un)register channels, etc..
  TN_PREFIX_CHANNEL       = $00140000;

  // Packets used to inform about change in stored config values and managing
  // this list.
  TN_PREFIX_CONFIG        = $00150000;

  // Packets used to inform about game log writes.  
  TN_PREFIX_LOG           = $00190000;


{==============================================================================}
{    Definintion of packet ID numbers and their structures.                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{    General communication packets                                             }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_PING packet id.)

  Sender: Any@br
  Packet: Basic

  Can be used to check connectivity or responsiveness of other side of the
  connection.
}
  TN_PACKET_PING = TN_PREFIX_COMMON or $0000;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_PING_RESPONSE packet id.)

  Sender: Any@br
  Packet: Basic

  Sent as immediate response to TN_PACKET_PING packet.
}
  TN_PACKET_PING_RESPONSE = TN_PREFIX_COMMON or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_HI packet id.)

  Sender: Client@br
  Packet: Extended

@preformatted(
  begin
    hash type       4B  (THashType)
    password hash   []  (password hash)
  end;
)
  Sent by client to the server as first packet immediately after successful
  connection. Field "hash type" contains identificator denoting type of hashing
  function used to create password hash.@br
  On server side - when hash does not match, client is immediately disconnected.
  Also, when server expects this packet and a different packet arrives, it is
  assumed the client does not known password and is immediatelly disconnected.
  When hash is right, the server sends information about its version
  (TN_PACKET_VERSION packet) as an response to this packet.
}
  TN_PACKET_HI = TN_PREFIX_COMMON or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_VERSION packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    server version    4B  (unsigned 32bit integer)
  end;)

  Sent as immediate response to TN_PACKET_HI.@br
  Contains server version (server capabilities and communication protocol).
  When client does not support given server version, it must immediately
  disconnect.
}
  TN_PACKET_VERSION = TN_PREFIX_COMMON or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_TELEMETRY_VERSION_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send information about running telemetry recipient.@br
  It can be sent any time, but is usually sent as immediate response to
  TN_PACKET_VERSION packet (if client stays connected, i.e. supports
  server version from TN_PACKET_VERSION).
}
  TN_PACKET_TELEMETRY_VERSION_GET = TN_PREFIX_COMMON or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_TELEMETRY_VERSION packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    telemetry version 4B      (scs_u32_t)
    game ID           String  (variable size)
    game version      4B      (scs_u32_t)
    game name         String  (variable size)
  end;)

  Sent as immediate response to TN_PACKET_TELEMETRY_VERSION_GET.@br
  Contains information about running telemetry recipient.@br
  Client can react to these values (e.g. when it cannot work with given
  telemetry version and disconnects itself), but it is not mandatory.
}
  TN_PACKET_TELEMETRY_VERSION = TN_PREFIX_COMMON or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_READY packet id.)

  Sender: Client@br
  Packet: Basic

  Sent to server when the client is ready to fully communicate. It is generated
  after the client receives and evaluates informations about server and
  telemetry/game versions. Until this packet is not received, server will not
  send or receive most of the packets for given connection.
}
  TN_PACKET_READY = TN_PREFIX_COMMON or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_PARAM_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    parameter id    4B  (TParameterID)
  end;)

  Instructs server to send value of parameter given by "parameter id" field.@br
  For available values of "paramter id" refer to TParameterID type declaration.
}
  TN_PACKET_PARAM_GET = TN_PREFIX_COMMON or $0007;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_PARAM packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    parameter id    4B  (TParameterID)
    data            []  (variable size, can have zero size)
  end;)

  Immediate response to TN_PACKET_PARAM_GET. Field "data" contains actual value
  of requested parameter (identificator of which is stored in field
  "parameter id").@br
  If requested parameter was not valid, then "parameter id" contains
  @code(pidInvalid) and "data" field is empty (has zero size).@br
  Size of the "data" field depends on actual parameter type.@br
  This packet is also sent automatically to all clients every time any server
  settings parameter is set.
}
  TN_PACKET_PARAM = TN_PREFIX_COMMON or $0008;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_BYE packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    disconnection reason  4B  (TDisconnectReason)
  end;)

  Server:@br
  Sent to client when it is going to be disconnected by the server.@br
  When client receives this packet, it can execute any actions preceding
  disconnection, but should not disconnect itself.

  Client:@br
  Sent to server when the client is about to disconnect.
}
  TN_PACKET_BYE = TN_PREFIX_COMMON or $0009;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_MESSAGE packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    message text    String  (variable size)
  end;)

  Used to send text messages to clients.
}
  TN_PACKET_MESSAGE = TN_PREFIX_COMMON or $000A;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_PACKET packet id.)

  Sender: Any@br
  Packet: Extended
@preformatted(
  begin
    payload   []  (data carried by packet, can be empty)
  end;)

  Used to send unspecified data. Size of "payload" field is stored in
  "PayloadSize" field of packet header.
}
  TN_PACKET_PACKET = TN_PREFIX_COMMON or $000B;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_RIGHTS_ADMIN_REQUEST packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    hash type       4B  (THashType)
    password hash   []  (password hash)
  end;)

  Sent by client that requests administrative rights. Field "hash type" contains
  identificator denoting type of hashing function used to create password
  hash.@br
  When the password hash is evaluated as valid, the server grants admin rights
  to sender client (see TN_PACKET_RIGHTS_ADMIN packet).

@bold(Note:) Admin password differs from general password.
}
  TN_PACKET_RIGHTS_ADMIN_REQUEST = TN_PREFIX_COMMON or $000C;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_RIGHTS_ADMIN packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    granted   1B    (boolean)
  end;)

  Server:@br
  Sent by server to client that has requested administrative rights or
  information about status of itself. When field "granted" is set to @true, then
  admin rights have been granted to the client, otherwise they were not.

  Client:@br
  Instructs server to send whether sender has administrative rights.
}
  TN_PACKET_RIGHTS_ADMIN = TN_PREFIX_COMMON or $000D;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ERROR packet id.)

  Sender: Any@br
  Packet: Extended
@preformatted(
  begin
    packet id           4B  (unsigned 32bit integer)
    packet time stamp   8B  (TDateTime)
    error type          4B  (TPacketErrorType)
    error code          4B  (unsigned 32bit integer)
  end;)

  Sent back when recipient receives erroneous packet. Fileds "packet id" and
  "packet time stamp" contains id and time stamp of packet that was evaluated as
  broken. "error type" contains identificator denoting the nature of error.
  Field "error code" contains system error code for last error - it is set only
  for selected error types, for other types, it is set to 0.
}
  TN_PACKET_ERROR = TN_PREFIX_COMMON or $000E;
  
//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_RIGHTS_SUPERADMIN_REQUEST packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    hash type       4B  (THashType)
    password hash   []  (password hash)
  end;)

  Sent by client that requests super-administrative rights. Field "hash type"
  contains identificator denoting type of hashing function used to create
  password hash.@br
  When the password hash is evaluated as valid, the server grants superadmin
  rights to sender client (see TN_PACKET_RIGHTS_SUPERADMIN packet).

@bold(Note:) SuperAdmin password differs from admin password.
}
  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST = TN_PREFIX_COMMON or $000F;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_RIGHTS_SUPERADMIN packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    granted   1B    (boolean)
  end;)

  Server:@br
  Sent by server to client that has requested super-administrative rights or
  information about status of itself. When field "granted" is set to @true, then
  superadmin rights have been granted to the client, otherwise they were not.

  Client:@br
  Instructs server to send whether sender has suer-administrative rights.
}
  TN_PACKET_RIGHTS_SUPERADMIN = TN_PREFIX_COMMON or $0010;

  

{------------------------------------------------------------------------------}
{    Admin level communication packets                                         }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_ADMIN_CLIENT_CONNECTING packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    client id     16B     (TGUID)
    client IP     String  (variable size)
    client port   4B      (unsigned 32bit integer)
  end;)

  Sent to all administrators when new client is connecting. At this stage,
  client is technically connected, but it has to be still checked against
  address list - this check can result in forced client disconnection.
}
  TN_PACKET_ADMIN_CLIENT_CONNECTING = TN_PREFIX_ADMIN or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_REJECTED packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    client id     16B     (TGUID)
    client IP     String  (variable size)
    client port   4B      (unsigned 32bit integer)
    in list       1B      (boolean)
  end;)

  Sent to all administrators when connecting client is rejected (e.g. because
  client address is listed in address list which is set to black list mode) and
  will be disconnected.@br
  "in list" is set to @true when rejected client was listed (means address list
  was propably set to @code(lmBlackList) or @code(lmDenyAll), otherwise @false.
}
  TN_PACKET_ADMIN_CLIENT_REJECTED = TN_PREFIX_ADMIN or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_CONNECTED packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    client id     16B     (TGUID)
    client IP     String  (variable size)
    client port   4B      (unsigned 32bit integer)
  end;)

  Sent to all administrators when client has successfuly finished connection. 
}
  TN_PACKET_ADMIN_CLIENT_CONNECTED = TN_PREFIX_ADMIN or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_DISCONNECTED packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    client id     16B     (TGUID)
    client IP     String  (variable size)
    client port   4B      (unsigned 32bit integer)
  end;)

  Sent to all administrators when client is disconnecting.
}
  TN_PACKET_ADMIN_CLIENT_DISCONNECTED = TN_PREFIX_ADMIN or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_CHANGE packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    client id             16B     (TGUID)
    client IP             String  (variable size)
    client port           4B      (unsigned 32bit integer)
    waiting for password  1B      (boolean)
    ready to work         1B      (boolean)
    admin rights          1B      (boolean)
    superadmin rights     1B      (boolean)
  end;)

  Sent to all administrators whenever status of any connected client changes
  (for example when client is granted administrative rights).
}
  TN_PACKET_ADMIN_CLIENT_CHANGE = TN_PREFIX_ADMIN or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_OPERATION packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    client id   16B     (TGUID)
    operation    4B     (TClientOperation)
  end;)

  Instructs server to execute operation (id of requested operation is stored in
  "operation" filed, see TClientOperation type for available operations) on
  client with identificator from "client id" field. When no client with given
  id is connected, no operation is performed and server send back error packet
  with error type set to petUnknownClient.
}
  TN_PACKET_ADMIN_CLIENT_OPERATION = TN_PREFIX_ADMIN or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of connected clients.
}
  TN_PACKET_ADMIN_CLIENT_COUNT_GET = TN_PREFIX_ADMIN or $0007;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Sent as a response to TN_PACKET_ADMIN_CLIENT_COUNT_GET packet. "count" field
  contains number of connected clients.
}
  TN_PACKET_ADMIN_CLIENT_COUNT = TN_PREFIX_ADMIN or $0008;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send information about client listed at position given by
  "index".
}
  TN_PACKET_ADMIN_CLIENT_INDEX_GET = TN_PREFIX_ADMIN or $0009;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index                 4B      (signed 32bit integer)
    client id             16B     (TGUID)
    client IP             String  (variable size)
    client port           4B      (unsigned 32bit integer)
    waiting for password  1B      (boolean)
    ready to work         1B      (boolean)
    admin rights          1B      (boolean)
    superadmin rights     1B      (boolean)
  end;)

  Immediate response to TN_PACKET_ADMIN_CLIENT_INDEX_GET packet.@br
  Contains information about client on position given by "index".
  If "index" in TN_PACKET_ADMIN_CLIENT_INDEX_GET pointed to invalid position,
  then "index" in this packet is set to -1 and other fields are set to 0, @false
  or empty string ("client id" contains cEmptyGUID constant value).
}
  TN_PACKET_ADMIN_CLIENT_INDEX = TN_PREFIX_ADMIN or $000A;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send informations about all connected clients. They are
  immediately sent as a stream of TN_PACKET_ADMIN_CLIENT_INDEX packets.
}
  TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET = TN_PREFIX_ADMIN or $000B;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send informations about all connected clients. They are
  all sent in one TN_PACKET_EVENT_KNOWN_ALL packet (see below).
}
  TN_PACKET_ADMIN_CLIENT_ALL_GET = TN_PREFIX_ADMIN or $000C;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CLIENT_ALL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count     4B  (signed 32bit integer, number of info substructures)
    info      substructure[]
    begin
      client id             16B     (TGUID)
      client IP             String  (variable size)
      client port           4B      (unsigned 32bit integer)
      waiting for password  1B      (boolean)
      ready to work         1B      (boolean)
      admin rights          1B      (boolean)
      superadmin rights     1B      (boolean)
    end;
  end;)

  Immediate response to TN_PACKET_ADMIN_CLIENT_ALL.
  Contains array of informations about all connected clients.
}
  TN_PACKET_ADMIN_CLIENT_ALL = TN_PREFIX_ADMIN or $000D;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of adresses in address list.
}
  TN_PACKET_ADMIN_LIST_COUNT_GET = TN_PREFIX_ADMIN or $000E;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Sent as a response to TN_PACKET_ADMIN_LIST_COUNT_GET packet. "count" field
  contains number of adresses in address list.
}
  TN_PACKET_ADMIN_LIST_COUNT = TN_PREFIX_ADMIN or $000F;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send address listed in address list at position given by
  "index".
}
  TN_PACKET_ADMIN_LIST_INDEX_GET = TN_PREFIX_ADMIN or $0010;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index   4B      (signed 32bit integer)
    item    String  (variable size)
  end;)

  Immediate response to TN_PACKET_ADMIN_LIST_INDEX_GET packet.@br
  Contains address listed at position given by "index".@br
  If "index" in TN_PACKET_ADMIN_LIST_INDEX_GET pointed to invalid position,
  then "index" in this packet is set to -1 and "item" is set to an empty string.
}
  TN_PACKET_ADMIN_LIST_INDEX = TN_PREFIX_ADMIN or $0011;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all addresses in adress list. They are immediately
  sent as a stream of TN_PACKET_ADMIN_LIST_INDEX packets.
}
  TN_PACKET_ADMIN_LIST_INDEX_ALL_GET = TN_PREFIX_ADMIN or $0012;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all addresses in adress list. They are all sent in
  one TN_PACKET_ADMIN_LIST_ALL packet (see below).
}
  TN_PACKET_ADMIN_LIST_ALL_GET = TN_PREFIX_ADMIN or $0013;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_ALL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count     4B        (signed 32bit integer, number of "item" fields)
    item      String[]  (variable size)
  end;)

  Immediate response to TN_PACKET_ADMIN_LIST_ALL_GET.
  Contains array of all adresses listed in adress list.
}
  TN_PACKET_ADMIN_LIST_ALL = TN_PREFIX_ADMIN or $0014;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_CLEAR packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to clear address list (delete all items).
}
  TN_PACKET_ADMIN_LIST_CLEAR = TN_PREFIX_ADMIN or $0015;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_ADD packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    item  String  (variable size)
  end;)

  Instructs server to add address in "item" field to address list.
}
  TN_PACKET_ADMIN_LIST_ADD = TN_PREFIX_ADMIN or $0016;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_REMOVE packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    item  String  (variable size)
  end;)

  Instructs server to remove address given in "item" field from adress list.
}
  TN_PACKET_ADMIN_LIST_REMOVE = TN_PREFIX_ADMIN or $0017;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_DELETE packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to delete item at position given by "index" from adress list.
  When index falls out of allowed boundary, error packet is sent back with error
  type set to @code(petGeneral).
}
  TN_PACKET_ADMIN_LIST_DELETE = TN_PREFIX_ADMIN or $0018;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_RELOAD packet id.)

  Sender: Client@br
  Packet: Basic

  Executed only when server property SaveSettingsToFile is set to @true.@br
  Instructs server to clear the address list and load its content from file
  (TAddressList.LoadFromFile method is called).
}
  TN_PACKET_ADMIN_LIST_RELOAD = TN_PREFIX_ADMIN or $0019;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_SAVE packet id.)

  Sender: Client@br
  Packet: Basic

  Executed only when server property SaveSettingsToFile is set to @true.@br
  Instructs server to save address list to a file (TAddressList.SaveToFile
  method is called).
}
  TN_PACKET_ADMIN_LIST_SAVE = TN_PREFIX_ADMIN or $001A;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_LIST_CHANGE packet id.)

  Sender: Server@br
  Packet: Basic

  Sent to all administrators whenever adress list changes (item is added,
  removed, etc.). When admin client receive this packet, it should update own
  remote address list.
}
  TN_PACKET_ADMIN_LIST_CHANGE = TN_PREFIX_ADMIN or $001B;
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Review behavior of address list management packets.'}
{$ENDIF}

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_SEND_MESSAGE packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    client    16B     (TGUID)
    message   String  (variable size)
  end;)

  Instructs server to send text message to client with given id. When no client
  with requested id is fount, server sends back error packet with error type set
  to petUnknownClient.  
}
  TN_PACKET_ADMIN_SEND_MESSAGE = TN_PREFIX_ADMIN or $001C;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_PASSWORD packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    password  String  (variable size)
  end;)

  Server:@br
  Sent to client that requested the password. "password" contains actual login
  password that is set on server.

  Client:@br
  Instructs server send back actual password.
}
  TN_PACKET_ADMIN_PASSWORD = TN_PREFIX_ADMIN or $001D;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_CHANGE_PASSWORD packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    password  String  (variable size)
  end;)

  Instructs server to change login password.

  @bold(Warning!) - All clients are disconnected when the password is changed.
}
  TN_PACKET_ADMIN_CHANGE_PASSWORD = TN_PREFIX_ADMIN or $001E;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_ADMIN_PARAM_SET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    parameter id    4B  (TParameterID)
    value           []  (variable size)
  end;)

  Instructs server to set selected settings parameter to defined value.@br
  If "parameter id" is not valid, or "value" size is too small, then no action
  is executed.@br
  Size of the "data" field depends on actual parameter type.@br
  This packet is intended to remotely change server settings at run-time.
}
  TN_PACKET_ADMIN_PARAM_SET = TN_PREFIX_ADMIN or $001F;


{------------------------------------------------------------------------------}
{    SuperAdmin level communication packets                                    }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_SUPERADMIN_ADMIN_PASSWORD packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    password  String  (variable size)
  end;)

  Server:@br
  Sent to client that requested the password. "password" contains actual admin
  password that is set on server.

  Client:@br
  Instructs server send back actual admin password.
}
  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD = TN_PREFIX_SUPERADMIN or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    password  String  (variable size)
  end;)

  Instructs server to change admin password.

  @bold(Warning!) - All administrators are stripped of their admin rights when
  the password is changed.
}
  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD = TN_PREFIX_SUPERADMIN or $0002;
  

{-------------------------------------------------------------------------------
***  Known events packets  *****************************************************
-------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_EVENT_KNOWN_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known events.
}
  TN_PACKET_EVENT_KNOWN_COUNT_GET = TN_PREFIX_EVENT_KNOWN or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_EVENT_KNOWN_COUNT_GET.@br
  Contains number of known events.
}
  TN_PACKET_EVENT_KNOWN_COUNT = TN_PREFIX_EVENT_KNOWN or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send known event on position given by "index".
}
  TN_PACKET_EVENT_KNOWN_INDEX_GET = TN_PREFIX_EVENT_KNOWN or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index   4B      (signed 32bit integer)
    event   4B      (scs_event_t)
    name    String  (variable size)
    valid   1B      (boolean)
    utility 1B      (boolean)
  end;)

  Immediate response to TN_PACKET_EVENT_KNOWN_INDEX_GET.@br
  Contains information about known event on position given by "index".@br
  If "index" in TN_PACKET_EVENT_KNOWN_INDEX_GET pointed to invalid position,
  then "index" in this packet is set to -1 and other fields (Event, Name, ...)
  are set to 0/@false/empty string.
}
  TN_PACKET_EVENT_KNOWN_INDEX = TN_PREFIX_EVENT_KNOWN or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known events. They are immediately sent as
  a stream of TN_PACKET_EVENT_KNOWN_INDEX packets.
}
  TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET = TN_PREFIX_EVENT_KNOWN or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known events. They are all sent in one
  TN_PACKET_EVENT_KNOWN_ALL packet (see below).
}
  TN_PACKET_EVENT_KNOWN_ALL_GET = TN_PREFIX_EVENT_KNOWN or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_KNOWN_ALL packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    count     4B  (signed 32bit integer, number of info substructures)
    info      substructure[]
    begin
      event     4B      (scs_event_t)
      name      String  (variable size)
      valid     1B      (boolean)
      utility   1B      (boolean)
    end;
  end;)

  Immediate response to TN_PACKET_EVENT_KNOWN_ALL.@br
  Contains array of known events informations.
}
  TN_PACKET_EVENT_KNOWN_ALL = TN_PREFIX_EVENT_KNOWN or $0007;


{------------------------------------------------------------------------------}
{    Known channels packets                                                    }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known channels.
}
  TN_PACKET_CHANNEL_KNOWN_COUNT_GET = TN_PREFIX_CHANNEL_KNOWN or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_CHANNEL_KNOWN_COUNT_GET.@br
  Contains number of known channels.
}
  TN_PACKET_CHANNEL_KNOWN_COUNT = TN_PREFIX_CHANNEL_KNOWN or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send known channel on position given by "index".
}
  TN_PACKET_CHANNEL_KNOWN_INDEX_GET = TN_PREFIX_CHANNEL_KNOWN or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index             4B      (signed 32bit integer)
    name              String  (variable size)
    id                4B      (TChannelID)
    primary type      4B      (scs_value_type_t)
    secondary type    4B      (scs_value_type_t)
    tertiary type     4B      (scs_value_type_t)
    indexed           1B      (boolean)
    index config      String  (variable size)
    index config id   4B      (TItemID)
  end;)

  Immediate response to TN_PACKET_CHANNEL_KNOWN_INDEX_GET.@br
  Contains information about known channel on position given by "index".@br
  If "index" in TN_PACKET_CHANNEL_KNOWN_INDEX_GET pointed to invalid position,
  then "index" in this packet is set to -1 and other fields (name, etc.)
  are set to 0/@false/empty string.
}
  TN_PACKET_CHANNEL_KNOWN_INDEX = TN_PREFIX_CHANNEL_KNOWN or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET packet id.)
  
  Sender: Client@br
  Packet: Basic

  Instructs server to send all known channels. They are immediately sent as
  a stream of TN_PACKET_CHANNEL_KNOWN_INDEX packets.
}
  TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET = TN_PREFIX_CHANNEL_KNOWN or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known channels. They are all sent in one
  TN_PACKET_CHANNEL_KNOWN_ALL packet (see below).
}
  TN_PACKET_CHANNEL_KNOWN_ALL_GET = TN_PREFIX_CHANNEL_KNOWN or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_KNOWN_ALL packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    count     4B  (signed 32bit integer, number of info substructures)
    info      substructure[]
    begin
      name:             String  (variable size)
      id                4B      (TChannelID)
      primary type      4B      (scs_value_type_t)
      secondary type    4B      (scs_value_type_t)
      tertiary type     4B      (scs_value_type_t)
      indexed:          1B      (boolean)
      index config      String  (variable size)
      index config id   4B      (TItemID)
    end;
  end; )

  Immediate response to TN_PACKET_CHANNEL_KNOWN_ALL.@br
  Contains array of known channels informations.
}
  TN_PACKET_CHANNEL_KNOWN_ALL = TN_PREFIX_CHANNEL_KNOWN or $0007;



{------------------------------------------------------------------------------}
{    Known configs packets                                                     }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_CONFIG_KNOWN_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of known configs.
}
  TN_PACKET_CONFIG_KNOWN_COUNT_GET = TN_PREFIX_CONFIG_KNOWN or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_CONFIG_KNOWN_COUNT_GET.@br
  Contains number of known configs.
}
  TN_PACKET_CONFIG_KNOWN_COUNT = TN_PREFIX_CONFIG_KNOWN or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send known config on position given by "index".
}
  TN_PACKET_CONFIG_KNOWN_INDEX_GET = TN_PREFIX_CONFIG_KNOWN or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index         4B      (signed 32bit integer)
    name          String  (variable size)
    id            4B      (TConfigID)
    value type    4B      (scs_value_type_t)
    indexed       1B      (boolean)
    binded        1B      (boolean)
  end;)

  Immediate response to TN_PACKET_CONFIG_KNOWN_INDEX_GET.@br
  Contains information about known config on position given by "index".@br
  If "index" in TN_PACKET_CONFIG_KNOWN_INDEX_GET pointed to invalid position,
  then "index" in this packet is set to -1 and other fields (name, etc.)
  are set to 0/@false/empty string.
}
  TN_PACKET_CONFIG_KNOWN_INDEX = TN_PREFIX_CONFIG_KNOWN or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET packet id.)
  
  Sender: Client@br
  Packet: Basic

  Instructs server to send all known configs. They are immediately sent as
  a stream of TN_PACKET_CONFIG_KNOWN_INDEX packets.
}
  TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET = TN_PREFIX_CONFIG_KNOWN or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all known configs. They are all sent in one
  TN_PACKET_CONFIG_KNOWN_ALL packet (see below).
}
  TN_PACKET_CONFIG_KNOWN_ALL_GET = TN_PREFIX_CONFIG_KNOWN or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_KNOWN_ALL packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    count     4B  (signed 32bit integer, number of info substructures)
    info      substructure[]
    begin
      name:         String  (variable size)
      id            4B      (TConfigID)
      value type:   4B      (scs_value_type_t)
      indexed:      1B      (boolean)
      binded:       1B      (boolean)
    end;
  end;)

  Immediate response to TN_PACKET_CONFIG_KNOWN_ALL.@br
  Contains array of known configs informations.
}
  TN_PACKET_CONFIG_KNOWN_ALL = TN_PREFIX_CONFIG_KNOWN or $0007;
  
{------------------------------------------------------------------------------}
{    Events communication packets                                              }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_EVENT_REG packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    event   4B  (scs_event_t)
    result  4B  (scs_result_t)
  end;)

  Server:@br
  Sent to all clients when given event is succesfuly registered ("result" field
  set to SCS_RESULT_ok).@br
  If registration of event fails, server send this packet back only to
  registering client with "result" field set to result code from registration.

  Client:@br
  Instructs server to register a particular event.@br
  "result" field should be set to SCS_RESULT_ok.
}
  TN_PACKET_EVENT_REG = TN_PREFIX_EVENT or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REG_ALL packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to register all known events.
}
  TN_PACKET_EVENT_REG_ALL = TN_PREFIX_EVENT or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_UNREG packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    event   4B  (scs_event_t)
    result  4B  (scs_result_t)
  end;)

  Server:@br
  Send to all clients when given event is succesfuly unregistered ("result"
  field set to SCS_RESULT_ok).@br
  If unregistration of event fails, server send this packet back only to
  unregistering client with "result" field set to result code from
  unregistration.

  Client:@br
  Instructs server to unregister a particular event.@br
  "result" field should be set to SCS_RESULT_ok.
}
  TN_PACKET_EVENT_UNREG = TN_PREFIX_EVENT or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_UNREG_ALL packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to unregister all known events.
}
  TN_PACKET_EVENT_UNREG_ALL = TN_PREFIX_EVENT or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_EVENT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    event       4B  (scs_event_t)
    data size   4B  (unsigned 32bit integer)
    data        []  (variable size, can be empty)
  end;)

  Sent to all clients on all game avents occurence. "data" field content depends
  on event type, and can be empty.

  Content of "data" for individual events:
@preformatted(
  SCS_TELEMETRY_EVENT_frame_start
    Contains one scs_telemetry_frame_start_t structure.

  SCS_TELEMETRY_EVENT_configuration
    begin
      id            String          (variable size)
      named value   substructure[]  (array of named values)
      begin
        name          String          (variable size)
        index         4B              (scs_u32_t)
        value type    4B              (scs_value_type_t)
        value         []              (variable size data, actual size depends
                                       on value type)
      end;
      0x00000000      4B            (array ends with an empty string)
    end;)

  At the moment, all other events have no data attached.
}
  TN_PACKET_EVENT_EVENT = TN_PREFIX_EVENT or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    event       4B  (scs_event_t)
    registered  1B  (boolean)
  end;)

  Server:@br
  Sent back to instructing client. Field "registered" contains @true when given
  event is registered, otherwise @false.

  Client:@br
  Instructs server to check whether given event is registered.@br
  "registered" field can have any value.
}
  TN_PACKET_EVENT_REGISTERED = TN_PREFIX_EVENT or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of registered events.
}
  TN_PACKET_EVENT_REGISTERED_COUNT_GET = TN_PREFIX_EVENT or $0007;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_EVENT_REGISTERED_COUNT_GET.@br
  Contains number of registered events.
}
  TN_PACKET_EVENT_REGISTERED_COUNT = TN_PREFIX_EVENT or $0008;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send information about registered event on list position
  given by "index".
}
  TN_PACKET_EVENT_REGISTERED_INDEX_GET = TN_PREFIX_EVENT or $0009;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
    event   4B  (scs_event_t)
    utility 1B  (boolean)
  end;)

  Immediate response to TN_PACKET_EVENT_REGISTERED_INDEX_GET.@br
  Contains information about registered event on position given by "index".@br
  If "index" in TN_PACKET_EVENT_REGISTERED_INDEX_GET pointed to invalid
  position, then "index" in this packet is set to -1, field "event" is set to
  SCS_TELEMETRY_EVENT_invalid (0) and "utility" to @false.
}
  TN_PACKET_EVENT_REGISTERED_INDEX = TN_PREFIX_EVENT or $000A;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send information about all registered events. They are
  immediately sent as a stream of TN_PACKET_EVENT_REGISTERED_INDEX packets.
}
  TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET = TN_PREFIX_EVENT or $000B;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all registered events. They are all sent in one
  TN_PACKET_EVENT_REGISTERED_ALL packet (see below).
}
  TN_PACKET_EVENT_REGISTERED_ALL_GET = TN_PREFIX_EVENT or $000C;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_EVENT_REGISTERED_ALL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count     4B    (signed 32bit integer, number of info substructures)
    info  substructure[]
    begin
      event     4B    (scs_event_t)
      utility   1B    (boolean)
    end;
  end;)

  Immediate response to TN_PACKET_EVENT_REGISTERED_ALL_GET.@br
  Contains array of registered events.
}
  TN_PACKET_EVENT_REGISTERED_ALL = TN_PREFIX_EVENT or $000D;


{------------------------------------------------------------------------------}
{    Channels communication packets                                            }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_CHANNEL_REG packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    name          String  (variable size)
    index         4B      (scs_u32_t)
    value type    4B      (scs_value_type_t)
    flags         4B      (scs_u32_t)
    result        4B      (scs_result_t)
  end;)

  Server:@br
  Sent to all clients when given channel is succesfuly registered ("result"
  field set to SCS_RESULT_ok).@br
  If registration of channel fails, server send this packet back only to
  registering client with "result" field set to result code from registration.

  Client:@br
  Instructs server to register a particular channel.@br
  "result" field should be set to SCS_RESULT_ok.
}
  TN_PACKET_CHANNEL_REG = TN_PREFIX_CHANNEL or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REG_ALL packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    primary types     1B  (boolean)
    secondary types   1B  (boolean)
    tertiary types    1B  (boolean)
  end;)

  Instructs server to register all known channels.@br
  For parameters explanation, refer to TTelemetryRecipient.ChannelRegisterAll
  method.
}
  TN_PACKET_CHANNEL_REG_ALL = TN_PREFIX_CHANNEL or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_UNREG packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    name          String  (variable size)
    index         4B      (scs_u32_t)
    value type    4B      (scs_value_type_t)
    result        4B      (scs_result_t)
  end;)

  Server:@br
  Sent to all clients when given channel is succesfuly unregistered ("result"
  field set to SCS_RESULT_ok).@br
  If unregistration of channel fails, server send this packet back only to
  unregistering client with "result" field set to result code from
  unregistration.

  Client:@br
  Instructs server to unregister a particular channel.@br
  "result" field should be set to SCS_RESULT_ok.
}
  TN_PACKET_CHANNEL_UNREG = TN_PREFIX_CHANNEL or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_UNREG_ALL packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to unregister all known channels.
}
  TN_PACKET_CHANNEL_UNREG_ALL = TN_PREFIX_CHANNEL or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_CHANNEL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    name        String  (variable size)
    id          4B      (TChannelID)
    index       4B      (scs_u32_t)
    value type  4B      (scs_value_type_t)
    value       []      (variable size data, actual size depends on value type)
  end;)

  Sent to all clients on every game channel callback. "value" field is never
  empty.
}
  TN_PACKET_CHANNEL_CHANNEL = TN_PREFIX_CHANNEL or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_CHANNEL_BUFFERED packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer, number of info substructures)
    info    substructure[]
    begin
      name        String  (variable size)
      id          4B      (TChannelID)
      index       4B      (scs_u32_t)
      value type  4B      (scs_value_type_t)
      value       []      (variable size data, actual size depends on value type)
    end;
  end;)

  Sent to all clients on FrameEnd event. Contains all values of all buffered
  channels.@br
  This packet is sent only when server is set to buffer channels. And if so,
  TN_PACKET_CHANNEL_CHANNEL packets are not sent.
}
  TN_PACKET_CHANNEL_CHANNEL_BUFFERED = TN_PREFIX_CHANNEL or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    name          String  (variable size)
    index         4B      (scs_u32_t)
    value type    4B      (scs_value_type_t)
    registered    1B      (boolean)
  end;)

  Server:@br
  Sent back to instructing client. Field "registered" contains @true when given
  channel is registered, otherwise @false.

  Client:@br
  Instructs server to check whether given channel is registered.@br
  "registered" field can have any value.
}
  TN_PACKET_CHANNEL_REGISTERED = TN_PREFIX_CHANNEL or $0007;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of registered channels.
}
  TN_PACKET_CHANNEL_REGISTERED_COUNT_GET = TN_PREFIX_CHANNEL or $0008;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_CHANNEL_REGISTERED_COUNT_GET.@br
  Contains number of registered channels.
}
  TN_PACKET_CHANNEL_REGISTERED_COUNT = TN_PREFIX_CHANNEL or $0009;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send registered channel on position given by "index".
}
  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET = TN_PREFIX_CHANNEL or $000A;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    list index        4B      (signed 32bit integer)
    name              String  (variable size)
    id                4B      (TChannelID)
    index             4B      (scs_u32_t)
    value type        4B      (scs_value_type_t)
    flags             4B      (scs_u32_t)
    index config id   4B      (TItemID)
  end;)

  Immediate response to TN_PACKET_CHANNEL_REGISTERED_INDEX_GET.@br
  Contains information about registered channel on position given by "index"
  (in TN_PACKET_CHANNEL_REGISTERED_INDEX_GET packet).@br
  If "index" in TN_PACKET_CHANNEL_REGISTERED_INDEX_GET pointed to invalid
  position, then "list index" in this packet is set to -1, field "name" contains
  an empty string, ud is set to 0, "index" is set to SCS_U32_NIL, "value type"
  is set to SCS_VALUE_TYPE_INVALID, "flags" is set to
  SCS_TELEMETRY_CHANNEL_FLAG_none and "index config" to 0.
}
  TN_PACKET_CHANNEL_REGISTERED_INDEX = TN_PREFIX_CHANNEL or $000B;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all registered events. They are immediately sent as
  a stream of TN_PACKET_CHANNEL_REGISTERED_INDEX packets.
}
  TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET = TN_PREFIX_CHANNEL or $000C;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all registered channels. They are all sent in one
  TN_PACKET_CHANNEL_REGISTERED_ALL packet (see below).
}
  TN_PACKET_CHANNEL_REGISTERED_ALL_GET = TN_PREFIX_CHANNEL or $000D;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_REGISTERED_ALL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count       4B    (signed 32bit integer, number of info substructures)
    info        substructure[]
    begin
      name              String  (variable size)
      id                4B      (TChannelID)
      index             4B      (scs_u32_t)
      value type        4B      (scs_value_type_t)
      flags             4B      (scs_u32_t)
      index config id   4B      (TItemID)
    end;
  end;)

  Immediate response to TN_PACKET_CHANNEL_REGISTERED_ALL_GET.@br
  Contains array of registered channels informations.
}
  TN_PACKET_CHANNEL_REGISTERED_ALL = TN_PREFIX_CHANNEL or $000E;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CHANNEL_STORED_SEND_ALL packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send channels stored in recipient. The are sent as a
  stream
}
  TN_PACKET_CHANNEL_STORED_SEND_ALL = TN_PREFIX_CHANNEL or $000F;


{------------------------------------------------------------------------------}
{    Configs communication packets                                             }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_CONFIG_CONFIG packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    name        String  (variable size)
    id          4B      (TConfigID)
    index       4B      (scs_u32_t)
    value type  4B      (scs_value_type_t)
    value       []      (variable size data, actual size depends on value type)
  end;)

  Sent to all clients when value of stored config changes or a new config is
  stored.@br
  "value" field is never empty.
}
  TN_PACKET_CONFIG_CONFIG = TN_PREFIX_CONFIG or $0001;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    name      String  (variable size)
    index     4B      (scs_u32_t)
    stored    1B      (boolean)
  end;)

  Server:@br
  Sent back to instructing client. Field "stored" contains @true when given
  config is stored, otherwise @false.

  Client:@br
  Instructs server to check whether given config is stored.@br
  "stored" field can have any value.
}
  TN_PACKET_CONFIG_STORED = TN_PREFIX_CONFIG or $0002;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_COUNT_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send number of stored configs.
}
  TN_PACKET_CONFIG_STORED_COUNT_GET = TN_PREFIX_CONFIG or $0003;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_COUNT packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count   4B  (signed 32bit integer)
  end;)

  Immediate response to TN_PACKET_CONFIG_STORED_COUNT_GET.@br
  Contains number of stored configs.
}
  TN_PACKET_CONFIG_STORED_COUNT = TN_PREFIX_CONFIG or $0004;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_INDEX_GET packet id.)

  Sender: Client@br
  Packet: Extended
@preformatted(
  begin
    index   4B  (signed 32bit integer)
  end;)

  Instructs server to send config stored on position given by "index".
}
  TN_PACKET_CONFIG_STORED_INDEX_GET = TN_PREFIX_CONFIG or $0005;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_INDEX packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    list index    4B      (signed 32bit integer)
    name          String  (variable size)
    id            4B      (TConfigID)
    index         4B      (scs_u32_t)
    value type    4B      (scs_value_type_t)
    value         []B     (variable size)
    binded        1B      (boolean)
  end;)

  Immediate response to TN_PACKET_CONFIG_STORED_INDEX_GET.@br
  Contains information about and actual value of stored config on position given
  by "index" (in TN_PACKET_CONFIG_STORED_INDEX_GET packet).@br
  If "index" in TN_PACKET_CONFIG_STORED_INDEX_GET pointed to invalid position,
  then "list index" in this packet is set to -1, field "name" contains an empty
  string, "id" is set to 0, "index" is set to SCS_U32_NIL, "value type" is set
  to SCS_VALUE_TYPE_INVALID, "value" contains one scs_value_t structure with
  _type set to SCS_VALUE_TYPE_INVALID and "binded" is @false.
}
  TN_PACKET_CONFIG_STORED_INDEX = TN_PREFIX_CONFIG or $0006;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_INDEX_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all stored configs. They are immediately sent as
  a stream of TN_PACKET_CONFIG_STORED_INDEX packets.
}
  TN_PACKET_CONFIG_STORED_INDEX_ALL_GET = TN_PREFIX_CONFIG or $0007;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_ALL_GET packet id.)

  Sender: Client@br
  Packet: Basic

  Instructs server to send all stored configs. They are all sent in one
  TN_PACKET_CONFIG_STORED_ALL packet (see below).
}
  TN_PACKET_CONFIG_STORED_ALL_GET = TN_PREFIX_CONFIG or $0008;

//------------------------------------------------------------------------------

{
  @abstract(TN_PACKET_CONFIG_STORED_ALL packet id.)

  Sender: Server@br
  Packet: Extended
@preformatted(
  begin
    count       4B    (signed 32bit integer, number of info substructures)
    info        substructure[]
    begin
      name          String  (variable size)
      id            4B      (TConfigID)
      index         4B      (scs_u32_t)
      value type    4B      (scs_value_type_t)
      value         []B     (variable size)
      binded        1B      (boolean)
    end;
  end;)

  Immediate response to TN_PACKET_CONFIG_STORED_ALL_GET.@br
  Contains array of stored configs values.
}
  TN_PACKET_CONFIG_STORED_ALL = TN_PREFIX_CONFIG or $0009;

{------------------------------------------------------------------------------}
{    Game log communication packets                                            }
{------------------------------------------------------------------------------}

{
  @abstract(TN_PACKET_LOG_LOG packet id.)

  Sender: Server, Client@br
  Packet: Extended
@preformatted(
  begin
    log type  4B      (scs_log_type_t)
    log text  String  (variable size)
  end;)

  Server:@br
  Sent to all clients when recipient tries to write to game log.

  Client: @br
  Instructs server to write to game log using telemetry recipient.
}
  TN_PACKET_LOG_LOG = TN_PREFIX_LOG or $0001;


{===============================================================================
     Functions definitions
===============================================================================}

// Function to extracting prefix number from packet ID number.
Function GetPacketIDPrefix(PacketID: LongWord): LongWord;

// @abstract(Function used to obtain packet header from passed packet.)
// If packet is too small to contain actual valid header, then an exception
// is raised.
Function GetPacketHeader(PacketBuffer: TPacketBuffer): TPacketHeader;

implementation

uses
  SysUtils;

//------------------------------------------------------------------------------

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
  raise Exception.Create('TelemetryNetPacketsBuilding.GetPacketHeader: Packet size is too small.');
end;

end.
