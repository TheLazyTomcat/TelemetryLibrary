{*******************************************************************************
@abstract(This unit provides functions for packet building.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-12)
@lastmod(2013-10-12)

  TelemetryNetPacketsBuilding

  ©František Milt, all rights reserved.

  Main purpose of this unit is to provide functions that simplifies packets
  building.@br
  There are four function groups and few other (not grouped) functions.@br
  The groups are:
@unorderedList(
  @item(@bold(Ptr_* functions) - These functions are designed to provide
        simplified way of writing and reading variables to/from general memory
        location given by pointer. They are important in packet payload
        building.@br
        All have paramater of type boolean called "Advance". When this parameter
        is set to @true, passed destination/source pointer is increased by
        number of bytes written, otherwise this pointer is not changed.)
  @item(@bold(Stream_* functions) - These functions are designed to provide
        simplified way of writing and reading variables to/from streams. They
        are used in some more complex payload building operations - first the
        stream is build and then content of this stream is copied into packet as
        a whole.)
  @item(@bold(@noAutoLink(BuildPacket) functions) - Set of functions providing
        ready-to-be-sent packet buffers. Use them to create packets with zero or
        one parameter (refer to individual functions ford etails).)
  @item(@bold(BuildPacket_TN_PACKET_* functions) - These functions are designed
        to provide complete, ready-to-be-sent, packets of specific packet IDs.
        Each function identifier starts with @code(BuildPacket_) followed by
        appropriate packet ID identifier (packet of this ID is returned by such
        function).@br
        Each function has such parameters that are required to build appropriate
        packet (paramerers for individual functions are not documented as they
        should be self-explanatory). Refer to packet definition or function
        implementation for details.)
)

  Last change:  2013-10-12

  Change List:@unorderedList(
    @item(2013-10-12  - First stable version.))

*******************************************************************************}
unit TelemetryNetPacketsBuilding;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  Classes, ScktComp,
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryNetCommon,
  TelemetryNetHashing,
  TelemetryNetCircularBuffers,
  TelemetryNetSockets,
  TelemetryNetPackets,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}

{
  @abstract(Returns number of bytes required for given string to be stored in
  packet payload.)
  When an empty string is passed, only size of string length (UInt32 => 4Bytes)
  is returned.
}
Function SizeOfPacketString(Str: AnsiString = ''): Integer;

//------------------------------------------------------------------------------
{
  @abstract(Writes string to general memory location.)
  Strings are stored as two fields - 32bit signed integer containing length of
  string in AnsiChars (length of following array), followed by an array of
  AnsiChars (the string itself, without terminating #0 character). For exmaple,
  string "ABC.Z" will be store as:@br
  @preformatted(
  05 00 00 00 41 42 43 2E 5A
  |- length -|-- string ---|
  )
  
  @param Destination Memory location where to write. Must not be @nil.
  @param Str         String to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteString(var Destination: Pointer; Str: AnsiString; Advance: Boolean = True): Integer;

{
  @abstract(Reads string from general memory location.)
  Can return an empty string.

  @param Source   Memory location where to read. Must not be @nil.
  @param Str      Output string variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadString(var Source: Pointer; out Str: AnsiString; Advance: Boolean = True): Integer;

{
  Writes 32bit integer value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteInteger(var Destination: Pointer; Value: LongInt; Advance: Boolean = True): Integer;

{
  Reads 32bit integer value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadInteger(var Source: Pointer; out Value: LongInt; Advance: Boolean = True): Integer;

{
  Writes 64bit integer value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteInt64(var Destination: Pointer; Value: Int64; Advance: Boolean = True): Integer;

{
  Reads 64bit integer value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadInt64(var Source: Pointer; out Value: Int64; Advance: Boolean = True): Integer;

{
  Writes 32bit floating point value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteSingle(var Destination: Pointer; Value: Single; Advance: Boolean = True): Integer;

{
  Reads 32bit floating point value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadSingle(var Source: Pointer; out Value: Single; Advance: Boolean = True): Integer;

{
  Writes 64bit floating point value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteDouble(var Destination: Pointer; Value: Double; Advance: Boolean = True): Integer;

{
  Reads 64bit floating point value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadDouble(var Source: Pointer; out Value: Double; Advance: Boolean = True): Integer;

{
  Writes Boolean value (1byte) to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteBoolean(var Destination: Pointer; Value: Boolean; Advance: Boolean = True): Integer;

{
  Reads Boolean value (1byte) from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadBoolean(var Source: Pointer; out Value: Boolean; Advance: Boolean = True): Integer;

{
  Writes one byte to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteByte(var Destination: Pointer; Value: Byte; Advance: Boolean = True): Integer;

{
  Reads one byte from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadByte(var Source: Pointer; out Value: Byte; Advance: Boolean = True): Integer;

{
  Writes buffer to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Buffer      Buffer to be written.
  @param Size        Size of the buffer in bytes.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes written.
}
Function Ptr_WriteBuffer(var Destination: Pointer; const Buffer; Size: Integer; Advance: Boolean = True): Integer;

{
  Reads buffer from general memory location.

  @param Destination Memory location where to read. Must not be @nil.
  @param Buffer      Output buffer.
  @param Size        Number of bytes to be read.
  @param(Advance     Indicating whether source pointer should be increased
                     by number of bytes written.)
  @returns Number of bytes read.
}
Function Ptr_ReadBuffer(var Source: Pointer; var Buffer; Size: Integer; Advance: Boolean = True): Integer;

//------------------------------------------------------------------------------

{
  @abstract(Writes string into stream.)
  Strings are writen in the same manner as in Ptr_WriteString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.
  @returns Number of bytes written.
}
Function Stream_WriteString(Stream: TStream; Str: AnsiString): Integer;

{
  Reads string from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.
  @returns Number of bytes read.
}
Function Stream_ReadString(Stream: TStream; out Str: AnsiString): Integer;

{
  Writes 32bit integer value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.
  @returns Number of bytes written.
}
Function Stream_WriteInteger(Stream: TStream; Value: LongInt): Integer;

{
  Reads 32bit integer value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.
  @returns Number of bytes read.
}
Function Stream_ReadInteger(Stream: TStream; out Value: LongInt): Integer;

{
  Writes buffer into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Buffer Buffer to be written.
  @param Size   Size of the buffer in bytes.
  @returns Number of bytes written.
}
Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: Integer): Integer;

{
  Reads buffer from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Buffer Output buffer.
  @param Size   Number of bytes to be read.
  @returns Number of bytes read.
}
Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: Integer): Integer;

//------------------------------------------------------------------------------

{
  @abstract(Function used for packet preparation.)
  It expects the field "Size" in "Packet" parameter to be set to size the packet
  will have (if it is set to value lower than TN_MIN_PACKET_SIZE, then an
  exception is raised). "Data" field of "Packet" parameter must not be
  initialized, since it will be initialized in this function (to size passed in
  Packet.Size). Function also fills all information in packet header.

  @param Packet   Packet that has to be initialized in this routine.
  @param PacketID ID of initialized packet.

  @returns(Pointer that points to beginning of packet payload (when packet has
  no payload, it point to the end of packet).)
}
Function PreparePacket(var Packet: TPacketBuffer; PacketID: Integer): Pointer;

//------------------------------------------------------------------------------

{
  This funtions returns ready-to-be-send packet buffers for selected packet IDs.
  When packet ID, for which (one of) this function cannot provide buffer,
  is required, then an exception is raised.
  First function creates no-payload packets, second can be used to create packet
  containing one 4byte integer value (with typecasting, anything 4 bytes long
  can be passed).

  Note - use of these functions is optional, packets which are created by these
         functions can be created manually too.

}
{
  Returns complete initialized no-payload packet of given ID.

  @param PacketID ID of created packet.
  @returns Initialized ready-to-be-sent packet.
}
Function BuildPacket(PacketID: Integer): TPacketBuffer; overload;
{
  Creates complete initialized packet of given ID with payload consisting of one
  32bit integer value.

  @param PacketID ID of created packet.
  @param Data     Packet payload.
  @returns Initialized ready-to-be-sent packet.
}
Function BuildPacket(PacketID: Integer; Data: Integer): TPacketBuffer; overload;
{
  Creates complete initialized packet of given ID with payload consisting of one
  string value.

  @param PacketID ID of created packet.
  @param Data     Packet payload.
  @returns Initialized ready-to-be-sent packet.
}
Function BuildPacket(PacketID: Integer; Data: String): TPacketBuffer; overload;

//------------------------------------------------------------------------------

{
  Group of functions for building specific packets. 
}

// Common
{ Creates and returns complete TN_PACKET_PING packet.}
Function BuildPacket_TN_PACKET_PING: TPacketBuffer;
{ Creates and returns complete TN_PACKET_PING_RESPONSE packet.}
Function BuildPacket_TN_PACKET_PING_RESPONSE: TPacketBuffer;
{ Creates and returns complete TN_PACKET_HI packet.}
Function BuildPacket_TN_PACKET_HI(Password: String; HashType: THashType = htMD5): TPacketBuffer;
{ Creates and returns complete TN_PACKET_VERSION packet.}
Function BuildPacket_TN_PACKET_VERSION(ServerVersion: LongWord): TPacketBuffer;
{ Creates and returns complete TN_PACKET_TELEMETRY_VERSION_GET packet.}
Function BuildPacket_TN_PACKET_TELEMETRY_VERSION_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_TELEMETRY_VERSION packet.}
Function BuildPacket_TN_PACKET_TELEMETRY_VERSION(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
{ Creates and returns complete TN_PACKET_READY packet.}
Function BuildPacket_TN_PACKET_READY: TPacketBuffer;
{ Creates and returns complete TN_PACKET_PARAM_GET packet.}
Function BuildPacket_TN_PACKET_PARAM_GET(Parameter: TParameterID): TPacketBuffer;
{ Creates and returns complete TN_PACKET_PARAM packet.}
Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterSize: Integer; ParameterValue: Pointer): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_PARAM packet.}
Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: Boolean): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_PARAM packet.}
Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: Integer): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_PARAM packet.}
Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: String): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_BYE packet.}
Function BuildPacket_TN_PACKET_BYE(DisconnectReason: TDisconnectReason): TPacketBuffer;
{ Creates and returns complete TN_PACKET_MESSAGE packet.}
Function BuildPacket_TN_PACKET_MESSAGE(MessageText: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_PACKET packet.}
Function BuildPacket_TN_PACKET_PACKET(PayloadSize: LongWord; Payload: Pointer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_RIGHTS_ADMIN_REQUEST packet.}
Function BuildPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Password: String; HashType: THashType = htMD5): TPacketBuffer;
{ Creates and returns complete TN_PACKET_RIGHTS_ADMIN packet.}
Function BuildPacket_TN_PACKET_RIGHTS_ADMIN(Granted: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ERROR packet.}
Function BuildPacket_TN_PACKET_ERROR(ErroneousPacket: TPacketBuffer; ErrorType: TPacketErrorType; ErrorCode: LongWord = 0): TPacketBuffer;
{ Creates and returns complete TN_PACKET_RIGHTS_SUPERADMIN_REQUEST packet.}
Function BuildPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Password: String; HashType: THashType = htMD5): TPacketBuffer;
{ Creates and returns complete TN_PACKET_RIGHTS_SUPERADMIN packet.}
Function BuildPacket_TN_PACKET_RIGHTS_SUPERADMIN(Granted: Boolean): TPacketBuffer;

// Admin
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_CONNECTING packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Client: TNSCWinSocket): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_REJECTED packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Client: TNSCWinSocket; InList: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_CONNECTED packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Client: TNSCWinSocket): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_DISCONNECTED packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Client: TNSCWinSocket): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_CHANGE packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Client: TNSCWinSocket): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_OPERATION packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(ClientID: TGUID; Operation: TClientOperation): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_COUNT packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_INDEX packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Server: TServerSocket; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_ALL_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CLIENT_ALL packet.}
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_ALL(Server: TServerSocket): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_COUNT packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_INDEX packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX(List: TStrings; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_ALL_GET packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_ALL packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_ALL(List: TStrings): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_CLEAR packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_CLEAR: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_ADD packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_ADD(NewItem: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_REMOVE packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_REMOVE(Item: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_DELETE packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_DELETE(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_RELOAD packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_RELOAD: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_SAVE packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_SAVE: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_LIST_CHANGE packet.}
Function BuildPacket_TN_PACKET_ADMIN_LIST_CHANGE: TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_SEND_MESSAGE packet.}
Function BuildPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Client: TGUID; MessageText: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_PASSWORD packet.}
Function BuildPacket_TN_PACKET_ADMIN_PASSWORD(Password: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_CHANGE_PASSWORD packet.}
Function BuildPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Password: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_ADMIN_PARAM_SET packet.}
Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterSize: Integer; ParameterValue: Pointer): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_ADMIN_PARAM_SET packet.}
Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: Boolean): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_ADMIN_PARAM_SET packet.}
Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: Integer): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_ADMIN_PARAM_SET packet.}
Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: String): TPacketBuffer; overload;

// Superadmin
{ Creates and returns complete TN_PACKET_SUPERADMIN_ADMIN_PASSWORD packet.}
Function BuildPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Password: String): TPacketBuffer;
{ Creates and returns complete TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD packet.}
Function BuildPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Password: String): TPacketBuffer;


// Known events
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_COUNT packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_INDEX packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_ALL_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_KNOWN_ALL packet.}
Function BuildPacket_TN_PACKET_EVENT_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;

// Known channels
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_COUNT packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_INDEX packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_KNOWN_ALL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;

// Known configs
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_COUNT packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_INDEX packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_KNOWN_ALL packet.}
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;

// Events
{ Creates and returns complete TN_PACKET_EVENT_REG packet.}
Function BuildPacket_TN_PACKET_EVENT_REG(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REG_ALL packet.}
Function BuildPacket_TN_PACKET_EVENT_REG_ALL: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_UNREG packet.}
Function BuildPacket_TN_PACKET_EVENT_UNREG(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_UNREG_ALL packet.}
Function BuildPacket_TN_PACKET_EVENT_UNREG_ALL: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_EVENT packet.}
Function BuildPacket_TN_PACKET_EVENT_EVENT(Event: scs_event_t; DataSize: LongWord; Data: Pointer): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_EVENT_EVENT packet.}
Function BuildPacket_TN_PACKET_EVENT_EVENT(Event: scs_event_t; Data: p_scs_telemetry_configuration_t): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED(Event: scs_event_t; Registered: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_COUNT packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_INDEX packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_ALL_GET packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_EVENT_REGISTERED_ALL packet.}
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;

// Channels
{ Creates and returns complete TN_PACKET_CHANNEL_REG packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REG(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; ResultCode: scs_result_t): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REG_ALL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REG_ALL(RegPrimaryTypes,RegSecondaryTypes,RegTertiaryTypes: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_UNREG packet.}
Function BuildPacket_TN_PACKET_CHANNEL_UNREG(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; ResultCode: scs_result_t): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_UNREG_ALL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_UNREG_ALL: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_CHANNEL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name: String; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_CHANNEL_CHANNEL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer; overload;
{ Creates and returns complete TN_PACKET_CHANNEL_CHANNEL_BUFFERED packet.}
Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(BufferedChannels: TCircularChannelsBuffer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_COUNT packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_INDEX packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_REGISTERED_ALL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CHANNEL_STORED_SEND_ALL packet.}
Function BuildPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL: TPacketBuffer;

// Configs
{ Creates and returns complete TN_PACKET_CONFIG_CONFIG packet.}
Function BuildPacket_TN_PACKET_CONFIG_CONFIG(Name: String; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED(Name: String; Index: scs_u32_t; Stored: Boolean): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_COUNT_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_COUNT_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_COUNT packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_COUNT(Count: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_INDEX_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_INDEX packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_INDEX_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_ALL_GET packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET: TPacketBuffer;
{ Creates and returns complete TN_PACKET_CONFIG_STORED_ALL packet.}
Function BuildPacket_TN_PACKET_CONFIG_STORED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;

// Log
{ Creates and returns complete TN_PACKET_LOG_LOG packet.}
Function BuildPacket_TN_PACKET_LOG_LOG(LogType: scs_log_type_t; LogText: String): TPacketBuffer;

implementation

uses
  Windows, SysUtils;

//------------------------------------------------------------------------------

Function SizeOfPacketString(Str: AnsiString = ''): Integer;
begin
Result := SizeOf(Integer){string length} + Length(Str) * SizeOf(AnsiChar)
end;

//==============================================================================

Function Ptr_WriteString(var Destination: Pointer; Str: AnsiString; Advance: Boolean = True): Integer;
begin
PInteger(Destination)^ := Length(Str);
CopyMemory(Pointer(NativeInt(Destination) + SizeOf(Integer)),PAnsiChar(Str),
           Length(Str) * SizeOf(AnsiChar));
Result := SizeOf(Integer) + (Length(Str) * SizeOf(AnsiChar));
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadString(var Source: Pointer; out Str: AnsiString; Advance: Boolean = True): Integer;
begin
SetLength(Str,PInteger(Source)^ div SizeOf(AnsiChar));
CopyMemory(PAnsiChar(Str),Pointer(NativeInt(Source) + SizeOf(Integer)),
           Length(Str) * SizeOf(AnsiChar));
Result := SizeOf(Integer) + (Length(Str) * SizeOf(AnsiChar));
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteInteger(var Destination: Pointer; Value: LongInt; Advance: Boolean = True): Integer;
begin
PLongInt(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadInteger(var Source: Pointer; out Value: LongInt; Advance: Boolean = True): Integer;
begin
Value := PLongInt(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteInt64(var Destination: Pointer; Value: Int64; Advance: Boolean = True): Integer;
begin
PInt64(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadInt64(var Source: Pointer; out Value: Int64; Advance: Boolean = True): Integer;
begin
Value := PInt64(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteSingle(var Destination: Pointer; Value: Single; Advance: Boolean = True): Integer;
begin
PSingle(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadSingle(var Source: Pointer; out Value: Single; Advance: Boolean = True): Integer;
begin
Value := PSingle(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;
//------------------------------------------------------------------------------

Function Ptr_WriteDouble(var Destination: Pointer; Value: Double; Advance: Boolean = True): Integer;
begin
PDouble(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadDouble(var Source: Pointer; out Value: Double; Advance: Boolean = True): Integer;
begin
Value := PDouble(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteBoolean(var Destination: Pointer; Value: Boolean; Advance: Boolean = True): Integer;
begin
PBoolean(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadBoolean(var Source: Pointer; out Value: Boolean; Advance: Boolean = True): Integer;
begin
Value := PBoolean(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteByte(var Destination: Pointer; Value: Byte; Advance: Boolean = True): Integer;
begin
PByte(Destination)^ := Value;
Result := SizeOf(Value);
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadByte(var Source: Pointer; out Value: Byte; Advance: Boolean = True): Integer;
begin
Value := PByte(Source)^;
Result := SizeOf(Value);
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteBuffer(var Destination: Pointer; const Buffer; Size: Integer; Advance: Boolean = True): Integer;
begin
CopyMemory(Destination,@Buffer,Size);
Result := Size;
If Advance then Destination := Pointer(NativeInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadBuffer(var Source: Pointer; var Buffer; Size: Integer; Advance: Boolean = True): Integer;
begin
CopyMemory(@Buffer,Source,Size);
Result := Size;
If Advance then Source := Pointer(NativeInt(Source) + Result);
end;

//==============================================================================

Function Stream_WriteString(Stream: TStream; Str: AnsiString): Integer;
var
  StringBytes: Integer;
begin
Result := 0;
StringBytes := Length(Str) * SizeOf(AnsiChar);
Inc(Result,Stream.Write(StringBytes,SizeOf(StringBytes)));
Inc(Result,Stream.Write(PAnsiChar(Str)^,StringBytes));
end;

//------------------------------------------------------------------------------

Function Stream_ReadString(Stream: TStream; out Str: AnsiString): Integer;
var
  StringBytes: Integer;
begin
Result := 0;
Inc(Result,Stream.Read(StringBytes,SizeOf(StringBytes)));
SetLength(Str,StringBytes div SizeOf(AnsiChar));
Inc(Result,Stream.Read(PAnsiChar(Str)^,StringBytes));
end;

//------------------------------------------------------------------------------

Function Stream_WriteInteger(Stream: TStream; Value: LongInt): Integer;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadInteger(Stream: TStream; out Value: LongInt): Integer;
begin
Result := Stream.Read(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: Integer): Integer;
begin
Result := Stream.Write(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: Integer): Integer;
begin
Result := Stream.Read(Buffer,Size);
end;

//==============================================================================

Function PreparePacket(var Packet: TPacketBuffer; PacketID: Integer): Pointer;
begin
If Packet.Size >= TN_MIN_PACKET_SIZE then
  begin
    Packet.Data := AllocMem(Packet.Size);
    PPacketHeader(Packet.Data)^.Signature := TN_PACKET_SIGNATURE;
    PPacketHeader(Packet.Data)^.PacketID := PacketID;
    PPacketHeader(Packet.Data)^.TimeStamp := Now;
    PPacketHeader(Packet.Data)^.PayloadSize := Packet.Size - SizeOf(TPacketHeader);
    Result := Pointer(NativeInt(Packet.Data) + SizeOf(TPacketHeader));
  end
else
  raise Exception.Create('TelemetryNetPacketsBuilding.PreparePacket: Packet size is too small.');
end;

//==============================================================================

Function BuildPacket(PacketID: Integer): TPacketBuffer;
begin
Result.Size := SizeOf(TPacketHeader);
PreparePacket(Result,PacketID);
end;

Function BuildPacket(PacketID: Integer; Data: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Data);
CurrPtr := PreparePacket(Result,PacketID);
Ptr_WriteInteger(CurrPtr,Data);
end;

Function BuildPacket(PacketID: Integer; Data: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOfPacketString(Data);
CurrPtr := PreparePacket(Result,PacketID);
Ptr_WriteString(CurrPtr,Data);
end;

//==============================================================================

{------------------------------------------------------------------------------}
{    Common                                                                    }
{------------------------------------------------------------------------------}

Function BuildPacket_TN_PACKET_PING: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_PING);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_PING_RESPONSE: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_PING_RESPONSE);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_HI structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
Function BuildPacket_TN_PACKET_HI(Password: String; HashType: THashType = htMD5): TPacketBuffer;
var
  CurrPtr:      Pointer;
  TempCRC32:    TCRC32Hash;
  TempMD5Hash:  TMD5Hash;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){hash type};
case HashType of
  htCRC32:  Inc(Result.Size,SizeOf(TCRC32Hash));
  htMD5:    Inc(Result.Size,SizeOf(TMD5Hash));
end;
CurrPtr := PreparePacket(Result,TN_PACKET_HI);
Ptr_WriteInteger(CurrPtr,Integer(HashType));
case HashType of
  htCRC32:  begin
              TempCRC32 := HashCRC32(Password);
              Ptr_WriteBuffer(CurrPtr,TempCRC32,SizeOf(TempCRC32));
            end;
  htMD5:    begin
              TempMD5Hash := HashMD5(Password);
              Ptr_WriteBuffer(CurrPtr,TempMD5Hash,SizeOf(TempMD5Hash));
            end;
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_VERSION structure:
//  begin
//    server version    4B  (unsigned 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_VERSION(ServerVersion: LongWord): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_VERSION,ServerVersion);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_TELEMETRY_VERSION_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_TELEMETRY_VERSION_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_TELEMETRY_VERSION structure:
//  begin
//    telemetry version 4B      (scs_u32_t)
//    game ID           String  (variable size)
//    game version      4B      (scs_u32_t)
//    game name         String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_TELEMETRY_VERSION(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_u32_t){telemetry version} +
               SizeOfPacketString(TelemetryRecipient.GameID){game ID} +
               SizeOf(scs_u32_t){game version} +
               SizeOfPacketString(TelemetryRecipient.GameName){game name};
CurrPtr := PreparePacket(Result,TN_PACKET_TELEMETRY_VERSION);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.TelemetryVersion);
Ptr_WriteString(CurrPtr,TelemetryRecipient.GameID);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.GameVersion);
Ptr_WriteString(CurrPtr,TelemetryRecipient.GameName);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_READY: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_READY);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM_GET structure:
//  begin
//    parameter id    4B  (TParameterID)
//  end;
Function BuildPacket_TN_PACKET_PARAM_GET(Parameter: TParameterID): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_PARAM_GET,Integer(Parameter));
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM structure:
//  begin
//    parameter id    4B  (TParameterID)
//    data            []  (variable size, can have zero size)
//  end;
Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterSize: Integer; ParameterValue: Pointer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){parameter id} +
               ParameterSize;
CurrPtr := PreparePacket(Result,TN_PACKET_PARAM);
Ptr_WriteInteger(CurrPtr,Integer(Parameter));
Ptr_WriteBuffer(CurrPtr,ParameterValue^,ParameterSize);
end;

Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: Boolean): TPacketBuffer;
begin
Result := BuildPacket_TN_PACKET_PARAM(Parameter, SizeOf(Boolean), @ParameterValue);
end;

Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: Integer): TPacketBuffer;
begin
Result := BuildPacket_TN_PACKET_PARAM(Parameter, SizeOf(Integer), @ParameterValue);
end;

Function BuildPacket_TN_PACKET_PARAM(Parameter: TParameterID; ParameterValue: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){parameter id} +
               SizeOfPacketString(ParameterValue){data};
CurrPtr := PreparePacket(Result,TN_PACKET_PARAM);
Ptr_WriteInteger(CurrPtr,Integer(Parameter));
Ptr_WriteString(CurrPtr,ParameterValue);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_BYE structure:
//  begin
//    disconnection reason  4B  (TDisconnectReason)
//  end;
Function BuildPacket_TN_PACKET_BYE(DisconnectReason: TDisconnectReason): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_BYE,Integer(DisconnectReason));
end;

//------------------------------------------------------------------------------

//  TN_PACKET_MESSAGE structure:
//  begin
//    message text    String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_MESSAGE(MessageText: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_MESSAGE,MessageText);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PACKET structure:
//  begin
//    payload   []  (data carried by packet, can be empty)
//  end;
Function BuildPacket_TN_PACKET_PACKET(PayloadSize: LongWord; Payload: Pointer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + PayloadSize;
CurrPtr := PreparePacket(Result,TN_PACKET_PACKET);
Ptr_WriteBuffer(CurrPtr,Payload^,PayloadSize);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_REQUEST_ADMIN_RIGHTS structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
Function BuildPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Password: String; HashType: THashType = htMD5): TPacketBuffer;
var
  CurrPtr:      Pointer;
  TempCRC32:    TCRC32Hash;
  TempMD5Hash:  TMD5Hash;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){hash type};
case HashType of
  htCRC32:  Inc(Result.Size,SizeOf(TCRC32Hash));
  htMD5:    Inc(Result.Size,SizeOf(TMD5Hash));
end;
CurrPtr := PreparePacket(Result,TN_PACKET_RIGHTS_ADMIN_REQUEST);
Ptr_WriteInteger(CurrPtr,Integer(HashType));
case HashType of
  htCRC32:  begin
              TempCRC32 := HashCRC32(Password);
              Ptr_WriteBuffer(CurrPtr,TempCRC32,SizeOf(TempCRC32));
            end;
  htMD5:    begin
              TempMD5Hash := HashMD5(Password);
              Ptr_WriteBuffer(CurrPtr,TempMD5Hash,SizeOf(TempMD5Hash));
            end;
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_RIGHTS structure:
//  begin
//    granted   1B    (boolean)
//  end;
Function BuildPacket_TN_PACKET_RIGHTS_ADMIN(Granted: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Boolean){granted};
CurrPtr := PreparePacket(Result,TN_PACKET_RIGHTS_ADMIN);
Ptr_WriteBoolean(CurrPtr,Granted);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PACKET_ERROR structure:
//  begin
//    packet id           4B  (unsigned 32bit integer)
//    packet time stamp   8B  (TDateTime)
//    error type          4B  (TPacketErrorType)
//    error code          4B  (unsigned 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ERROR(ErroneousPacket: TPacketBuffer; ErrorType: TPacketErrorType; ErrorCode: LongWord = 0): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(LongWord){packet id} +
               SizeOf(TDateTime){packet time stamp} +
               SizeOf(Integer){error type} +
               SizeOf(LongWord){error code};
CurrPtr := PreparePacket(Result,TN_PACKET_ERROR);
Ptr_WriteInteger(CurrPtr,GetPacketHeader(ErroneousPacket).PacketID);
Ptr_WriteDouble(CurrPtr,GetPacketHeader(ErroneousPacket).TimeStamp);
Ptr_WriteInteger(CurrPtr,Integer(ErrorType));
Ptr_WriteInteger(CurrPtr,ErrorCode);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_REQUEST_SUPERADMIN_RIGHTS structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
Function BuildPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Password: String; HashType: THashType = htMD5): TPacketBuffer;
var
  CurrPtr:      Pointer;
  TempCRC32:    TCRC32Hash;
  TempMD5Hash:  TMD5Hash;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){hash type};
case HashType of
  htCRC32:  Inc(Result.Size,SizeOf(TCRC32Hash));
  htMD5:    Inc(Result.Size,SizeOf(TMD5Hash));
end;
CurrPtr := PreparePacket(Result,TN_PACKET_RIGHTS_SUPERADMIN_REQUEST);
Ptr_WriteInteger(CurrPtr,Integer(HashType));
case HashType of
  htCRC32:  begin
              TempCRC32 := HashCRC32(Password);
              Ptr_WriteBuffer(CurrPtr,TempCRC32,SizeOf(TempCRC32));
            end;
  htMD5:    begin
              TempMD5Hash := HashMD5(Password);
              Ptr_WriteBuffer(CurrPtr,TempMD5Hash,SizeOf(TempMD5Hash));
            end;
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_SUPERADMIN_RIGHTS structure:
//  begin
//    granted   1B    (boolean)
//  end;
Function BuildPacket_TN_PACKET_RIGHTS_SUPERADMIN(Granted: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Boolean){granted};
CurrPtr := PreparePacket(Result,TN_PACKET_RIGHTS_SUPERADMIN);
Ptr_WriteBoolean(CurrPtr,Granted);
end;

{------------------------------------------------------------------------------}
{    Admin                                                                     }
{------------------------------------------------------------------------------}

//  TN_PACKET_ADMIN_CLIENT_CONNECTING structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Client: TNSCWinSocket): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOfPacketString(Client.RemoteAddress){client IP} +
               SizeOf(Integer){client port};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_CONNECTING);
Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
Ptr_WriteString(CurrPtr,Client.RemoteAddress);
Ptr_WriteInteger(CurrPtr,Client.RemotePort);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_REJECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//    in list       1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Client: TNSCWinSocket; InList: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOfPacketString(Client.RemoteAddress){client IP} +
               SizeOf(Integer){client port} +
               SizeOf(Boolean){InList};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_CONNECTING);
Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
Ptr_WriteString(CurrPtr,Client.RemoteAddress);
Ptr_WriteInteger(CurrPtr,Client.RemotePort);
Ptr_WriteBoolean(CurrPtr,InList);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_CONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Client: TNSCWinSocket): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOfPacketString(Client.RemoteAddress){client IP} +
               SizeOf(Integer){client port};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_CONNECTED);
Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
Ptr_WriteString(CurrPtr,Client.RemoteAddress);
Ptr_WriteInteger(CurrPtr,Client.RemotePort);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_DISCONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Client: TNSCWinSocket): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOfPacketString(Client.RemoteAddress){client IP} +
               SizeOf(Integer){client port};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_DISCONNECTED);
Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
Ptr_WriteString(CurrPtr,Client.RemoteAddress);
Ptr_WriteInteger(CurrPtr,Client.RemotePort);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_CHANGE structure:
//  begin
//    client id             16B     (TGUID)
//    client IP             String  (variable size)
//    client port           4B      (unsigned 32bit integer)
//    waiting for password  1B      (boolean)
//    ready to work         1B      (boolean)
//    admin rights          1B      (boolean)
//    super-admin rights    1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Client: TNSCWinSocket): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOfPacketString(Client.RemoteAddress){client IP} +
               SizeOf(Integer){client port} +
               4 * SizeOf(Boolean){statuses};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_CHANGE);
Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
Ptr_WriteString(CurrPtr,Client.RemoteAddress);
Ptr_WriteInteger(CurrPtr,Client.RemotePort);
Ptr_WriteBoolean(CurrPtr,Client.WaitingForPassword);
Ptr_WriteBoolean(CurrPtr,Client.ReadyToWork);
Ptr_WriteBoolean(CurrPtr,Client.AdminRights);
Ptr_WriteBoolean(CurrPtr,Client.SuperAdminRights);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_OPERATION structure:
//  begin
//    client id   16B     (TGUID)
//    operation    4B     (TClientOperation)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(ClientID: TGUID; Operation: TClientOperation): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client id} +
               SizeOf(Integer){operation};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_OPERATION);
Ptr_WriteBuffer(CurrPtr,ClientID,SizeOf(ClientID));
Ptr_WriteInteger(CurrPtr,Integer(Operation));
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CLIENT_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CLIENT_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CLIENT_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_INDEX structure:
//  begin
//    index                 4B      (signed 32bit integer)
//    client id             16B     (TGUID)
//    client IP             String  (variable size)
//    client port           4B      (unsigned 32bit integer)
//    waiting for password  1B      (boolean)
//    ready to work         1B      (boolean)
//    admin rights          1B      (boolean)
//    super-admin rights    1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Server: TServerSocket; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
  Client:   TNSCWinSocket;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index} +
               SizeOf(TGUID){client id} +
               SizeOf(Integer){client port} +
               4 * SizeOf(Boolean){statuses};
If (Index >= 0) and (Index < Server.Socket.ActiveConnections) then
  begin
    // Index is valid.
    Client := TNSCWinSocket(Server.Socket.Connections[Index]);
    Inc(Result.Size,SizeOfPacketString(Client.RemoteAddress){client IP});
    CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
    Ptr_WriteString(CurrPtr,Client.RemoteAddress);
    Ptr_WriteInteger(CurrPtr,Client.RemotePort);
    Ptr_WriteBoolean(CurrPtr,Client.WaitingForPassword);
    Ptr_WriteBoolean(CurrPtr,Client.ReadyToWork);
    Ptr_WriteBoolean(CurrPtr,Client.AdminRights);
    Ptr_WriteBoolean(CurrPtr,Client.SuperAdminRights);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString(''){client IP});
    CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteBuffer(CurrPtr,cEmptyGUID,SizeOf(cEmptyGUID));
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteBoolean(CurrPtr,False);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CLIENT_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      client id             16B     (TGUID)
//      client IP             String  (variable size)
//      client port           4B      (unsigned 32bit integer)
//      waiting for password  1B      (boolean)
//      ready to work         1B      (boolean)
//      admin rights          1B      (boolean)
//      super-admin rights    1B      (boolean)
//    end;
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CLIENT_ALL(Server: TServerSocket): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
  Client:   TNSCWinSocket;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size for all clients.
For i := 0 to (Server.Socket.ActiveConnections - 1) do
  begin
    Client := TNSCWinSocket(Server.Socket.Connections[i]);
    Inc(Result.Size,SizeOf(TGUID){client id} +
                    SizeOfPacketString(Client.RemoteAddress){client IP} +
                    SizeOf(Integer){client port} +
                    4 * SizeOf(Boolean){statuses});
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_CLIENT_ALL);
Ptr_WriteInteger(CurrPtr,Server.Socket.ActiveConnections);
// Fill info about all clients.
For i := 0 to (Server.Socket.ActiveConnections - 1) do
  begin
    Client := TNSCWinSocket(Server.Socket.Connections[i]);
    Ptr_WriteBuffer(CurrPtr,Client.UniqueIdentificator,SizeOf(Client.UniqueIdentificator));
    Ptr_WriteString(CurrPtr,Client.RemoteAddress);
    Ptr_WriteInteger(CurrPtr,Client.RemotePort);
    Ptr_WriteBoolean(CurrPtr,Client.WaitingForPassword);
    Ptr_WriteBoolean(CurrPtr,Client.ReadyToWork);
    Ptr_WriteBoolean(CurrPtr,Client.AdminRights);
    Ptr_WriteBoolean(CurrPtr,Client.SuperAdminRights);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX structure:
//  begin
//    index   4B      (signed 32bit integer)
//    item    String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX(List: TStrings; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index};
If (Index >= 0) and (Index < List.Count) then
  begin
    // Index is valid.
    Inc(Result.Size,SizeOfPacketString(List[Index]){item});
    CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_LIST_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteString(CurrPtr,List[Index]);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString(''){item});
    CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_LIST_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteString(CurrPtr,'');
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ALL structure:
//  begin
//    count     4B        (signed 32bit integer, number of "item" fields)
//    item      String[]  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_ALL(List: TStrings): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size for all items.
For i := 0 to (List.Count - 1) do
  Inc(Result.Size,SizeOfPacketString(List[i]){item});
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_LIST_ALL);
Ptr_WriteInteger(CurrPtr,List.Count);
// Fill all items.
For i := 0 to (List.Count - 1) do
  Ptr_WriteString(CurrPtr,List[i]);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_CLEAR: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_CLEAR);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ADD structure:
//  begin
//    item  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_ADD(NewItem: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_ADD,NewItem);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_REMOVE structure:
//  begin
//    item  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_REMOVE(Item: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_REMOVE,Item);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_DELETE structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_LIST_DELETE(Index: Integer): TPacketBuffer;
begin
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_DELETE,Index);
end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_RELOAD: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_RELOAD);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_SAVE: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_SAVE);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_ADMIN_LIST_CHANGE: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_LIST_CHANGE);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_SEND_MESSAGE structure:
//  begin
//    client    16B     (TGUID)
//    message   String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Client: TGUID; MessageText: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(TGUID){client} +
               SizeOfPacketString(MessageText){message};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_SEND_MESSAGE);
Ptr_WriteBuffer(CurrPtr,Client,SizeOf(Client));
Ptr_WriteString(CurrPtr,MessageText);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_PASSWORD(Password: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_PASSWORD,Password);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CHANGE_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Password: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_ADMIN_CHANGE_PASSWORD,Password);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PARAM_SET structure:
//  begin
//    parameter id    4B  (TParameterID)
//    value           []  (variable size)
//  end;
Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterSize: Integer; ParameterValue: Pointer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){parameter id} +
               ParameterSize;
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_PARAM_SET);
Ptr_WriteInteger(CurrPtr,Integer(Parameter));
Ptr_WriteBuffer(CurrPtr,ParameterValue^,ParameterSize);
end;

Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: Boolean): TPacketBuffer;
begin
Result := BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter, SizeOf(Boolean), @ParameterValue);
end;

Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: Integer): TPacketBuffer;
begin
Result := BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter, SizeOf(Integer), @ParameterValue);
end;

Function BuildPacket_TN_PACKET_ADMIN_PARAM_SET(Parameter: TParameterID; ParameterValue: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){parameter id} +
               SizeOfPacketString(ParameterValue){data};
CurrPtr := PreparePacket(Result,TN_PACKET_ADMIN_PARAM_SET);
Ptr_WriteInteger(CurrPtr,Integer(Parameter));
Ptr_WriteString(CurrPtr,ParameterValue);
end;

{------------------------------------------------------------------------------}
{    Superadmin                                                                }
{------------------------------------------------------------------------------}

//  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD  structure:
//  begin
//    password  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Password: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_SUPERADMIN_ADMIN_PASSWORD,Password);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Password: String): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD,Password);
end;

{------------------------------------------------------------------------------}
{    Known events                                                              }
{------------------------------------------------------------------------------}

Function BuildPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_KNOWN_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_EVENT_KNOWN_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_KNOWN_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end
Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_KNOWN_COUNT,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_INDEX structure:
//  begin
//    index   4B      (signed 32bit integer)
//    event   4B      (scs_event_t)
//    name    String  (variable size)
//    valid   1B      (boolean)
//    utility 1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:    Pointer;
  EventInfo:  TKnownEvent;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index} +
               SizeOf(scs_event_t){event} +
               SizeOf(Boolean){valid} +
               SizeOf(Boolean){utility};
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownEvents.Count) then
  begin
    // Index is valid.
    EventInfo := TelemetryInfoProvider.KnownEvents[Index];
    Inc(Result.Size,SizeOfPacketString(EventInfo.Name){name});
    CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteInteger(CurrPtr,EventInfo.Event);
    Ptr_WriteString(CurrPtr,EventInfo.Name);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Valid);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Utility);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString{name});
    CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteInteger(CurrPtr,SCS_TELEMETRY_EVENT_invalid);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteBoolean(CurrPtr,False);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_KNOWN_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_KNOWN_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      event     4B      (scs_event_t)
//      name      String  (variable size)
//      valid     1B      (boolean)
//      utility   1B      (boolean)
//    end;
//  end;
Function BuildPacket_TN_PACKET_EVENT_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:          Integer;
  CurrPtr:    Pointer;
  EventInfo:  TKnownEvent;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer) {count};
// Get required size for all known events.
For i := 0 to (TelemetryInfoProvider.KnownEvents.Count - 1) do
  Inc(Result.Size, SizeOf(scs_event_t){event} +
                   SizeOfPacketString(TelemetryInfoProvider.KnownEvents[i].Name){name} +
                   SizeOf(Boolean){valid} +
                   SizeOf(Boolean){utility});
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_KNOWN_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownEvents.Count);
// Fill info about all known events.
For i := 0 to (TelemetryInfoProvider.KnownEvents.Count - 1) do
  begin
    EventInfo := TelemetryInfoProvider.KnownEvents[i];
    Ptr_WriteInteger(CurrPtr,EventInfo.Event);
    Ptr_WriteString(CurrPtr,EventInfo.Name);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Valid);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Utility);
  end;
end;

{------------------------------------------------------------------------------}
{    Known channels                                                            }
{------------------------------------------------------------------------------}

Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_KNOWN_COUNT_GET)
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_KNOWN_COUNT,Count)
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_KNOWN_INDEX_GET, Index)
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_INDEX structure:
//  begin
//    index             4B      (signed 32bit integer)
//    name              String  (variable size)
//    id                4B      (TChannelID)
//    primary type      4B      (scs_value_type_t)
//    secondary type    4B      (scs_value_type_t)
//    tertiary type     4B      (scs_value_type_t)
//    indexed           1B      (boolean)
//    index config      String  (variable size)
//    index config id   4B      (TItemID)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:      Pointer;
  ChannelInfo:  TKnownChannel;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index} +
               SizeOf(TChannelID){id} +
               SizeOf(scs_value_type_t){primary value type} +
               SizeOf(scs_value_type_t){secondary value type} +
               SizeOf(scs_value_type_t){tertiary value type} +
               SizeOf(Boolean){indexed} +
               SizeOf(TItemID){index config id};
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownChannels.Count) then
  begin
    // Index is valid.
    ChannelInfo := TelemetryInfoProvider.KnownChannels[Index];
    Inc(Result.Size,SizeOfPacketString(ChannelInfo.Name){name} +
                    SizeOfPacketString(ChannelInfo.IndexConfig){index config});
    CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteString(CurrPtr,ChannelInfo.Name);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ID);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.PrimaryType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.SecondaryType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.TertiaryType);
    Ptr_WriteBoolean(CurrPtr,ChannelInfo.Indexed);
    Ptr_WriteString(CurrPtr,ChannelInfo.IndexConfig);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.IndexConfigID);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,2 * SizeOfPacketString{three empty string values});
    CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET)
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_KNOWN_ALL_GET)
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      name:             String  (variable size)
//      id                4B      (TChannelID)
//      primary type      4B      (scs_value_type_t)
//      secondary type    4B      (scs_value_type_t)
//      tertiary type     4B      (scs_value_type_t)
//      indexed:          1B      (boolean)
//      index config      String  (variable size)
//      index config id   4B      (TItemID)
//      value config      String  (variable size)
//      value config id   4B      (TItemID)
//    end;
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:            Integer;
  CurrPtr:      Pointer;
  ChannelInfo:  TKnownChannel;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size for all known channels.
For i := 0 to (TelemetryInfoProvider.KnownChannels.Count - 1) do
  begin
    ChannelInfo := TelemetryInfoProvider.KnownChannels[i];
    Inc(Result.Size,SizeOfPacketString(ChannelInfo.Name){name} +
                    SizeOf(TChannelID){id} +
                    SizeOf(scs_value_type_t){primary value type} +
                    SizeOf(scs_value_type_t){secondary value type} +
                    SizeOf(scs_value_type_t){tertiary value type} +
                    SizeOf(Boolean){indexed} +
                    SizeOfPacketString(ChannelInfo.IndexConfig){index config} +
                    SizeOf(TItemID){index config id});
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_KNOWN_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownChannels.Count);
// Fill info about all known channels.
For i := 0 to (TelemetryInfoProvider.KnownChannels.Count - 1) do
  begin
    ChannelInfo := TelemetryInfoProvider.KnownChannels[i];
    Ptr_WriteString(CurrPtr,ChannelInfo.Name);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ID);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.PrimaryType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.SecondaryType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.TertiaryType);
    Ptr_WriteBoolean(CurrPtr,ChannelInfo.Indexed);
    Ptr_WriteString(CurrPtr,ChannelInfo.IndexConfig);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.IndexConfigID);
  end;
end;

{------------------------------------------------------------------------------}
{    Known configs                                                             }
{------------------------------------------------------------------------------}

Function BuildPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_KNOWN_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_KNOWN_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_COUNT structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_KNOWN_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_INDEX structure:
//  begin
//    index         4B      (signed 32bit integer)
//    name          String  (variable size)
//    id            4B      (TConfigID)
//    value type    4B      (scs_value_type_t)
//    indexed       1B      (boolean)
//    binded        1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:    Pointer;
  ConfigInfo: TKnownConfig;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index} +
               SizeOf(TConfigID){id} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(Boolean){indexed} +
               SizeOf(Boolean){binded};
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownConfigs.Count) then
  begin
    // Index is valid.
    ConfigInfo := TelemetryInfoProvider.KnownConfigs[Index];
    Inc(Result.Size,SizeOfPacketString(ConfigInfo.Name){name});
    CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteString(CurrPtr,ConfigInfo.Name);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ID);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ValueType);
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Indexed);
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Binded);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString{empty name});
    CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_KNOWN_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteBoolean(CurrPtr,False);
    Ptr_WriteBoolean(CurrPtr,False);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_KNOWN_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      name:         String  (variable size)
//      id            4B      (TConfigID)
//      value type:   4B      (scs_value_type_t)
//      indexed:      1B      (boolean)
//      binded:       1B      (boolean)
//    end;
//  end;
Function BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:          Integer;
  CurrPtr:    Pointer;
  ConfigInfo: TKnownConfig;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size for all known configs.
For i := 0 to (TelemetryInfoProvider.KnownConfigs.Count - 1) do
  begin
    ConfigInfo := TelemetryInfoProvider.KnownConfigs[i];
    Inc(Result.Size,SizeOfPacketString(ConfigInfo.Name){name} +
                    SizeOf(TConfigID){id} +
                    SizeOf(scs_value_type_t){value type} +
                    SizeOf(Boolean){indexed} +
                    SizeOf(Boolean){binded});
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_KNOWN_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownConfigs.Count);
// Fill info about all known configs.
For i := 0 to (TelemetryInfoProvider.KnownConfigs.Count - 1) do
  begin
    ConfigInfo := TelemetryInfoProvider.KnownConfigs[i];
    Ptr_WriteString(CurrPtr,ConfigInfo.Name);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ID);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ValueType);
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Indexed);
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Binded);
  end;
end;

{------------------------------------------------------------------------------}
{    Events                                                                    }
{------------------------------------------------------------------------------}

//  TN_PACKET_EVENT_REG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
Function BuildPAcket_TN_PACKET_EVENT_REG(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){event} +
               SizeOf(scs_result_t){result};
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_REG);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_REG_ALL: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REG_ALL);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_UNREG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
Function BuildPAcket_TN_PACKET_EVENT_UNREG(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){event} +
               SizeOf(scs_result_t){result};
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_UNREG);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_UNREG_ALL: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_UNREG_ALL);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_EVENT structure:
//  begin
//    event       4B  (scs_event_t)
//    data size   4B  (unsigned 32bit integer)
//    data        []  (variable size, can be 0)
//  end;
Function BuildPacket_TN_PACKET_EVENT_EVENT(Event: scs_event_t; DataSize: LongWord; Data: Pointer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If not Assigned(Data) then DataSize := 0;
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){event} +
               SizeOf(LongWord){data size} +
               DataSize{data};
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_EVENT);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteInteger(CurrPtr,DataSize);
If DataSize > 0 then Ptr_WriteBuffer(CurrPtr,Data^,DataSize);
end;

//  SCS_TELEMETRY_EVENT_configuration packet payload structure: 
//    begin
//      id            String          (variable size)
//      named value   substructure[]  (array of named values)
//      begin
//        name          String          (variable size)
//        index         4B              (scs_u32_t)
//        value type    4B              (scs_value_type_t)
//        value         []              (variable size data, actual size depends
//                                       on value type)
//      end;
//      0x00000000      4B            (array ends with an empty string)
//    end;
Function BuildPacket_TN_PACKET_EVENT_EVENT(Event: scs_event_t; Data: p_scs_telemetry_configuration_t): TPacketBuffer; overload;
var
  TempStream:   TMemoryStream;
  TempValue:    p_scs_named_value_t;
begin
TempStream := TMemoryStream.Create;
try
  Stream_WriteString(TempStream,APIStringToTelemetryString(Data^.id));
  TempValue := Data^.attributes;
  while Assigned(TempValue^.name) do
    begin
      Stream_WriteString(TempStream,APIStringToTelemetryString(TempValue^.name));
      Stream_WriteInteger(TempStream,TempValue^.index);
      Stream_WriteInteger(TempStream,TempValue^.value._type);
      case TempValue^.value._type of
        SCS_VALUE_TYPE_string:
          Stream_WriteString(TempStream,APIStringToTelemetryString(TempValue^.value.value_string.value));
      else
        TempStream.WriteBuffer(TempValue^.value,SizeOf(TempValue^.value));
      end;
      Inc(TempValue);
    end;
  Stream_WriteString(TempStream,'');
  // Create event packet from stream.
  Result := BuildPacket_TN_PACKET_EVENT_EVENT(Event,TempStream.Size,TempStream.Memory);
finally
  TempStream.Free;
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED structure:
//  begin
//    event       4B  (scs_event_t)
//    registered  1B  (boolean)
//  end;
Function BuildPacket_TN_PACKET_EVENT_REGISTERED(Event: scs_event_t; Registered: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){event} +
               SizeOf(Boolean){registered};
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_REGISTERED);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteBoolean(CurrPtr,Registered);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REGISTERED_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REGISTERED_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REGISTERED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX structure:
//  begin
//    index   4B  (signed 32bit integer)
//    event   4B  (scs_event_t)
//    utility 1B  (boolean)
//  end
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:    Pointer;
  EventInfo:  TEventInfo;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){index} +
               SizeOf(scs_event_t){event} +
               SizeOf(Boolean){utility};
If (Index >= 0) and (Index < TelemetryRecipient.RegisteredEvents.Count) then
  begin
    // Index is valid.
    EventInfo := TelemetryRecipient.RegisteredEvents[Index];
    CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteInteger(CurrPtr,EventInfo.Event);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Utility);
  end
else
  begin
    // Index is NOT valid.
    CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteInteger(CurrPtr,SCS_TELEMETRY_EVENT_invalid);
    Ptr_WriteBoolean(CurrPtr,False);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_EVENT_REGISTERED_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_ALL structure:
//  begin
//    count     4B    (signed 32bit integer, number of info substructures)
//    info  substructure[]
//    begin
//      event     4B    (scs_event_t)
//      utility   1B    (boolean)
//    end;
//  end;
Function BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:          Integer;
  CurrPtr:    Pointer;
  EventInfo:  TEventInfo;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size to store all registered events.
For i := 0 to (TelemetryRecipient.RegisteredEvents.Count - 1) do
  Inc(Result.Size,SizeOf(scs_event_t){event} +
                  SizeOf(Boolean){utility});
CurrPtr := PreparePacket(Result,TN_PACKET_EVENT_REGISTERED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.RegisteredEvents.Count);
// Fill info about all registered events.
For i := 0 to (TelemetryRecipient.RegisteredEvents.Count - 1) do
  begin
    EventInfo := TelemetryRecipient.RegisteredEvents[i];
    Ptr_WriteInteger(CurrPtr,EventInfo.Event);
    Ptr_WriteBoolean(CurrPtr,EventInfo.Utility);
  end;
end;

{------------------------------------------------------------------------------}
{    Channels                                                                  }
{------------------------------------------------------------------------------}

//  TN_PACKET_CHANNEL_REG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    flags         4B      (scs_u32_t)
//    result        4B      (scs_result_t)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REG(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(scs_u32_t){flags} +
               SizeOf(scs_result_t){result};
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REG);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteInteger(CurrPtr,Flags);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REG_ALL structure:
//  begin
//    primary types     1B  (boolean)
//    secondary types   1B  (boolean)
//    tertiary types    1B  (boolean)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REG_ALL(RegPrimaryTypes,RegSecondaryTypes,RegTertiaryTypes: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               3 * SizeOf(Boolean){Primary,Secondary,Tertiary};
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REG_ALL);
Ptr_WriteBoolean(CurrPtr,RegPrimaryTypes);
Ptr_WriteBoolean(CurrPtr,RegSecondaryTypes);
Ptr_WriteBoolean(CurrPtr,RegTertiaryTypes);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_UNREG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    result        4B      (scs_result_t)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_UNREG(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(scs_result_t){result};
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_UNREG);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_UNREG_ALL: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_UNREG_ALL);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_CHANNEL structure:
//  begin
//    name        String  (variable size)
//    id          4B      (TChannelID)
//    index       4B      (scs_u32_t)
//    value type  4B      (scs_value_type_t)
//    value       []      (variable size data, actual size depends on value type)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name: String; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(TChannelID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type};
// Add true size of value.
If Assigned(Value) then
  case Value^._type of
    SCS_VALUE_TYPE_string:
      Inc(Result.Size,SizeOfPacketString(APIStringToTelemetryString(Value^.value_string.value)));
  else
    Inc(Result.Size,SizeOf(scs_value_t));
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_CHANNEL);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,ID);
Ptr_WriteInteger(CurrPtr,Index);
If Assigned(Value) then Ptr_WriteInteger(CurrPtr,Value^._type)
  else Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
If Assigned(Value) then
  case Value^._type of
    SCS_VALUE_TYPE_string:
      Ptr_WriteString(CurrPtr,APIStringToTelemetryString(Value^.value_string.value));
  else
    Ptr_WriteBuffer(CurrPtr,Value^,SizeOf(scs_value_t));
  end
else
  Ptr_WriteBuffer(CurrPtr,cEmptySCSValue,SizeOf(scs_value_t));
end;

Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name: String; ID: TChannelID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(TChannelID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type};
// Add true size of value.
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    Inc(Result.Size,SizeOfPacketString(Value.StringData));
else
  Inc(Result.Size,SizeOf(Value.BinaryData));
end;
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_CHANNEL);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,ID);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,Value.ValueType);
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    Ptr_WriteString(CurrPtr,Value.StringData);
else
  Ptr_WriteBuffer(CurrPtr,Value.BinaryData,SizeOf(Value.BinaryData));
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_CHANNEL_BUFFERED structure:
//  begin
//    count   4B  (signed 32bit integer, number of info substructures)
//    info    substructure[]
//    begin
//      name        String  (variable size)
//      id          4B      (TChannelID)
//      index       4B      (scs_u32_t)
//      value type  4B      (scs_value_type_t)
//      value       []      (variable size data, actual size depends on value type)
//    end;
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(BufferedChannels: TCircularChannelsBuffer): TPacketBuffer;
var
  ChannelInfo:  TBufferedChannel;
  TempStream:   TMemoryStream;
  Counter:      Integer;
  CurrPtr:      Pointer;
begin
TempStream := TMemoryStream.Create;
try
  Counter := 0;
  Stream_WriteInteger(TempStream,Counter);
  while BufferedChannels.ContainsChannel do
    begin
      ChannelInfo := BufferedChannels.PeekChannel;
      Stream_WriteString(TempStream,ChannelInfo.Name);
      Stream_WriteInteger(TempStream,ChannelInfo.ID);
      Stream_WriteInteger(TempStream,ChannelInfo.Index);
      Stream_WriteInteger(TempStream,ChannelInfo.Value.ValueType);
      case ChannelInfo.Value.ValueType of
        SCS_VALUE_TYPE_string:
          Stream_WriteString(TempStream,ChannelInfo.Value.StringData);
      else
        Stream_WriteBuffer(TempStream,ChannelInfo.Value.BinaryData,SizeOf(ChannelInfo.Value.BinaryData));
      end;
      Inc(Counter);
      BufferedChannels.RemoveChannel;
    end;
    TempStream.Seek(0,soFromBeginning);
    Stream_WriteInteger(TempStream,Counter);
    // Write stream content to packet payload.
    Result.Size := SizeOf(TPacketHeader) + TempStream.Size{packet payload};
    CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_CHANNEL_BUFFERED);
    Ptr_WriteBuffer(CurrPtr,TempStream.Memory^,TempStream.Size);
finally
  TempStream.Free;
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    registered    1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(boolean){registered};
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REGISTERED);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteBoolean(CurrPtr,Registered);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_REGISTERED_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_REGISTERED_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_REGISTERED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_INDEX structure:
//  begin
//    list index        4B      (signed 32bit integer)
//    name              String  (variable size)
//    id                4B      (TChannelID)
//    index             4B      (scs_u32_t)
//    value type        4B      (scs_value_type_t)
//    flags             4B      (scs_u32_t)
//    index config id   4B      (TItemID)
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){list index} +
               SizeOf(TChannelID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(scs_u32_t){flags} +
               SizeOf(TItemID){index config id};;
If (Index >= 0) and (Index < TelemetryRecipient.RegisteredChannels.Count) then
  begin
    // Index is valid.
    ChannelInfo := TelemetryRecipient.RegisteredChannels[Index];
    Inc(Result.Size,SizeOfPacketString(ChannelInfo.Name){name});
    CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteString(CurrPtr,ChannelInfo.Name);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ID);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.Index);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ValueType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.Flags);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.IndexConfigID);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString(''){name});
    CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
    Ptr_WriteInteger(CurrPtr,Integer(SCS_U32_NIL));
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteInteger(CurrPtr,SCS_TELEMETRY_CHANNEL_FLAG_none);
    Ptr_WriteInteger(CurrPtr,0);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_REGISTERED_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_ALL structure:
//  begin
//    count       4B    (signed 32bit integer, number of info substructures)
//    info        substructure[]
//    begin
//      name              String  (variable size)
//      id                4B      (TChannelID)
//      index             4B      (scs_u32_t)
//      value type        4B      (scs_value_type_t)
//      flags             4B      (scs_u32_t)
//      index config id   4B      (TItemID)
//    end;
//  end;
Function BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:            Integer;
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size to store all registered channels.
For i := 0 to (TelemetryRecipient.RegisteredChannels.Count - 1) do
  begin
    ChannelInfo := TelemetryRecipient.RegisteredChannels[i];
    Inc(Result.Size,SizeOfPacketString(ChannelInfo.Name){name} +
                    SizeOf(TChannelID){id} +
                    SizeOf(scs_u32_t){index} +
                    SizeOf(scs_value_type_t){value type} +
                    SizeOf(scs_u32_t){flags} +
                    SizeOf(TItemID){index config id});;
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_CHANNEL_REGISTERED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.RegisteredChannels.Count);
// Fill info about all registered channels.
For i := 0 to (TelemetryRecipient.RegisteredChannels.Count - 1) do
  begin
    ChannelInfo := TelemetryRecipient.RegisteredChannels[i];
    Ptr_WriteString(CurrPtr,ChannelInfo.Name);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ID);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.Index);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ValueType);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.Flags);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.IndexConfigID);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CHANNEL_STORED_SEND_ALL);
end;


{------------------------------------------------------------------------------}
{    Configs                                                                   }
{------------------------------------------------------------------------------}

//  TN_PACKET_CONFIG_CONFIG structure:
//  begin
//    name        String  (variable size)
//    id          4B      (TConfigID)
//    index       4B      (scs_u32_t)
//    value type  4B      (scs_value_type_t)
//    value       []      (variable size data, actual size depends on value type)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_CONFIG(Name: String; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(TConfigID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type};
// Add true size of value.
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    Inc(Result.Size,SizeOfPacketString(Value.StringData));
else
  Inc(Result.Size,SizeOf(scs_value_t));
end;
CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_CONFIG);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,ID);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,Value.ValueType);
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    Ptr_WriteString(CurrPtr,Value.StringData);
else
  Ptr_WriteBuffer(CurrPtr,Value.BinaryData,SizeOf(Value.BinaryData));
end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED structure:
//  begin
//    name      String  (variable size)
//    index     4B      (scs_u32_t)
//    stored    1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_STORED(Name: String; Index: scs_u32_t; Stored: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfPacketString(Name){name} +
               SizeOf(Boolean){stored};
CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_STORED);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteBoolean(CurrPtr,Stored);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CONFIG_STORED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_STORED_COUNT_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_STORED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_STORED_COUNT,Count);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_STORED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_INDEX structure:
//  begin
//    list index    4B      (signed 32bit integer)
//    name          String  (variable size)
//    id            4B      (TConfigID)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    value         []B     (variable size)
//    binded        1B      (boolean)
//  end;
Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:    Pointer;
  ConfigInfo: TStoredConfig;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){list index} +
               SizeOf(TConfigID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(Boolean){binded};
If (Index >= 0) and (Index < TelemetryRecipient.StoredConfigs.Count - 1) then
  begin
    // Index is valid.
    ConfigInfo := TelemetryRecipient.StoredConfigs[Index];
    Inc(Result.Size,SizeOfPacketString(ConfigInfo.Name){name});
    case ConfigInfo.Value.ValueType of
      SCS_VALUE_TYPE_string:
        Inc(Result.Size,SizeOfPacketString(ConfigInfo.Value.StringData){string value});
    else
      Inc(Result.Size,SizeOf(scs_value_t){binary value});
    end;
    CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_STORED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_WriteString(CurrPtr,ConfigInfo.Name);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ID);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.Index);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.Value.ValueType);
    case ConfigInfo.Value.ValueType of
      SCS_VALUE_TYPE_string:  Ptr_WriteString(CurrPtr,ConfigInfo.Value.StringData)
    else
      Ptr_WriteBuffer(CurrPtr,ConfigInfo.Value.BinaryData,SizeOf(ConfigInfo.Value.BinaryData));
    end;
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Binded);
  end
else
  begin
    // Index is NOT valid.
    Inc(Result.Size,SizeOfPacketString(''){name} +
                    SizeOf(scs_value_t){empty binary value});
    CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_STORED_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_WriteString(CurrPtr,'');
    Ptr_WriteInteger(CurrPtr,0);
    Ptr_WriteInteger(CurrPtr,Integer(SCS_U32_NIL));
    Ptr_WriteInteger(CurrPtr,SCS_VALUE_TYPE_INVALID);
    Ptr_WriteBuffer(CurrPtr,cEmptySCSValue,SizeOf(scs_value_t));
    Ptr_WriteBoolean(CurrPtr,False);
  end;
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_STORED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TN_PACKET_CONFIG_STORED_ALL_GET);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_ALL structure:
//  begin
//    count       4B    (signed 32bit integer, number of info substructures)
//    info        substructure[]
//    begin
//      name          String  (variable size)
//      id            4B      (TConfigID)
//      index         4B      (scs_u32_t)
//      value type    4B      (scs_value_type_t)
//      value         []B     (variable size)
//      binded        1B      (boolean)
//    end;
//  end;
Function BuildPacket_TN_PACKET_CONFIG_STORED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:          Integer;
  CurrPtr:    Pointer;
  ConfigInfo: TStoredConfig;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){count};
// Get required size for all stored configs.
For i := 0 to (TelemetryRecipient.StoredConfigs.Count - 1) do
  begin
    ConfigInfo := TelemetryRecipient.StoredConfigs[i];
    Inc(Result.Size,SizeOfPacketString(ConfigInfo.Name){name} +
                    SizeOf(TConfigID){id} +
                    SizeOf(scs_u32_t){index} +
                    SizeOf(scs_value_type_t){value type} +
                    SizeOf(Boolean){binded});
    case ConfigInfo.Value.ValueType of
      SCS_VALUE_TYPE_string:
        Inc(Result.Size,SizeOfPacketString(ConfigInfo.Value.StringData){binary value});
    else
      Inc(Result.Size,SizeOf(scs_value_t){binary value});
    end;
  end;
CurrPtr := PreparePacket(Result,TN_PACKET_CONFIG_STORED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.StoredConfigs.Count);
For i := 0 to (TelemetryRecipient.StoredConfigs.Count - 1) do
  begin
    ConfigInfo := TelemetryRecipient.StoredConfigs[i];
    Ptr_WriteString(CurrPtr,ConfigInfo.Name);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.ID);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.Index);
    Ptr_WriteInteger(CurrPtr,ConfigInfo.Value.ValueType);
    case ConfigInfo.Value.ValueType of
      SCS_VALUE_TYPE_string:  Ptr_WriteString(CurrPtr,ConfigInfo.Value.StringData);
    else
      Ptr_WriteBuffer(CurrPtr,ConfigInfo.Value.BinaryData,SizeOf(ConfigInfo.Value.BinaryData));
    end;
    Ptr_WriteBoolean(CurrPtr,ConfigInfo.Binded);
  end;
end;

{------------------------------------------------------------------------------}
{    Log                                                                       }
{------------------------------------------------------------------------------}

//  TN_PACKET_LOG_LOG structure:
//  begin
//    log type  4B      (scs_log_type_t)
//    log text  String  (variable size)
//  end;
Function BuildPacket_TN_PACKET_LOG_LOG(LogType: scs_log_type_t; LogText: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_log_type_t){log type} +
               SizeOfPacketString(LogText){log text};
CurrPtr := PreparePacket(Result,TN_PACKET_LOG_LOG);
Ptr_WriteInteger(CurrPtr,LogType);
Ptr_WriteString(CurrPtr,LogText);
end;

end.
