{*******************************************************************************
@abstract(This unit provides functions for packet reading and processing.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-13)
@lastmod(2013-10-13)

  TelemetryNetPacketsResolving

  ©František Milt, all rights reserved.

  This unit is designated to provide functions that simplifies packets reading
  and processing.@br
  There are three function groups and few other (not grouped) functions.@br
  The groups are:
@unorderedList(
  @item(@noAutoLink(@bold(ReusePacket functions)) - Functions intended to
        provide a way to reuse allready created packets (reducimg memory
        allocation). For example, you can change ID of just received packet and
        send it back as response.)
  @item(@bold(Ptr_Readout* functions) - Functions designed to provide simple way
        of reading variables from general memory location given by pointer. Read
        value is returned as function result.)
  @item(@bold(Payload_Readout* functions) - These functions read value from
        payload of given packet. They all have paramter "Offset", which tells
        the function offset in bytes from the beginning of packet payload at
        which function should start reading. When the offset is not specified,
        function reads right at the beginning of payload.) 
)

  Last change:  2013-10-13

  Change List:@unorderedList(
    @item(2013-10-13  - First stable version.))

*******************************************************************************}
unit TelemetryNetPacketsResolving;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryNetPackets,
{$IFDEF Documentation}
  TelemetryRecipientAux,
  TelemetryNetPacketsBuilding,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value;
{$ENDIF}


{
  Returns address of payload in memory image of given packet.
  @param Packet Packet from which the payload address is requested.
  @returns Address of payload of given packet.
}
Function GetPayloadAddress(Packet: TPacketBuffer): Pointer;

{
  @abstract(Returns minimal packet payload size.)
  Each packets payload must have minimal size given by its structure. This
  function returns this size for each packet type - if unknown packet type ID is
  passed, an exception is raised.@br
  This function should be used to check received packet before further
  processing.@br
  For actual values, refer to function implementation.
  @param PacketID ID of packet type for ehich the minimum size is required.
  @returns Minimal size of payload for given packet ID.
}
Function MinimalPacketPayloadSize(PacketID: LongWord): Integer;

{
  Checks whether passed packet payload is large enough for its type id.
  @param Packet Packet whose payload size will be checked.
  @returns(@True when packet payload is large enough, @false when it is too
           small.)
}
Function CheckPacketPayloadSize(Packet: TPacketBuffer): Boolean;

//------------------------------------------------------------------------------

{
  Use to change packets ID and reuse it.
  @param Packet      Packet whose id should be changed.
  @param NewPacketID New packet ID to be written to passed packet.
}
procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: LongWord); overload;

//------------------------------------------------------------------------------

{
  @abstract(Use to change packets ID and store new data to it.)
  Data are store at position given by offset from packet payload beginning. When
  data would not fit into packet, an exception is raised.
  @param Packet      Packet to be changed.
  @param NewPacketID New packet ID to be written to passed packet.
  @param DataOffset  Position offset from payload beginning where to write data.
  @param DataSize    Size of data to be written.
  @param Data        Data to be writen to packet.
}
procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: LongWord; DataOffset, DataSize: Integer; Data: Pointer); overload;

//------------------------------------------------------------------------------

{
  Reads string from general memory location given by pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read string.
}
Function Ptr_ReadoutString(var Source: Pointer; Advance: Boolean = True): String;

{
  Reads 32bit integer value from general memory location given by pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read integer value.
}
Function Ptr_ReadoutInteger(var Source: Pointer; Advance: Boolean = True): LongInt;

{
  Reads 64bit integer value from general memory location given by pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read integer value.
}
Function Ptr_ReadoutInt64(var Source: Pointer; Advance: Boolean = True): Int64;

{
  Reads 32bit floating point value from general memory location given by
  pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read floating point value.
}
Function Ptr_ReadoutSingle(var Source: Pointer; Advance: Boolean = True): Single;

{
  Reads 64bit floating point value from general memory location given by
  pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read floating point value.
}
Function Ptr_ReadoutDouble(var Source: Pointer; Advance: Boolean = True): Double;

{
  Reads boolean value (one byte) from general memory location given by pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read boolean value.
}
Function Ptr_ReadoutBoolean(var Source: Pointer; Advance: Boolean = True): Boolean;

{
  Reads one byte from general memory location given by pointer.
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)
  @returns Read byte value.
}
Function Ptr_ReadoutByte(var Source: Pointer; Advance: Boolean = True): Byte;

//------------------------------------------------------------------------------

{
  Reads string from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read string.
}
Function Payload_ReadoutString(const Packet: TPacketBuffer; Offset: Integer = 0): String;

{
  Reads 32bit integer value from packet payload.
  @param Packet Packet from whitch the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read integer value.
}
Function Payload_ReadoutInteger(const Packet: TPacketBuffer; Offset: Integer = 0): LongInt;

{
  Reads 64bit integer value from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read integer value.
}
Function Payload_ReadoutInt64(const Packet: TPacketBuffer; Offset: Integer = 0): Int64;

{
  Reads 32bit floating point value from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read floating point value.
}
Function Payload_ReadoutSingle(const Packet: TPacketBuffer; Offset: Integer = 0): Single;

{
  Reads 64bit floating point value from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read floating point value.
}
Function Payload_ReadoutDouble(const Packet: TPacketBuffer; Offset: Integer = 0): Double;

{
  Reads boolean value (one byte) from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read boolean value.
}
Function Payload_ReadoutBoolean(const Packet: TPacketBuffer; Offset: Integer = 0): Boolean;

{
  Reads one byte from packet payload.
  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.
  @returns Read byte value.
}
Function Payload_ReadoutByte(const Packet: TPacketBuffer; Offset: Integer = 0): Byte;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of data in TN_PACKET_EVENT_EVENT
  packet for SCS_TELEMETRY_EVENT_configuration event.)
  It is intended as replacement for function
  TelemetryRecipientAux.TelemetryEventConfigurationToStr, because structure of
  payload in mentioned packet differs form structure passed by configuration
  event.

  @param Data            Pointer to packet payload.
  @param(Type name       When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fileds descriptors are shown in component
                         values.)

  @returns Textual representation of given packet payload.
}
Function PacketEventConfigurationToStr(const Data: Pointer; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;

implementation

uses
  Windows, SysUtils,
  TelemetryStrings, TelemetryNetPacketsBuilding;

//------------------------------------------------------------------------------

Function GetPayloadAddress(Packet: TPacketBuffer): Pointer;
begin
Result := Pointer(NativeInt(Packet.Data) + SizeOf(TPacketHeader));
end;

//------------------------------------------------------------------------------

Function MinimalPacketPayloadSize(PacketID: LongWord): Integer;
begin
case PacketID of
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Replace SizeOf(Integer) with sizes of appropriate types.'}
{$ENDIF}
  // TN_PREFIX_COMMON prefix.
  TN_PACKET_PING:                       Result := 0;
  TN_PACKET_PING_RESPONSE:              Result := 0;
  TN_PACKET_HI:                         Result := Succ(SizeOf(Integer));
  TN_PACKET_VERSION:                    Result := SizeOf(Integer);
  TN_PACKET_TELEMETRY_VERSION_GET:      Result := 0;
  TN_PACKET_TELEMETRY_VERSION:          Result := 2 * SizeOf(scs_u32_t) + 2 * SizeOfPacketString;
  TN_PACKET_READY:                      Result := 0;
  TN_PACKET_PARAM_GET:                  Result := SizeOf(Integer);
  TN_PACKET_PARAM:                      Result := Succ(SizeOf(Integer));
  TN_PACKET_BYE:                        Result := SizeOf(Integer);
  TN_PACKET_MESSAGE:                    Result := SizeOfPacketString;
  TN_PACKET_PACKET:                     Result := 0;
  TN_PACKET_RIGHTS_ADMIN_REQUEST:       Result := Succ(SizeOf(Integer));
  TN_PACKET_RIGHTS_ADMIN:               Result := SizeOf(Boolean);
  TN_PACKET_ERROR:                      Result := 3 * SizeOf(Integer) + SizeOf(TDateTime);
  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST:  Result := Succ(SizeOf(Integer));
  TN_PACKET_RIGHTS_SUPERADMIN:          Result := SizeOf(Boolean);
  
  // TN_PREFIX_ADMIN prefix.
  TN_PACKET_ADMIN_CLIENT_CONNECTING:      Result := SizeOf(TGUID) + SizeOfPacketString + SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_REJECTED:        Result := SizeOf(TGUID) + SizeOfPacketString + SizeOf(Integer) + SizeOf(Boolean);
  TN_PACKET_ADMIN_CLIENT_CONNECTED:       Result := SizeOf(TGUID) + SizeOfPacketString + SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_DISCONNECTED:    Result := SizeOf(TGUID) + SizeOfPacketString + SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_CHANGE:          Result := SizeOf(TGUID) + SizeOfPacketString + SizeOf(Integer) + 4 * SizeOf(Boolean);
  TN_PACKET_ADMIN_CLIENT_OPERATION:       Result := SizeOf(TGUID) + SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_COUNT_GET:       Result := 0;
  TN_PACKET_ADMIN_CLIENT_COUNT:           Result := SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_INDEX_GET:       Result := SizeOf(Integer);
  TN_PACKET_ADMIN_CLIENT_INDEX:           Result := 2 * SizeOf(Integer) + SizeOf(TGUID) + SizeOfPacketString + 4 * SizeOf(Boolean);
  TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET:   Result := 0;
  TN_PACKET_ADMIN_CLIENT_ALL_GET:         Result := 0;
  TN_PACKET_ADMIN_CLIENT_ALL:             Result := SizeOf(Integer);
  TN_PACKET_ADMIN_LIST_COUNT_GET:         Result := 0;
  TN_PACKET_ADMIN_LIST_COUNT:             Result := SizeOf(Integer);
  TN_PACKET_ADMIN_LIST_INDEX_GET:         Result := SizeOf(Integer);
  TN_PACKET_ADMIN_LIST_INDEX:             Result := SizeOf(Integer) + SizeOfPacketString;
  TN_PACKET_ADMIN_LIST_INDEX_ALL_GET:     Result := 0;
  TN_PACKET_ADMIN_LIST_ALL_GET:           Result := 0;
  TN_PACKET_ADMIN_LIST_ALL:               Result := SizeOf(Integer);
  TN_PACKET_ADMIN_LIST_CLEAR:             Result := 0;
  TN_PACKET_ADMIN_LIST_ADD:               Result := SizeOfPacketString;
  TN_PACKET_ADMIN_LIST_REMOVE:            Result := SizeOfPacketString;
  TN_PACKET_ADMIN_LIST_DELETE:            Result := SizeOf(Integer);
  TN_PACKET_ADMIN_LIST_RELOAD:            Result := 0;
  TN_PACKET_ADMIN_LIST_SAVE:              Result := 0;
  TN_PACKET_ADMIN_LIST_CHANGE:            Result := 0;
  TN_PACKET_ADMIN_SEND_MESSAGE:           Result := SizeOf(TGUID) + SizeOfPacketString;
  TN_PACKET_ADMIN_PASSWORD:               Result := SizeOfPacketString;
  TN_PACKET_ADMIN_CHANGE_PASSWORD:        Result := SizeOfPacketString;
  TN_PACKET_ADMIN_PARAM_SET:              Result := Succ(SizeOf(Integer));

  // TN_PREFIX_SUPERADMIN prefix.  
  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD:        Result := SizeOfPacketString;
  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD: Result := SizeOfPacketString;

  // TN_PREFIX_EVENT_KNOWN prefix.
  TN_PACKET_EVENT_KNOWN_COUNT_GET:      Result := 0;
  TN_PACKET_EVENT_KNOWN_COUNT:          Result := SizeOf(Integer);
  TN_PACKET_EVENT_KNOWN_INDEX_GET:      Result := SizeOf(Integer);
  TN_PACKET_EVENT_KNOWN_INDEX:          Result := 2 * SizeOf(Integer) + SizeOfPacketString + 2 * SizeOf(Boolean);
  TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET:  Result := 0;
  TN_PACKET_EVENT_KNOWN_ALL_GET:        Result := 0;
  TN_PACKET_EVENT_KNOWN_ALL:            Result := SizeOf(Integer);

  // TN_PREFIX_CHANNEL_KNOWN prefix.
  TN_PACKET_CHANNEL_KNOWN_COUNT_GET:      Result := 0;
  TN_PACKET_CHANNEL_KNOWN_COUNT:          Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_KNOWN_INDEX_GET:      Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_KNOWN_INDEX:          Result := 6 * SizeOf(Integer) + 2 * SizeOfPacketString + SizeOf(Boolean);
  TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET:  Result := 0;
  TN_PACKET_CHANNEL_KNOWN_ALL_GET:        Result := 0;
  TN_PACKET_CHANNEL_KNOWN_ALL:            Result := SizeOf(Integer);

  // TN_PREFIX_CONFIG_KNOWN prefix.
  TN_PACKET_CONFIG_KNOWN_COUNT_GET:     Result := 0;
  TN_PACKET_CONFIG_KNOWN_COUNT:         Result := SizeOf(Integer);
  TN_PACKET_CONFIG_KNOWN_INDEX_GET:     Result := SizeOf(Integer);
  TN_PACKET_CONFIG_KNOWN_INDEX:         Result := 3 * SizeOf(Integer) + SizeOfPacketString + 2 * SizeOf(Boolean);
  TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET: Result := 0;
  TN_PACKET_CONFIG_KNOWN_ALL_GET:       Result := 0;
  TN_PACKET_CONFIG_KNOWN_ALL:           Result := SizeOf(Integer);

  // TN_PREFIX_EVENT prefix.
  TN_PACKET_EVENT_REG:                      Result := 2 * SizeOf(Integer);
  TN_PACKET_EVENT_REG_ALL:                  Result := 0;
  TN_PACKET_EVENT_UNREG:                    Result := 2 * SizeOf(Integer);
  TN_PACKET_EVENT_UNREG_ALL:                Result := 0;
  TN_PACKET_EVENT_EVENT:                    Result := 2 * SizeOf(Integer);
  TN_PACKET_EVENT_REGISTERED:               Result := SizeOf(Integer) + SizeOf(Boolean);
  TN_PACKET_EVENT_REGISTERED_COUNT_GET:     Result := 0;
  TN_PACKET_EVENT_REGISTERED_COUNT:         Result := SizeOf(Integer);
  TN_PACKET_EVENT_REGISTERED_INDEX_GET:     Result := SizeOf(Integer);
  TN_PACKET_EVENT_REGISTERED_INDEX:         Result := 2 * SizeOf(Integer) + SizeOf(Boolean);
  TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: Result := 0;
  TN_PACKET_EVENT_REGISTERED_ALL_GET:       Result := 0;
  TN_PACKET_EVENT_REGISTERED_ALL:           Result := SizeOf(Integer);

  // TN_PREFIX_CHANNEL prefix.
  TN_PACKET_CHANNEL_REG:                      Result := SizeOfPacketString + 4 * SizeOf(Integer);
  TN_PACKET_CHANNEL_REG_ALL:                  Result := 3 * SizeOf(Boolean);
  TN_PACKET_CHANNEL_UNREG:                    Result := SizeOfPacketString + 3 * SizeOf(Integer);
  TN_PACKET_CHANNEL_UNREG_ALL:                Result := 0;
  TN_PACKET_CHANNEL_CHANNEL:                  Result := Succ(SizeOfPacketString + 3 * SizeOf(Integer));
  TN_PACKET_CHANNEL_CHANNEL_BUFFERED:         Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_REGISTERED:               Result := SizeOfPacketString + 2 * SizeOf(Integer) + SizeOf(Boolean);
  TN_PACKET_CHANNEL_REGISTERED_COUNT_GET:     Result := 0;
  TN_PACKET_CHANNEL_REGISTERED_COUNT:         Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET:     Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_REGISTERED_INDEX:         Result := 6 * SizeOf(Integer) + SizeOfPacketString;
  TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: Result := 0;
  TN_PACKET_CHANNEL_REGISTERED_ALL_GET:       Result := 0;
  TN_PACKET_CHANNEL_REGISTERED_ALL:           Result := SizeOf(Integer);
  TN_PACKET_CHANNEL_STORED_SEND_ALL:          Result := 0;

  // TN_PREFIX_CONFIG prefix.
  TN_PACKET_CONFIG_CONFIG:                Result := Succ(SizeOfPacketString + 3 * SizeOf(Integer));
  TN_PACKET_CONFIG_STORED:                Result := SizeOfPacketString + SizeOf(Boolean);
  TN_PACKET_CONFIG_STORED_COUNT_GET:      Result := 0;
  TN_PACKET_CONFIG_STORED_COUNT:          Result := SizeOf(Integer);
  TN_PACKET_CONFIG_STORED_INDEX_GET:      Result := SizeOf(Integer);
  TN_PACKET_CONFIG_STORED_INDEX:          Result := Succ(4 * SizeOf(Integer) + SizeOfPacketString + SizeOf(Boolean));
  TN_PACKET_CONFIG_STORED_INDEX_ALL_GET:  Result := 0;
  TN_PACKET_CONFIG_STORED_ALL_GET:        Result := 0;
  TN_PACKET_CONFIG_STORED_ALL:            Result := SizeOf(Integer);

  // TN_PREFIX_LOG prefix.
  TN_PACKET_LOG_LOG:  Result := SizeOfPacketString;
else
  raise Exception.Create('TelemetryNetPacketsResolving.MinimalPacketPayloadSize: Unknown packet (0x' +
                         IntToHex(PacketID,8) + ').');
end;
end;

//------------------------------------------------------------------------------

Function CheckPacketPayloadSize(Packet: TPacketBuffer): Boolean;
begin
Result := GetPacketHeader(Packet).PayloadSize >=
          MinimalPacketPayloadSize(GetPacketHeader(Packet).PacketID);
end;

//==============================================================================

procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: LongWord);
begin
PPacketHeader(Packet.Data)^.PacketID := NewPacketID;
PPacketHeader(Packet.Data)^.TimeStamp := Now;
end;

//------------------------------------------------------------------------------

procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: LongWord; DataOffset, DataSize: Integer; Data: Pointer);
begin
If (DataOffset + DataSize) <= GetPacketHeader(Packet).PayloadSize then
  begin
    PPacketHeader(Packet.Data)^.PacketID := NewPacketID;
    PPacketHeader(Packet.Data)^.TimeStamp := Now;
    CopyMemory(Pointer(NativeInt(GetPayloadAddress(Packet)) + DataOffset),Data,DataSize);
  end
else
  raise Exception.Create('TelemetryNetPacketsResolving.ReusePacket(Data): Data cannot fit into reused packet.');
end;

//==============================================================================

Function Ptr_ReadoutString(var Source: Pointer; Advance: Boolean = True): String;
begin
Ptr_ReadString(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutInteger(var Source: Pointer; Advance: Boolean = True): LongInt;
begin
Ptr_ReadInteger(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutInt64(var Source: Pointer; Advance: Boolean = True): Int64;
begin
Ptr_ReadInt64(Source,Result,Advance);
end;

Function Ptr_ReadoutSingle(var Source: Pointer; Advance: Boolean = True): Single;
begin
Ptr_ReadSingle(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutDouble(var Source: Pointer; Advance: Boolean = True): Double;
begin
Ptr_ReadDouble(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutBoolean(var Source: Pointer; Advance: Boolean = True): Boolean;
begin
Ptr_ReadBoolean(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutByte(var Source: Pointer; Advance: Boolean = True): Byte;
begin
Ptr_ReadByte(Source,Result,Advance);
end;

//==============================================================================

Function Payload_ReadoutString(const Packet: TPacketBuffer; Offset: Integer = 0): String;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutString(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutInteger(const Packet: TPacketBuffer; Offset: Integer = 0): LongInt;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutInteger(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutInt64(const Packet: TPacketBuffer; Offset: Integer = 0): Int64;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutInt64(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutSingle(const Packet: TPacketBuffer; Offset: Integer = 0): Single;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutSingle(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutDouble(const Packet: TPacketBuffer; Offset: Integer = 0): Double;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutDouble(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutBoolean(const Packet: TPacketBuffer; Offset: Integer = 0): Boolean;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutBoolean(TempPtr,False);
end;

//------------------------------------------------------------------------------

Function Payload_ReadoutByte(const Packet: TPacketBuffer; Offset: Integer = 0): Byte;
var
  TempPtr:  Pointer;
begin
TempPtr := Pointer(NativeInt(GetPayloadAddress(Packet)) + Offset);
Result := Ptr_ReadoutByte(TempPtr,False);
end;

//==============================================================================

Function PacketEventConfigurationToStr(const Data: Pointer; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
var
  CurrPtr:    Pointer;
  TempStr:    String;
  TempInt:    Integer;
  TempValStr: String;
  TempValue:  scs_value_t;
begin
CurrPtr := Data;
Ptr_ReadString(CurrPtr,Result){id};
While Ptr_ReadString(CurrPtr,TempStr) > SizeOfPacketString do
  begin
    TempStr := sLineBreak + TempStr;
    Ptr_ReadInteger(CurrPtr,TempInt);
    If LongWord(TempInt) <> SCS_U32_NIL then
      TempStr := TempStr + '[' + IntToStr(TempInt) + ']';
    Ptr_ReadInteger(CurrPtr,TempInt);
    case scs_value_type_t(TempInt) of
      SCS_VALUE_TYPE_string: Ptr_ReadString(CurrPtr,TempValStr);
    else
      Ptr_ReadBuffer(CurrPtr,TempValue,SizeOf(TempValue));
      TempValStr := SCSValueToStr(TempValue,TypeName,ShowDescriptors);
    end;
    TempStr := TempStr + ': ' + TempValStr;
    Result := Result + TempStr;
  end;
end;

end.
