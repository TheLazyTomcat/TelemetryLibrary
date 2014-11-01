{@html(<hr>)
@abstract(Provides routines that can be used when resolving a packet.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-15)
@lastmod(2014-05-15)

  @bold(@NoAutoLink(TelemetryCommPacketsResolving))

  ©František Milt, all rights reserved.

  Provides routines used when resolving a packet (eg. reading values from
  payload).

  Last change:  2014-05-15

  Change List:@unorderedList(
    @item(2014-05-15 - First stable version.))

@html(<hr>)}
unit TelemetryCommPacketsResolving;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
{$IFDEF Documentation}
  TelemetryStreaming,
{$ENDIF}
  TelemetryCommPackets;

{==============================================================================}
{    Functions declarations                                                    }
{==============================================================================}

{
  Reads string from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read string.
}
Function Payload_ReadoutString(const Packet: TPacketBuffer; Offset: Integer = 0): String;

//------------------------------------------------------------------------------

{
  Reads 32bit integer value from packet payload.

  @param Packet Packet from whitch the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read integer value.
}
Function Payload_ReadoutInteger(const Packet: TPacketBuffer; Offset: Integer = 0): LongInt;
 
//------------------------------------------------------------------------------

{
  Reads 64bit integer value from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read integer value.
}
Function Payload_ReadoutInt64(const Packet: TPacketBuffer; Offset: Integer = 0): Int64;
 
//------------------------------------------------------------------------------

{
  Reads 32bit floating point value from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read floating point value.
}
Function Payload_ReadoutSingle(const Packet: TPacketBuffer; Offset: Integer = 0): Single;
  
//------------------------------------------------------------------------------

{
  Reads 64bit floating point value from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read floating point value.
}
Function Payload_ReadoutDouble(const Packet: TPacketBuffer; Offset: Integer = 0): Double;
  
//------------------------------------------------------------------------------

{
  Reads boolean value (one byte) from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read boolean value.
}
Function Payload_ReadoutBoolean(const Packet: TPacketBuffer; Offset: Integer = 0): Boolean;
  
//------------------------------------------------------------------------------

{
  Reads one byte from packet payload.

  @param Packet Packet from which the value should be read.
  @param Offset Position from payload start where to read.

  @returns Read byte value.
}
Function Payload_ReadoutByte(const Packet: TPacketBuffer; Offset: Integer = 0): Byte;

implementation

uses
  TelemetryStreaming;

{==============================================================================}
{    Functions implementation                                                  }
{==============================================================================}

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

end.
