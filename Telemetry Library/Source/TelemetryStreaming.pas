{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{@html(<hr>)
@abstract(Routines designed to store and load complex variables to/from memory
          or stream.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-06)
@lastmod(2014-11-24)

  @bold(@NoAutoLink(TelemetryStreaming))

  ©František Milt, all rights reserved.

  This unit contains routines for storing and loading variables and data
  into/from general memory and streams.@br
  @br
  Aside from minor things, there are four main groups of functions in this
  unit:@br
@unorderedList(
  @item(First group (@code(Ptr_*)) consists of functions intended to provide
        simplified way of writing and reading simple variables to/from general
        memory location given by pointer. These functions all have boolean
        paramater called @code(Advance) - when this parameter is set to @true,
        passed destination/source pointer is increased by number of bytes
        written/read, otherwise this pointer is not changed. This behavior is
        there for situations where you are writing data in sequence, so you do
        not need to manually recalculate pointer after every read/write call.)
  @item(Second group (@code(Stream_*)) functions are designed to provide
        simplified way of writing and reading simple variables to/from streams.)
  @item(Third group are function used to store and read selected SDK types
        to/from memory and/or streams. Their objective is to provide simple and
        uniform way of storing structures that have non-localized data, that is,
        some of their fields are only pointers, meaning they cannon be stored
        directly as only pointer and not actual data would be stored.)
  @item(Fourth group consists of functions used to store structures used in
        telemetry library (mainly structures used to store informations about
        events, channels and configs).)
)

  Last change:  2014-11-24

  Change List:@unorderedList(
    @item(2014-05-06 - First stable version.)
    @item(2014-11-02 - Small implementation changes.)
    @item(2014-11-05 - Type of result changed from signed to unsigned integer in
                       functions returning number of written/read bytes or size
                       of some structure or required memory (there is too many
                       such functions to list them here).)
    @item(2014-11-05 - Type of parameters @code(Size) and @code(BytesRead)
                       changed from signed to unsigned integer in all functions
                       with those parameters (there is too many such functions
                       to list them here).)
    @item(2014-11-24 - Changes due to a new system of storing and passing
                       secondary types of channel value. Resulting data layout
                       changed for these functions:
                       @unorderedList(
                         @itemSpacing(Compact)
                         @item(Ptr_Write_KnownChannel)
                         @item(Ptr_Read_KnownChannel)
                         @item(Stream_Write_KnownChannel)
                         @item(Stream_Read_KnownChannel)))
    @item(2015-06-27 - All calls to function @code(CopyMemory) replaced by
                       @code(Move).)
    @item(2015-06-27 - Added variant of function @code(SizeOfString) that
                       accepts API string (pointer, type scs_string_t) as
                       a parameter.)
    @item(2015-06-27 - Added variant of functions @code(Ptr_WriteString) and
                       @code(Stream_WriteString) that accepts API string as
                       an input parameter.)
    @item(2015-06-28 - Implementation changes.))

@html(<hr>)}
unit TelemetryStreaming;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  Classes,
{$ENDIF}  
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

{==============================================================================}
{   Unit public types                                                          }
{==============================================================================}

type
{
  Prototype of function used to convert item name into its ID.

  @param Name      Item name.
  @param UserData  Any data that the callback needs to work. Can be @nil.
  
  @returns Item ID computed from its name.
}
  TNameIDFunc = Function(const Name: TelemetryString; UserData: Pointer): TItemID;

{
  Prototype of function used to convert item ID back into its name.

  @param ID        Item ID.
  @param UserData  Any data that the callback needs to work. Can be @nil.

  @returns Item name corresponding to passed ID.
}
  TIDNameFunc = Function(ID: TItemID; UserData: Pointer): TelemetryString;

{==============================================================================}
{   Unit functions and procedures declarations                                 }
{==============================================================================}

{
  @abstract(Returns number of bytes required for a given string to be stored in
  memory.)
  When an empty string is passed, only size of string length (Int32 => 4Bytes)
  is returned.

  @param Str String whose size will be returned.

  @returns Size required for storing passed string in general memory.
}
Function SizeOfString(const Str: UTF8String = ''): TMemSize; overload;

//------------------------------------------------------------------------------

{
  @abstract(Returns number of bytes required for a given string to be stored in
  memory.)
  When passed string is is not assigned or is empty, then only size of string
  length (Int32 => 4Bytes) is returned.

  @param Str String whose size will be returned.

  @returns Size required for storing passed string in general memory.
}
Function SizeOfString(Str: scs_string_t): TMemSize; overload;

{==============================================================================}
{   Simple varibles storing and loading (memory)                               }
{==============================================================================}
{
  @abstract(Writes string to general memory location.)
  Strings are stored as two fields - signed 32bit integer containing length of
  string in bytes (length of following array), followed by an array of bytes
  (the string itself, without terminating #0 character). For exmaple, string
  "ABC.Z" will be stored as:@br
@preformatted(
  05 00 00 00 41 42 43 2E 5A
  |- length -|-- string ---|
)
  @bold(Note) - stored string is always UTF8 encoded. If you want to store
  string with different encoding, use conversion functions.
  
  @param Destination Memory location where to write. Must not be @nil.
  @param Str         String to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteString(var Destination: Pointer; const Str: UTF8String; Advance: Boolean = True): TMemSize; overload;

//------------------------------------------------------------------------------

{
  @abstract(Writes string to general memory location.)
  Resulting memory layour is the same as in case of function that accepts normal
  string as an input parameter.

  @param Destination Memory location where to write. Must not be @nil.
  @param Str         String to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteString(var Destination: Pointer; Str: scs_string_t; Advance: Boolean = True): TMemSize; overload;

//------------------------------------------------------------------------------

{
  @abstract(Reads string from general memory location.)
  Can return an empty string.

  @param Source   Memory location where to read. Must not be @nil.
  @param Str      Output string variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadString(var Source: Pointer; out Str: UTF8String; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes 32bit integer value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteInteger(var Destination: Pointer; Value: LongInt; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit integer value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadInteger(var Source: Pointer; out Value: LongInt; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes 64bit integer value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteInt64(var Destination: Pointer; Value: Int64; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit integer value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadInt64(var Source: Pointer; out Value: Int64; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes 32bit floating point value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteSingle(var Destination: Pointer; Value: Single; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit floating point value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadSingle(var Source: Pointer; out Value: Single; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes 64bit floating point value to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteDouble(var Destination: Pointer; Value: Double; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit floating point value from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadDouble(var Source: Pointer; out Value: Double; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes Boolean value (1byte) to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteBoolean(var Destination: Pointer; Value: Boolean; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads Boolean value (1byte) from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadBoolean(var Source: Pointer; out Value: Boolean; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes one byte to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteByte(var Destination: Pointer; Value: Byte; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads one byte from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Value    Output variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadByte(var Source: Pointer; out Value: Byte; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Writes buffer to general memory location.

  @param Destination Memory location where to write. Must not be @nil.
  @param Buffer      Buffer to be written.
  @param Size        Size of the buffer in bytes.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteBuffer(var Destination: Pointer; const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads buffer from general memory location.

  @param Source   Memory location where to read. Must not be @nil.
  @param Buffer   Output buffer.
  @param Size     Number of bytes to be read.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadBuffer(var Source: Pointer; var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;

//==============================================================================

{
  Reads string from general memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read string.
}
Function Ptr_ReadoutString(var Source: Pointer; Advance: Boolean = True): UTF8String;

//------------------------------------------------------------------------------

{
  Reads 32bit integer value from general memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read integer value.
}
Function Ptr_ReadoutInteger(var Source: Pointer; Advance: Boolean = True): LongInt;

//------------------------------------------------------------------------------

{
  Reads 64bit integer value from general memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read integer value.
}
Function Ptr_ReadoutInt64(var Source: Pointer; Advance: Boolean = True): Int64;

//------------------------------------------------------------------------------

{
  Reads 32bit floating point value from general memory location given by
  pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read floating point value.
}
Function Ptr_ReadoutSingle(var Source: Pointer; Advance: Boolean = True): Single;

//------------------------------------------------------------------------------

{
  Reads 64bit floating point value from general memory location given by
  pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read floating point value.
}
Function Ptr_ReadoutDouble(var Source: Pointer; Advance: Boolean = True): Double;

//------------------------------------------------------------------------------

{
  Reads boolean value (one byte) from general memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read boolean value.
}
Function Ptr_ReadoutBoolean(var Source: Pointer; Advance: Boolean = True): Boolean;

//------------------------------------------------------------------------------

{
  Reads one byte from general memory location given by pointer.
  
  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read byte value.
}
Function Ptr_ReadoutByte(var Source: Pointer; Advance: Boolean = True): Byte;

{==============================================================================}
{   Simple varibles storing and loading (stream)                               }
{==============================================================================}

{
  @abstract(Writes string into stream.)
  Strings are writen in the same manner as in Ptr_WriteString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteString(Stream: TStream; const Str: UTF8String): TMemSize; overload;

//------------------------------------------------------------------------------

{
  @abstract(Writes string into stream.)
  Strings are writen in the same manner as in Ptr_WriteString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteString(Stream: TStream; Str: scs_string_t): TMemSize; overload;

//------------------------------------------------------------------------------

{
  Reads string from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadString(Stream: TStream; out Str: UTF8String): TMemSize;

//------------------------------------------------------------------------------

{
  Reads string from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutString(Stream: TStream): UTF8String;

//------------------------------------------------------------------------------

{
  Writes 32bit integer value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteInteger(Stream: TStream; Value: LongInt): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit integer value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadInteger(Stream: TStream; out Value: LongInt): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit integer value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutInteger(Stream: TStream): LongInt;

//------------------------------------------------------------------------------

{
  Writes 64bit integer value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteInt64(Stream: TStream; Value: Int64): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit integer value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadInt64(Stream: TStream; out Value: Int64): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit integer value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutInt64(Stream: TStream): Int64;

//------------------------------------------------------------------------------

{
  Writes 32bit floating point value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteSingle(Stream: TStream; Value: Single): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit floating point value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadSingle(Stream: TStream; out Value: Single): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 32bit floating point value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutSingle(Stream: TStream): Single;

//------------------------------------------------------------------------------

{
  Writes 64bit floating point value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteDouble(Stream: TStream; Value: Double): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit floating point value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadDouble(Stream: TStream; out Value: Double): TMemSize;

//------------------------------------------------------------------------------

{
  Reads 64bit floating point value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutDouble(Stream: TStream): Double;

//------------------------------------------------------------------------------

{
  Writes boolean (one byte) value into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteBoolean(Stream: TStream; Value: Boolean): TMemSize;

//------------------------------------------------------------------------------

{
  Reads boolean (one byte) value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean): TMemSize;

//------------------------------------------------------------------------------

{
  Reads boolean (one byte) value from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutBoolean(Stream: TStream): Boolean;

//------------------------------------------------------------------------------

{
  Writes one byte into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteByte(Stream: TStream; Value: Byte): TMemSize;

//------------------------------------------------------------------------------

{
  Reads one byte from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadByte(Stream: TStream; out Value: Byte): TMemSize;

//------------------------------------------------------------------------------

{
  Reads one byte from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Output value.
}
Function Stream_ReadoutByte(Stream: TStream): Byte;

//------------------------------------------------------------------------------

{
  Writes buffer into stream.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Buffer Buffer to be written.
  @param Size   Size of the buffer in bytes.

  @returns Number of bytes written.
}
Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: TMemSize): TMemSize;

//------------------------------------------------------------------------------

{
  Reads buffer from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Buffer Output buffer.
  @param Size   Number of bytes to be read.

  @returns Number of bytes read.
}
Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: TMemSize): TMemSize;

{==============================================================================}
{   SDK types storing and loading                                              }
{==============================================================================}

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value    Actual value for which the size is requested.
  @param(Minimize When @true, function returns only minimal size required for
                  storing passed value (see Ptr_Write_scs_value function for
                  details).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_value(Value: scs_value_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_value_t) into memory location
            given by pointer.)
  Data can be stored in three ways, standard binary, minimized binary and third
  type is when value contains string data. When stored as standard, the whole
  structure is stored. When minimized, only used value type (eg. @code(float),
  @code(fvector)) is stored, thus saving space. Strings are stored using
  function Ptr_WriteString, refer to this functions for details how the string
  is actually stored.@br
  @br
  Memory layout for different styles of storing:
@preformatted(
          value     |   size    |   value type

  String:
        ValueType     4 bytes     scs_value_type_t (should be set to SCS_VALUE_TYPE_string)
        String data   variable    String

  Standard binary:
        Value         48 bytes    scs_value_t

  Minimized binary:
        ValueType     4 bytes     scs_value_type_t
        Value         variable    depends on ValueType (eg. scs_u32_t, scs_value_euler_t, ...)
)
  @br
  @bold(Note) - in all cases, first four bytes contains type of stored value.
                You should read this information first and then, according to
                its value, decide how to continue reading.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_value), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)

  @returns Number of bytes written.
}
Function Ptr_Write_scs_value(var Destination: Pointer; Value: scs_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type @code(scs_value_t) into memory.)
  Unlike Ptr_Write_scs_value, this function does not need preallocated memory.
  Instead, it first allocates memory required for storing of passed value and
  then writes the value into it.@br
  It internally calls Ptr_Write_scs_value, so all saving options and resulting
  memory layout are the same.

  @param(Ptr      Pointer to memory location where the value was saved. Can be
                  @nil.)
  @param Value    Value to be stored.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_value(out Ptr: Pointer; Value: scs_value_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_value_t) from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param Value     Output variable.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)
  @param(Minimized Indicating whether value was saved minimized (valid only for
                   binary values).)

  @returns Number of bytes read.
}
Function Ptr_Read_scs_value(var Source: Pointer; out Value: scs_value_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_value_t) from memory location given by pointer.

  @param Source     Memory location where to read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Advance    Indicating whether source pointer should be increased by
                    number of bytes read.)
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Ptr_Readout_scs_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_value_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_value_t) into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_value.

  @param Stream   Stream to which the value will be written. Must not be @nil.
  @param Value    Value to be written.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns Number of bytes written.
}
Function Stream_Write_scs_value(Stream: TStream; Value: scs_value_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_value_t) from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param Value      Output variable.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Number of bytes read.
}
Function Stream_Read_scs_value(Stream: TStream; out Value: scs_value_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_value_t) from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Stream_Readout_scs_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_t;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value    Actual value for which the size is requested.
  @param(Minimize When @true, function returns only minimal size required for
                  storing passed value (see Ptr_Write_scs_value_localized
                  function for details).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_value_localized(Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_value_localized_t into memory
            location given by pointer.)
  All saving options and resulting memory layout is exactly the same as in
  function Ptr_Write_scs_value (refer to it for details). It also means that it
  does not matter whether you save the data in this function or in
  Ptr_Write_scs_value, result should be the same and you can use any reading
  function on it.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_value_localized), then nothing is
                     stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)

  @returns Number of bytes written.
}
Function Ptr_Write_scs_value_localized(var Destination: Pointer; Value: scs_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type scs_value_localized_t into memory.)
  Unlike Ptr_Write_scs_value_localized, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed 
  value and then writes the value into it.@br
  It internally calls Ptr_Write_scs_value_localized, so all saving options and
  resulting memory layout are the same.

  @param(Ptr      Pointer to memory location where the value was saved. Can be
                  @nil.)
  @param Value    Value to be stored.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_value_localized(out Ptr: Pointer; Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_value_localized_t from memory location given by
  pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param Value     Output variable.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)
  @param(Minimized Indicating whether value was saved minimized (valid only for
                   binary values).)

  @returns Number of bytes read.
}
Function Ptr_Read_scs_value_localized(var Source: Pointer; out Value: scs_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_value_localized_t from memory location given by
  pointer.

  @param Source     Memory location where to read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Advance    Indicating whether source pointer should be increased by
                    number of bytes read.)
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Ptr_Readout_scs_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_value_localized_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_value_localized_t into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_value_localized.

  @param Stream   Stream to which the value will be written. Must not be @nil.
  @param Value    Value to be written.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns Number of bytes written.
}
Function Stream_Write_scs_value_localized(Stream: TStream; Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_value_localized_t from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param Value      Output variable.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Number of bytes read.
}
Function Stream_Read_scs_value_localized(Stream: TStream; out Value: scs_value_localized_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_value_localized_t from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Stream_Readout_scs_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_localized_t;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see Ptr_Write_scs_named_value function
                    for details).)
  @param(ItemIDOnly Indicates whether @code(name) field of stored value is saved
                    only as ID (4 bytes), or as full string (4+ bytes).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_named_value(Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_named_value_t) into memory
            location given by pointer.)
  Field @code(value) of passed variable is saved using Ptr_Write_scs_value
  function, refer to this function for details about resulting memory layout.@br
  @br
  Resulting memory layout for different saving options:
@preformatted(
          value     |   size    |   value type

  ItemIDOnly set to false:
        name          variable    String
        index         4 bytes     scs_u32_t
        value         variable    scs_value_t (see function Ptr_Write_scs_value)

  ItemIDOnly set to true:
        ID            4 bytes     TItemID
        index         4 bytes     scs_u32_t
        value         variable    scs_value_t (see function Ptr_Write_scs_value)
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_named_value), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param Minimize    Indicating whether value should be saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.

  @returns Number of bytes written.
}
Function Ptr_Write_scs_named_value(var Destination: Pointer; Value: scs_named_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type @code(scs_named_value_t) into memory.)
  Unlike Ptr_Write_scs_named_value, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed 
  value and then writes the value into it.@br
  It internally calls Ptr_Write_scs_named_value, so all saving options and
  resulting memory layout are the same.

  @param(Ptr         Pointer to memory location where the value was saved. Can
                     be @nil.)
  @param Value       Value to be stored.
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_named_value(out Ptr: Pointer; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_named_value_t) from memory location given by
  pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_named_value(var Source: Pointer; out Value: scs_named_value_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_named_value_t) from memory location given by
  pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Ptr_Readout_scs_named_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_named_value_t) into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_named_value.

  @param(Stream      Stream to which the value will be written. Must not be
                     @nil.)
  @param Value       Value to be written.
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.

  @returns Number of bytes written.
}
Function Stream_Write_scs_named_value(Stream: TStream; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_named_value_t) from a stream.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Stream_Read_scs_named_value(Stream: TStream; out Value: scs_named_value_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type @code(scs_named_value_t) from a stream.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Stream_Readout_scs_named_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see
                    Ptr_Write_scs_named_value_localized function for details).)
  @param(ItemIDOnly Indicates whether @code(Name) field of stored value is saved
                    only as ID (4 bytes), or as full string (4+ bytes).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_named_value_localized(Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_named_value_localized_t into memory
            location given by pointer.)
  Field @code(Value) of passed variable is saved using
  Ptr_Write_scs_value_localized function, refer to this function for details
  about resulting memory layout.@br
  All saving options and resulting memory layout is exactly the same as in
  function Ptr_Write_scs_named_value (refer to it for details). It also means
  that it does not matter whether you save the data in this function or in
  Ptr_Write_scs_named_value, result should be the same and you can use any
  reading function on it.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_named_value_localized), then nothing is
                     stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param Minimize    Indicating whether value should be saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(Name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(Name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns Number of bytes written.
}
Function Ptr_Write_scs_named_value_localized(var Destination: Pointer; Value: scs_named_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type @code(scs_named_value_t) into memory.)
  Unlike Ptr_Write_scs_named_value_localized, this function does not need
  preallocated memory. Instead, it first allocates memory required for storing 
  of passed value and then writes the value into it.@br
  It internally calls Ptr_Write_scs_named_value_localized, so all saving options
  and resulting memory layout are the same.

  @param(Ptr         Pointer to memory location where the value was saved. Can
                     be @nil.)
  @param Value       Value to be stored.
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(Name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_named_value_localized(out Ptr: Pointer; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_named_value_localized_t from memory location given by
  pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(Name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_named_value_localized(var Source: Pointer; out Value: scs_named_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_named_value_localized_t from memory location given by
  pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(Name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Ptr_Readout_scs_named_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_named_value_localized_t into a
            stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_named_value_localized.

  @param(Stream      Stream to which the value will be written. Must not be
                     @nil.)
  @param Value       Value to be written.
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) of passed value is not
                     stored as string, but is converted to ID using callback
                     function passed in parameter @code(NameIDFunc) and this ID
                     is then stored instead. This option takes effect only when
                     @code(NameIDFunc) is assigned.)
  @param(NameIDFunc  Callback function used to convert @code(Name) field to ID
                     when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns Number of bytes written.
}
Function Stream_Write_scs_named_value_localized(Stream: TStream; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_named_value_localized_t from a stream.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(Name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Stream_Read_scs_named_value_localized(Stream: TStream; out Value: scs_named_value_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type scs_named_value_localized_t from a stream.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Minimized   Indicating whether value was saved minimized (valid only
                     for binary values).)
  @param(ItemIDOnly  When set to @true, field @code(Name) is expected to be
                     stored only as ID, not as string. After this ID is read, it
                     is converted to string using function provided in parameter
                     @code(IDNameFunc). This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     field @code(Name) when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Stream_Readout_scs_named_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see Ptr_Write_scs_value function for
                    details).)
  @param(ItemIDOnly Indicates whether @code(name) fields of attributes is saved
                    only as ID (4 bytes), or as full string (4+ bytes).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_telemetry_configuration(Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_telemetry_configuration_t)
            into memory location given by pointer.)
  Individual attributes of passed configuration are saved using
  Ptr_Write_scs_named_value function, refer to this function for details about
  resulting memory layout.@br
  @br
  Resulting memory layout:
@preformatted(
          value     |   size    |   value type

        id            variable    String
        Count         4 bytes     32bit signed integer
        attributes    variable[]  array of scs_name_value_t (see function Ptr_Write_scs_named_value)
)
  @br
  @bold(Note) - @code(Count) contains number of all items in attributes array,
  including terminating empty value. This field must contain precise value, so
  you can use it for iteration or memory allocation in reading.@br
  When @code(ItemIdOnly) is set to @false, all strings are saved as usual, but
  when set to @true, followind process is applied:
@unorderedList(@itemSpacing(Compact)
  @item(@code(id) is stored as string)
  @item(for each attribute, @code(name) is merged with configuration @code(id)
        using function ConfigMergeIDAndAttribute, ...)
  @item(...resulting conglomerate is passed to @code(NameIDFunc) function...)
  @item(...and returned ID is then saved instead of attribute name (see function
        Ptr_Write_scs_named_value for details.))
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_telemetry_configuration), then nothing is
                     stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns Number of bytes written.
}
Function Ptr_Write_scs_telemetry_configuration(var Destination: Pointer; Value: scs_telemetry_configuration_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type @code(scs_telemetry_configuration_t)
  into memory.)
  Unlike Ptr_Write_scs_telemetry_configuration, this function does not need
  preallocated memory. Instead, it first allocates memory required for storing 
  of passed value and then writes the value into it.@br
  It internally calls Ptr_Write_scs_telemetry_configuration, so all saving
  options and resulting memory layout are the same.


  @param(Ptr         Pointer to memory location where the value was saved. Can
                     be @nil.)
  @param Value       Value to be stored.
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_telemetry_configuration(out Ptr: Pointer; Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from memory 
  location given by pointer.)
  @bold(Note) - when names of individual attributes are stored only as IDs -
  after conversion back to string, configuration @code(id) is removed from it
  using function ConfigRemoveIDFromName and what is left is returned in output
  value.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_telemetry_configuration(var Source: Pointer; out Value: scs_telemetry_configuration_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from memory
  location given by pointer.)
  @bold(Note) - when names of individual attributes are stored only as IDs -
  after conversion back to string, configuration @code(id) is removed from it
  using function ConfigRemoveIDFromName and what is left is returned in output
  value.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Ptr_Readout_scs_telemetry_configuration(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type @code(scs_telemetry_configuration_t)
  into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_telemetry_configuration.

  @param(Stream      Stream to which the value will be written. Must not be
                     @nil.)
  @param Value       Value to be written.
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns Number of bytes written.
}
Function Stream_Write_scs_telemetry_configuration(Stream: TStream; Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Stream_Read_scs_telemetry_configuration(Stream: TStream; out Value: scs_telemetry_configuration_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Stream_Readout_scs_telemetry_configuration(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_t;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see Ptr_Write_scs_value_localized
                    function for details).)
  @param(ItemIDOnly Indicates whether @code(Name) fields of attributes is saved
                    only as ID (4 bytes), or as full string (4+ bytes).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_telemetry_configuration_localized(Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_telemetry_configuration_localized_t
            into memory location given by pointer.)
  Individual attributes of passed configuration are saved using
  Ptr_Write_scs_named_value_localized function, refer to this function for
  details about resulting memory layout.
  All saving options and resulting memory layout is exactly the same as in
  function Ptr_Write_scs_telemetry_configuration (refer to it for details).
  It also means that it does not matter whether you save the data in this
  function or in Ptr_Write_scs_telemetry_configuration, result should be the
  same and you can use any reading function on it.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_scs_telemetry_configuration_localized), then
                     nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(Name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns Number of bytes written.
}
Function Ptr_Write_scs_telemetry_configuration_localized(var Destination: Pointer; Value: scs_telemetry_configuration_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type scs_telemetry_configuration_localized_t
  into memory.)
  Unlike Ptr_Write_scs_telemetry_configuration_localized, this function does not
  need preallocated memory. Instead, it first allocates memory required for
  storing of passed value and then writes the value into it.@br
  It internally calls Ptr_Write_scs_telemetry_configuration_localized, so all
  saving options and resulting memory layout are the same.

  @param(Ptr         Pointer to memory location where the value was saved. Can
                     be @nil.)
  @param Value       Value to be stored.
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(Name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.  

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_telemetry_configuration_localized(out Ptr: Pointer; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from 
  memory location given by pointer.)
  @bold(Note) - when names of individual attributes are stored only as IDs -
  after conversion back to string, configuration @code(ID) is removed from it
  using function ConfigRemoveIDFromName and what is left is returned in output
  value.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(Name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_telemetry_configuration_localized(var Source: Pointer; out Value: scs_telemetry_configuration_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from 
  memory location given by pointer.)
  @bold(Note) - when names of individual attributes are stored only as IDs -
  after conversion back to string, configuration @code(ID) is removed from it
  using function ConfigRemoveIDFromName and what is left is returned in output
  value.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(Name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Ptr_Readout_scs_telemetry_configuration_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_localized_t;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type scs_telemetry_configuration_localized_t
  into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_telemetry_configuration_localized.

  @param(Stream      Stream to which the value will be written. Must not be
                     @nil.)
  @param Value       Value to be written.
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is not stored as string, but is converted to ID
                     using callback function passed in parameter
                     @code(NameIDFunc) and this ID is then stored instead. This
                     option takes effect only when @code(NameIDFunc) is
                     assigned.)
  @param(NameIDFunc  Callback function used to convert attributes @code(Name)
                     fields to ID when @code(ItemIDOnly) is set to @true.)
  @param UserData    User data passed to @code(NameIDFunc) function.

  @returns Number of bytes written.
}
Function Stream_Write_scs_telemetry_configuration_localized(Stream: TStream; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration_localized.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Number of bytes read.
}
Function Stream_Read_scs_telemetry_configuration_localized(Stream: TStream; out Value: scs_telemetry_configuration_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration_localized.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param Minimized   Indicating whether attributes values were saved minimized.
  @param(ItemIDOnly  When set to @true, field @code(Name) of individual
                     attributes is expected to be stored only as ID, not as
                     string. This option takes effect only when
                     @code(IDNameFunc) is assigned.)
  @param(IDNameFunc  Callback function used to convert ID back to string for
                     attributes field @code(Name) when @code(ItemIDOnly) is set
                     to @true.)
  @param UserData    User data passed to @code(IDNameFunc) function.

  @returns Output value.
}
Function Stream_Readout_scs_telemetry_configuration_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_localized_t;

{==============================================================================}
{   Telemetry library types storing and loading                                }
{==============================================================================}

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownEvent(Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownEvent into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value  |   size    |   value type

        Event      4 bytes     scs_event_t
        Name       variable    String
        Valid      1 byte      Boolean
        Utility    1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_KnownEvent), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_KnownEvent(var Destination: Pointer; Value: TKnownEvent; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TKnownEvent into memory.)
  Unlike Ptr_Write_KnownEvent, this function does not need preallocated memory.
  Instead, it first allocates memory required for storing of passed value and
  then writes the value into it.@br
  It internally calls Ptr_Write_KnownEvent, so all saving options and resulting
  memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_KnownEvent(out Ptr: Pointer; Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownEvent from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownEvent(var Source: Pointer; out Value: TKnownEvent; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownEvent from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownEvent(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownEvent;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownEvent into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownEvent.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownEvent(Stream: TStream; Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownEvent from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownEvent(Stream: TStream; out Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownEvent from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownEvent(Stream: TStream; out BytesRead: TMemSize): TKnownEvent;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownChannel(Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownChannel into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value       |   size    |   value type

        Name            variable    String
        ID              4 bytes     TChannelID
        PrimaryType     4 bytes     scs_value_type_t
        SecondaryTypes  4 bytes     TValueTypeBitmask
        Indexed         1 byte      Boolean
        IndexConfig     variable    String
        IndexConfigID   4 bytes     TConfigID
        MaxIndex        4 bytes     scs_u32_t
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_KnownChannel), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_KnownChannel(var Destination: Pointer; Value: TKnownChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TKnownChannel into memory.)
  Unlike Ptr_Write_KnownChannel, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed 
  value and then writes the value into it.@br
  It internally calls Ptr_Write_KnownChannel, so all saving options and
  resulting memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_KnownChannel(out Ptr: Pointer; Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownChannel from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownChannel(var Source: Pointer; out Value: TKnownChannel; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownChannel from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownChannel;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownChannel into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownChannel.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownChannel(Stream: TStream; Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownChannel from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownChannel(Stream: TStream; out Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownChannel from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownChannel(Stream: TStream; out BytesRead: TMemSize): TKnownChannel;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownConfig(Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownConfig into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value       |   size    |   value type

        Name            variable    String
        ID              4 bytes     TConfigID
        ValueType       4 bytes     scs_value_type_t
        Indexed         1 byte      Boolean
        Binded          1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_KnownConfig), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_KnownConfig(var Destination: Pointer; Value: TKnownConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TKnownConfig into memory.)
  Unlike Ptr_Write_KnownConfig, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed 
  value and then writes the value into it.@br
  It internally calls Ptr_Write_KnownConfig, so all saving options and
  resulting memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_KnownConfig(out Ptr: Pointer; Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownConfig from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownConfig(var Source: Pointer; out Value: TKnownConfig; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownConfig from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownConfig;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TKnownConfig into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownConfig.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownConfig(Stream: TStream; Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownConfig from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownConfig(Stream: TStream; out Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TKnownConfig from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownConfig(Stream: TStream; out BytesRead: TMemSize): TKnownConfig;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_EventInfo({%H-}Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TEventInfo into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value   |   size    |   value type

        Event       4 bytes     scs_event_t
        Utility     1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_EventInfo), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_EventInfo(var Destination: Pointer; Value: TEventInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TEventInfo into memory.)
  Unlike Ptr_Write_EventInfo, this function does not need preallocated memory.
  Instead, it first allocates memory required for storing of passed value and
  then writes the value into it.@br
  It internally calls Ptr_Write_EventInfo, so all saving options and resulting
  memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_EventInfo(out Ptr: Pointer; Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TEventInfo from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_EventInfo(var Source: Pointer; out Value: TEventInfo; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TEventInfo from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_EventInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TEventInfo;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TEventInfo into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_EventInfo.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_EventInfo(Stream: TStream; Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TEventInfo from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_EventInfo(Stream: TStream; out Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TEventInfo from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_EventInfo(Stream: TStream; out BytesRead: TMemSize): TEventInfo;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_ChannelInfo(Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TChannelInfo into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value       |   size    |   value type

        Name            variable    String
        ID              4 bytes     TChannelID
        Index           4 bytes     scs_u32_t
        ValueType       4 bytes     scs_value_type_t
        Flags           4 bytes     scs_u32_t
        IndexConfigID   4 bytes     TItemID
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_ChannelInfo), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_ChannelInfo(var Destination: Pointer; Value: TChannelInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TChannelInfo into memory.)
  Unlike Ptr_Write_ChannelInfo, this function does not need preallocated memory.
  Instead, it first allocates memory required for storing of passed value and
  then writes the value into it.@br
  It internally calls Ptr_Write_ChannelInfo, so all saving options and resulting
  memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_ChannelInfo(out Ptr: Pointer; Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TChannelInfo from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_ChannelInfo(var Source: Pointer; out Value: TChannelInfo; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TChannelInfo from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_ChannelInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TChannelInfo;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TChannelInfo into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_ChannelInfo.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_ChannelInfo(Stream: TStream; Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TChannelInfo from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_ChannelInfo(Stream: TStream; out Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TChannelInfo from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_ChannelInfo(Stream: TStream; out BytesRead: TMemSize): TChannelInfo;

//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_StoredConfig(Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TStoredConfig into memory location given
            by pointer.)
  Field @code(Value) is saved using function Ptr_Write_scs_value_localized or
  its alternatives with parameter @code(Minimize) set to @true (refer to them
  for details about resulting layout).

  Resulting memory layout:
@preformatted(
         value  |   size    |   value type

        Name      variable    String
        ID        4 bytes     TConfigID
        Index     4 bytes     scs_u32_t
        Value     variable    scs_value_localized_t
        Binded    1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_StoredConfig), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_StoredConfig(var Destination: Pointer; Value: TStoredConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TStoredConfig into memory.)
  Unlike Ptr_Write_StoredConfig, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed
  value and then writes the value into it.@br
  It internally calls Ptr_Write_StoredConfig, so all saving options and
  resulting memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_StoredConfig(out Ptr: Pointer; Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredConfig from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_StoredConfig(var Source: Pointer; out Value: TStoredConfig; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredConfig from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_StoredConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredConfig;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TStoredConfig into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_StoredConfig.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_StoredConfig(Stream: TStream; Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredConfig from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_StoredConfig(Stream: TStream; out Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredConfig from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_StoredConfig(Stream: TStream; out BytesRead: TMemSize): TStoredConfig;


//==============================================================================

{
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_StoredChannel(Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TStoredChannel into memory location
            given by pointer.)
  Field @code(Value) is saved using function Ptr_Write_scs_value_localized or
  its alternatives with parameter @code(Minimize) set to @true (refer to them
  for details about resulting layout).

  Resulting memory layout:
@preformatted(
         value  |   size    |   value type

        Name      variable    String
        ID        4 bytes     TConfigID
        Index     4 bytes     scs_u32_t
        Value     variable    scs_value_localized_t
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param(Size        Number of bytes available. If this value is smaller than
                     number of bytes required to store passed value (see
                     function Size_StoredChannel), then nothing is stored.)
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_StoredChannel(var Destination: Pointer; Value: TStoredChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  @abstract(Stores passed value of type TStoredChannel into memory.)
  Unlike Ptr_Write_StoredChannel, this function does not need preallocated
  memory. Instead, it first allocates memory required for storing of passed
  value and then writes the value into it.@br
  It internally calls Ptr_Write_StoredChannel, so all saving options and
  resulting memory layout are the same.

  @param(Ptr   Pointer to memory location where the value was saved. Can be
               @nil.)
  @param Value Value to be stored.

  @returns(Number of bytes allocated for storing (you can use this value to free
           allocated memory).)
}
Function Ptr_Store_StoredChannel(out Ptr: Pointer; Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredChannel from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_StoredChannel(var Source: Pointer; out Value: TStoredChannel; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredChannel from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_StoredChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredChannel;

//------------------------------------------------------------------------------

{
  @abstract(Writes passed value of type TStoredChannel into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_StoredChannel.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_StoredChannel(Stream: TStream; Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredChannel from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_StoredChannel(Stream: TStream; out Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{
  Reads value of type TStoredChannel from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_StoredChannel(Stream: TStream; out BytesRead: TMemSize): TStoredChannel;

implementation

uses
  SysUtils;

{==============================================================================}
{   Unit functions and procedures implementation                               }
{==============================================================================}

Function SizeOfString(const Str: UTF8String = ''): TMemSize;
begin
Result := SizeOf(Integer){string length} + Length(Str) * SizeOf(TUTF8Char);
end;

//------------------------------------------------------------------------------

Function SizeOfString(Str: scs_string_t): TMemSize;
begin
If Assigned(Str) then
  Result := SizeOf(Integer){string length} + Length(PAnsiChar(Str)) * SizeOf(TUTF8Char)
else
  Result := SizeOf(Integer){string length};
end;

{==============================================================================}
{   Simple varibles storing and loading (memory)                               }
{==============================================================================}

Function Ptr_WriteString(var Destination: Pointer; const Str: UTF8String; Advance: Boolean = True): TMemSize;
begin
Integer(Destination^) := Length(Str);
Move(PUTF8Char(Str)^,{%H-}Pointer({%H-}PtrUInt(Destination) + SizeOf(Integer))^,Length(Str) * SizeOf(TUTF8Char));
Result := SizeOf(Integer) + (Length(Str) * SizeOf(TUTF8Char));
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteString(var Destination: Pointer; Str: scs_string_t; Advance: Boolean = True): TMemSize;
begin
Integer(Destination^) := Length(PAnsiChar(Str));
Move(Str^,{%H-}Pointer({%H-}PtrUInt(Destination) + SizeOf(Integer))^,Length(PAnsiChar(Str)) * SizeOf(TUTF8Char));
Result := SizeOf(Integer) + (Length(PAnsiChar(Str)) * SizeOf(TUTF8Char));
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadString(var Source: Pointer; out Str: UTF8String; Advance: Boolean = True): TMemSize;
begin
SetLength(Str,Integer(Source^) div SizeOf(TUTF8Char));
Move({%H-}Pointer({%H-}PtrUInt(Source) + SizeOf(Integer))^,PUTF8Char(Str)^,Length(Str) * SizeOf(TUTF8Char));
Result := SizeOf(Integer) + (Length(Str) * SizeOf(TUTF8Char));
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteInteger(var Destination: Pointer; Value: LongInt; Advance: Boolean = True): TMemSize;
begin
LongInt(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadInteger(var Source: Pointer; out Value: LongInt; Advance: Boolean = True): TMemSize;
begin
Value := LongInt(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteInt64(var Destination: Pointer; Value: Int64; Advance: Boolean = True): TMemSize;
begin
Int64(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadInt64(var Source: Pointer; out Value: Int64; Advance: Boolean = True): TMemSize;
begin
Value := Int64(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteSingle(var Destination: Pointer; Value: Single; Advance: Boolean = True): TMemSize;
begin
Single(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadSingle(var Source: Pointer; out Value: Single; Advance: Boolean = True): TMemSize;
begin
Value := Single(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;
//------------------------------------------------------------------------------

Function Ptr_WriteDouble(var Destination: Pointer; Value: Double; Advance: Boolean = True): TMemSize;
begin
Double(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadDouble(var Source: Pointer; out Value: Double; Advance: Boolean = True): TMemSize;
begin
Value := Double(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteBoolean(var Destination: Pointer; Value: Boolean; Advance: Boolean = True): TMemSize;
begin
Boolean(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadBoolean(var Source: Pointer; out Value: Boolean; Advance: Boolean = True): TMemSize;
begin
Value := Boolean(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteByte(var Destination: Pointer; Value: Byte; Advance: Boolean = True): TMemSize;
begin
Byte(Destination^) := Value;
Result := SizeOf(Value);
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadByte(var Source: Pointer; out Value: Byte; Advance: Boolean = True): TMemSize;
begin
Value := Byte(Source^);
Result := SizeOf(Value);
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_WriteBuffer(var Destination: Pointer; const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Move(Buffer,Destination^,Size);
Result := Size;
If Advance then Destination := {%H-}Pointer({%H-}PtrUInt(Destination) + Result);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadBuffer(var Source: Pointer; var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Move(Source^,Buffer,Size);
Result := Size;
If Advance then Source := {%H-}Pointer({%H-}PtrUInt(Source) + Result);
end;

//==============================================================================

Function Ptr_ReadoutString(var Source: Pointer; Advance: Boolean = True): UTF8String;
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

{==============================================================================}
{   Simple varibles storing and loading (stream)                               }
{==============================================================================}

Function Stream_WriteString(Stream: TStream; const Str: UTF8String): TMemSize;
var
  StringBytes: Integer;
begin
Result := 0;
StringBytes := Length(Str) * SizeOf(TUTF8Char);
Inc(Result,TMemSize(Stream.Write(StringBytes,SizeOf(StringBytes))));
Inc(Result,TMemSize(Stream.Write(PUTF8Char(Str)^,StringBytes)));
end;

//------------------------------------------------------------------------------

Function Stream_WriteString(Stream: TStream; Str: scs_string_t): TMemSize;
var
  StringBytes: Integer;
begin
Result := 0;
StringBytes := Length(PAnsiChar(Str)) * SizeOf(TUTF8Char);
Inc(Result,TMemSize(Stream.Write(StringBytes,SizeOf(StringBytes))));
Inc(Result,TMemSize(Stream.Write(Str^,StringBytes)));
end;

//------------------------------------------------------------------------------

Function Stream_ReadString(Stream: TStream; out Str: UTF8String): TMemSize;
var
  StringBytes: Integer;
begin
Result := 0;
Inc(Result,TMemSize(Stream.Read({%H-}StringBytes,SizeOf(StringBytes))));
SetLength(Str,StringBytes div SizeOf(TUTF8Char));
Inc(Result,TMemSize(Stream.Read(PUTF8Char(Str)^,Length(Str) * SizeOf(TUTF8Char))));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutString(Stream: TStream): UTF8String;
begin
Stream_ReadString(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteInteger(Stream: TStream; Value: LongInt): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadInteger(Stream: TStream; out Value: LongInt): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutInteger(Stream: TStream): Integer;
begin
Stream_ReadInteger(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteInt64(Stream: TStream; Value: Int64): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadInt64(Stream: TStream; out Value: Int64): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutInt64(Stream: TStream): Int64;
begin
Stream_ReadInt64(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteSingle(Stream: TStream; Value: Single): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadSingle(Stream: TStream; out Value: Single): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutSingle(Stream: TStream): Single;
begin
Stream_ReadSingle(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteDouble(Stream: TStream; Value: Double): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadDouble(Stream: TStream; out Value: Double): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutDouble(Stream: TStream): Double;
begin
Stream_ReadDouble(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteBoolean(Stream: TStream; Value: Boolean): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutBoolean(Stream: TStream): Boolean;
begin
Stream_ReadBoolean(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteByte(Stream: TStream; Value: Byte): TMemSize;
begin
Result := Stream.Write(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadByte(Stream: TStream; out Value: Byte): TMemSize;
begin
Result := Stream.Read({%H-}Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutByte(Stream: TStream): Byte;
begin
Stream_ReadByte(Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: TMemSize): TMemSize;
begin
Result := Stream.Write(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: TMemSize): TMemSize;
begin
Result := Stream.Read({%H-}Buffer,Size);
end;

{==============================================================================}
{   SDK types storing and loading                                              }
{==============================================================================}

Function Size_scs_value(Value: scs_value_t; Minimize: Boolean = False): TMemSize;
begin
If Value._type = SCS_VALUE_TYPE_string then
  Result := SizeOf(scs_value_type_t) + SizeOfString(Value.value_string.value)
else
  begin
    If Minimize then
      begin
        case Value._type of
          SCS_VALUE_TYPE_bool:        Result := SizeOf(scs_value_type_t) + SizeOf(scs_u8_t);
          SCS_VALUE_TYPE_s32:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_s32_t);
          SCS_VALUE_TYPE_u32:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_u32_t);
          SCS_VALUE_TYPE_u64:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_u64_t);
          SCS_VALUE_TYPE_float:       Result := SizeOf(scs_value_type_t) + SizeOf(scs_float_t);
          SCS_VALUE_TYPE_double:      Result := SizeOf(scs_value_type_t) + SizeOf(scs_double_t);
          SCS_VALUE_TYPE_fvector:     Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_fvector_t);
          SCS_VALUE_TYPE_dvector:     Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_dvector_t);
          SCS_VALUE_TYPE_euler:       Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_euler_t);
          SCS_VALUE_TYPE_fplacement:  Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_fplacement_t);
          SCS_VALUE_TYPE_dplacement:  Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_dplacement_t);
        else
          raise Exception.CreateFmt('Size_scs_value_t: Unknown value type (%d).',[Value._type]);
        end;
      end
    else Result := SizeOf(scs_value_t);
  end;
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_value(var Destination: Pointer; Value: scs_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_value(Value,Minimize) then
  begin
    WorkPtr := Destination;
    If Value._type = SCS_VALUE_TYPE_string then
      begin
        Result := Ptr_WriteInteger(WorkPtr,LongInt(Value._type),True);
        Inc(Result,Ptr_WriteString(WorkPtr,Value.value_string.value,True));
      end
    else
      begin
        If Minimize then
          begin
            Result := Ptr_WriteInteger(WorkPtr,LongInt(Value._type),True);
            case Value._type of
              SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_WriteByte(WorkPtr,Value.value_bool.value,True));
              SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_WriteInteger(WorkPtr,Value.value_s32.value,True));
              SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.value_u32.value),True));
              SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_WriteInt64(WorkPtr,Int64(Value.value_u64.value),True));
              SCS_VALUE_TYPE_float:       Inc(Result,Ptr_WriteSingle(WorkPtr,Value.value_float.value,True));
              SCS_VALUE_TYPE_double:      Inc(Result,Ptr_WriteDouble(WorkPtr,Value.value_double.value,True));
              SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_fvector,SizeOf(scs_value_fvector_t),True));
              SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_dvector,SizeOf(scs_value_dvector_t),True));
              SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_euler,SizeOf(scs_value_euler_t),True));
              SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_fplacement,SizeOf(scs_value_fplacement_t),True));
              SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_dplacement,SizeOf(scs_value_dplacement_t),True));
            else
              raise Exception.CreateFmt('Ptr_Write_scs_value_t: Unknown value type (%d).',[Value._type]);
            end;
          end
        else Result := Ptr_WriteBuffer(WorkPtr,Value,SizeOf(scs_value_t),True);
      end;
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_value: Output buffer too small (got %d, expected %d).',[Size,Size_scs_value(Value,Minimize)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_value(out Ptr: Pointer; Value: scs_value_t; Minimize: Boolean = False): TMemSize;
begin
Result := Size_scs_value(Value,Minimize);
Ptr := AllocMem(Result);
Ptr_Write_scs_value(Ptr,Value,Result,False,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_value(var Source: Pointer; out Value: scs_value_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
  TempStr:  TelemetryString;
begin
WorkPtr := Source;
Result := Ptr_ReadInteger(WorkPtr,LongInt(Value._type),True);
If Value._type = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(TempStr),True));
    Value.value_string.value := TelemetryStringToAPIString(TempStr);
  end
else
  begin
    If Minimized then
      begin
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_ReadByte(WorkPtr,Value.value_bool.value,True));
          SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_ReadInteger(WorkPtr,Value.value_s32.value,True));
          SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.value_u32.value),True));
          SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_ReadInt64(WorkPtr,Int64(Value.value_u64.value),True));
          SCS_VALUE_TYPE_float:       Inc(Result,Ptr_ReadSingle(WorkPtr,Value.value_float.value,True));
          SCS_VALUE_TYPE_double:      Inc(Result,Ptr_ReadDouble(WorkPtr,Value.value_double.value,True));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_fvector,SizeOf(scs_value_fvector_t),True));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_dvector,SizeOf(scs_value_dvector_t),True));
          SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_euler,SizeOf(scs_value_euler_t),True));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_fplacement,SizeOf(scs_value_fplacement_t),True));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_dplacement,SizeOf(scs_value_dplacement_t),True));
        else
          raise Exception.CreateFmt('Ptr_Read_scs_value_t: Unknown value type (%d).',[Value._type]);
        end;
      end
    else
      begin
        WorkPtr := Source;
        Result := Ptr_ReadBuffer(WorkPtr,Value,SizeOf(scs_value_t),True);
      end;
  end;
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_value_t;
begin
BytesRead := Ptr_Read_scs_value(Source,Result,Advance,Minimized);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_value(Stream: TStream; Value: scs_value_t; Minimize: Boolean = False): TMemSize;
begin
If Value._type = SCS_VALUE_TYPE_string then
  begin
    Result := Stream_WriteInteger(Stream,LongInt(Value._type));
    Inc(Result,Stream_WriteString(Stream,Value.value_string.value));
  end
else
  begin
    If Minimize then
      begin
        Result := Stream_WriteInteger(Stream,LongInt(Value._type));
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_WriteByte(Stream,Value.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_WriteInteger(Stream,Value.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.value_u32.value)));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_WriteInt64(Stream,Int64(Value.value_u64.value)));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_WriteSingle(Stream,Value.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_WriteDouble(Stream,Value.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_WriteBuffer(Stream,Value.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise Exception.CreateFmt('Stream_Write_scs_value_t: Unknown value type (%d).',[Value._type]);
        end;
      end
    else Result := Stream_WriteBuffer(Stream,Value,SizeOf(scs_value_t));
  end;
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_value(Stream: TStream; out Value: scs_value_t; Minimized: Boolean = False): TMemSize;
var
  TempStr:  TelemetryString;
begin
Result := Stream_ReadInteger(Stream,LongInt(Value._type));
If Value._type = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Stream_ReadString(Stream,UTF8String(TempStr)));
    Value.value_string.value := TelemetryStringToAPIString(TempStr);
  end
else
  begin
    If Minimized then
      begin
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_ReadByte(Stream,Value.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_ReadInteger(Stream,Value.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.value_u32.value)));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_ReadInt64(Stream,Int64(Value.value_u64.value)));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_ReadSingle(Stream,Value.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_ReadDouble(Stream,Value.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_ReadBuffer(Stream,Value.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise Exception.CreateFmt('Stream_Read_scs_value_t: Unknown value type (%d).',[Value._type]);
        end;
      end
    else
      begin
        Stream.Seek(-Result,soCurrent);
        Result := Stream_ReadBuffer(Stream,Value,SizeOf(scs_value_t));
      end;
  end;
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_t;
begin
BytesRead := Stream_Read_scs_value(Stream,Result,Minimized);
end;

//==============================================================================

Function Size_scs_value_localized(Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;
begin
If Value.ValueType = SCS_VALUE_TYPE_string then
  Result := SizeOf(scs_value_type_t) + SizeOfString(Value.StringData)
else
  begin
    If Minimize then
      begin
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Result := SizeOf(scs_value_type_t) + SizeOf(scs_u8_t);
          SCS_VALUE_TYPE_s32:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_s32_t);
          SCS_VALUE_TYPE_u32:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_u32_t);
          SCS_VALUE_TYPE_u64:         Result := SizeOf(scs_value_type_t) + SizeOf(scs_u64_t);
          SCS_VALUE_TYPE_float:       Result := SizeOf(scs_value_type_t) + SizeOf(scs_float_t);
          SCS_VALUE_TYPE_double:      Result := SizeOf(scs_value_type_t) + SizeOf(scs_double_t);
          SCS_VALUE_TYPE_fvector:     Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_fvector_t);
          SCS_VALUE_TYPE_dvector:     Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_dvector_t);
          SCS_VALUE_TYPE_euler:       Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_euler_t);
          SCS_VALUE_TYPE_fplacement:  Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_fplacement_t);
          SCS_VALUE_TYPE_dplacement:  Result := SizeOf(scs_value_type_t) + SizeOf(scs_value_dplacement_t);
        else
          raise Exception.CreateFmt('Size_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
        end;
      end
    else Result := SizeOf(scs_value_t);
  end;
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_value_localized(var Destination: Pointer; Value: scs_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_value_localized(Value,Minimize) then
  begin
    WorkPtr := Destination;
    If Value.ValueType = SCS_VALUE_TYPE_string then
      begin
        Result := Ptr_WriteInteger(WorkPtr,LongInt(Value.ValueType),True);
        Inc(Result,Ptr_WriteString(WorkPtr,Value.StringData,True));
      end
    else
      begin
        If Minimize then
          begin
            Result := Ptr_WriteInteger(WorkPtr,LongInt(Value.ValueType),True);
            case Value.ValueType of
              SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_WriteByte(WorkPtr,Value.BinaryData.value_bool.value,True));
              SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_WriteInteger(WorkPtr,Value.BinaryData.value_s32.value,True));
              SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.BinaryData.value_u32.value),True));
              SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_WriteInt64(WorkPtr,Int64(Value.BinaryData.value_u64.value),True));
              SCS_VALUE_TYPE_float:       Inc(Result,Ptr_WriteSingle(WorkPtr,Value.BinaryData.value_float.value,True));
              SCS_VALUE_TYPE_double:      Inc(Result,Ptr_WriteDouble(WorkPtr,Value.BinaryData.value_double.value,True));
              SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t),True));
              SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t),True));
              SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t),True));
              SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t),True));
              SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t),True));
            else
              raise Exception.CreateFmt('Ptr_Write_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
            end;
          end
        else
          begin
            Value.BinaryData._type := Value.ValueType;
            Result := Ptr_WriteBuffer(WorkPtr,Value.BinaryData,SizeOf(scs_value_t),True);
          end;
      end;
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_value_localized: Output buffer too small (got %d, expected %d).',[Size,Size_scs_value_localized(Value,Minimize)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_value_localized(out Ptr: Pointer; Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;
begin
Result := Size_scs_value_localized(Value,Minimize);
Ptr := AllocMem(Result);
Ptr_Write_scs_value_localized(Ptr,Value,Result,False,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_value_localized(var Source: Pointer; out Value: scs_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadInteger(WorkPtr,LongInt(Value.ValueType),True);
If Value.ValueType = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(Value.StringData),True));
    Value.BinaryData._type := SCS_VALUE_TYPE_INVALID;
  end
else
  begin
    Value.StringData := '';
    If Minimized then
      begin
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_ReadByte(WorkPtr,Value.BinaryData.value_bool.value,True));
          SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_ReadInteger(WorkPtr,Value.BinaryData.value_s32.value,True));
          SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.BinaryData.value_u32.value),True));
          SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_ReadInt64(WorkPtr,Int64(Value.BinaryData.value_u64.value),True));
          SCS_VALUE_TYPE_float:       Inc(Result,Ptr_ReadSingle(WorkPtr,Value.BinaryData.value_float.value,True));
          SCS_VALUE_TYPE_double:      Inc(Result,Ptr_ReadDouble(WorkPtr,Value.BinaryData.value_double.value,True));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t),True));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t),True));
          SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t),True));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t),True));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t),True));
        else
          raise Exception.CreateFmt('Ptr_Read_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
        end;
        Value.BinaryData._type := Value.ValueType;
      end
    else
      begin
        WorkPtr := Source;
        Result := Ptr_ReadBuffer(WorkPtr,Value.BinaryData,SizeOf(scs_value_t),True);
      end;
  end;
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_value_localized_t;
begin
BytesRead := Ptr_Read_scs_value_localized(Source,Result,Advance,Minimized);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_value_localized(Stream: TStream; Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;
begin
If Value.ValueType = SCS_VALUE_TYPE_string then
  begin
    Result := Stream_WriteInteger(Stream,LongInt(Value.ValueType));
    Inc(result,Stream_WriteString(Stream,Value.StringData));
  end
else
  begin
    If Minimize then
      begin
        Result := Stream_WriteInteger(Stream,LongInt(Value.ValueType));
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_WriteByte(Stream,Value.BinaryData.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_WriteInteger(Stream,Value.BinaryData.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.BinaryData.value_u32.value)));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_WriteInt64(Stream,Int64(Value.BinaryData.value_u64.value)));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_WriteSingle(Stream,Value.BinaryData.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_WriteDouble(Stream,Value.BinaryData.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise Exception.CreateFmt('Stream_Write_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
        end;
      end
    else
      begin
        Value.BinaryData._type := Value.ValueType;
        Result := Stream_WriteBuffer(Stream,Value.BinaryData,SizeOf(scs_value_t));
      end;
  end;
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_value_localized(Stream: TStream; out Value: scs_value_localized_t; Minimized: Boolean = False): TMemSize;
begin
Result := Stream_ReadInteger(Stream,LongInt(Value.ValueType));
If Value.ValueType = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Stream_ReadString(Stream,UTF8String(Value.StringData)));
    Value.BinaryData._type := SCS_VALUE_TYPE_INVALID;
  end
else
  begin
    Value.StringData := '';
    If Minimized then
      begin
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_ReadByte(Stream,Value.BinaryData.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_ReadInteger(Stream,Value.BinaryData.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.BinaryData.value_u32.value)));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_ReadInt64(Stream,Int64(Value.BinaryData.value_u64.value)));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_ReadSingle(Stream,Value.BinaryData.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_ReadDouble(Stream,Value.BinaryData.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise Exception.CreateFmt('Stream_Read_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
        end;
        Value.BinaryData._type := Value.ValueType;
      end
    else
      begin
        Stream.Seek(-Result,soCurrent);
        Result := Stream_ReadBuffer(Stream,Value.BinaryData,SizeOf(scs_value_t));
      end;
  end;
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_localized_t;
begin
BytesRead := Stream_Read_scs_value_localized(Stream,Result,Minimized);
end;

//==============================================================================

Function Size_scs_named_value(Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;
begin
If ItemIDOnly then
  Result := SizeOf(TItemID) + SizeOf(scs_u32_t) + Size_scs_value(Value.value,Minimize)
else
  Result := SizeOfString(Value.name) + SizeOf(scs_u32_t) + Size_scs_value(Value.value,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_named_value(var Destination: Pointer; Value: scs_named_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_named_value(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Result := Ptr_WriteInteger(WorkPtr,LongInt(NameIDFunc(APIStringToTelemetryString(Value.name),UserData)),True)
    else
      Result := Ptr_WriteString(WorkPtr,Value.name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.index),True));
    Inc(Result,Ptr_Write_scs_value(WorkPtr,Value.value,Size - Result,True,Minimize));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_named_value: Output buffer too small (got %d, expected %d).',[Size,Size_scs_named_value(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_named_value(out Ptr: Pointer; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_named_value(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_named_value(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_named_value(var Source: Pointer; out Value: scs_named_value_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  TempStr:  TelemetryString;
  TempID:   TItemID;
begin
WorkPtr := Source;
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Ptr_ReadInteger(WorkPtr,LongInt(TempID),True);
    Value.name := TelemetryStringToAPIString(IDNameFunc(TempID,UserData));
  end
else
  begin
    Result := Ptr_ReadString(WorkPtr,UTF8String(TempStr),True);
    Value.name := TelemetryStringToAPIString(TempStr);
  end;
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.index),True));
Inc(Result,Ptr_Read_scs_value(WorkPtr,Value.value,True,Minimized));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_named_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;
begin
BytesRead := Ptr_Read_scs_named_value(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_named_value(Stream: TStream; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
If ItemIDOnly and Assigned(NameIDFunc) then
  Result := Stream_WriteInteger(Stream,LongInt(NameIDFunc(APIStringToTelemetryString(Value.name),UserData)))
else
  Result := Stream_WriteString(Stream,Value.name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.index)));
Inc(Result,Stream_Write_scs_value(Stream,Value.value,Minimize));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_named_value(Stream: TStream; out Value: scs_named_value_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  TempStr:  TelemetryString;
  TempID:   TItemID;
begin
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Stream_ReadInteger(Stream,LongInt(TempID));
    Value.name := TelemetryStringToAPIString(IDNameFunc(TempID,UserData));
  end
else
  begin
    Result := Stream_ReadString(Stream,UTF8String(TempStr));
    Value.name := TelemetryStringToAPIString(TempStr);
  end;
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.index)));
Inc(Result,Stream_Read_scs_value(Stream,Value.value,Minimized));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_named_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;
begin
BytesRead := Stream_Read_scs_named_value(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//==============================================================================

Function Size_scs_named_value_localized(Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;
begin
If ItemIDOnly then
  Result := SizeOf(TItemID) + SizeOf(scs_u32_t) + Size_scs_value_localized(Value.Value,Minimize)
else
  Result := SizeOfString(Value.Name) + SizeOf(scs_u32_t) + Size_scs_value_localized(Value.Value,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_named_value_localized(var Destination: Pointer; Value: scs_named_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_named_value_localized(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Result := Ptr_WriteInteger(WorkPtr,LongInt(NameIDFunc(Value.Name,UserData)),True)
    else
      Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.Index),True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,Minimize));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_named_value_localized: Output buffer too small (got %d, expected %d).',[Size,Size_scs_named_value_localized(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_named_value_localized(out Ptr: Pointer; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_named_value_localized(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_named_value_localized(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_named_value_localized(var Source: Pointer; out Value: scs_named_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  TempID:   TItemID;
begin
WorkPtr := Source;
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Ptr_ReadInteger(WorkPtr,LongInt(TempID),True);
    Value.Name := IDNameFunc(TempID,UserData);
  end
else
  Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.Index),True));
Inc(Result,Ptr_Read_scs_value_localized(WorkPtr,Value.Value,True,Minimized));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_named_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;
begin
BytesRead := Ptr_Read_scs_named_value_localized(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_named_value_localized(Stream: TStream; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
If ItemIDOnly and Assigned(NameIDFunc) then
  Result := Stream_WriteInteger(Stream,LongInt(NameIDFunc(Value.Name,UserData)))
else
  Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,Minimize));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_named_value_localized(Stream: TStream; out Value: scs_named_value_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  TempID: TItemID;
begin
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Stream_ReadInteger(Stream,LongInt(TempID));
    Value.Name := IDNameFunc(TempID,UserData);
  end
else
  Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Read_scs_value_localized(Stream,Value.Value,Minimized));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_named_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;
begin
BytesRead := Stream_Read_scs_named_value_localized(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//==============================================================================

Function Size_scs_telemetry_configuration(Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;
var
  CurrAttrPtr:  p_scs_named_value_t;
begin
Result := SizeOfString(Value.id) + SizeOf(Integer);
CurrAttrPtr := Value.attributes;
while Assigned(CurrAttrPtr^.name) do
  begin
    Inc(Result,Size_scs_named_value(CurrAttrPtr^,Minimize,ItemIDOnly));
    Inc(CurrAttrPtr);
  end;
Inc(Result,SizeOfString);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_telemetry_configuration(var Destination: Pointer; Value: scs_telemetry_configuration_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:      Pointer;
  ConfigID:     TelemetryString;
  CurrAttrPtr:  p_scs_named_value_t;
  Count:        Integer;
  CountPtr:     PInteger;
  TempStr:      scs_string_t;
begin
If Size >= Size_scs_telemetry_configuration(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    ConfigID := APIStringToTelemetryString(Value.id);
    Result := Ptr_WriteString(WorkPtr,ConfigID,True);
    CountPtr := WorkPtr;
    Count := 1;
    Inc(Result,Ptr_WriteInteger(WorkPtr,Count,True));
    CurrAttrPtr := Value.attributes;
    while Assigned(CurrAttrPtr^.name) do
      begin
        TempStr := nil;
        If ItemIDOnly and Assigned(NameIDFunc) then
          begin
            TempStr := CurrAttrPtr^.name;
            CurrAttrPtr^.name := TelemetryStringToAPIString(ConfigMergeIDAndAttribute(ConfigID,APIStringToTelemetryString(CurrAttrPtr^.name)));
          end;
        Inc(Result,Ptr_Write_scs_named_value(WorkPtr,CurrAttrPtr^,Size - Result,True,Minimize,ItemIDOnly,NameIDFunc,UserData));
        If Assigned(TempStr) then
          begin
            APIStringFree(CurrAttrPtr^.name);
            CurrAttrPtr^.name := TempStr;
          end;
        Inc(CurrAttrPtr);
        Inc(Count);
      end;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Inc(Result,Ptr_WriteInteger(WorkPtr,0,True))
    else
      Inc(Result,Ptr_WriteString(WorkPtr,'',True));
    CountPtr^ := Count;
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_telemetry_configuration: Output buffer too small (got %d, expected %d).',[Size,Size_scs_telemetry_configuration(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_telemetry_configuration(out Ptr: Pointer; Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_telemetry_configuration(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_telemetry_configuration(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_telemetry_configuration(var Source: Pointer; out Value: scs_telemetry_configuration_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:      Pointer;
  ConfigID:     TelemetryString;
  TempStr:      TelemetryString;
  i,Count:      Integer;
  CurrAttrPtr:  p_scs_named_value_t;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(ConfigID),True);
Value.id := TelemetryStringToAPIString(ConfigID);
Inc(Result,Ptr_ReadInteger(WorkPtr,Count,True));
Value.attributes := AllocMem(Count * SizeOf(scs_named_value_t));
CurrAttrPtr := Value.attributes;
For i := 2 to Count do
  begin
    Inc(Result,Ptr_Read_scs_named_value(WorkPtr,CurrAttrPtr^,True,Minimized,ItemIDOnly,IDNameFunc,UserData));
    If ItemIDOnly and Assigned(IDNameFunc) then
      begin
        TempStr := APIStringToTelemetryString(CurrAttrPtr^.name);
        APIStringFree(CurrAttrPtr^.name);
        CurrAttrPtr^.name := TelemetryStringToAPIString(ConfigRemoveIDFromName(TempStr,ConfigID));
      end;
    Inc(CurrAttrPtr);
  end;
If ItemIDOnly and Assigned(IDNameFunc) then
  Inc(Result,Ptr_ReadInteger(WorkPtr,i,True))
else
  Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(TempStr),True));
CurrAttrPtr^.name := nil;
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_telemetry_configuration(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_t;
begin
BytesRead := Ptr_Read_scs_telemetry_configuration(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_telemetry_configuration(Stream: TStream; Value: scs_telemetry_configuration_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  ConfigID:     TelemetryString;
  Count:        Integer;
  CurrAttrPtr:  p_scs_named_value_t;
  CountPos:     Int64;
  EndPos:       Int64;
  TempStr:      scs_string_t;
begin
ConfigID := APIStringToTelemetryString(Value.id);
Result := Stream_WriteString(Stream,ConfigID);
CountPos := Stream.Position;
Count := 1;
Inc(Result,Stream_WriteInteger(Stream,Count));
CurrAttrPtr := Value.attributes;
while Assigned(CurrAttrPtr^.name) do
  begin
    TempStr := nil;
    If ItemIDOnly and Assigned(NameIDFunc) then
      begin
        TempStr := CurrAttrPtr^.name;
        CurrAttrPtr^.name := TelemetryStringToAPIString(ConfigMergeIDAndAttribute(ConfigID,APIStringToTelemetryString(CurrAttrPtr^.name)));
      end;
    Inc(Result,Stream_Write_scs_named_value(Stream,CurrAttrPtr^,Minimize,ItemIDOnly,NameIDFunc,UserData));
    If Assigned(TempStr) then
      begin
        APIStringFree(CurrAttrPtr^.name);
        CurrAttrPtr^.name := TempStr;
      end;
    Inc(CurrAttrPtr);
    Inc(Count);
  end;
If ItemIDOnly and Assigned(NameIDFunc) then
  Inc(Result,Stream_WriteInteger(Stream,0))
else
  Inc(Result,Stream_WriteString(Stream,''));
EndPos := Stream.Position;
Stream.Seek(CountPos,soBeginning);
Stream_WriteInteger(Stream,Count);
Stream.Seek(EndPos,soBeginning);
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_telemetry_configuration(Stream: TStream; out Value: scs_telemetry_configuration_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  ConfigID:     TelemetryString;
  TempStr:      TelemetryString;
  i,Count:      Integer;
  CurrAttrPtr:  p_scs_named_value_t;
begin
Result := Stream_ReadString(Stream,UTF8String(ConfigID));
Value.id := TelemetryStringToAPIString(ConfigID);
Inc(Result,Stream_ReadInteger(Stream,Count));
Value.attributes := AllocMem(Count * SizeOf(scs_named_value_t));
CurrAttrPtr := Value.attributes;
For i := 2 to Count do
  begin
    Inc(Result,Stream_Read_scs_named_value(Stream,CurrAttrPtr^,Minimized,ItemIDOnly,IDNameFunc,UserData));
    If ItemIDOnly and Assigned(IDNameFunc) then
      begin
        TempStr := APIStringToTelemetryString(CurrAttrPtr^.name);
        APIStringFree(CurrAttrPtr^.name);
        CurrAttrPtr^.name := TelemetryStringToAPIString(ConfigRemoveIDFromName(TempStr,ConfigID));
      end;
    Inc(CurrAttrPtr);
  end;
If ItemIDOnly and Assigned(IDNameFunc) then
  Inc(Result,Stream_ReadInteger(Stream,i))
else
  Inc(Result,Stream_ReadString(Stream,UTF8String(TempStr)));
CurrAttrPtr^.name := nil;
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_telemetry_configuration(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_t;
begin
BytesRead := Stream_Read_scs_telemetry_configuration(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//==============================================================================

Function Size_scs_telemetry_configuration_localized(Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;
var
  i: Integer;
begin
Result := SizeOfString(Value.ID) + SizeOf(Integer);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  Inc(Result,Size_scs_named_value_localized(Value.Attributes[i],Minimize,ItemIDOnly));
Inc(Result,SizeOfString);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_telemetry_configuration_localized(var Destination: Pointer; Value: scs_telemetry_configuration_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  i:        Integer;
begin
If Size >= Size_scs_telemetry_configuration_localized(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.ID,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,Length(Value.Attributes) + 1,True));
    For i := Low(Value.Attributes) to High(Value.Attributes) do
      begin
        If ItemIDOnly and Assigned(NameIDFunc) then
          Value.Attributes[i].name := ConfigMergeIDAndAttribute(Value.ID,Value.Attributes[i].Name);
        Inc(Result,Ptr_Write_scs_named_value_localized(WorkPtr,Value.Attributes[i],Size - Result,True,Minimize,ItemIDOnly,NameIDFunc,UserData));
      end;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Inc(Result,Ptr_WriteInteger(WorkPtr,0,True))
    else
      Inc(Result,Ptr_WriteString(WorkPtr,'',True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_scs_telemetry_configuration_localized: Output buffer too small (got %d, expected %d).',
                               [Size,Size_scs_telemetry_configuration_localized(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_telemetry_configuration_localized(out Ptr: Pointer; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_telemetry_configuration_localized(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_telemetry_configuration_localized(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,Userdata);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_telemetry_configuration_localized(var Source: Pointer; out Value: scs_telemetry_configuration_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  TempStr:  TelemetryString;
  i,Count:  Integer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.ID),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,Count,True));
SetLength(Value.Attributes,Count - 1);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  begin
    Inc(Result,Ptr_Read_scs_named_value_localized(WorkPtr,Value.Attributes[i],True,Minimized,ItemIDOnly,IDNameFunc,UserData));
    If ItemIDOnly and Assigned(IDNameFunc) then
      Value.Attributes[i].Name := ConfigRemoveIDFromName(Value.Attributes[i].Name,Value.ID);
  end;
If ItemIDOnly and Assigned(IDNameFunc) then
  Inc(Result,Ptr_ReadInteger(WorkPtr,i,True))
else
  Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(TempStr),True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_telemetry_configuration_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_localized_t;
begin
BytesRead := Ptr_Read_scs_telemetry_configuration_localized(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_telemetry_configuration_localized(Stream: TStream; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  i:  Integer;
begin
Result := Stream_WriteString(Stream,Value.ID);
Inc(Result,Stream_WriteInteger(Stream,Length(Value.Attributes) + 1));
For i := Low(Value.Attributes) to High(Value.Attributes) do
  begin
    If ItemIDOnly and Assigned(NameIDFunc) then
      Value.Attributes[i].Name := ConfigMergeIDAndAttribute(Value.ID,Value.Attributes[i].Name);
    Inc(Result,Stream_Write_scs_named_value_localized(Stream,Value.Attributes[i],Minimize,ItemIDOnly,NameIDFunc,UserData));
  end;
If ItemIDOnly and Assigned(NameIDFunc) then
  Inc(Result,Stream_WriteInteger(Stream,0))
else
  Inc(Result,Stream_WriteString(Stream,''));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_telemetry_configuration_localized(Stream: TStream; out Value: scs_telemetry_configuration_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  TempStr:  TelemetryString;
  i,Count:  Integer;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.ID));
Inc(Result,Stream_ReadInteger(Stream,Count));
SetLength(Value.Attributes,Count - 1);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  begin
    Inc(Result,Stream_Read_scs_named_value_localized(Stream,Value.Attributes[i],Minimized,ItemIDOnly,IDNameFunc,UserData));
    If ItemIDOnly and Assigned(IDNameFunc) then
      Value.Attributes[i].Name := ConfigRemoveIDFromName(Value.Attributes[i].Name,Value.ID);
  end;
If ItemIDOnly and Assigned(IDNameFunc) then
  Inc(Result,Stream_ReadInteger(Stream,i))
else
  Inc(Result,Stream_ReadString(Stream,UTF8String(TempStr)));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_telemetry_configuration_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDNameFunc = nil; UserData: Pointer = nil): scs_telemetry_configuration_localized_t;
begin
BytesRead := Stream_Read_scs_telemetry_configuration_localized(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,Userdata);
end;

{==============================================================================}
{   Telemetry library types storing and loading                                }
{==============================================================================}

Function Size_KnownEvent(Value: TKnownEvent): TMemSize;
begin
Result := SizeOf(scs_event_t) + SizeOfString(Value.Name) + 2 * SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownEvent(var Destination: Pointer; Value: TKnownEvent; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownEvent(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteInteger(WorkPtr,LongInt(Value.Event),True);
    Inc(Result,Ptr_WriteString(WorkPtr,Value.Name,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Valid,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Utility,True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_KnownEvent: Output buffer too small (got %d, expected %d).',[Size,Size_KnownEvent(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_KnownEvent(out Ptr: Pointer; Value: TKnownEvent): TMemSize;
begin
Result := Size_KnownEvent(Value);
Ptr := AllocMem(Result);
Ptr_Write_KnownEvent(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_KnownEvent(var Source: Pointer; out Value: TKnownEvent; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadInteger(WorkPtr,LongInt(Value.Event),True);
Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Valid,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Utility,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_KnownEvent(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownEvent;
begin
BytesRead := Ptr_Read_KnownEvent(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_KnownEvent(Stream: TStream; Value: TKnownEvent): TMemSize;
begin
Result := Stream_WriteInteger(Stream,LongInt(Value.Event));
Inc(Result,Stream_WriteString(Stream,Value.Name));
Inc(Result,Stream_WriteBoolean(Stream,Value.Valid));
Inc(Result,Stream_WriteBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownEvent(Stream: TStream; out Value: TKnownEvent): TMemSize;
begin
Result := Stream_ReadInteger(Stream,LongInt(Value.Event));
Inc(Result,Stream_ReadString(Stream,UTF8String(Value.Name)));
Inc(Result,Stream_ReadBoolean(Stream,Value.Valid));
Inc(Result,Stream_ReadBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_KnownEvent(Stream: TStream; out BytesRead: TMemSize): TKnownEvent;
begin
BytesRead := Stream_Read_KnownEvent(Stream,Result);
end;

//==============================================================================

Function Size_KnownChannel(Value: TKnownChannel): TMemSize;
begin
Result := SizeOfString(Value.Name) + SizeOf(TChannelID) + 3 * SizeOf(scs_value_type_t) +
          SizeOf(Boolean) + SizeOf(TConfigID) + SizeOfString(Value.IndexConfig) + SizeOf(scs_u32_t);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownChannel(var Destination: Pointer; Value: TKnownChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownChannel(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.PrimaryType),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.SecondaryTypes),True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Indexed,True));
    Inc(Result,Ptr_WriteString(WorkPtr,Value.IndexConfig,True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.IndexConfigID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.MaxIndex),True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_KnownChannel: Output buffer too small (got %d, expected %d).',[Size,Size_KnownChannel(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_KnownChannel(out Ptr: Pointer; Value: TKnownChannel): TMemSize;
begin
Result := Size_KnownChannel(Value);
Ptr := AllocMem(Result);
Ptr_Write_KnownChannel(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_KnownChannel(var Source: Pointer; out Value: TKnownChannel; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.PrimaryType),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.SecondaryTypes),True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Indexed,True));
Inc(Result,Ptr_ReadString(WorkPtr,UTF8String(Value.IndexConfig),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.IndexConfigID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.MaxIndex),True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_KnownChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownChannel;
begin
BytesRead := Ptr_Read_KnownChannel(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_KnownChannel(Stream: TStream; Value: TKnownChannel): TMemSize;
begin
Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.PrimaryType)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.SecondaryTypes)));
Inc(Result,Stream_WriteBoolean(Stream,Value.Indexed));
Inc(Result,Stream_WriteString(Stream,Value.IndexConfig));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.IndexConfigID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.MaxIndex)));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownChannel(Stream: TStream; out Value: TKnownChannel): TMemSize;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.PrimaryType)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.SecondaryTypes)));
Inc(Result,Stream_ReadBoolean(Stream,Value.Indexed));
Inc(Result,Stream_ReadString(Stream,UTF8String(Value.IndexConfig)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.IndexConfigID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.MaxIndex)));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_KnownChannel(Stream: TStream; out BytesRead: TMemSize): TKnownChannel;
begin
BytesRead := Stream_Read_KnownChannel(Stream,Result);
end;

//==============================================================================

Function Size_KnownConfig(Value: TKnownConfig): TMemSize;
begin
Result := SizeOfString(Value.Name) + SizeOf(TConfigID) + SizeOf(scs_value_type_t) + 2 * SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownConfig(var Destination: Pointer; Value: TKnownConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownConfig(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ValueType),True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Indexed,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Binded,True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_KnownConfig: Output buffer too small (got %d, expected %d).',[Size,Size_KnownConfig(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_KnownConfig(out Ptr: Pointer; Value: TKnownConfig): TMemSize;
begin
Result := Size_KnownConfig(Value);
Ptr := AllocMem(Result);
Ptr_Write_KnownConfig(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_KnownConfig(var Source: Pointer; out Value: TKnownConfig; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ValueType),True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Indexed,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Binded,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_KnownConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownConfig;
begin
BytesRead := Ptr_Read_KnownConfig(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_KnownConfig(Stream: TStream; Value: TKnownConfig): TMemSize;
begin
Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ValueType)));
Inc(Result,Stream_WriteBoolean(Stream,Value.Indexed));
Inc(Result,Stream_WriteBoolean(Stream,Value.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownConfig(Stream: TStream; out Value: TKnownConfig): TMemSize;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ValueType)));
Inc(Result,Stream_ReadBoolean(Stream,Value.Indexed));
Inc(Result,Stream_ReadBoolean(Stream,Value.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_KnownConfig(Stream: TStream; out BytesRead: TMemSize): TKnownConfig;
begin
BytesRead := Stream_Read_KnownConfig(Stream,Result);
end;

//==============================================================================

Function Size_EventInfo(Value: TEventInfo): TMemSize;
begin
Result := SizeOf(scs_event_t) + SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_EventInfo(var Destination: Pointer; Value: TEventInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_EventInfo(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteInteger(WorkPtr,LongInt(Value.Event),True);
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Utility,True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_EventInfo: Output buffer too small (got %d, expected %d).',[Size,Size_EventInfo(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_EventInfo(out Ptr: Pointer; Value: TEventInfo): TMemSize;
begin
Result := Size_EventInfo(Value);
Ptr := AllocMem(Result);
Ptr_Write_EventInfo(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_EventInfo(var Source: Pointer; out Value: TEventInfo; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadInteger(WorkPtr,LongInt(Value.Event),True);
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Utility,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_EventInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TEventInfo;
begin
BytesRead := Ptr_Read_EventInfo(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_EventInfo(Stream: TStream; Value: TEventInfo): TMemSize;
begin
Result := Stream_WriteInteger(Stream,LongInt(Value.Event));
Inc(Result,Stream_WriteBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Read_EventInfo(Stream: TStream; out Value: TEventInfo): TMemSize;
begin
Result := Stream_ReadInteger(Stream,LongInt(Value.Event));
Inc(Result,Stream_ReadBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_EventInfo(Stream: TStream; out BytesRead: TMemSize): TEventInfo;
begin
BytesRead := Stream_Read_EventInfo(Stream,Result);
end;

//==============================================================================

Function Size_ChannelInfo(Value: TChannelInfo): TMemSize;
begin
Result := SizeOfString(Value.Name) + SizeOf(TChannelID) + SizeOf(scs_u32_t) +
          SizeOf(scs_value_type_t) + SizeOf(scs_u32_t) + SizeOf(TItemID);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_ChannelInfo(var Destination: Pointer; Value: TChannelInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_ChannelInfo(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.Index),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ValueType),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.Flags),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.IndexConfigID),True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_ChannelInfo: Output buffer too small (got %d, expected %d).',[Size,Size_ChannelInfo(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_ChannelInfo(out Ptr: Pointer; Value: TChannelInfo): TMemSize;
begin
Result := Size_ChannelInfo(Value);
Ptr := AllocMem(Result);
Ptr_Write_ChannelInfo(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_ChannelInfo(var Source: Pointer; out Value: TChannelInfo; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.Index),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ValueType),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.Flags),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.IndexConfigID),True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_ChannelInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TChannelInfo;
begin
BytesRead := Ptr_Read_ChannelInfo(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_ChannelInfo(Stream: TStream; Value: TChannelInfo): TMemSize;
begin
Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ValueType)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.Flags)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.IndexConfigID)));
end;

//------------------------------------------------------------------------------

Function Stream_Read_ChannelInfo(Stream: TStream; out Value: TChannelInfo): TMemSize;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ValueType)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.Flags)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.IndexConfigID)));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_ChannelInfo(Stream: TStream; out BytesRead: TMemSize): TChannelInfo;
begin
BytesRead := Stream_Read_ChannelInfo(Stream,Result);
end;

//==============================================================================

Function Size_StoredConfig(Value: TStoredConfig): TMemSize;
begin
Result := SizeOfString(Value.Name) + SizeOf(TConfigID) + SizeOf(scs_u32_t) +
          Size_scs_value_localized(Value.Value,True) + SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_StoredConfig(var Destination: Pointer; Value: TStoredConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_StoredConfig(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.Index),True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Binded,True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_StoredConfig: Output buffer too small (got %d, expected %d).',[Size,Size_StoredConfig(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_StoredConfig(out Ptr: Pointer; Value: TStoredConfig): TMemSize;
begin
Result := Size_StoredConfig(Value);
Ptr := AllocMem(Result);
Ptr_Write_StoredConfig(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_StoredConfig(var Source: Pointer; out Value: TStoredConfig; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.Index),True));
Inc(Result,Ptr_Read_scs_value_localized(WorkPtr,Value.Value,True,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Binded,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_StoredConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredConfig;
begin
BytesRead := Ptr_Read_StoredConfig(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_StoredConfig(Stream: TStream; Value: TStoredConfig): TMemSize;
begin
Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,True));
Inc(Result,Stream_WriteBoolean(Stream,Value.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Read_StoredConfig(Stream: TStream; out Value: TStoredConfig): TMemSize;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Read_scs_value_localized(Stream,Value.Value,True));
Inc(Result,Stream_ReadBoolean(Stream,Value.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_StoredConfig(Stream: TStream; out BytesRead: TMemSize): TStoredConfig;
begin
BytesRead := Stream_Read_StoredConfig(Stream,Result);
end;

//==============================================================================

Function Size_StoredChannel(Value: TStoredChannel): TMemSize;
begin
Result := SizeOfString(Value.Name) + SizeOf(TChannelID) + SizeOf(scs_u32_t) +
          Size_scs_value_localized(Value.Value,True);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_StoredChannel(var Destination: Pointer; Value: TStoredChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_StoredChannel(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.ID),True));
    Inc(Result,Ptr_WriteInteger(WorkPtr,LongInt(Value.Index),True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,True));
    If Advance then Destination := WorkPtr;
  end
else raise Exception.CreateFmt('Ptr_Write_StoredChannel: Output buffer too small (got %d, expected %d).',[Size,Size_StoredChannel(Value)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_StoredChannel(out Ptr: Pointer; Value: TStoredChannel): TMemSize;
begin
Result := Size_StoredChannel(Value);
Ptr := AllocMem(Result);
Ptr_Write_StoredChannel(Ptr,Value,Result,False);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_StoredChannel(var Source: Pointer; out Value: TStoredChannel; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Source;
Result := Ptr_ReadString(WorkPtr,UTF8String(Value.Name),True);
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.ID),True));
Inc(Result,Ptr_ReadInteger(WorkPtr,LongInt(Value.Index),True));
Inc(Result,Ptr_Read_scs_value_localized(WorkPtr,Value.Value,True,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_StoredChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredChannel;
begin
BytesRead := Ptr_Read_StoredChannel(Source,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_Write_StoredChannel(Stream: TStream; Value: TStoredChannel): TMemSize;
begin
Result := Stream_WriteString(Stream,Value.Name);
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_WriteInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,True));
end;

//------------------------------------------------------------------------------

Function Stream_Read_StoredChannel(Stream: TStream; out Value: TStoredChannel): TMemSize;
begin
Result := Stream_ReadString(Stream,UTF8String(Value.Name));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.ID)));
Inc(Result,Stream_ReadInteger(Stream,LongInt(Value.Index)));
Inc(Result,Stream_Read_scs_value_localized(Stream,Value.Value,True));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_StoredChannel(Stream: TStream; out BytesRead: TMemSize): TStoredChannel;
begin
BytesRead := Stream_Read_StoredChannel(Stream,Result);
end;

end.
