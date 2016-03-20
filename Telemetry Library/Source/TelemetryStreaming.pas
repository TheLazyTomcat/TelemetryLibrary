{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Routines designed to store and load complex variables to/from memory
          or stream.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-06)
@lastmod(2015-07-09)

  @bold(@NoAutoLink(TelemetryStreaming))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-20 

  This unit contains routines for storing and loading variables and data
  into/from general memory and streams.@br
  Aside from minor things, there are three main groups of functions in this
  unit:@br
@unorderedList(
  @item(First group consists of functions providing an uniform way of storing
        strings used in telemetry (UTF8-encoded). There are also functions for
        writing and reading API strings.)
  @item(Second group are function used to store and read selected SDK types
        and their localized counterparts to/from memory and streams. Their main
        objective is to provide simple and uniform way of storing structures
        that have non-localized data, that is, some of their fields are only
        pointers, meaning they cannon be stored directly as only pointer and
        not actual data would be stored.)
  @item(Third group consists of functions used to store structures used in
        telemetry library (mainly structures used to store information about
        events, channels and configs).)
)

  @bold(Warning) - You have to be cautious when using functions which outputs
  following types: @code(scs_string_t), @code(scs_value_t),
  @code(scs_named_value_t) and @code(scs_telemetry_configuration_t).@br
  Variables, end therefore the returned values, of these types must be manually
  freed when you are done using them. For this, use function scs_string_free
  from telemetry headers, scs_value_free, scs_named_value_free and
  scs_telemetry_configuration_free from unit TelemetryConversions.

@html(<hr>)}
unit TelemetryStreaming;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  Classes,
  AuxTypes,  
{$ENDIF}
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists
{$IFNDEF Documentation},
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}
{$ELSE};
{$ENDIF}

{==============================================================================}
{   Unit public types                                                          }
{==============================================================================}

type
{:
  Prototype of function used to convert item name into its ID.

  @param Name      Item name.
  @param UserData  Any data that the callback needs to work. Can be @nil.
  
  @returns Item ID computed from its name.
}
  TNameToIDFunc = Function(const Name: TelemetryString; UserData: Pointer): TItemID;

{:
  Prototype of function used to convert item ID back into its name.

  @param ID        Item ID.
  @param UserData  Any data that the callback needs to work. Can be @nil.

  @returns Item name corresponding to passed ID.
}
  TIDToNameFunc = Function(ID: TItemID; UserData: Pointer): TelemetryString;

{------------------------------------------------------------------------------}
{==============================================================================}
{   Unit functions and procedures declarations                                 }
{==============================================================================}
{------------------------------------------------------------------------------}

{:
  @abstract(Returns number of bytes required for a given string to be stored in
  memory.)
  When an empty string is passed, only size of string length (Int32 => 4Bytes)
  is returned.

  @param Str String whose size will be returned.

  @returns Size required for storing passed string in general memory.
}
Function TelemetryStringBytes(const Str: TelemetryString = ''): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Returns number of bytes required for a given string to be stored in
  memory.)
  When passed string is is not assigned or is empty, then only size of string
  length (Int32 => 4Bytes) is returned.

  @param Str String whose size will be returned.

  @returns Size required for storing passed string in general memory.
}
Function APIStringBytes(Str: scs_string_t = nil): TMemSize;

{==============================================================================}
{   String types storing and loading (memory)                                  }
{==============================================================================}
{:
  @abstract(Writes telemetry string (UTF8 encoded) to general memory location.)
  Strings are stored as two fields - signed 32bit integer containing length of
  string in bytes (length of following array), followed by an array of bytes
  (the string itself, without terminating #0 character). For exmaple, string
  "ABC.Z" will be stored as:@br
@preformatted(
  05 00 00 00 41 42 43 2E 5A
  |- length -|-- string ---|
)
  @bold(Note) - stored string is always UTF8 encoded.
  
  @param Destination Memory location where to write. Must not be @nil.
  @param Str         String to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_WriteTelemetryString(var Destination: Pointer; const Str: TelemetryString; Advance: Boolean = True): TMemSize; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Reads telemetry string from general memory location.)
  Can return an empty string.

  @param Source   Memory location where to read. Must not be @nil.
  @param Str      Output string variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_ReadTelemetryString(var Source: Pointer; out Str: TelemetryString; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads telemetry string from general memory location given by
            a pointer.)
  Can return an empty string.

  @param Source  Memory location where to read. Must not be @nil.
  @param(Advance Indicating whether source pointer should be increased by
                 number of bytes read.)

  @returns Read string.
}
Function Ptr_ReadoutTelemetryString(var Source: Pointer; Advance: Boolean = True): TelemetryString;

//==============================================================================

{:
  @abstract(Writes API string to general memory location.)
  Resulting memory layour is the same as in case of function
  Ptr_WriteTelemetryString.

  @param Destination Memory location where to write. Must not be @nil.
  @param Str         String to be written.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.
}
Function Ptr_Write_scs_string(var Destination: Pointer; Str: scs_string_t; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads API string from general memory location.)
  Can return nil.

  @param Source   Memory location where to read. Must not be @nil.
  @param Str      Output string variable.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_scs_string(var Source: Pointer; out Str: scs_string_t; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads API string from general memory location.)
  Can return nil.

  @param Source   Memory location where to read. Must not be @nil.
  @param(Advance  Indicating whether source pointer should be increased by
                  number of bytes read.)

  @returns Read string.
}
Function Ptr_Readout_scs_string(var Source: Pointer; Advance: Boolean = True): scs_string_t;

{==============================================================================}
{   String types storing and loading (stream)                                  }
{==============================================================================}

{:
  @abstract(Writes telemetry string into stream.)
  Strings are writen in the same manner as in Ptr_WriteTelemetryString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.

  @returns Number of bytes written.
}
Function Stream_WriteTelemetryString(Stream: TStream; const Str: TelemetryString): TMemSize; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Writes API string into stream.)
  Strings are writen in the same manner as in Ptr_WriteTelemetryString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.

  @returns Number of bytes written.
}
//Function Stream_WriteAPIString(Stream: TStream; Str: scs_string_t): TMemSize; overload;

//------------------------------------------------------------------------------

{:
  Reads telemetry string from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output value.

  @returns Number of bytes read.
}
Function Stream_ReadTelemetryString(Stream: TStream; out Str: TelemetryString): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads telemetry string from stream.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Read string.
}
Function Stream_ReadoutTelemetryString(Stream: TStream): TelemetryString;

//==============================================================================

{:
  @abstract(Writes API string into stream.)
  Strings are writen in the same manner as in Ptr_WriteTelemetryString function.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Str    Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_scs_string(Stream: TStream; Str: scs_string_t): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads API string from stream.)
  Can return nil.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Str    Output string variable.

  @returns Number of bytes read.
}
Function Stream_Read_scs_string(Stream: TStream; out Str: scs_string_t): TMemSize;

{:
  @abstract(Reads API string from stream.)
  Can return nil.

  @param Stream Stream from which the value will be read. Must not be @nil.

  @returns Read string.
}
Function Stream_Readout_scs_string(Stream: TStream): scs_string_t;

{==============================================================================}
{   Complex SDK types storing and loading                                      }
{==============================================================================}

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value    Actual value for which the size is requested.
  @param(Minimize When @true, function returns only minimal size required for
                  storing passed value (see Ptr_Write_scs_value function for
                  details).)

  @returns Number of bytes required for storing passed value.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
}
Function Size_scs_value(Value: scs_value_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type @code(scs_value_t) into memory location
            given by pointer.)
  Data can be stored in three ways, standard binary, minimized binary and third
  type is when value contains string data. When stored as standard, the whole
  structure is stored. When minimized, only used value type (eg. @code(float),
  @code(fvector)) is stored, thus saving space. Strings are stored using
  function Ptr_WriteAPIString, refer to this functions for details how the
  string is actually stored.@br
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
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)

  @returns Number of bytes written.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_value(var Destination: Pointer; Value: scs_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type @code(scs_value_t) from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param Value     Output variable.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)
  @param(Minimized Indicating whether value was saved minimized (valid only for
                   binary values).)

  @returns Number of bytes read.

  @raises ETLUnknownData When read data are of unknown type.
}
Function Ptr_Read_scs_value(var Source: Pointer; out Value: scs_value_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  @abstract(Writes passed value of type @code(scs_value_t) into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_value.

  @param Stream   Stream to which the value will be written. Must not be @nil.
  @param Value    Value to be written.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns Number of bytes written.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
}
Function Stream_Write_scs_value(Stream: TStream; Value: scs_value_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type @code(scs_value_t) from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param Value      Output variable.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Number of bytes read.

  @raises ETLUnknownData When read data are of unknown type.
}
Function Stream_Read_scs_value(Stream: TStream; out Value: scs_value_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type @code(scs_value_t) from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Stream_Readout_scs_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_t;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value    Actual value for which the size is requested.
  @param(Minimize When @true, function returns only minimal size required for
                  storing passed value (see Ptr_Write_scs_value_localized
                  function for details).)

  @returns Number of bytes required for storing passed value.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
}
Function Size_scs_value_localized(Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type scs_value_localized_t into memory
            location given by pointer.)
  All saving options and resulting memory layout is exactly the same as in
  function Ptr_Write_scs_value (refer to it for details). It also means that it
  does not matter whether you save the data in this function or in
  Ptr_Write_scs_value, result should be the same and you can use any reading
  function on it.

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether value should be saved minimized (valid
                     only for binary values).)

  @returns Number of bytes written.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_value_localized(var Destination: Pointer; Value: scs_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type scs_value_localized_t from memory location given by
  pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param Value     Output variable.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)
  @param(Minimized Indicating whether value was saved minimized (valid only for
                   binary values).)

  @returns Number of bytes read.

  @raises ETLUnknownData When read data are of unknown type.
}
Function Ptr_Read_scs_value_localized(var Source: Pointer; out Value: scs_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  @abstract(Writes passed value of type scs_value_localized_t into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_value_localized.

  @param Stream   Stream to which the value will be written. Must not be @nil.
  @param Value    Value to be written.
  @param(Minimize Indicating whether value should be saved minimized (valid only
                  for binary values).)

  @returns Number of bytes written.

  @raises ETLUnknownData When parameter @code(Value) is of unknown type.
}
Function Stream_Write_scs_value_localized(Stream: TStream; Value: scs_value_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type scs_value_localized_t from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param Value      Output variable.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Number of bytes read.

  @raises ETLUnknownData When read data are of unknown type.
}
Function Stream_Read_scs_value_localized(Stream: TStream; out Value: scs_value_localized_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type scs_value_localized_t from a stream.

  @param Stream     Stream from which the value will be read. Must not be @nil.
  @param BytesRead  Number of bytes read.
  @param(Minimized  Indicating whether value was saved minimized (valid only for
                    binary values).)

  @returns Output value.
}
Function Stream_Readout_scs_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_value_localized_t;

//==============================================================================

{:
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

{:
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
  @param Size        Number of bytes available in destination buffer.
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

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_named_value(var Destination: Pointer; Value: scs_named_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Store_scs_named_value(out Ptr: Pointer; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Read_scs_named_value(var Source: Pointer; out Value: scs_named_value_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Readout_scs_named_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;

//------------------------------------------------------------------------------

{:
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
Function Stream_Write_scs_named_value(Stream: TStream; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Stream_Read_scs_named_value(Stream: TStream; out Value: scs_named_value_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Stream_Readout_scs_named_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;

//==============================================================================

{:
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

{:
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
  @param Size        Number of bytes available in destination buffer.
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

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_named_value_localized(var Destination: Pointer; Value: scs_named_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Store_scs_named_value_localized(out Ptr: Pointer; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Read_scs_named_value_localized(var Source: Pointer; out Value: scs_named_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Ptr_Readout_scs_named_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;

//------------------------------------------------------------------------------

{:
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
Function Stream_Write_scs_named_value_localized(Stream: TStream; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Stream_Read_scs_named_value_localized(Stream: TStream; out Value: scs_named_value_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;

//------------------------------------------------------------------------------

{:
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
Function Stream_Readout_scs_named_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see Ptr_Write_scs_value function for
                    details).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_telemetry_configuration(Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_telemetry_configuration(var Destination: Pointer; Value: scs_telemetry_configuration_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_telemetry_configuration(out Ptr: Pointer; Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type @code(scs_telemetry_configuration_t) from memory location
  given by pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_telemetry_configuration(var Source: Pointer; out Value: scs_telemetry_configuration_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type @code(scs_telemetry_configuration_t) from memory location
  given by pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Output value.
}
Function Ptr_Readout_scs_telemetry_configuration(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_telemetry_configuration_t;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type @code(scs_telemetry_configuration_t)
  into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_telemetry_configuration.

  @param(Stream      Stream to which the value will be written. Must not be
                     @nil.)
  @param Value       Value to be written.
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)

  @returns Number of bytes written.
}
Function Stream_Write_scs_telemetry_configuration(Stream: TStream; Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Number of bytes read.
}
Function Stream_Read_scs_telemetry_configuration(Stream: TStream; out Value: scs_telemetry_configuration_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads value of type @code(scs_telemetry_configuration_t) from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Output value.
}
Function Stream_Readout_scs_telemetry_configuration(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_telemetry_configuration_t;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value      Actual value for which the size is requested.
  @param(Minimize   When @true, function returns only minimal size required for
                    storing passed value (see Ptr_Write_scs_value_localized
                    function for details).)

  @returns Number of bytes required for storing passed value.
}
Function Size_scs_telemetry_configuration_localized(Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)
  @param(Minimize    Indicating whether attributes values should be saved
                     minimized.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_scs_telemetry_configuration_localized(var Destination: Pointer; Value: scs_telemetry_configuration_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
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

  @returns(Number of bytes allocated for storing (you can use this value for
           freeing returned pointer).)
}
Function Ptr_Store_scs_telemetry_configuration_localized(out Ptr: Pointer; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type scs_telemetry_configuration_localized_t from memory
  location given by pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param Value       Output variable.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Number of bytes read.
}
Function Ptr_Read_scs_telemetry_configuration_localized(var Source: Pointer; out Value: scs_telemetry_configuration_localized_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type scs_telemetry_configuration_localized_t from memory
  location given by pointer.

  @param Source      Memory location where to read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param(Advance     Indicating whether source pointer should be increased by
                     number of bytes read.)
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Output value.
}
Function Ptr_Readout_scs_telemetry_configuration_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_telemetry_configuration_localized_t;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type scs_telemetry_configuration_localized_t
  into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_scs_telemetry_configuration_localized.

  @param(Stream   Stream to which the value will be written. Must not be
                  @nil.)
  @param Value    Value to be written.
  @param(Minimize Indicating whether attributes values should be saved
                  minimized.)

  @returns Number of bytes written.
}
Function Stream_Write_scs_telemetry_configuration_localized(Stream: TStream; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration_localized.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param Value       Output variable.
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Number of bytes read.
}
Function Stream_Read_scs_telemetry_configuration_localized(Stream: TStream; out Value: scs_telemetry_configuration_localized_t; Minimized: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Reads value of type scs_telemetry_configuration_localized_t from a
  stream.)
  All loading options are the same as in function
  Ptr_Read_scs_telemetry_configuration_localized.

  @param Stream      Stream from which the value will be read. Must not be @nil.
  @param BytesRead   Number of bytes read.
  @param Minimized   Indicating whether attributes values were saved minimized.

  @returns Output value.
}
Function Stream_Readout_scs_telemetry_configuration_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_telemetry_configuration_localized_t;

{==============================================================================}
{   Telemetry library complex types storing and loading                        }
{==============================================================================}

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownEvent(Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{:
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
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_KnownEvent(var Destination: Pointer; Value: TKnownEvent; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TKnownEvent from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownEvent(var Source: Pointer; out Value: TKnownEvent; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownEvent from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownEvent(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownEvent;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TKnownEvent into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownEvent.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownEvent(Stream: TStream; Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownEvent from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownEvent(Stream: TStream; out Value: TKnownEvent): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownEvent from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownEvent(Stream: TStream; out BytesRead: TMemSize): TKnownEvent;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownChannel(Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{:
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
        IndexConfigID   variable    String
        IndexConfigAttr variable    String
        MaxIndex        4 bytes     scs_u32_t
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_KnownChannel(var Destination: Pointer; Value: TKnownChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TKnownChannel from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownChannel(var Source: Pointer; out Value: TKnownChannel; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownChannel from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownChannel;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TKnownChannel into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownChannel.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownChannel(Stream: TStream; Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownChannel from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownChannel(Stream: TStream; out Value: TKnownChannel): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownChannel from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownChannel(Stream: TStream; out BytesRead: TMemSize): TKnownChannel;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_KnownConfig(Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TKnownConfig into memory location given
            by pointer.)

  Resulting memory layout:
@preformatted(
          value       |   size    |   value type

        ID              variable    String
        AttrName        variable    String
        ValueType       4 bytes     scs_value_type_t
        Indexed         1 byte      Boolean
        Binded          1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_KnownConfig(var Destination: Pointer; Value: TKnownConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TKnownConfig from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_KnownConfig(var Source: Pointer; out Value: TKnownConfig; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownConfig from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_KnownConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TKnownConfig;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TKnownConfig into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_KnownConfig.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_KnownConfig(Stream: TStream; Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownConfig from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_KnownConfig(Stream: TStream; out Value: TKnownConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TKnownConfig from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_KnownConfig(Stream: TStream; out BytesRead: TMemSize): TKnownConfig;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_EventInfo({%H-}Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{:
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
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_EventInfo(var Destination: Pointer; Value: TEventInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TEventInfo from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_EventInfo(var Source: Pointer; out Value: TEventInfo; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TEventInfo from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_EventInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TEventInfo;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TEventInfo into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_EventInfo.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_EventInfo(Stream: TStream; Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TEventInfo from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_EventInfo(Stream: TStream; out Value: TEventInfo): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TEventInfo from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_EventInfo(Stream: TStream; out BytesRead: TMemSize): TEventInfo;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_ChannelInfo(Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{:
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
        IndexConfigID   variable    String
        IndexConfigAttr variable    String
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_ChannelInfo(var Destination: Pointer; Value: TChannelInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TChannelInfo from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_ChannelInfo(var Source: Pointer; out Value: TChannelInfo; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TChannelInfo from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_ChannelInfo(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TChannelInfo;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TChannelInfo into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_ChannelInfo.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_ChannelInfo(Stream: TStream; Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TChannelInfo from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_ChannelInfo(Stream: TStream; out Value: TChannelInfo): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TChannelInfo from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_ChannelInfo(Stream: TStream; out BytesRead: TMemSize): TChannelInfo;

//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_StoredConfig(Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TStoredConfig into memory location given
            by pointer.)
  Field @code(Value) is saved using function Ptr_Write_scs_value_localized or
  its alternatives with parameter @code(Minimize) set to @true (refer to them
  for details about resulting layout).

  Resulting memory layout:
@preformatted(
         value  |   size    |   value type

        ID        variable    String
        AttrName  variable    String
        Index     4 bytes     scs_u32_t
        Value     variable    scs_value_localized_t
        Binded    1 byte      Boolean
)

  @param Destination Memory location where to write. Must not be @nil.
  @param Value       Value to be written.
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_StoredConfig(var Destination: Pointer; Value: TStoredConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TStoredConfig from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_StoredConfig(var Source: Pointer; out Value: TStoredConfig; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredConfig from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_StoredConfig(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredConfig;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TStoredConfig into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_StoredConfig.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_StoredConfig(Stream: TStream; Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredConfig from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_StoredConfig(Stream: TStream; out Value: TStoredConfig): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredConfig from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_StoredConfig(Stream: TStream; out BytesRead: TMemSize): TStoredConfig;


//==============================================================================

{:
  Returns number of bytes required for storing passed value into memory or
  stream.

  @param Value Actual value for which the size is requested.

  @returns Number of bytes required for storing passed value.
}
Function Size_StoredChannel(Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{:
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
  @param Size        Number of bytes available in destination buffer.
  @param(Advance     Indicating whether destination pointer should be increased
                     by number of bytes written.)

  @returns Number of bytes written.

  @raises(ETLBufferTooSmall When destination buffer is too small for a data
                            being written.)
}
Function Ptr_Write_StoredChannel(var Destination: Pointer; Value: TStoredChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
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

{:
  Reads value of type TStoredChannel from memory location given by pointer.

  @param Source  Memory location where to read. Must not be @nil.
  @param Value   Output variable.
  @param(Advance Indicating whether source pointer should be increased by number
                 of bytes read.)

  @returns Number of bytes read.
}
Function Ptr_Read_StoredChannel(var Source: Pointer; out Value: TStoredChannel; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredChannel from memory location given by pointer.

  @param Source    Memory location where to read. Must not be @nil.
  @param BytesRead Number of bytes read.
  @param(Advance   Indicating whether source pointer should be increased by
                   number of bytes read.)

  @returns Output value.
}
Function Ptr_Readout_StoredChannel(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True): TStoredChannel;

//------------------------------------------------------------------------------

{:
  @abstract(Writes passed value of type TStoredChannel into a stream.)
  All saving options and resulting memory layout are the same as in function
  Ptr_Write_StoredChannel.

  @param Stream Stream to which the value will be written. Must not be @nil.
  @param Value  Value to be written.

  @returns Number of bytes written.
}
Function Stream_Write_StoredChannel(Stream: TStream; Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredChannel from a stream.

  @param Stream Stream from which the value will be read. Must not be @nil.
  @param Value  Output variable.

  @returns Number of bytes read.
}
Function Stream_Read_StoredChannel(Stream: TStream; out Value: TStoredChannel): TMemSize;

//------------------------------------------------------------------------------

{:
  Reads value of type TStoredChannel from a stream.

  @param Stream    Stream from which the value will be read. Must not be @nil.
  @param BytesRead Number of bytes read.

  @returns Output value.
}
Function Stream_Readout_StoredChannel(Stream: TStream; out BytesRead: TMemSize): TStoredChannel;

implementation

uses
  SysUtils,
  BinaryStreaming;

{------------------------------------------------------------------------------}
{==============================================================================}
{   Unit functions and procedures implementation                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function TelemetryStringBytes(const Str: TelemetryString = ''): TMemSize;
begin
Result := SizeOf(Int32){string length} + Length(Str) * SizeOf(TUTF8Char);
end;

//------------------------------------------------------------------------------

Function APIStringBytes(Str: scs_string_t): TMemSize;
begin
If Assigned(Str) then
  Result := SizeOf(Int32){string length} + Length(PAnsiChar(Str)) * SizeOf(TUTF8Char)
else
  Result := SizeOf(Int32){string length};
end;

{==============================================================================}
{   String types storing and loading (memory)                                  }
{==============================================================================}

Function Ptr_WriteTelemetryString(var Destination: Pointer; const Str: TelemetryString; Advance: Boolean = True): TMemSize;
begin
Result := Ptr_WriteUTF8String(Destination,UTF8String(Str),Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadTelemetryString(var Source: Pointer; out Str: TelemetryString; Advance: Boolean = True): TMemSize;
begin
Result := Ptr_ReadUTF8String(Source,UTF8String(Str),Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_ReadoutTelemetryString(var Source: Pointer; Advance: Boolean = True): TelemetryString;
begin
Result := TelemetryString(Ptr_ReadUTF8String(Source,Advance));
end;

//==============================================================================

Function Ptr_Write_scs_string(var Destination: Pointer; Str: scs_string_t; Advance: Boolean = True): TMemSize;
var
  TempStr:  TelemetryString;
begin
TempStr := APIStringToTelemetryString(Str);
Result := Ptr_WriteTelemetryString(Destination,TempStr,Advance);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_string(var Source: Pointer; out Str: scs_string_t; Advance: Boolean = True): TMemSize;
var
  TempStr:  TelemetryString;
begin
Result := Ptr_ReadTelemetryString(Source,TempStr,Advance);
Str := TelemetryStringtoAPIString(TempStr);
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_string(var Source: Pointer; Advance: Boolean = True): scs_string_t;
begin
Ptr_Read_scs_string(Source,Result,Advance);
end;

{==============================================================================}
{   String types storing and loading (stream)                                  }
{==============================================================================}

Function Stream_WriteTelemetryString(Stream: TStream; const Str: TelemetryString): TMemSize;
begin
Result := Stream_WriteUTF8String(Stream,UTF8String(Str));
end;

//------------------------------------------------------------------------------

Function Stream_ReadTelemetryString(Stream: TStream; out Str: TelemetryString): TMemSize;
begin
Result := Stream_ReadUTF8String(Stream,UTF8String(Str));
end;

//------------------------------------------------------------------------------

Function Stream_ReadoutTelemetryString(Stream: TStream): TelemetryString;
begin
Result := TelemetryString(Stream_ReadUTF8String(Stream));
end;

//==============================================================================

Function Stream_Write_scs_string(Stream: TStream; Str: scs_string_t): TMemSize;
var
  TempStr:  TelemetryString;
begin
TempStr := APIStringToTelemetryString(Str);
Result := Stream_WriteTelemetryString(Stream,TempStr);
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_string(Stream: TStream; out Str: scs_string_t): TMemSize;
var
  TempStr:  TelemetryString;
begin
Result := Stream_ReadTelemetryString(Stream,TempStr);
Str := TelemetryStringToAPIString(TempStr);
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_string(Stream: TStream): scs_string_t;
begin
Stream_Read_scs_string(Stream,Result);
end;

{==============================================================================}
{   Complex SDK types storing and loading                                      }
{==============================================================================}

Function Size_scs_value(Value: scs_value_t; Minimize: Boolean = False): TMemSize;
begin
If Value._type = SCS_VALUE_TYPE_string then
  Result := SizeOf(scs_value_type_t) + APIStringBytes(Value.value_string.value)
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
          raise ETLUnknownData.CreateFmt('Size_scs_value: Unknown value type (%d).',[Value._type]);
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
        Result := Ptr_WriteUInt32(WorkPtr,Value._type,True);
        Inc(Result,Ptr_Write_scs_string(WorkPtr,Value.value_string.value,True));
      end
    else
      begin
        If Minimize then
          begin
            Result := Ptr_WriteUInt32(WorkPtr,Value._type,True);
            case Value._type of
              SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_WriteUInt8(WorkPtr,Value.value_bool.value,True));
              SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_WriteInt32(WorkPtr,Value.value_s32.value,True));
              SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.value_u32.value,True));
              SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_WriteUInt64(WorkPtr,Value.value_u64.value,True));
              SCS_VALUE_TYPE_float:       Inc(Result,Ptr_WriteFloat32(WorkPtr,Value.value_float.value,True));
              SCS_VALUE_TYPE_double:      Inc(Result,Ptr_WriteFloat64(WorkPtr,Value.value_double.value,True));
              SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_fvector,SizeOf(scs_value_fvector_t),True));
              SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_dvector,SizeOf(scs_value_dvector_t),True));
              SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_euler,SizeOf(scs_value_euler_t),True));
              SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_fplacement,SizeOf(scs_value_fplacement_t),True));
              SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.value_dplacement,SizeOf(scs_value_dplacement_t),True));
            else
              raise ETLUnknownData.CreateFmt('Ptr_Write_scs_value_t: Unknown value type (%d).',[Value._type]);
            end;
          end
        else Result := Ptr_WriteBuffer(WorkPtr,Value,SizeOf(scs_value_t),True);
      end;
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_value: Output buffer too small (got %d, expected %d).',[Size,Size_scs_value(Value,Minimize)]);
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
begin
WorkPtr := Source;
Result := Ptr_ReadUInt32(WorkPtr,Value._type,True);
If Value._type = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Ptr_Read_scs_string(WorkPtr,Value.value_string.value,True));
  end
else
  begin
    If Minimized then
      begin
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_ReadUInt8(WorkPtr,Value.value_bool.value,True));
          SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_ReadInt32(WorkPtr,Value.value_s32.value,True));
          SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.value_u32.value,True));
          SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_ReadUInt64(WorkPtr,Value.value_u64.value,True));
          SCS_VALUE_TYPE_float:       Inc(Result,Ptr_ReadFloat32(WorkPtr,Value.value_float.value,True));
          SCS_VALUE_TYPE_double:      Inc(Result,Ptr_ReadFloat64(WorkPtr,Value.value_double.value,True));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_fvector,SizeOf(scs_value_fvector_t),True));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_dvector,SizeOf(scs_value_dvector_t),True));
          SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_euler,SizeOf(scs_value_euler_t),True));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_fplacement,SizeOf(scs_value_fplacement_t),True));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.value_dplacement,SizeOf(scs_value_dplacement_t),True));
        else
          raise ETLUnknownData.CreateFmt('Ptr_Read_scs_value_t: Unknown value type (%d).',[Value._type]);
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
    Result := Stream_WriteUInt32(Stream,Value._type);
    Inc(Result,Stream_Write_scs_string(Stream,Value.value_string.value));
  end
else
  begin
    If Minimize then
      begin
        Result := Stream_WriteUInt32(Stream,Value._type);
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_WriteUInt8(Stream,Value.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_WriteInt32(Stream,Value.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_WriteUInt32(Stream,Value.value_u32.value));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_WriteUInt64(Stream,Value.value_u64.value));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_WriteFloat32(Stream,Value.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_WriteFloat64(Stream,Value.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_WriteBuffer(Stream,Value.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise ETLUnknownData.CreateFmt('Stream_Write_scs_value_t: Unknown value type (%d).',[Value._type]);
        end;
      end
    else Result := Stream_WriteBuffer(Stream,Value,SizeOf(scs_value_t));
  end;
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_value(Stream: TStream; out Value: scs_value_t; Minimized: Boolean = False): TMemSize;
begin
Result := Stream_ReadUInt32(Stream,Value._type);
If Value._type = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Stream_Read_scs_string(Stream,Value.value_string.value));
  end
else
  begin
    If Minimized then
      begin
        case Value._type of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_ReadUInt8(Stream,Value.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_ReadInt32(Stream,Value.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_ReadUInt32(Stream,Value.value_u32.value));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_ReadUInt64(Stream,Value.value_u64.value));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_ReadFloat32(Stream,Value.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_ReadFloat64(Stream,Value.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_ReadBuffer(Stream,Value.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise ETLUnknownData.CreateFmt('Stream_Read_scs_value_t: Unknown value type (%d).',[Value._type]);
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
  Result := SizeOf(scs_value_type_t) + TelemetryStringBytes(Value.StringData)
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
          raise ETLUnknownData.CreateFmt('Size_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
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
        Result := Ptr_WriteUInt32(WorkPtr,Value.ValueType,True);
        Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.StringData,True));
      end
    else
      begin
        If Minimize then
          begin
            Result := Ptr_WriteUInt32(WorkPtr,Value.ValueType,True);
            case Value.ValueType of
              SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_WriteUInt8(WorkPtr,Value.BinaryData.value_bool.value,True));
              SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_WriteInt32(WorkPtr,Value.BinaryData.value_s32.value,True));
              SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.BinaryData.value_u32.value,True));
              SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_WriteUInt64(WorkPtr,Value.BinaryData.value_u64.value,True));
              SCS_VALUE_TYPE_float:       Inc(Result,Ptr_WriteFloat32(WorkPtr,Value.BinaryData.value_float.value,True));
              SCS_VALUE_TYPE_double:      Inc(Result,Ptr_WriteFloat64(WorkPtr,Value.BinaryData.value_double.value,True));
              SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t),True));
              SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t),True));
              SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t),True));
              SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t),True));
              SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_WriteBuffer(WorkPtr,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t),True));
            else
              raise ETLUnknownData.CreateFmt('Ptr_Write_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
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
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_value_localized: Output buffer too small (got %d, expected %d).',[Size,Size_scs_value_localized(Value,Minimize)]);
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
Result := Ptr_ReadUInt32(WorkPtr,Value.ValueType,True);
If Value.ValueType = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.StringData,True));
    Value.BinaryData._type := SCS_VALUE_TYPE_INVALID;
  end
else
  begin
    Value.StringData := '';
    If Minimized then
      begin
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Ptr_ReadUInt8(WorkPtr,Value.BinaryData.value_bool.value,True));
          SCS_VALUE_TYPE_s32:         Inc(Result,Ptr_ReadInt32(WorkPtr,Value.BinaryData.value_s32.value,True));
          SCS_VALUE_TYPE_u32:         Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.BinaryData.value_u32.value,True));
          SCS_VALUE_TYPE_u64:         Inc(Result,Ptr_ReadUInt64(WorkPtr,Value.BinaryData.value_u64.value,True));
          SCS_VALUE_TYPE_float:       Inc(Result,Ptr_ReadFloat32(WorkPtr,Value.BinaryData.value_float.value,True));
          SCS_VALUE_TYPE_double:      Inc(Result,Ptr_ReadFloat64(WorkPtr,Value.BinaryData.value_double.value,True));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t),True));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t),True));
          SCS_VALUE_TYPE_euler:       Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t),True));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t),True));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Ptr_ReadBuffer(WorkPtr,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t),True));
        else
          raise ETLUnknownData.CreateFmt('Ptr_Read_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
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
    Result := Stream_WriteUInt32(Stream,Value.ValueType);
    Inc(result,Stream_WriteTelemetryString(Stream,Value.StringData));
  end
else
  begin
    If Minimize then
      begin
        Result := Stream_WriteUInt32(Stream,Value.ValueType);
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_WriteUInt8(Stream,Value.BinaryData.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_WriteInt32(Stream,Value.BinaryData.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_WriteUInt32(Stream,Value.BinaryData.value_u32.value));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_WriteUInt64(Stream,Value.BinaryData.value_u64.value));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_WriteFloat32(Stream,Value.BinaryData.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_WriteFloat64(Stream,Value.BinaryData.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_WriteBuffer(Stream,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise ETLUnknownData.CreateFmt('Stream_Write_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
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
Result := Stream_ReadUInt32(Stream,Value.ValueType);
If Value.ValueType = SCS_VALUE_TYPE_string then
  begin
    Inc(Result,Stream_ReadTelemetryString(Stream,Value.StringData));
    Value.BinaryData._type := SCS_VALUE_TYPE_INVALID;
  end
else
  begin
    Value.StringData := '';
    If Minimized then
      begin
        case Value.ValueType of
          SCS_VALUE_TYPE_bool:        Inc(Result,Stream_ReadUInt8(Stream,Value.BinaryData.value_bool.value));
          SCS_VALUE_TYPE_s32:         Inc(Result,Stream_ReadInt32(Stream,Value.BinaryData.value_s32.value));
          SCS_VALUE_TYPE_u32:         Inc(Result,Stream_ReadUInt32(Stream,Value.BinaryData.value_u32.value));
          SCS_VALUE_TYPE_u64:         Inc(Result,Stream_ReadUInt64(Stream,Value.BinaryData.value_u64.value));
          SCS_VALUE_TYPE_float:       Inc(Result,Stream_ReadFloat32(Stream,Value.BinaryData.value_float.value));
          SCS_VALUE_TYPE_double:      Inc(Result,Stream_ReadFloat64(Stream,Value.BinaryData.value_double.value));
          SCS_VALUE_TYPE_fvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_fvector,SizeOf(scs_value_fvector_t)));
          SCS_VALUE_TYPE_dvector:     Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_dvector,SizeOf(scs_value_dvector_t)));
          SCS_VALUE_TYPE_euler:       Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_euler,SizeOf(scs_value_euler_t)));
          SCS_VALUE_TYPE_fplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_fplacement,SizeOf(scs_value_fplacement_t)));
          SCS_VALUE_TYPE_dplacement:  Inc(Result,Stream_ReadBuffer(Stream,Value.BinaryData.value_dplacement,SizeOf(scs_value_dplacement_t)));
        else
          raise ETLUnknownData.CreateFmt('Stream_Read_scs_value_localized_t: Unknown value type (%d).',[Value.ValueType]);
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
  Result := APIStringBytes(Value.name) + SizeOf(scs_u32_t) + Size_scs_value(Value.value,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_named_value(var Destination: Pointer; Value: scs_named_value_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_named_value(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Result := Ptr_WriteUInt32(WorkPtr,NameIDFunc(APIStringToTelemetryString(Value.name),UserData),True)
    else
      Result := Ptr_Write_scs_string(WorkPtr,Value.name,True);
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.index,True));
    Inc(Result,Ptr_Write_scs_value(WorkPtr,Value.value,Size - Result,True,Minimize));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_named_value: Output buffer too small (got %d, expected %d).',[Size,Size_scs_named_value(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_named_value(out Ptr: Pointer; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_named_value(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_named_value(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_named_value(var Source: Pointer; out Value: scs_named_value_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  TempID:   TItemID;
begin
WorkPtr := Source;
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Ptr_ReadUInt32(WorkPtr,TempID,True);
    Value.name := TelemetryStringToAPIString(IDNameFunc(TempID,UserData));
  end
else Result := Ptr_Read_scs_string(WorkPtr,Value.name,True);
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.index,True));
Inc(Result,Ptr_Read_scs_value(WorkPtr,Value.value,True,Minimized));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_named_value(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;
begin
BytesRead := Ptr_Read_scs_named_value(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_named_value(Stream: TStream; Value: scs_named_value_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
If ItemIDOnly and Assigned(NameIDFunc) then
  Result := Stream_WriteUInt32(Stream,NameIDFunc(APIStringToTelemetryString(Value.name),UserData))
else
  Result := Stream_Write_scs_string(Stream,Value.name);
Inc(Result,Stream_WriteUInt32(Stream,Value.index));
Inc(Result,Stream_Write_scs_value(Stream,Value.value,Minimize));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_named_value(Stream: TStream; out Value: scs_named_value_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  TempID: TItemID;
begin
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Stream_ReadUInt32(Stream,TempID);
    Value.name := TelemetryStringToAPIString(IDNameFunc(TempID,UserData));
  end
else Result := Stream_Read_scs_string(Stream,Value.name);
Inc(Result,Stream_ReadUInt32(Stream,Value.index));
Inc(Result,Stream_Read_scs_value(Stream,Value.value,Minimized));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_named_value(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_t;
begin
BytesRead := Stream_Read_scs_named_value(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//==============================================================================

Function Size_scs_named_value_localized(Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False): TMemSize;
begin
If ItemIDOnly then
  Result := SizeOf(TItemID) + SizeOf(scs_u32_t) + Size_scs_value_localized(Value.Value,Minimize)
else
  Result := TelemetryStringBytes(Value.Name) + SizeOf(scs_u32_t) + Size_scs_value_localized(Value.Value,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_named_value_localized(var Destination: Pointer; Value: scs_named_value_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_scs_named_value_localized(Value,Minimize,ItemIDOnly) then
  begin
    WorkPtr := Destination;
    If ItemIDOnly and Assigned(NameIDFunc) then
      Result := Ptr_WriteUInt32(WorkPtr,NameIDFunc(Value.Name,UserData),True)
    else
      Result := Ptr_WriteTelemetryString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Index,True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,Minimize));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_named_value_localized: Output buffer too small (got %d, expected %d).',[Size,Size_scs_named_value_localized(Value,Minimize,ItemIDOnly)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_named_value_localized(out Ptr: Pointer; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
Result := Size_scs_named_value_localized(Value,Minimize,ItemIDOnly);
Ptr := AllocMem(Result);
Ptr_Write_scs_named_value_localized(Ptr,Value,Result,False,Minimize,ItemIDOnly,NameIDFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_named_value_localized(var Source: Pointer; out Value: scs_named_value_localized_t; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  WorkPtr:  Pointer;
  TempID:   TItemID;
begin
WorkPtr := Source;
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Ptr_ReadUInt32(WorkPtr,TempID,True);
    Value.Name := IDNameFunc(TempID,UserData);
  end
else Result := Ptr_ReadTelemetryString(WorkPtr,Value.Name,True);
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Index,True));
Inc(Result,Ptr_Read_scs_value_localized(WorkPtr,Value.Value,True,Minimized));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_named_value_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;
begin
BytesRead := Ptr_Read_scs_named_value_localized(Source,Result,Advance,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_named_value_localized(Stream: TStream; Value: scs_named_value_localized_t; Minimize: Boolean = False; ItemIDOnly: Boolean = False; NameIDFunc: TNameToIDFunc = nil; UserData: Pointer = nil): TMemSize;
begin
If ItemIDOnly and Assigned(NameIDFunc) then
  Result := Stream_WriteUInt32(Stream,NameIDFunc(Value.Name,UserData))
else
  Result := Stream_WriteTelemetryString(Stream,Value.Name);
Inc(Result,Stream_WriteUInt32(Stream,Value.Index));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,Minimize));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_named_value_localized(Stream: TStream; out Value: scs_named_value_localized_t; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): TMemSize;
var
  TempID: TItemID;
begin
If ItemIDOnly and Assigned(IDNameFunc) then
  begin
    Result := Stream_ReadUInt32(Stream,TempID);
    Value.Name := IDNameFunc(TempID,UserData);
  end
else Result := Stream_ReadTelemetryString(Stream,Value.Name);
Inc(Result,Stream_ReadUInt32(Stream,Value.Index));
Inc(Result,Stream_Read_scs_value_localized(Stream,Value.Value,Minimized));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_named_value_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False; ItemIDOnly: Boolean = False; IDNameFunc: TIDToNameFunc = nil; UserData: Pointer = nil): scs_named_value_localized_t;
begin
BytesRead := Stream_Read_scs_named_value_localized(Stream,Result,Minimized,ItemIDOnly,IDNameFunc,UserData);
end;

//==============================================================================

Function Size_scs_telemetry_configuration(Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;
var
  CurrAttrPtr:  p_scs_named_value_t;
begin
Result := APIStringBytes(Value.id) + SizeOf(Integer);
CurrAttrPtr := Value.attributes;
while Assigned(CurrAttrPtr^.name) do
  begin
    Inc(Result,Size_scs_named_value(CurrAttrPtr^,Minimize));
    Inc(CurrAttrPtr);
  end;
Inc(Result,APIStringBytes);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_telemetry_configuration(var Destination: Pointer; Value: scs_telemetry_configuration_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;
var
  WorkPtr:      Pointer;
  CurrAttrPtr:  p_scs_named_value_t;
  Count:        Integer;
  CountPtr:     PInteger;
begin
If Size >= Size_scs_telemetry_configuration(Value,Minimize) then
  begin
    WorkPtr := Destination;
    Result := Ptr_Write_scs_string(WorkPtr,Value.id,True);
    CountPtr := WorkPtr;
    Count := 1;
    Inc(Result,Ptr_WriteInt32(WorkPtr,Count,True));
    CurrAttrPtr := Value.attributes;
    while Assigned(CurrAttrPtr^.name) do
      begin
        Inc(Result,Ptr_Write_scs_named_value(WorkPtr,CurrAttrPtr^,Size - Result,True,Minimize));
        Inc(CurrAttrPtr);
        Inc(Count);
      end;
    Inc(Result,Ptr_Write_scs_string(WorkPtr,nil,True));
    CountPtr^ := Count;
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_telemetry_configuration: Output buffer too small (got %d, expected %d).',[Size,Size_scs_telemetry_configuration(Value,Minimize)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_telemetry_configuration(out Ptr: Pointer; Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;
begin
Result := Size_scs_telemetry_configuration(Value,Minimize);
Ptr := AllocMem(Result);
Ptr_Write_scs_telemetry_configuration(Ptr,Value,Result,False,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_telemetry_configuration(var Source: Pointer; out Value: scs_telemetry_configuration_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;
var
  WorkPtr:      Pointer;
  i,Count:      Integer;
  CurrAttrPtr:  p_scs_named_value_t;
begin
WorkPtr := Source;
Result := Ptr_Read_scs_string(WorkPtr,Value.id,True);
Inc(Result,Ptr_ReadInt32(WorkPtr,Count,True));
Value.attributes := AllocMem(Count * SizeOf(scs_named_value_t));
CurrAttrPtr := Value.attributes;
For i := 2 to Count do
  begin
    Inc(Result,Ptr_Read_scs_named_value(WorkPtr,CurrAttrPtr^,True,Minimized));
    Inc(CurrAttrPtr);
  end;
Inc(Result,Ptr_Read_scs_string(WorkPtr,CurrAttrPtr^.name,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_telemetry_configuration(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_telemetry_configuration_t;
begin
BytesRead := Ptr_Read_scs_telemetry_configuration(Source,Result,Advance,Minimized);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_telemetry_configuration(Stream: TStream; Value: scs_telemetry_configuration_t; Minimize: Boolean = False): TMemSize;
var
  Count:        Integer;
  CurrAttrPtr:  p_scs_named_value_t;
  CountPos:     Int64;
  EndPos:       Int64;
begin
Result := Stream_Write_scs_string(Stream,Value.id);
CountPos := Stream.Position;
Count := 1;
Inc(Result,Stream_WriteInt32(Stream,Count));
CurrAttrPtr := Value.attributes;
while Assigned(CurrAttrPtr^.name) do
  begin
    Inc(Result,Stream_Write_scs_named_value(Stream,CurrAttrPtr^,Minimize));
    Inc(CurrAttrPtr);
    Inc(Count);
  end;
Inc(Result,Stream_Write_scs_string(Stream,nil));
EndPos := Stream.Position;
Stream.Seek(CountPos,soBeginning);
Stream_WriteInt32(Stream,Count);
Stream.Seek(EndPos,soBeginning);
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_telemetry_configuration(Stream: TStream; out Value: scs_telemetry_configuration_t; Minimized: Boolean = False): TMemSize;
var
  i,Count:      Integer;
  CurrAttrPtr:  p_scs_named_value_t;
begin
Result := Stream_Read_scs_string(Stream,Value.id);
Inc(Result,Stream_ReadInt32(Stream,Count));
Value.attributes := AllocMem(Count * SizeOf(scs_named_value_t));
CurrAttrPtr := Value.attributes;
For i := 2 to Count do
  begin
    Inc(Result,Stream_Read_scs_named_value(Stream,CurrAttrPtr^,Minimized));
    Inc(CurrAttrPtr);
  end;
Inc(Result,Stream_Read_scs_string(Stream,CurrAttrPtr^.name));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_telemetry_configuration(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_telemetry_configuration_t;
begin
BytesRead := Stream_Read_scs_telemetry_configuration(Stream,Result,Minimized);
end;

//==============================================================================

Function Size_scs_telemetry_configuration_localized(Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;
var
  i: Integer;
begin
Result := TelemetryStringBytes(Value.ID) + SizeOf(Integer);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  Inc(Result,Size_scs_named_value_localized(Value.Attributes[i],Minimize));
Inc(Result,TelemetryStringBytes);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_scs_telemetry_configuration_localized(var Destination: Pointer; Value: scs_telemetry_configuration_localized_t; Size: TMemSize; Advance: Boolean = True; Minimize: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
  i:        Integer;
begin
If Size >= Size_scs_telemetry_configuration_localized(Value,Minimize) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.ID,True);
    Inc(Result,Ptr_WriteInt32(WorkPtr,Length(Value.Attributes) + 1,True));
    For i := Low(Value.Attributes) to High(Value.Attributes) do
      Inc(Result,Ptr_Write_scs_named_value_localized(WorkPtr,Value.Attributes[i],Size - Result,True,Minimize));
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,'',True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_scs_telemetry_configuration_localized: Output buffer too small (got %d, expected %d).',
                               [Size,Size_scs_telemetry_configuration_localized(Value,Minimize)]);
end;

//------------------------------------------------------------------------------

Function Ptr_Store_scs_telemetry_configuration_localized(out Ptr: Pointer; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;
begin
Result := Size_scs_telemetry_configuration_localized(Value,Minimize);
Ptr := AllocMem(Result);
Ptr_Write_scs_telemetry_configuration_localized(Ptr,Value,Result,False,Minimize);
end;

//------------------------------------------------------------------------------

Function Ptr_Read_scs_telemetry_configuration_localized(var Source: Pointer; out Value: scs_telemetry_configuration_localized_t; Advance: Boolean = True; Minimized: Boolean = False): TMemSize;
var
  WorkPtr:  Pointer;
  TempStr:  TelemetryString;
  i,Count:  Integer;
begin
WorkPtr := Source;
Result := Ptr_ReadTelemetryString(WorkPtr,Value.ID,True);
Inc(Result,Ptr_ReadInt32(WorkPtr,Count,True));
SetLength(Value.Attributes,Count - 1);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  Inc(Result,Ptr_Read_scs_named_value_localized(WorkPtr,Value.Attributes[i],True,Minimized));
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,TempStr,True));
If Advance then Source := WorkPtr;
end;

//------------------------------------------------------------------------------

Function Ptr_Readout_scs_telemetry_configuration_localized(var Source: Pointer; out BytesRead: TMemSize; Advance: Boolean = True; Minimized: Boolean = False): scs_telemetry_configuration_localized_t;
begin
BytesRead := Ptr_Read_scs_telemetry_configuration_localized(Source,Result,Advance,Minimized);
end;

//------------------------------------------------------------------------------

Function Stream_Write_scs_telemetry_configuration_localized(Stream: TStream; Value: scs_telemetry_configuration_localized_t; Minimize: Boolean = False): TMemSize;
var
  i:  Integer;
begin
Result := Stream_WriteTelemetryString(Stream,Value.ID);
Inc(Result,Stream_WriteInt32(Stream,Length(Value.Attributes) + 1));
For i := Low(Value.Attributes) to High(Value.Attributes) do
  Inc(Result,Stream_Write_scs_named_value_localized(Stream,Value.Attributes[i],Minimize));
Inc(Result,Stream_WriteTelemetryString(Stream,''));
end;

//------------------------------------------------------------------------------

Function Stream_Read_scs_telemetry_configuration_localized(Stream: TStream; out Value: scs_telemetry_configuration_localized_t; Minimized: Boolean = False): TMemSize;
var
  TempStr:  TelemetryString;
  i,Count:  Integer;
begin
Result := Stream_ReadTelemetryString(Stream,Value.ID);
Inc(Result,Stream_ReadInt32(Stream,Count));
SetLength(Value.Attributes,Count - 1);
For i := Low(Value.Attributes) to High(Value.Attributes) do
  Inc(Result,Stream_Read_scs_named_value_localized(Stream,Value.Attributes[i],Minimized));
Inc(Result,Stream_ReadTelemetryString(Stream,TempStr));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_scs_telemetry_configuration_localized(Stream: TStream; out BytesRead: TMemSize; Minimized: Boolean = False): scs_telemetry_configuration_localized_t;
begin
BytesRead := Stream_Read_scs_telemetry_configuration_localized(Stream,Result,Minimized);
end;

{==============================================================================}
{   Telemetry library complex types storing and loading                        }
{==============================================================================}

Function Size_KnownEvent(Value: TKnownEvent): TMemSize;
begin
Result := SizeOf(scs_event_t) + TelemetryStringBytes(Value.Name) + 2 * SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownEvent(var Destination: Pointer; Value: TKnownEvent; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownEvent(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteUInt32(WorkPtr,Value.Event,True);
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.Name,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Valid,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Utility,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_KnownEvent: Output buffer too small (got %d, expected %d).',[Size,Size_KnownEvent(Value)]);
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
Result := Ptr_ReadUInt32(WorkPtr,Value.Event,True);
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.Name,True));
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
Result := Stream_WriteUInt32(Stream,Value.Event);
Inc(Result,Stream_WriteTelemetryString(Stream,Value.Name));
Inc(Result,Stream_WriteBoolean(Stream,Value.Valid));
Inc(Result,Stream_WriteBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownEvent(Stream: TStream; out Value: TKnownEvent): TMemSize;
begin
Result := Stream_ReadUInt32(Stream,Value.Event);
Inc(Result,Stream_ReadTelemetryString(Stream,Value.Name));
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
Result := TelemetryStringBytes(Value.Name) + SizeOf(TChannelID) + 3 * SizeOf(scs_value_type_t) +
          SizeOf(Boolean) + TelemetryStringBytes(Value.IndexConfig.ID) +
          TelemetryStringBytes(Value.IndexConfig.Attribute) + SizeOf(scs_u32_t);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownChannel(var Destination: Pointer; Value: TKnownChannel; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownChannel(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.ID,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.PrimaryType,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.SecondaryTypes,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Indexed,True));
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.IndexConfig.ID,True));
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.IndexConfig.Attribute,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.MaxIndex,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_KnownChannel: Output buffer too small (got %d, expected %d).',[Size,Size_KnownChannel(Value)]);
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
Result := Ptr_ReadTelemetryString(WorkPtr,Value.Name,True);
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.ID,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.PrimaryType,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.SecondaryTypes,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Indexed,True));
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.IndexConfig.ID,True));
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.IndexConfig.Attribute,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.MaxIndex,True));
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
Result := Stream_WriteTelemetryString(Stream,Value.Name);
Inc(Result,Stream_WriteUInt32(Stream,Value.ID));
Inc(Result,Stream_WriteUInt32(Stream,Value.PrimaryType));
Inc(Result,Stream_WriteUInt32(Stream,Value.SecondaryTypes));
Inc(Result,Stream_WriteBoolean(Stream,Value.Indexed));
Inc(Result,Stream_WriteTelemetryString(Stream,Value.IndexConfig.ID));
Inc(Result,Stream_WriteTelemetryString(Stream,Value.IndexConfig.Attribute));
Inc(Result,Stream_WriteUInt32(Stream,Value.MaxIndex));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownChannel(Stream: TStream; out Value: TKnownChannel): TMemSize;
begin
Result := Stream_ReadTelemetryString(Stream,Value.Name);
Inc(Result,Stream_ReadUInt32(Stream,Value.ID));
Inc(Result,Stream_ReadUInt32(Stream,Value.PrimaryType));
Inc(Result,Stream_ReadUInt32(Stream,Value.SecondaryTypes));
Inc(Result,Stream_ReadBoolean(Stream,Value.Indexed));
Inc(Result,Stream_ReadTelemetryString(Stream,Value.IndexConfig.ID));
Inc(Result,Stream_ReadTelemetryString(Stream,Value.IndexConfig.Attribute));
Inc(Result,Stream_ReadUInt32(Stream,Value.MaxIndex));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_KnownChannel(Stream: TStream; out BytesRead: TMemSize): TKnownChannel;
begin
BytesRead := Stream_Read_KnownChannel(Stream,Result);
end;

//==============================================================================

Function Size_KnownConfig(Value: TKnownConfig): TMemSize;
begin
Result := TelemetryStringBytes(Value.ID) + TelemetryStringBytes(Value.Attribute.Name) +
          SizeOf(scs_value_type_t) + 2 * SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_KnownConfig(var Destination: Pointer; Value: TKnownConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_KnownConfig(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.ID,True);
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.Attribute.Name,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Attribute.ValueType,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Attribute.Indexed,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Attribute.Binded,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_KnownConfig: Output buffer too small (got %d, expected %d).',[Size,Size_KnownConfig(Value)]);
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
Result := Ptr_ReadTelemetryString(WorkPtr,Value.ID,True);
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.Attribute.Name,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Attribute.ValueType,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Attribute.Indexed,True));
Inc(Result,Ptr_ReadBoolean(WorkPtr,Value.Attribute.Binded,True));
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
Result := Stream_WriteTelemetryString(Stream,Value.ID);
Inc(Result,Stream_WriteTelemetryString(Stream,Value.Attribute.Name));
Inc(Result,Stream_WriteUInt32(Stream,Value.Attribute.ValueType));
Inc(Result,Stream_WriteBoolean(Stream,Value.Attribute.Indexed));
Inc(Result,Stream_WriteBoolean(Stream,Value.Attribute.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Read_KnownConfig(Stream: TStream; out Value: TKnownConfig): TMemSize;
begin
Result := Stream_ReadTelemetryString(Stream,Value.ID);
Inc(Result,Stream_ReadTelemetryString(Stream,Value.Attribute.Name));
Inc(Result,Stream_ReadUInt32(Stream,Value.Attribute.ValueType));
Inc(Result,Stream_ReadBoolean(Stream,Value.Attribute.Indexed));
Inc(Result,Stream_ReadBoolean(Stream,Value.Attribute.Binded));
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
    Result := Ptr_WriteUInt32(WorkPtr,Value.Event,True);
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Utility,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_EventInfo: Output buffer too small (got %d, expected %d).',[Size,Size_EventInfo(Value)]);
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
Result := Ptr_ReadUInt32(WorkPtr,Value.Event,True);
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
Result := Stream_WriteUInt32(Stream,Value.Event);
Inc(Result,Stream_WriteBoolean(Stream,Value.Utility));
end;

//------------------------------------------------------------------------------

Function Stream_Read_EventInfo(Stream: TStream; out Value: TEventInfo): TMemSize;
begin
Result := Stream_ReadUInt32(Stream,Value.Event);
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
Result := TelemetryStringBytes(Value.Name) + SizeOf(TChannelID) + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t) +
          SizeOf(scs_u32_t) + TelemetryStringBytes(Value.IndexConfig.ID) + TelemetryStringBytes(Value.IndexConfig.Attribute);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_ChannelInfo(var Destination: Pointer; Value: TChannelInfo; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_ChannelInfo(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.ID,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Index,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.ValueType,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Flags,True));
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.IndexConfig.ID,True));
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.IndexConfig.Attribute,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_ChannelInfo: Output buffer too small (got %d, expected %d).',[Size,Size_ChannelInfo(Value)]);
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
Result := Ptr_ReadTelemetryString(WorkPtr,Value.Name,True);
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.ID,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Index,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.ValueType,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Flags,True));
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.IndexConfig.ID,True));
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.IndexConfig.Attribute,True));
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
Result := Stream_WriteTelemetryString(Stream,Value.Name);
Inc(Result,Stream_WriteUInt32(Stream,Value.ID));
Inc(Result,Stream_WriteUInt32(Stream,Value.Index));
Inc(Result,Stream_WriteUInt32(Stream,Value.ValueType));
Inc(Result,Stream_WriteUInt32(Stream,Value.Flags));
Inc(Result,Stream_WriteTelemetryString(Stream,Value.IndexConfig.ID));
Inc(Result,Stream_WriteTelemetryString(Stream,Value.IndexConfig.Attribute));
end;

//------------------------------------------------------------------------------

Function Stream_Read_ChannelInfo(Stream: TStream; out Value: TChannelInfo): TMemSize;
begin
Result := Stream_ReadTelemetryString(Stream,Value.Name);
Inc(Result,Stream_ReadUInt32(Stream,Value.ID));
Inc(Result,Stream_ReadUInt32(Stream,Value.Index));
Inc(Result,Stream_ReadUInt32(Stream,Value.ValueType));
Inc(Result,Stream_ReadUInt32(Stream,Value.Flags));
Inc(Result,Stream_ReadTelemetryString(Stream,Value.IndexConfig.ID));
Inc(Result,Stream_ReadTelemetryString(Stream,Value.IndexConfig.Attribute));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_ChannelInfo(Stream: TStream; out BytesRead: TMemSize): TChannelInfo;
begin
BytesRead := Stream_Read_ChannelInfo(Stream,Result);
end;

//==============================================================================

Function Size_StoredConfig(Value: TStoredConfig): TMemSize;
begin
Result := TelemetryStringBytes(Value.Reference.ID) + TelemetryStringBytes(Value.Reference.Attribute) +
          SizeOf(scs_u32_t) + Size_scs_value_localized(Value.Value,True) + SizeOf(Boolean);
end;

//------------------------------------------------------------------------------

Function Ptr_Write_StoredConfig(var Destination: Pointer; Value: TStoredConfig; Size: TMemSize; Advance: Boolean = True): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Size >= Size_StoredConfig(Value) then
  begin
    WorkPtr := Destination;
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.Reference.ID,True);
    Inc(Result,Ptr_WriteTelemetryString(WorkPtr,Value.Reference.Attribute,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Index,True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,True));
    Inc(Result,Ptr_WriteBoolean(WorkPtr,Value.Binded,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_StoredConfig: Output buffer too small (got %d, expected %d).',[Size,Size_StoredConfig(Value)]);
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
Result := Ptr_ReadTelemetryString(WorkPtr,Value.Reference.ID,True);
Inc(Result,Ptr_ReadTelemetryString(WorkPtr,Value.Reference.Attribute,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Index,True));
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
Result := Stream_WriteTelemetryString(Stream,Value.Reference.ID);
Inc(Result,Stream_WriteTelemetryString(Stream,Value.Reference.Attribute));
Inc(Result,Stream_WriteUInt32(Stream,Value.Index));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,True));
Inc(Result,Stream_WriteBoolean(Stream,Value.Binded));
end;

//------------------------------------------------------------------------------

Function Stream_Read_StoredConfig(Stream: TStream; out Value: TStoredConfig): TMemSize;
begin
Result := Stream_ReadTelemetryString(Stream,Value.Reference.ID);
Inc(Result,Stream_ReadTelemetryString(Stream,Value.Reference.Attribute));
Inc(Result,Stream_ReadUInt32(Stream,Value.Index));
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
Result := TelemetryStringBytes(Value.Name) + SizeOf(TChannelID) + SizeOf(scs_u32_t) +
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
    Result := Ptr_WriteTelemetryString(WorkPtr,Value.Name,True);
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.ID,True));
    Inc(Result,Ptr_WriteUInt32(WorkPtr,Value.Index,True));
    Inc(Result,Ptr_Write_scs_value_localized(WorkPtr,Value.Value,Size - Result,True,True));
    If Advance then Destination := WorkPtr;
  end
else raise ETLBufferTooSmall.CreateFmt('Ptr_Write_StoredChannel: Output buffer too small (got %d, expected %d).',[Size,Size_StoredChannel(Value)]);
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
Result := Ptr_ReadTelemetryString(WorkPtr,Value.Name,True);
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.ID,True));
Inc(Result,Ptr_ReadUInt32(WorkPtr,Value.Index,True));
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
Result := Stream_WriteTelemetryString(Stream,Value.Name);
Inc(Result,Stream_WriteUInt32(Stream,Value.ID));
Inc(Result,Stream_WriteUInt32(Stream,Value.Index));
Inc(Result,Stream_Write_scs_value_localized(Stream,Value.Value,True));
end;

//------------------------------------------------------------------------------

Function Stream_Read_StoredChannel(Stream: TStream; out Value: TStoredChannel): TMemSize;
begin
Result := Stream_ReadTelemetryString(Stream,Value.Name);
Inc(Result,Stream_ReadUInt32(Stream,Value.ID));
Inc(Result,Stream_ReadUInt32(Stream,Value.Index));
Inc(Result,Stream_Read_scs_value_localized(Stream,Value.Value,True));
end;

//------------------------------------------------------------------------------

Function Stream_Readout_StoredChannel(Stream: TStream; out BytesRead: TMemSize): TStoredChannel;
begin
BytesRead := Stream_Read_StoredChannel(Stream,Result);
end;

end.
