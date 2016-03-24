{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Provides types, constants and routines used for manipulation with
          value types.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-12-03)
@lastmod(2016-03-20)

  @bold(@NoAutoLink(TelemetryValueTypeUtils))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-20  

  Each telemetry channel has a primary type, which is a native type for such
  channel. But telemetry is also able to provide value of some channels as
  a secondary type when registered so, eg. as 64bit integer for a channel whose
  native type is a 32bit integer.@br
  Following is table of secondary types for each existing primary type ("/"
  means this primary type does not allow any secondary type to be used):
@preformatted(
    Primary type             -    Secondary types
 -------------------------------------------------------------------
  SCS_VALUE_TYPE_bool        -  /
  SCS_VALUE_TYPE_s32         -  /
  SCS_VALUE_TYPE_u32         -  u64
  SCS_VALUE_TYPE_u64         -  /
  SCS_VALUE_TYPE_float       -  double
  SCS_VALUE_TYPE_double      -  float
  SCS_VALUE_TYPE_fvector     -  dvector
  SCS_VALUE_TYPE_dvector     -  fvector
  SCS_VALUE_TYPE_euler       -  /
  SCS_VALUE_TYPE_fplacement  -  dplacement, fvector, dvector, euler
  SCS_VALUE_TYPE_dplacement  -  fplacement, dvector, fvector, euler
  SCS_VALUE_TYPE_string      -  /
)
  This unit is designed to provide any material (functions, types, ...) needed
  for identification and registration of secondary types for any telemetry
  channel.

@html(<hr>)}
unit TelemetryValueTypeUtils;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  AuxTypes
{$ELSE}
  TelemetryStrings
{$ENDIF}
{$IFNDEF Documentation},
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk_value;
{$ENDIF}
{$ELSE};
{$ENDIF}

const
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #1.
  TVT_REG_SEC_1   = 1;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #2.
  TVT_REG_SEC_2   = 2;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #3.
  TVT_REG_SEC_3   = 4;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #4.
  TVT_REG_SEC_4   = 8;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #5.
  TVT_REG_SEC_5   = 16;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #6.
  TVT_REG_SEC_6   = 32;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #7.
  TVT_REG_SEC_7   = 64;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #8.
  TVT_REG_SEC_8   = 128;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #9.
  TVT_REG_SEC_9   = 256;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #10.
  TVT_REG_SEC_10  = 512;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #11.
  TVT_REG_SEC_11  = 1024;
  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select secondary type #12.
  TVT_REG_SEC_12  = 2048;

  //:Use for parameter @code(@link(SelectSupportedValueTypes
  //:SecondarySelectionMask)) to select all possible secondary types.
  TVT_REG_SEC_ALL = $FFFFFFFF;

type
{:
  @abstract(Bitmask type used to identify any number of value types.)
  
  Each bit in this bitmask corresponds to exactly one known value type. When a
  particular bit is set, it means such type is used or selected.
  Currently, individual bits are mapped as follows:
  @preformatted(
    bit 0   - SCS_VALUE_TYPE_bool
    bit 1   - SCS_VALUE_TYPE_s32
    bit 2   - SCS_VALUE_TYPE_u32
    bit 3   - SCS_VALUE_TYPE_u64
    bit 4   - SCS_VALUE_TYPE_float
    bit 5   - SCS_VALUE_TYPE_double
    bit 6   - SCS_VALUE_TYPE_fvector
    bit 7   - SCS_VALUE_TYPE_dvector
    bit 8   - SCS_VALUE_TYPE_euler
    bit 9   - SCS_VALUE_TYPE_fplacement
    bit 10  - SCS_VALUE_TYPE_dplacement
    bit 11  - SCS_VALUE_TYPE_string
    bit 12+ - unused
  )
  Note that @code(SCS_VALUE_TYPE_INVALID) type is not mapped, hence you cannot
  use or select it.
}
  TValueTypeBitmask = UInt32;

{:
  @abstract(An array type used in the same manner as TValueTypeBitmask, but
            individual types does not have its hardcoded placement.)

  All valid items (types) must be placed at the start of the array, and first
  item that contains an @code(SCS_VALUE_TYPE_INVALID) type marks the end of the
  array. If the first item in this array is invalid, then whole array should be
  considered empty.
}
  TValueTypesArray  = Array[0..SizeOf(TValueTypeBitmask) * 8] of scs_value_type_t; {33 items}

const
  //:Empty value type bitmask (ie. no value type is marked as used/selected).
  NoValueType = TValueTypeBitmask(0);

{==============================================================================}
{   Unit functions and procedures declarations                                 }
{==============================================================================}

{:
  @abstract(Compresses value types array, that is, it moves all valid value
            types to the start of the array so there are no invalid types
            between valid ones.)

  For example, following array:
  @preformatted(
  s32, s64, invalid, float, invalid, invalid, double)
  will look like this after the compression:
  @preformatted(
  s32, s64, float, double)

  @param ValueTypes Array that has to be comressed.
}
procedure CompressValueTypesArray(var ValueTypes: TValueTypesArray);

//------------------------------------------------------------------------------

{:
  @abstract(Produces string listing of all valid types stored in the array.)

  It calls SCSValueTypeToStr function to get string representation of individual
  value types.

  @param ValueTypes Array of types that will be listed.

  @returns(Listing of valid value types, an enpty string when there is no
           valid type in the array.)
}
Function ValueTypesArrayToStr(const ValueTypes: TValueTypesArray): String;

//==============================================================================

{:
  @abstract(Returns a value type bitmask where bit corresponding to type passed
            in the parameter @code(ValueType) will be set.)

  All other bits will be set to 0.@br
  For selecting multiple values, use function ValueTypesBitmask.

  @param ValueType Value type that should be selected.

  @returns Value type bitmask where bit corresponding to a desired type is set.
}
Function ValueTypeBitmask(ValueType: scs_value_type_t): TValueTypeBitmask;

//------------------------------------------------------------------------------

{:
  @abstract(Returns a value type bitmask where bits corresponding to types
            passed in the parameter @code(ValueTypes) will be set.)

  All other bits will be set to 0.

  @param ValueType Array of value types that should be selected.

  @returns(Value type bitmask where bits corresponding to a desired types are
           set.)
}
Function ValueTypesBitmask(ValueTypes: Array of scs_value_type_t): TValueTypeBitmask;

//==============================================================================

{:
  @abstract(Returns value type corresponding to first set bit (counting from
            bit 0 up) in the @code(Bitmask) parameter.)

  If no bit is set, @code(SCS_VALUE_TYPE_INVALID) will be returned.@br
  For example, for bitmask 0x0000001A (...00011010), @code(SCS_VALUE_TYPE_s32)
  will be returned as it corresponds to bit 1.@br
  To obtain all selected types, use function BitmaskValueTypes.

  @param Bitmask Bitmask containing selected type(s).

  @returns First selected value type from the bitmask.
}
Function BitmaskValueType(Bitmask: TValueTypeBitmask): scs_value_type_t;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array of all value types selected in the passed bitmask.)

  For Example, for bitmask 0x00000009 (...00001001), following array will be
  returned:
  @preformatted(
  bool, u64)

  @param Bitmask Bitmask containing selected types.

  @returns Array of all selected value types.
}
Function BitmaskValueTypes(Bitmask: TValueTypeBitmask): TValueTypesArray;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array of all value types selected in the passed bitmask
            including passed primary value type.)

  Primary value type will be always in the first item, which also means the
  resulting array will newer be empty if you pass valid primary type. If you
  pass an invalid type as primary, the array will be empty as the first item
  will be set to invalid.@br
  For Example, for bitmask 0x00000009 (...00001101) and primary type of
  @code(SCS_VALUE_TYPE_double), following array will be returned:
  @preformatted(
  double, bool, u32, u64)

  @param Bitmask          Bitmask containing selected types.
  @param PrimaryValueType Primary value type added to the result.

  @returns(Array of all selected value types with primary value type as the
           first item.)
}
Function BitmaskValueTypesAddPrimary(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): TValueTypesArray;

//==============================================================================

{:
  @abstract(Marks value type passed in parameter @code(ValueType) as selected
            (sets the appropriate bit) in a bitmask passed in parameter
            @code(Bitmask).)

  If the requsted type is already selected, then nothing will change.

  @param Bitmask   Value type bitmask in which the value type will be selected.
  @param ValueType Value type to be selected.

  @returns Old selection state of the requested value type.
}
Function ValueTypeBitmaskAdd(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;

//------------------------------------------------------------------------------

{:
  @abstract(Marks value type passed in parameter @code(ValueType) as not
            selected (resets the appropriate bit) in a bitmask passed in
            parameter @code(Bitmask).)

  If the requsted type is not already selected, then nothing will change.

  @param(Bitmask   Value type bitmask in which the value type will be
                   unselected.)
  @param ValueType Value type to be unselected.

  @returns Old selection state of the requested value type.
}
Function ValueTypeBitmaskRemove(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;

//==============================================================================

{:
  @abstract(Returns a bitmask where all possible secondary value types for a
            given primary type are selected (their bits are set).)

  For details about which secondary types are bound to any primary type, refer
  to unit documentation.

  @param(PrimaryValueType Type for which bitmask of all secondary types will be
                          returned.)

  @returns Bitmask of all secondary value types for a given primary type.
}
Function SecondaryValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array of all possible secondary value types for a given
            primary type.)

  For details about which secondary types are bound to any primary type, refer
  to unit documentation.

  @param(PrimaryValueType Type for which array of all secondary types will be
                          returned.)

  @returns Array of all secondary value types for a given primary type.
}
Function SecondaryValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;

//------------------------------------------------------------------------------

{:
  @abstract(Returns number of secondary value types for a given primary type.)

  Will return 0 if selected primary type does not have any secondary type.

  @param(PrimaryValueType Primary value type for which to return number of
                          secondary types.)

  @returns Number of secondary value types for selected primary value type.
}
Function SecondaryValueTypesCount(PrimaryValueType: scs_value_type_t): Integer;

//------------------------------------------------------------------------------

{:
  Returns a bitmask where all possible secondary value types for a given primary
  type and the primary type itself are selected (their bits are set).

  @param PrimaryValueType Type for which secondary types will be selected.

  @returns Bitmask of all secondary value types plus the primary type itself.
}
Function SupportedValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array that contains all possible secondary value types
            for a given primary type and the primary type itself.)

  Primary type is always stored in the first item.

  @param PrimaryValueType Type for which secondary types will be selected.

  @returns Array of all secondary value types plus the primary type itself.
}
Function SupportedValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;

//==============================================================================

{:
  Checks whether selected types in a given bitmask are all secondaries for a
  passed primary value type.
  
  @param Bitmask          Bitmask that will be checked.
  @param(PrimaryValueType Type for which the selected secondary types are
                          checked.)

  @returns(@True when all selected types are secondary types for a given primary
           type, @false otherwise.)
}
Function ValidateSecondaryValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;

//------------------------------------------------------------------------------

{:
  Checks whether selected types in a given bitmask are all secondaries for a
  passed primary value type, or the primary itself.

  @param Bitmask          Bitmask that will be checked.
  @param(PrimaryValueType Type for which the selected types are checked.)

  @returns(@True when all selected types are secondary types or the primary
           itself, @false otherwise.)
}
Function ValidateSupportedValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;

//------------------------------------------------------------------------------

{:
  Unselects all selected types that does not belong between secondaries of the
  selected primary type.

  @param Bitmask          Bitmask where the types are checked an unselected.
  @param(PrimaryValueType Type for which the selected types are checked.)
}
procedure MakeValidSecondaryValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);

//------------------------------------------------------------------------------

{:
  Unselects all selected types that either does not belong between secondaries
  of the selected primary type, or are not the primary itself.

  @param Bitmask          Bitmask where the types are checked an unselected.
  @param(PrimaryValueType Type for which the selected types are checked.)
}
procedure MakeValidSupportedValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);

//==============================================================================

{:
  @abstract(Returns an array of value types supported by a selected primary type
            according to a selection mask.)
  
  Array is constructed in the following way:@unorderedList(
    @item(The array is emptied (all items are set to
          @code(SCS_VALUE_TYPE_INVALID)))
    @item(If @code(SelectPrimary) is @true, then first item is set to a passed
          primary value type)
    @item(Other items are filled from parameter
          @noAutoLink(@code(SecondaryValueTypesBitmask)))
    @item(All items in the array (excluding the primary if it is selected) are
          checked against corresponding bit in the selection mask, when the bit
          is not set, they are removed from the array)
    @item(The array is compressed))

  Let's explain more it on some examples:@unorderedList(
    @item(@bold(Example 1)

      @code(PrimaryValueType) = @code(SCS_VALUE_TYPE_fplacement)@br
      @noAutoLink(@code(SecondaryValueTypesBitmask)) = 0x000005C0
                                                       (...0000010111000000)@br
      @code(SelectPrimary) = @true@br
      @code(SecondarySelectionMask) = 0x00000005 (...00000101)

      Intermediate array will look like so:
      @preformatted(
  fplacement, fvector, dvector, euler, dplacement)

      Then a selection mask is applied (remember that primary type is selected
      and it cannot be removed, so the selection mask is applied from second
      item) with this result:
      @preformatted(
  fplacement, fvector, invalid, euler, invalid)

      And finally, after compression, the result is:
      @preformatted(
  fplacement, fvector, euler))

  @item(@bold(Example 2)
  
      @code(PrimaryValueType) = @code(SCS_VALUE_TYPE_dplacement)@br
      @noAutoLink(@code(SecondaryValueTypesBitmask)) = 0x000002C0
                                                       (...0000001011000000)@br
      @code(SelectPrimary) = @false@br
      @code(SecondarySelectionMask) = 0x00000001 (...0000001)

      Intermediate array:
      @preformatted(
  fvector, dvector, dplacement)

      After selection mask is applied, final result is:
      @preformatted(
  fvector)))

  @param(PrimaryValueType           Primary value type. Can be set to
                                    @code(SCS_VALUE_TYPE_INVALID) if
                                    @code(SelectPrimary) is set to @false.)
  @param(SecondaryValueTypesBitmask Bitmask selecting secondary types to be
                                    included in the result.)
  @param(SelectPrimary              When se to @true, the type passed as primary
                                    will be included in the result.)
  @param(SecondarySelectionMask     Bitmask selecting secondary value types.
                                    Note that primary type is not affected by
                                    this mask.)

  @returns Array of value types that is build according to passed parameters.
}
Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SecondaryValueTypesBitmask: TValueTypeBitmask; SelectPrimary: Boolean; SecondarySelectionMask: UInt32): TValueTypesArray; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array of value types supported by a selected primary type
            according to a selection mask.)

  Calls previous variant of this function, with the parameter
  @noAutoLink(@code(SecondaryValueTypesBitmask)) filled by a function
  SecondaryValueTypesBitmask called with its parameter @code(PrimaryValueType)
  set to a value from local parameter of the same name.@br
  For more details, refer to mentioned function.

  @param(PrimaryValueType           Primary value type. Can be set to
                                    @code(SCS_VALUE_TYPE_INVALID) if
                                    @code(SelectPrimary) is set to @false.)
  @param(SelectPrimary              When se to @true, the type passed as primary
                                    will be included in the result.)
  @param(SecondarySelectionMask     Bitmask selecting secondary value types.
                                    Note that primary type is not affected by
                                    this mask.)

  @returns Array of value types that is build according to passed parameters.
}
Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SelectPrimary: Boolean; SecondarySelectionMask: UInt32): TValueTypesArray; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Returns an array of secondary value types of a selected primary type
            according to a selection mask.)

  Calls first variant of this function, with the parameter
  @noAutoLink(@code(SecondaryValueTypesBitmask)) filled by a function
  SecondaryValueTypesBitmask called with its parameter @code(PrimaryValueType)
  set to a value from local parameter of the same name, and parameter
  @code(SelectPrimary) set to @false.@br
  For more details, refer to mentioned function.

  @param PrimaryValueType           Primary value type for required secondaries.
  @param SecondarySelectionMask     Bitmask selecting secondary value types.

  @returns Array of value types that is build according to passed parameters.
}
Function SelectSecondaryValueTypes(PrimaryValueType: scs_value_type_t; SecondarySelectionMask: UInt32): TValueTypesArray;

implementation

uses
  BitOps,
  TelemetryStrings;

{==============================================================================}
{   Unit functions and procedures implementation                               }
{==============================================================================}

procedure CompressValueTypesArray(var ValueTypes: TValueTypesArray);
var
  LastValid:  Integer;
  i:          Integer;
begin
LastValid := Low(ValueTypes);
For i := Low(ValueTypes) to High(ValueTypes) do
  If ValueTypes[i] <> SCS_VALUE_TYPE_INVALID then
    begin
      If i <> LastValid then
        begin
          ValueTypes[LastValid] := ValueTypes[i];
          ValueTypes[i] := SCS_VALUE_TYPE_INVALID;
        end;
      Inc(LastValid)
    end;
end;

//------------------------------------------------------------------------------

Function ValueTypesArrayToStr(const ValueTypes: TValueTypesArray): String;
var
  i:  Integer;
begin
Result := '';
i := Low(TValueTypesArray);
while (ValueTypes[i] <> SCS_VALUE_TYPE_INVALID) and (i <= High(TValueTypesArray)) do
  begin
    If Result <> '' then
      Result := Result + ', ' + SCSValueTypeToStr(ValueTypes[i])
    else
      Result := SCSValueTypeToStr(ValueTypes[i]);
    Inc(i);
  end;
end;

//==============================================================================

Function ValueTypeBitmask(ValueType: scs_value_type_t): TValueTypeBitmask;
begin
If (ValueType > 0) and (ValueType <= SCS_VALUE_TYPE_LAST) then
  Result := ROL(UInt32($80000000),Byte(ValueType))
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function ValueTypesBitmask(ValueTypes: Array of scs_value_type_t): TValueTypeBitmask;
var
  i:  Integer;
begin
Result := 0;
For i := Low(ValueTypes) to High(ValueTypes) do
  Result := Result or ValueTypeBitmask(ValueTypes[i]);
end;

//==============================================================================

Function BitmaskValueType(Bitmask: TValueTypeBitmask): scs_value_type_t;
begin
Result := BSF(Bitmask) + 1;
If (Result > SCS_VALUE_TYPE_LAST) then Result := 0; 
end;

//------------------------------------------------------------------------------

Function BitmaskValueTypes(Bitmask: TValueTypeBitmask): TValueTypesArray;
var
  i,j:  Integer;
begin
FillChar(Addr(Result)^,SizeOf(Result),0);
If Bitmask <> 0 then
  begin
    j := Low(Result);
    For i := 0 to Pred(SCS_VALUE_TYPE_LAST) do
      If BT(Bitmask,Byte(i)) then
        begin
          Result[j] := scs_value_type_t(i + 1);
          Inc(j);
        end;
  end;
end;

//------------------------------------------------------------------------------

Function BitmaskValueTypesAddPrimary(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): TValueTypesArray;
var
  i:  Integer;
begin
Result := BitmaskValueTypes(Bitmask);
For i := Pred(High(Result)) downto Low(Result) do
  Result[i + 1] := Result[i];
Result[Low(Result)] := PrimaryValueType;
end;

//==============================================================================

Function ValueTypeBitmaskAdd(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
begin
If ValueType <> SCS_VALUE_TYPE_INVALID then
  Result := BTS(Bitmask,Byte(Pred(ValueType)))
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function ValueTypeBitmaskRemove(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
begin
If ValueType <> SCS_VALUE_TYPE_INVALID then
  Result := BTR(Bitmask,Byte(Pred(ValueType)))
else
  Result := False;
end;

//==============================================================================

Function SecondaryValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
begin
case PrimaryValueType of
  SCS_VALUE_TYPE_u32:         Result := ValueTypeBitmask(SCS_VALUE_TYPE_u64);
  SCS_VALUE_TYPE_float:       Result := ValueTypeBitmask(SCS_VALUE_TYPE_double);
  SCS_VALUE_TYPE_double:      Result := ValueTypeBitmask(SCS_VALUE_TYPE_float);
  SCS_VALUE_TYPE_fvector:     Result := ValueTypeBitmask(SCS_VALUE_TYPE_dvector);
  SCS_VALUE_TYPE_dvector:     Result := ValueTypeBitmask(SCS_VALUE_TYPE_fvector);
  SCS_VALUE_TYPE_fplacement:  Result := ValueTypesBitmask([SCS_VALUE_TYPE_dplacement,
                                SCS_VALUE_TYPE_euler,SCS_VALUE_TYPE_dvector,SCS_VALUE_TYPE_fvector]);
  SCS_VALUE_TYPE_dplacement:  Result := ValueTypesBitmask([SCS_VALUE_TYPE_fplacement,
                                SCS_VALUE_TYPE_euler,SCS_VALUE_TYPE_dvector,SCS_VALUE_TYPE_fvector]);
else
  Result := $00000000;
end;
end;

//------------------------------------------------------------------------------

Function SecondaryValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;
begin
Result := BitmaskValueTypes(SecondaryValueTypesBitmask(PrimaryValueType));
end;

//------------------------------------------------------------------------------

Function SecondaryValueTypesCount(PrimaryValueType: scs_value_type_t): Integer;
begin
Result := PopCount(SecondaryValueTypesBitmask(PrimaryValueType));
end;

//------------------------------------------------------------------------------

Function SupportedValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
begin
Result := ValueTypeBitmask(PrimaryValueType) or SecondaryValueTypesBitmask(PrimaryValueType);
end;

//------------------------------------------------------------------------------

Function SupportedValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;
begin
Result := BitmaskValueTypesAddPrimary(SecondaryValueTypesBitmask(PrimaryValueType),PrimaryValueType);
end;

//==============================================================================

Function ValidateSecondaryValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
var
  i:    Integer;
  Temp: TValueTypeBitmask;
begin
Result := False;
Temp := SecondaryValueTypesBitmask(PrimaryValueType);
For i := 0 to Pred(SizeOf(TValueTypeBitmask) * 8) do
  If BT(Bitmask,Byte(i)) and not BT(Temp,Byte(i)) then Exit;
Result := True;
end;

//------------------------------------------------------------------------------

Function ValidateSupportedValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
var
  i:    Integer;
  Temp: TValueTypeBitmask;
begin
Result := False;
Temp := SupportedValueTypesBitmask(PrimaryValueType);
For i := 0 to Pred(SizeOf(TValueTypeBitmask) * 8) do
  If BT(Bitmask,Byte(i)) and not BT(Temp,Byte(i)) then Exit;
Result := True;
end;

//------------------------------------------------------------------------------

procedure MakeValidSecondaryValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);
begin
Bitmask := Bitmask and SecondaryValueTypesBitmask(PrimaryValueType);
end;

//------------------------------------------------------------------------------

procedure MakeValidSupportedValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);
begin
Bitmask := Bitmask and SupportedValueTypesBitmask(PrimaryValueType);
end;

//==============================================================================

Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SecondaryValueTypesBitmask: TValueTypeBitmask; SelectPrimary: Boolean; SecondarySelectionMask: UInt32): TValueTypesArray;
var
  i:  Integer;
begin
If SelectPrimary then
  Result := BitmaskValueTypesAddPrimary(SecondaryValueTypesBitmask,PrimaryValueType)
else
  Result := BitmaskValueTypes(SecondaryValueTypesBitmask);
For i := 0 to Pred(SizeOf(SecondarySelectionMask) * 8) do
  If not BT(SecondarySelectionMask,Byte(i)) then
    begin
      If SelectPrimary then Result[i + 1] := SCS_VALUE_TYPE_INVALID
        else Result[i] := SCS_VALUE_TYPE_INVALID;
    end;
CompressValueTypesArray(Result);
end;

//------------------------------------------------------------------------------

Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SelectPrimary: Boolean; SecondarySelectionMask: UInt32): TValueTypesArray;
begin
Result := SelectSupportedValueTypes(PrimaryValueType,SecondaryValueTypesBitmask(PrimaryValueType),SelectPrimary,SecondarySelectionMask);
end;

//------------------------------------------------------------------------------

Function SelectSecondaryValueTypes(PrimaryValueType: scs_value_type_t; SecondarySelectionMask: UInt32): TValueTypesArray;
begin
Result := SelectSupportedValueTypes(PrimaryValueType,SecondaryValueTypesBitmask(PrimaryValueType),False,SecondarySelectionMask);
end;

end.
