// todo: documentation
unit TelemetryValueTypeUtils;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFDEF Documentation}
  BitOps,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk_value;
{$ENDIF}

(*
    primary type                  secondary types

  SCS_VALUE_TYPE_bool       ->  /
  SCS_VALUE_TYPE_s32        ->  /
  SCS_VALUE_TYPE_u32        ->  u64
  SCS_VALUE_TYPE_u64        ->  /
  SCS_VALUE_TYPE_float      ->  double
  SCS_VALUE_TYPE_double     ->  float
  SCS_VALUE_TYPE_fvector    ->  dvector
  SCS_VALUE_TYPE_dvector    ->  fvector
  SCS_VALUE_TYPE_euler      ->  /
  SCS_VALUE_TYPE_fplacement ->  dplacement, fvector, dvector, euler
  SCS_VALUE_TYPE_dplacement ->  fplacement, dvector, fvector, euler
  SCS_VALUE_TYPE_string     ->  /
*)

type
  TValueTypeBitmask = LongWord;
  TValueTypesArray  = Array[0..SizeOf(TValueTypeBitmask) * 8] of scs_value_type_t; {33 items}

const
  cNoValueType = TValueTypeBitmask(0);

  procedure CompressValueTypesArray(var ValueTypes: TValueTypesArray);  

  Function ValueTypeBitmask(ValueType: scs_value_type_t): TValueTypeBitmask;
  Function ValueTypesBitmask(ValueTypes: Array of scs_value_type_t): TValueTypeBitmask;

  Function BitmaskValueType(Bitmask: TValueTypeBitmask): scs_value_type_t;
  Function BitmaskValueTypes(Bitmask: TValueTypeBitmask): TValueTypesArray;

  Function ValueTypeBitmaskAdd(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
  Function ValueTypeBitmaskRemove(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;

  Function SecondaryValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
  Function SecondaryValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;
  Function SecondaryValueTypesCount(PrimaryValueType: scs_value_type_t): Integer;
  Function SupportedValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
  Function SupportedValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;

  Function ValidateSecondaryValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
  Function ValidateSupportedValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
  procedure MakeValidSecondaryValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);
  procedure MakeValidSupportedValueTypesBitmask(var Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t);

const
  TVT_REG_SEC_1   = 1;
  TVT_REG_SEC_2   = 2;
  TVT_REG_SEC_3   = 4;
  TVT_REG_SEC_4   = 8;
  TVT_REG_SEC_5   = 16;
  TVT_REG_SEC_6   = 32;
  TVT_REG_SEC_7   = 64;
  TVT_REG_SEC_8   = 128;
  TVT_REG_SEC_9   = 256;
  TVT_REG_SEC_10  = 512;
  TVT_REG_SEC_11  = 1024;
  TVT_REG_SEC_12  = 2048;
  TVT_REG_SEC_ALL = $FFFFFFFF;

  Function SelectSecondaryValueTypes(PrimaryValueType: scs_value_type_t; SecondarySelectionMask: LongWord): TValueTypesArray;
  Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SelectPrimary: Boolean; SecondarySelectionMask: LongWord): TValueTypesArray;

implementation

uses
  BitOps;

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

//==============================================================================

Function ValueTypeBitmask(ValueType: scs_value_type_t): TValueTypeBitmask;
begin
If (ValueType > 0) and (ValueType <= SCS_VALUE_TYPE_LAST) then
  Result := ROL($80000000,Integer(ValueType))
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

//------------------------------------------------------------------------------

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
FillChar(Result,SizeOf(Result),0);
If Bitmask <> 0 then
  begin
    j := Low(Result);
    For i := 0 to Pred(SCS_VALUE_TYPE_LAST) do
      If BT(Bitmask,i) then
        begin
          Result[j] := scs_value_type_t(i + 1);
          Inc(j);
        end;
  end;
end;

//------------------------------------------------------------------------------

Function ValueTypeBitmaskAdd(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
begin
Result := BTS(Bitmask,Pred(ValueType));
end;

//------------------------------------------------------------------------------

Function ValueTypeBitmaskRemove(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
begin
Result := BTR(Bitmask,Pred(ValueType));
end;

//------------------------------------------------------------------------------

Function SecondaryValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
begin
case PrimaryValueType of
  SCS_VALUE_TYPE_u32:         Result := ValueTypeBitmask(SCS_VALUE_TYPE_u64);
  SCS_VALUE_TYPE_float:       Result := ValueTypeBitmask(SCS_VALUE_TYPE_double);
  SCS_VALUE_TYPE_double:      Result := ValueTypeBitmask(SCS_VALUE_TYPE_float);
  SCS_VALUE_TYPE_fvector:     Result := ValueTypeBitmask(SCS_VALUE_TYPE_dvector);
  SCS_VALUE_TYPE_dvector:     Result := ValueTypeBitmask(SCS_VALUE_TYPE_fvector);
  SCS_VALUE_TYPE_fplacement:  Result := ValueTypesBitmask([SCS_VALUE_TYPE_dplacement,SCS_VALUE_TYPE_euler,
                                                           SCS_VALUE_TYPE_dvector,SCS_VALUE_TYPE_fvector]);
  SCS_VALUE_TYPE_dplacement:  Result := ValueTypesBitmask([SCS_VALUE_TYPE_fplacement,SCS_VALUE_TYPE_euler,
                                                           SCS_VALUE_TYPE_dvector,SCS_VALUE_TYPE_fvector]);
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
var
  i:        Integer;
  Template: TValueTypeBitmask;
begin
Result := 0;
Template := SecondaryValueTypesBitmask(PrimaryValueType);
For i := 0 to Pred(SizeOf(TValueTypeBitmask) * 8) do
  If BT(Template,i) then Inc(Result);
end;

//------------------------------------------------------------------------------

Function SupportedValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
begin
Result := ValueTypeBitmask(PrimaryValueType) or SecondaryValueTypesBitmask(PrimaryValueType);
end;

//------------------------------------------------------------------------------

Function SupportedValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;
var
  i:  Integer;
begin
Result := SecondaryValueTypes(PrimaryValueType);
For i := Pred(High(Result)) downto Low(Result) do Result[i + 1] := Result[i];
Result[Low(Result)] := PrimaryValueType;
end;

//------------------------------------------------------------------------------

Function ValidateSecondaryValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
var
  i:        Integer;
  Template: TValueTypeBitmask;
begin
Result := False;
Template := SecondaryValueTypesBitmask(PrimaryValueType);
For i := 0 to Pred(SizeOf(TValueTypeBitmask) * 8) do
  If BT(Bitmask,i) and not BT(Template,i) then Exit;
Result := True;
end;

//------------------------------------------------------------------------------

Function ValidateSupportedValueTypesBitmask(Bitmask: TValueTypeBitmask; PrimaryValueType: scs_value_type_t): Boolean;
var
  i:        Integer;
  Template: TValueTypeBitmask;
begin
Result := False;
Template := SupportedValueTypesBitmask(PrimaryValueType);
For i := 0 to Pred(SizeOf(TValueTypeBitmask) * 8) do
  If BT(Bitmask,i) and not BT(Template,i) then Exit;
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

Function SelectSecondaryValueTypes(PrimaryValueType: scs_value_type_t; SecondarySelectionMask: LongWord): TValueTypesArray;
var
  i:  Integer;
begin
Result := SecondaryValueTypes(PrimaryValueType);
For i := 0 to Pred(SizeOf(SecondarySelectionMask) * 8) do
  If not BT(SecondarySelectionMask,i) then Result[i] := SCS_VALUE_TYPE_INVALID;
CompressValueTypesArray(Result);
end;

//------------------------------------------------------------------------------

Function SelectSupportedValueTypes(PrimaryValueType: scs_value_type_t; SelectPrimary: Boolean; SecondarySelectionMask: LongWord): TValueTypesArray;
var
  i:  Integer;
begin
Result := SupportedValueTypes(PrimaryValueType);
If not SelectPrimary then Result[0] := SCS_VALUE_TYPE_INVALID;
For i := 0 to Pred(SizeOf(SecondarySelectionMask) * 8) do
  If not BT(SecondarySelectionMask,i) then Result[i + 1] := SCS_VALUE_TYPE_INVALID;
CompressValueTypesArray(Result); 
end;

end.
