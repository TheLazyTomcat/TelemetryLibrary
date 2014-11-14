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
  TValueTypesArray = Array of scs_value_type_t;

  Function ValueTypeBitmask(ValueType: scs_value_type_t): TValueTypeBitmask;
  Function ValueTypesBitmask(ValueTypes: Array of scs_value_type_t): TValueTypeBitmask;

  Function BitmaskValueType(Bitmask: TValueTypeBitmask): scs_value_type_t;
  Function BitmaskValueTypes(Bitmask: TValueTypeBitmask): TValueTypesArray;

//  Function ValueTypeBitmaskAdd(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;
//  Function ValueTypeBitmaskRemove(var Bitmask: TValueTypeBitmask; ValueType: scs_value_type_t): Boolean;

//  Function SecondaryValueTypes(PrimaryValueType: scs_value_type_t): TValueTypesArray;
  Function SecondaryValueTypesBitmask(PrimaryValueType: scs_value_type_t): TValueTypeBitmask;
//  Function SecondaryValueTypesCount(PrimaryValueType: scs_value_type_t): Integer;

implementation

uses
  BitOps;

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
If Result > SCS_VALUE_TYPE_LAST then Result := 0; 
end;

//------------------------------------------------------------------------------

Function BitmaskValueTypes(Bitmask: TValueTypeBitmask): TValueTypesArray;
var
  i:  Integer;
begin
If Bitmask <> 0 then
  begin
    // todo
  end
else
  begin
    SetLength(Result,1);
    Result[High(Result)] := SCS_VALUE_TYPE_INVALID;
  end;
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

end.
