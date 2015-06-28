program Telemetry_TestPrg;

{$mode objfpc}{$H+}
{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes,

{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed,
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_telemetry_event,
  scssdk_telemetry_channel,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2,
{$ENDIF}

  CRC32,
  MD5,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}
  SimpleLog,
  BitOps,

  TelemetryCommon,                                           //* -
  TelemetryIDs,                                              //* -
  TelemetryConversions,                                      //* -
  TelemetryStrings,                                          //* -
  TelemetryValueTypeUtils,                                   //* -
  TelemetryLists,                                            //* -
  TelemetryStreaming,                                        //* -
  TelemetryVersionObjects,                                   //* -
  TelemetryInfoProvider,                                     //*
//  TelemetryRecipient,                                        //*
//  TelemetryRecipientBinder,

//  TelemetrySCS_Examples_telemetry,                           //*
//  TelemetrySCS_Examples_telemetry_position,                  //*
//  TelemetrySCS_Examples_telemetry_mem,                       //*

//  TelemetryLogText,                                          //*
//  TelemetryLogBinary,                                        //*
//  TelemetryLogBinaryParser;                                  //*
  strutils; //remove

procedure RandomizeArray(out Value: TValueTypesArray);
var
  ii: Integer;
begin
  Randomize;
  For ii := Low(Value) to High(Value) do
    If Random(2) = 0 then
      Value[ii] := Random(SCS_VALUE_TYPE_LAST + 1)
    else
      Value[ii] := SCS_VALUE_TYPE_INVALID;
end;

procedure WriteArray(Value: TValueTypesArray; Names: Boolean);
var
  TempStr:  String;
  ii:       Integer;
begin
  TempStr := '';
  For ii := Low(Value) to High(Value) do
    If Names then
      TempStr := TempStr + SCSValueTypeToStr(Value[ii]) + ' '
    else
      TempStr := TempStr + IntToStr(Value[ii]) + ' ';
  WriteLn(TempStr);
end;

var
  BM:   TValueTypeBitmask;
  ARR:  TValueTypesArray;
  i:    Integer;
  VT:   scs_value_type_t;
  BT:   Boolean;
//  TIP:  TTelemetryInfoProvider;
//  TSL:  TStringList;
//  STR:  String;

begin
  Randomize;

  // CompressValueTypesArray
  WriteLn('CompressValueTypesArray');
  RandomizeArray(ARR);
  WriteArray(ARR,False);
  CompressValueTypesArray(ARR);
  WriteArray(ARR,False);

  // ValueTypeBitmask
  WriteLn;
  WriteLn('ValueTypeBitmask');
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_INVALID)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_bool)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_s32)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_u32)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_u64)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_float)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_double)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_fvector)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_dvector)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_euler)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_fplacement)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_dplacement)));
  WriteLn(NumberToBits(ValueTypeBitmask(SCS_VALUE_TYPE_string)));

  // ValueTypesBitmask (open array)
  WriteLn;
  WriteLn('ValueTypesBitmask (open array)');
  WriteLn(NumberToBits(ValueTypesBitmask([SCS_VALUE_TYPE_bool,SCS_VALUE_TYPE_u64,SCS_VALUE_TYPE_dplacement,SCS_VALUE_TYPE_string])));
  WriteLn(NumberToBits(ValueTypesBitmask([SCS_VALUE_TYPE_s32,SCS_VALUE_TYPE_dplacement,SCS_VALUE_TYPE_fplacement,25])));

  // ValueTypesBitmask
  WriteLn;
  WriteLn('ValueTypesBitmask');
  RandomizeArray(ARR);
  WriteArray(ARR, False);
  WriteLn(NumberToBits(ValueTypesBitmask(ARR)));
  RandomizeArray(ARR);
  ARR[15] := 19;
  WriteArray(ARR, False);
  WriteLn(NumberToBits(ValueTypesBitmask(ARR)));

  // BitmaskValueType
  WriteLn;
  WriteLn('BitmaskValueType');
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_INVALID))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_bool))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_s32))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_u32))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_u64))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_float))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_double))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_fvector))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_dvector))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_euler))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_fplacement))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_dplacement))));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(ValueTypeBitmask(SCS_VALUE_TYPE_string))));
  WriteLn;
  BM := ValueTypeBitmask(SCS_VALUE_TYPE_INVALID) or ValueTypeBitmask(SCS_VALUE_TYPE_u32);
  WriteLn(NumberToBits(BM));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(BM)));
  BM := ValueTypeBitmask(SCS_VALUE_TYPE_float) or ValueTypeBitmask(SCS_VALUE_TYPE_dvector);
  WriteLn(NumberToBits(BM));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(BM)));
  BM := (1 shl 24) or ValueTypeBitmask(SCS_VALUE_TYPE_string);
  WriteLn(NumberToBits(BM));
  WriteLn(SCSValueTypeToStr(BitmaskValueType(BM)));

  // BitmaskValueTypes
  WriteLn;
  WriteLn('BitmaskValueTypes');
  For i := 1 to 5 do
    begin
      BM := Random((1 shl 11) + 1);
      WriteLn(NumberToBits(BM));
      WriteArray(BitmaskValueTypes(BM),False);
    end;
  BM := Random(High(TValueTypeBitmask));
  WriteLn(NumberToBits(BM));
  WriteArray(BitmaskValueTypes(BM),False);

  // BitmaskValueTypesAddPrimary
  WriteLn;
  WriteLn('BitmaskValueTypesAddPrimary');
  For i := 1 to 5 do
    begin
      BM := Random((1 shl 11) + 1);
      VT := Random(SCS_VALUE_TYPE_LAST);
      WriteLn(NumberToBits(BM) + ' ' + SCSValueTypeToStr(VT));
      WriteArray(BitmaskValueTypesAddPrimary(BM,VT),False);
    end;
  BM := Random(High(TValueTypeBitmask));
  VT := Random(SCS_VALUE_TYPE_LAST);
  WriteLn(NumberToBits(BM) + ' ' + SCSValueTypeToStr(VT));
  WriteArray(BitmaskValueTypesAddPrimary(BM,VT),False);

  // ValueTypeBitmaskAdd
  WriteLn;
  WriteLn('ValueTypeBitmaskAdd');
  For i := 1 to 5 do
    begin
      BM := Random((1 shl 11) + 1);
      VT := Random(SCS_VALUE_TYPE_LAST + 1);
      Write(NumberToBits(BM) + ' ' + IntToStr(VT) + ' ');
      WriteLn(BoolToStr(ValueTypeBitmaskAdd(BM,VT),True));
      WriteLn(NumberToBits(BM));
    end;

  // ValueTypeBitmaskRemove
  WriteLn;
  WriteLn('ValueTypeBitmaskRemove');
  For i := 1 to 5 do
    begin
      BM := Random((1 shl 11) + 1);
      VT := Random(SCS_VALUE_TYPE_LAST + 1);
      Write(NumberToBits(BM) + ' ' + IntToStr(VT) + ' ');
      WriteLn(BoolToStr(ValueTypeBitmaskRemove(BM,VT),True));
      WriteLn(NumberToBits(BM));
    end;

  // SecondaryValueTypesBitmask
  WriteLn;
  WriteLn('SecondaryValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn(NumberToBits(SecondaryValueTypesBitmask(i)) + ' (' + IntToStr(i) + ')' + SCSValueTypeToStr(i));

  // SecondaryValueTypes
  WriteLn;
  WriteLn('SecondaryValueTypes');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      Write('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ': ');
      WriteArray(SecondaryValueTypes(i),False);
    end;

  // SecondaryValueTypesCount
  WriteLn;
  WriteLn('SecondaryValueTypesCount');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ' (' + NumberToBits(SecondaryValueTypesBitmask(i)) +
            '): ' + IntToStr(SecondaryValueTypesCount(i)));

  // SupportedValueTypesBitmask
  WriteLn;
  WriteLn('SupportedValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn(NumberToBits(SupportedValueTypesBitmask(i)) + ' (' + IntToStr(i) + ')' + SCSValueTypeToStr(i));

  // SupportedValueTypes
  WriteLn;
  WriteLn('SupportedValueTypes');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      Write('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ': ');
      WriteArray(SupportedValueTypes(i),False);
    end;

  // ValidateSecondaryValueTypesBitmask
  WriteLn;
  WriteLn('ValidateSecondaryValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SecondaryValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + '): ' + BoolToStr(ValidateSecondaryValueTypesBitmask(BM,i),True));
    end;

  // ValidateSupportedValueTypesBitmask
  WriteLn;
  WriteLn('ValidateSupportedValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SupportedValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + '): ' + BoolToStr(ValidateSupportedValueTypesBitmask(BM,i),True));
    end;

  // MakeValidSecondaryValueTypesBitmask
  WriteLn;
  WriteLn('MakeValidSecondaryValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SecondaryValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      Write(NumberToBits(BM) + ' ');
      MakeValidSecondaryValueTypesBitmask(BM,i);
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + ')');
    end;

  // MakeValidSupportedValueTypesBitmask
  WriteLn;
  WriteLn('MakeValidSupportedValueTypesBitmask');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SupportedValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      Write(NumberToBits(BM) + ' ');
      MakeValidSupportedValueTypesBitmask(BM,i);
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + ')');
    end;

  // SelectSecondaryValueTypes
  WriteLn;
  WriteLn('SelectSecondaryValueTypes');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      WriteLn('(' + SCSValueTypeToStr(i) + ') ');
      WriteArray(SecondaryValueTypes(i),False);
      BM := Random(High(LongWord));
      WriteLn(NumberToBits(BM) + ' ');
      WriteArray(SelectSecondaryValueTypes(i,BM),False);
    end;

  // SelectSupportedValueTypes
  WriteLn;
  WriteLn('SelectSupportedValueTypes');
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      WriteLn('(' + SCSValueTypeToStr(i) + ') ');
      WriteArray(SupportedValueTypes(i),False);
      BM := Random(High(LongWord));
      BT := Random(2) = 0;
      WriteLn(NumberToBits(BM) + ' (' + BoolToStr(Bt,True) + ')');
      WriteArray(SelectSupportedValueTypes(i,BT,BM),False);
    end;
(*
  TIP := TTelemetryInfoProvider.CreateCurrent('eut2');
  try
    TSL := TStringList.Create;
    try
      For i := 0 to Pred(TIP.KnownChannels.Count) do
        begin
          STR := '(' + ItemIDToStr(TIP.KnownChannels[i].ID) + ')' + TIP.KnownChannels[i].Name;
          If TIP.KnownChannels[i].Indexed then
            STR := STR + '[(' + ItemIDToStr(TIP.KnownChannels[i].IndexConfigID) + ')' +
                   TIP.KnownChannels[i].IndexConfig + '(' + IntToStr(TIP.KnownChannels[i].MaxIndex) + ')]';
          STR := STR + ' ' + SCSValueTypeToStr(TIP.KnownChannels[i].PrimaryType);
          If TIP.KnownChannels[i].SecondaryTypes <> 0 then
            STR := STR + ' (' + ValueTypesArrayToStr(BitmaskValueTypes(TIP.KnownChannels[i].SecondaryTypes)) + ')';
          TSL.Add(STR);
        end;
//    TSL.SaveToFile('channels.txt');
    finally
      STR := '';
      TSL.Free;
    end;
  finally
    TIP.Free;
  end;
*)
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.

