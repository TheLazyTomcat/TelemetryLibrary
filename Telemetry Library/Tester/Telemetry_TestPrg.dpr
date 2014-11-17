program Telemetry_TestPrg;

{$APPTYPE CONSOLE}
{$INCLUDE '..\Source\Telemetry_defs.inc'}

uses
  FastMM4         in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',
  SysUtils,
  Classes,

{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed                   in '..\..\Condensed API Headers\SCS_Telemetry_Condensed.pas',
{$ELSE}
  scssdk                                    in '..\..\Telemetry API Headers\scssdk.pas',
  scssdk_value                              in '..\..\Telemetry API Headers\scssdk_value.pas',
  scssdk_telemetry                          in '..\..\Telemetry API Headers\scssdk_telemetry.pas',
  scssdk_telemetry_event                    in '..\..\Telemetry API Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel                  in '..\..\Telemetry API Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry_common_configs           in '..\..\Telemetry API Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels          in '..\..\Telemetry API Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels  in '..\..\Telemetry API Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels    in '..\..\Telemetry API Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2                               in '..\..\Telemetry API Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2                     in '..\..\Telemetry API Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
{$ENDIF}

  CRC32           in '..\Source\Libs\CRC32.pas',
  MD5             in '..\Source\Libs\MD5.pas',
{$IFDEF MulticastEvents}
  MulticastEvent  in '..\Source\Libs\MulticastEvent.pas',
{$ENDIF}
  SimpleLog       in '..\Source\Libs\SimpleLog.pas',
  BitOps          in '..\Source\Libs\BitOps.pas',

  TelemetryCommon           in '..\Source\TelemetryCommon.pas',
  TelemetryValueTypeUtils   in '..\Source\TelemetryValueTypeUtils.pas',
  TelemetryIDs              in '..\Source\TelemetryIDs.pas',
  TelemetryConversions      in '..\Source\TelemetryConversions.pas',
  TelemetryStrings          in '..\Source\TelemetryStrings.pas',
  TelemetryLists            in '..\Source\TelemetryLists.pas',
  TelemetryStreaming        in '..\Source\TelemetryStreaming.pas',
  TelemetryVersionObjects   in '..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\Source\TelemetryRecipientBinder.pas',

  TelemetrySCS_Examples_telemetry          in '..\Source\SCS\TelemetrySCS_Examples_telemetry.pas',
  TelemetrySCS_Examples_telemetry_position in '..\Source\SCS\TelemetrySCS_Examples_telemetry_position.pas',
  TelemetrySCS_Examples_telemetry_mem      in '..\Source\SCS\TelemetrySCS_Examples_telemetry_mem.pas',  
  
  TelemetryLogText          in '..\Source\Log\TelemetryLogText.pas',
  TelemetryLogBinary        in '..\Source\Log\TelemetryLogBinary.pas',
  TelemetryLogBinaryParser  in '..\Source\Log\TelemetryLogBinaryParser.pas',

  TelemetryCommCommon           in '..\Source\Comm\TelemetryCommCommon.pas',
  TelemetryCommPackets          in '..\Source\Comm\TelemetryCommPackets.pas',
  TelemetryCommCircularBuffers  in '..\Source\Comm\TelemetryCommCircularBuffers.pas',
  TelemetryCommPacketsAllocator in '..\Source\Comm\TelemetryCommPacketsAllocator.pas',
  TelemetryCommPacketsBuilder   in '..\Source\Comm\TelemetryCommPacketsBuilder.pas',
  TelemetryCommPacketsResolving in '..\Source\Comm\TelemetryCommPacketsResolving.pas',
  TelemetryCommRemoteRecipient  in '..\Source\Comm\TelemetryCommRemoteRecipient.pas',
  TelemetryCommTransmitter      in '..\Source\Comm\TelemetryCommTransmitter.pas',
  TelemetryCommReceiver         in '..\Source\Comm\TelemetryCommReceiver.pas',
  TelemetryCommCommunicator     in '..\Source\Comm\TelemetryCommCommunicator.pas';

  procedure RandomizeArray(var Value: TValueTypesArray);
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

begin
  Randomize;

  // CompressValueTypesArray
  RandomizeArray(ARR);
  WriteArray(ARR,False);
  CompressValueTypesArray(ARR);
  WriteArray(ARR,False);

  // ValueTypeBitmask
  WriteLn;
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
  WriteLn(NumberToBits(ValueTypesBitmask([SCS_VALUE_TYPE_bool,SCS_VALUE_TYPE_u64,SCS_VALUE_TYPE_dplacement,SCS_VALUE_TYPE_string])));
  WriteLn(NumberToBits(ValueTypesBitmask([SCS_VALUE_TYPE_s32,SCS_VALUE_TYPE_dplacement,SCS_VALUE_TYPE_fplacement,25])));

  // ValueTypesBitmask
  WriteLn;
  RandomizeArray(ARR);
  WriteArray(ARR, False);
  WriteLn(NumberToBits(ValueTypesBitmask(ARR)));
  RandomizeArray(ARR);
  ARR[15] := 19;
  WriteArray(ARR, False);
  WriteLn(NumberToBits(ValueTypesBitmask(ARR)));

  // BitmaskValueType
  WriteLn;
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
  For i := 1 to 5 do
    begin
      BM := Random((1 shl 11) + 1);
      WriteLn(NumberToBits(BM));
      WriteArray(BitmaskValueTypes(BM),False);
    end;
  BM := Random(High(TValueTypeBitmask));
  WriteLn(NumberToBits(BM));
  WriteArray(BitmaskValueTypes(BM),False);

  // ValueTypeBitmaskAdd
  WriteLn;
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
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn(NumberToBits(SecondaryValueTypesBitmask(i)) + ' (' + IntToStr(i) + ')' + SCSValueTypeToStr(i));

  // SecondaryValueTypes
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      Write('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ': ');
      WriteArray(SecondaryValueTypes(i),False);
    end;

  // SecondaryValueTypesCount
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ' (' + NumberToBits(SecondaryValueTypesBitmask(i)) +
            '): ' + IntToStr(SecondaryValueTypesCount(i)));

  // SupportedValueTypesBitmask
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    WriteLn(NumberToBits(SupportedValueTypesBitmask(i)) + ' (' + IntToStr(i) + ')' + SCSValueTypeToStr(i));

  // SupportedValueTypes
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      Write('(' + IntToStr(i) + ')' + SCSValueTypeToStr(i) + ': ');
      WriteArray(SupportedValueTypes(i),False);
    end;

  // ValidateSecondaryValueTypesBitmask
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SecondaryValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + '): ' + BoolToStr(ValidateSecondaryValueTypesBitmask(BM,i),True));
    end;

  // ValidateSupportedValueTypesBitmask
  WriteLn;
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      BM := SupportedValueTypesBitmask(i);
      If Random(2) = 0 then
        BM := BM or ValueTypeBitmask(Random(SCS_VALUE_TYPE_LAST + 1));
      WriteLn(NumberToBits(BM) + ' (' + SCSValueTypeToStr(i) + '): ' + BoolToStr(ValidateSupportedValueTypesBitmask(BM,i),True));
    end;

  // MakeValidSecondaryValueTypesBitmask
  WriteLn;
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
  For i := 0 to SCS_VALUE_TYPE_LAST do
    begin
      WriteLn('(' + SCSValueTypeToStr(i) + ') ');
      WriteArray(SupportedValueTypes(i),False);
      BM := Random(High(LongWord));
      BT := Random(2) = 0;
      WriteLn(NumberToBits(BM) + ' (' + BoolToStr(Bt,True) + ')');
      WriteArray(SelectSupportedValueTypes(i,BT,BM),False);
    end;

  WriteLn;
  WriteLn('Press enter to end...'); ReadLn;
end.
