program Telemetry_TestPrg;

{$APPTYPE CONSOLE}
{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  FastMM4         in '..\..\..\Libs\FastMM\Prg\FastMM4.pas',
  FastMM4Messages in '..\..\..\Libs\FastMM\Prg\FastMM4Messages.pas',
  SysUtils,
  Classes,

{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed                   in '..\..\..\Condensed API Headers\SCS_Telemetry_Condensed.pas',
{$ELSE}
  scssdk                                    in '..\..\..\Telemetry API Headers\scssdk.pas',
  scssdk_value                              in '..\..\..\Telemetry API Headers\scssdk_value.pas',
  scssdk_telemetry                          in '..\..\..\Telemetry API Headers\scssdk_telemetry.pas',
  scssdk_telemetry_event                    in '..\..\..\Telemetry API Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel                  in '..\..\..\Telemetry API Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry_common_configs           in '..\..\..\Telemetry API Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels          in '..\..\..\Telemetry API Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels  in '..\..\..\Telemetry API Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels    in '..\..\..\Telemetry API Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2                               in '..\..\..\Telemetry API Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2                     in '..\..\..\Telemetry API Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
{$ENDIF}

  CRC32           in '..\..\Source\Libs\CRC32.pas',
  MD5             in '..\..\Source\Libs\MD5.pas',
{$IFDEF MulticastEvents}
  MulticastEvent  in '..\..\Source\Libs\MulticastEvent.pas',
{$ENDIF}
  SimpleLog       in '..\..\Source\Libs\SimpleLog.pas',
  BitOps          in '..\..\Source\Libs\BitOps.pas',

  TelemetryCommon           in '..\..\Source\TelemetryCommon.pas',
  TelemetryIDs              in '..\..\Source\TelemetryIDs.pas',
  TelemetryConversions      in '..\..\Source\TelemetryConversions.pas',
  TelemetryStrings          in '..\..\Source\TelemetryStrings.pas',
  TelemetryValueTypeUtils   in '..\..\Source\TelemetryValueTypeUtils.pas',
  TelemetryLists            in '..\..\Source\TelemetryLists.pas',
//  TelemetryStreaming        in '..\..\Source\TelemetryStreaming.pas',
//  TelemetryVersionObjects   in '..\..\Source\TelemetryVersionObjects.pas',
//  TelemetryInfoProvider     in '..\..\Source\TelemetryInfoProvider.pas',
//  TelemetryRecipient        in '..\..\Source\TelemetryRecipient.pas',
//  TelemetryRecipientBinder  in '..\..\Source\TelemetryRecipientBinder.pas',

//  TelemetrySCSExample_telemetry          in '..\..\Source\SCS\TelemetrySCSExample_telemetry.pas',
//  TelemetrySCSExample_telemetry_position in '..\..\Source\SCS\TelemetrySCSExample_telemetry_position.pas',
//  TelemetrySCSExample_telemetry_mem      in '..\..\Source\SCS\TelemetrySCSExample_telemetry_mem.pas',

//  TelemetryLogText          in '..\..\Source\Log\TelemetryLogText.pas',
//  TelemetryLogBinary        in '..\..\Source\Log\TelemetryLogBinary.pas',
//  TelemetryLogBinaryParser  in '..\..\Source\Log\TelemetryLogBinaryParser.pas';

  strutils; //remove

//var
//  TIP:  TTelemetryInfoProvider;
//  TSL:  TStringList;
//  STR:  String;

var
  KCL:  TKnownConfigsList;

begin
KCL := TKnownConfigsList.Create;
try
  KCL.AddConfiguration('conf_0');
  KCL.AddConfiguration('conf_2');
  WriteLn(KCL.IndexOfConfiguration('conf_2'));
  KCL.InsertConfiguration(1,'conf_1');
  WriteLn(KCL.IndexOfConfiguration('conf_2'));

  KCL.Add( 'conf_1','1_1',0,True,True);
  KCL.Add( 'conf_5','5_1',0,True,True);
  KCL.Add( 'conf_5','5_1',0,True,True);

  WriteLn(KCL.ConfigurationCount);
  WriteLn(KCL.Count);
finally
  KCL.Free;
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
