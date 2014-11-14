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

var
  Test: TTelemetryInfoProvider;
  Temp: TStringList;
  i:    Integer;

begin
  Test := TTelemetryInfoProvider.CreateCurrent(SCS_GAME_ID_EUT2);
  try
    Temp := TStringList.Create;
    try
      For i := 0 to Pred(Test.KnownChannels.Count) do
        Temp.Add(IntToHex(Test.KnownChannels[i].ID,8) + ' : ' + TelemetryStringDecode(Test.KnownChannels[i].Name));
      WriteLn(Temp.Text);

      Temp.Clear;
      WriteLn;

      For i := 0 to Pred(Test.KnownConfigs.Count) do
        Temp.Add(IntToHex(Test.KnownConfigs[i].ID,8) + ' : ' + TelemetryStringDecode(Test.KnownConfigs[i].Name));
      WriteLn(Temp.Text);
    finally
      Temp.Free;
    end;
  finally
    Test.Free;
  end;

  WriteLn('Press enter to end...'); ReadLn;
end.
