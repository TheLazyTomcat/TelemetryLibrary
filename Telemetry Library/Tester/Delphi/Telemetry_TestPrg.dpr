program Telemetry_TestPrg;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
  Classes,

  TelemetryCommon           in '..\..\Source\TelemetryCommon.pas',
  TelemetryIDs              in '..\..\Source\TelemetryIDs.pas',
  TelemetryConversions      in '..\..\Source\TelemetryConversions.pas',
  TelemetryStrings          in '..\..\Source\TelemetryStrings.pas',
  TelemetryValueTypeUtils   in '..\..\Source\TelemetryValueTypeUtils.pas',
  TelemetryLists            in '..\..\Source\TelemetryLists.pas',
  TelemetryStreaming        in '..\..\Source\TelemetryStreaming.pas',
  TelemetryVersionObjects   in '..\..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\..\Source\TelemetryRecipientBinder.pas'

//  TelemetrySCSExample_telemetry          in '..\..\Source\SCS\TelemetrySCSExample_telemetry.pas'
//  TelemetrySCSExample_telemetry_position in '..\..\Source\SCS\TelemetrySCSExample_telemetry_position.pas'
//  TelemetrySCSExample_telemetry_mem      in '..\..\Source\SCS\TelemetrySCSExample_telemetry_mem.pas'

//  TelemetryLogText          in '..\..\Source\Log\TelemetryLogText.pas'
//  TelemetryLogBinary        in '..\..\Source\Log\TelemetryLogBinary.pas'
//  TelemetryLogBinaryParser  in '..\..\Source\Log\TelemetryLogBinaryParser.pas'
;

var
  test: TStoredConfigsList;

begin
  Test := TStoredConfigsList.Create;
  try
    Test.Add('ID','Attr1',2,nil,True);
    Test.Add('ID','Attr2',0,nil,True);
    WriteLn(Test.IndexOf('ID','Attr2'));
    Test.Remove(ConfigReference('ID','Attr1'),2);
    WriteLn(Test.Count);
    WriteLn(Test.IndexOf('ID','Attr2'));
  finally
    Test.Free;
  end;
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.
