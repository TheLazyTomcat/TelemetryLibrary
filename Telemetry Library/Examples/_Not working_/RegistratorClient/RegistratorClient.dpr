program RegistratorClient;

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  FastMM4 in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',
  Forms,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed in '..\..\..\Condensed\SCS_Telemetry_Condensed.pas',
{$ELSE}
  scssdk in '..\..\..\Headers\scssdk.pas',
  scssdk_value in '..\..\..\Headers\scssdk_value.pas',
  scssdk_telemetry in '..\..\..\Headers\scssdk_telemetry.pas',
  scssdk_telemetry_event in '..\..\..\Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel in '..\..\..\Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry_common_configs in '..\..\..\Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels in '..\..\..\Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels in '..\..\..\Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels in '..\..\..\Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2 in '..\..\..\Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2 in '..\..\..\Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
{$ENDIF}
  CRC32 in '..\..\Telemetry\Libs\CRC32.pas',
  MD5 in '..\..\Telemetry\Libs\MD5.pas',
  TelemetryCommon in '..\..\Telemetry\TelemetryCommon.pas',
  TelemetryVersionObjects in '..\..\Telemetry\TelemetryVersionObjects.pas',
  TelemetryLists in '..\..\Telemetry\TelemetryLists.pas',
  TelemetryInfoProvider in '..\..\Telemetry\TelemetryInfoProvider.pas',
  TelemetryRecipient in '..\..\Telemetry\TelemetryRecipient.pas',
  TelemetryRecipientAux in '..\..\Telemetry\TelemetryRecipientAux.pas',
  TelemetryNetCommon in '..\..\Telemetry\NET\TelemetryNetCommon.pas',
  TelemetryNetHashing in '..\..\Telemetry\NET\TelemetryNetHashing.pas',
  TelemetryNetLists in '..\..\Telemetry\NET\TelemetryNetLists.pas',
  TelemetryNetIncomingStream in '..\..\Telemetry\NET\TelemetryNetIncomingStream.pas',
  TelemetryNetCircularBuffers in '..\..\Telemetry\NET\TelemetryNetCircularBuffers.pas',
  TelemetryNetSockets in '..\..\Telemetry\NET\TelemetryNetSockets.pas',  
  TelemetryNetPackets in '..\..\Telemetry\NET\TelemetryNetPackets.pas',
  TelemetryNetPacketsBuilding in '..\..\Telemetry\NET\TelemetryNetPacketsBuilding.pas',
  TelemetryNetPacketsResolving in '..\..\Telemetry\NET\TelemetryNetPacketsResolving.pas',
  TelemetryNetClientBase in '..\..\Telemetry\NET\TelemetryNetClientBase.pas',    
  TelemetryNetClientRecipient in '..\..\Telemetry\NET\TelemetryNetClientRecipient.pas',  
  MainForm in 'MainForm.pas' {frmMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Registrator Client';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
