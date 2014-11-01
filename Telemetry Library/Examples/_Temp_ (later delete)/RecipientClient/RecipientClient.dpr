program RecipientClient;

{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  FastMM4 in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',
  Forms,
  SysUtils,
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

  TelemetryCommon in '..\..\Source\TelemetryCommon.pas',
  TelemetryIDs            in '..\..\Source\TelemetryIDs.pas',
  TelemetryStrings        in '..\..\Source\TelemetryStrings.pas',
  TelemetryVersionObjects in '..\..\Source\TelemetryVersionObjects.pas',
  TelemetryLists in '..\..\Source\TelemetryLists.pas',
  TelemetryInfoProvider in '..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient in '..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientAux in '..\..\Source\TelemetryRecipientAux.pas',
  TelemetryNetCommon in '..\..\Source\NET\TelemetryNetCommon.pas',
  TelemetryNetHashing in '..\..\Source\NET\TelemetryNetHashing.pas',
  TelemetryNetLists in '..\..\Source\NET\TelemetryNetLists.pas',
  TelemetryNetIncomingStream in '..\..\Source\NET\TelemetryNetIncomingStream.pas',
  TelemetryNetCircularBuffers in '..\..\Source\NET\TelemetryNetCircularBuffers.pas',
  TelemetryNetSockets in '..\..\Source\NET\TelemetryNetSockets.pas',
  TelemetryNetPackets in '..\..\Source\NET\TelemetryNetPackets.pas',
  TelemetryNetPacketsBuilding in '..\..\Source\NET\TelemetryNetPacketsBuilding.pas',
  TelemetryNetPacketsResolving in '..\..\Source\NET\TelemetryNetPacketsResolving.pas',
  TelemetryNetClientBase in '..\..\Source\NET\TelemetryNetClientBase.pas',
  TelemetryNetClientRecipient in '..\..\Source\NET\TelemetryNetClientRecipient.pas',
  MainForm in 'MainForm.pas' {frmMainForm},
  RegisterEventForm in 'RegisterEventForm.pas' {frmRegisterEvent},
  ChannelsForm in 'ChannelsForm.pas' {frmChannelsForm},
  RegisterChannelForm in 'RegisterChannelForm.pas' {frmRegisterChannel};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Recipient Client';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmRegisterEvent, frmRegisterEvent);
  Application.CreateForm(TfrmChannelsForm, frmChannelsForm);
  Application.CreateForm(TfrmRegisterChannel, frmRegisterChannel);
  Application.Run;
end.
