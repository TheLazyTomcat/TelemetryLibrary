library SCSTelemetry_Server;

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  FastMM4 in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',
  SysUtils,
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
  TelemetryNetServer in '..\..\Telemetry\NET\TelemetryNetServer.pas';

var
  Server: TTelemetryNetServer = nil;

{$R *.res}

{=== Library exported routines ================================================}

Function TelemetryLibraryInit(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;
begin
If not TTelemetryRecipient.SupportsTelemetryAndGameVersion(
  Version,params^.common.game_id,params^.common.game_version) then
  begin
    Result := SCS_RESULT_unsupported;
  end
else
  begin
    Server := TTelemetryNetServer.Create(TTelemetryRecipient.Create(version,params^));
    Server.TelemetryRecipient.Log('== Server running ==');
    Server.TelemetryRecipient.StoreChannelValues := True;
    Result := SCS_RESULT_ok;
  end;
end;

procedure TelemetryLibraryFinal;
begin
Server.TelemetryRecipient.Free;
end;

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';

begin
end.
