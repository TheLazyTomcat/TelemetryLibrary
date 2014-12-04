{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program TelemetryMemReader;

{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  FastMM4 in 'Libs\FastMM\FastMM4.pas',
  FastMM4Messages in 'Libs\FastMM\FastMM4Messages.pas',

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
  TelemetryVersionObjects   in '..\..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\..\Source\TelemetryRecipientBinder.pas',

  TelemetrySCS_Examples_telemetry_mem in '..\..\Source\SCS\TelemetrySCS_Examples_telemetry_mem.pas',

  Forms,
  MainForm in 'MainForm.pas' {fMainForm},
  ImgDraw in 'ImgDraw.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Telemetry Mem Reader';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.Run;
end.
