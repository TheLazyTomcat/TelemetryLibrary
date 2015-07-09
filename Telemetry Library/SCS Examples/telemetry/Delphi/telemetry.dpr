{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
library telemetry;

{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  FastMM4 in '..\..\..\..\Libs\FastMM\Lib\FastMM4.pas',
  FastMM4Messages in '..\..\..\..\Libs\FastMM\Lib\FastMM4Messages.pas',
  SysUtils,

{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed                   in '..\..\..\..\Condensed API Headers\SCS_Telemetry_Condensed.pas',
{$ELSE}
  scssdk                                    in '..\..\..\..\Telemetry API Headers\scssdk.pas',
  scssdk_value                              in '..\..\..\..\Telemetry API Headers\scssdk_value.pas',
  scssdk_telemetry                          in '..\..\..\..\Telemetry API Headers\scssdk_telemetry.pas',
  scssdk_telemetry_event                    in '..\..\..\..\Telemetry API Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel                  in '..\..\..\..\Telemetry API Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry_common_configs           in '..\..\..\..\Telemetry API Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels          in '..\..\..\..\Telemetry API Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels  in '..\..\..\..\Telemetry API Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels    in '..\..\..\..\Telemetry API Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2                               in '..\..\..\..\Telemetry API Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2                     in '..\..\..\..\Telemetry API Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
{$ENDIF}

  CRC32           in '..\..\..\Source\Libs\CRC32.pas',
  MD5             in '..\..\..\Source\Libs\MD5.pas',
{$IFDEF MulticastEvents}
  MulticastEvent  in '..\..\..\Source\Libs\MulticastEvent.pas',
{$ENDIF}
  SimpleLog       in '..\..\..\Source\Libs\SimpleLog.pas',
  BitOps          in '..\..\..\Source\Libs\BitOps.pas',
  
  TelemetryCommon           in '..\..\..\Source\TelemetryCommon.pas',
  TelemetryIDs              in '..\..\..\Source\TelemetryIDs.pas',
  TelemetryConversions      in '..\..\..\Source\TelemetryConversions.pas',
  TelemetryStrings          in '..\..\..\Source\TelemetryStrings.pas',
  TelemetryValueTypeUtils   in '..\..\..\Source\TelemetryValueTypeUtils.pas',
  TelemetryLists            in '..\..\..\Source\TelemetryLists.pas',
  TelemetryVersionObjects   in '..\..\..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\..\..\Source\TelemetryRecipientBinder.pas',

  TelemetrySCSExample_telemetry in '..\..\..\Source\SCS\TelemetrySCSExample_telemetry.pas';

{$R *.res}

var
  Recipient:        TTelemetryRecipient = nil;
  TelemetryLogger:  TSCSExm_Telemetry = nil;

Function TelemetryLibraryInit(version: scs_u32_t; params: p_scs_telemetry_init_params_t): scs_result_t; stdcall;
begin
If not TTelemetryRecipient.SupportsTelemetryVersion(version) then
  begin
    Result := SCS_RESULT_unsupported;
  end
else
  begin
    Recipient := TTelemetryRecipient.Create;
    Recipient.SetGameCallbacks(params^);
    Recipient.SetAPIInfo(version,params^);
    try
      TelemetryLogger := TSCSExm_Telemetry.Create(Recipient);
      Result := SCS_RESULT_ok;
    except
      Result := SCS_RESULT_generic_error;    
      FreeAndNil(Recipient);
    end;
  end;
end;

procedure TelemetryLibraryFinal; stdcall;
begin
FreeAndNil(TelemetryLogger);
FreeAndNil(Recipient);
end;

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';

begin
end.
