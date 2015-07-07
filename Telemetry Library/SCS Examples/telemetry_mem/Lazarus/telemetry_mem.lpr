{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
library telemetry_mem;

{$mode objfpc}{$H+}
{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  SysUtils,

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

  TelemetryCommon,
  TelemetryIDs,
  TelemetryConversions,
  TelemetryStrings,
  TelemetryValueTypeUtils,
  TelemetryLists,
  TelemetryVersionObjects,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryRecipientBinder,

  TelemetrySCSExample_telemetry_mem;

var
  Recipient:        TTelemetryRecipient = nil;
  TelemetryLogger:  TSCSExm_TelemetryMem = nil;

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
      TelemetryLogger := TSCSExm_TelemetryMem.Create(Recipient);
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
