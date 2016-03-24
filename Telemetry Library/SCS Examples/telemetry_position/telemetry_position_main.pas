{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit telemetry_position_main;

interface

implementation

uses
  SysUtils,
  SCS_Telemetry_Condensed,
  TelemetryRecipient,
  TelemetrySCSExample_telemetry_position;

var
  Recipient:        TTelemetryRecipient = nil;
  TelemetryLogger:  TSCSExm_TelemetryPosition = nil;

//------------------------------------------------------------------------------

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
      TelemetryLogger := TSCSExm_TelemetryPosition.Create(Recipient);
      Result := SCS_RESULT_ok;
    except
      Result := SCS_RESULT_generic_error;    
      FreeAndNil(Recipient);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TelemetryLibraryFinal; stdcall;
begin
FreeAndNil(TelemetryLogger);
FreeAndNil(Recipient);
end;

//------------------------------------------------------------------------------

exports
  TelemetryLibraryInit name 'scs_telemetry_init',
  TelemetryLibraryFinal name 'scs_telemetry_shutdown';

end.
