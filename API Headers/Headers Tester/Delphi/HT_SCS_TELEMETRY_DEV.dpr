{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program HT_SCS_TELEMETRY_DEV;

{$APPTYPE CONSOLE}

uses
  scssdk in '..\..\Headers\scssdk.pas',
  scssdk_value in '..\..\Headers\scssdk_value.pas',
  scssdk_telemetry_event in '..\..\Headers\scssdk_telemetry_event.pas',
  scssdk_telemetry_channel in '..\..\Headers\scssdk_telemetry_channel.pas',
  scssdk_telemetry in '..\..\Headers\scssdk_telemetry.pas',
  scssdk_telemetry_common_configs in '..\..\Headers\common\scssdk_telemetry_common_configs.pas',
  scssdk_telemetry_common_channels in '..\..\Headers\common\scssdk_telemetry_common_channels.pas',
  scssdk_telemetry_trailer_common_channels in '..\..\Headers\common\scssdk_telemetry_trailer_common_channels.pas',
  scssdk_telemetry_truck_common_channels in '..\..\Headers\common\scssdk_telemetry_truck_common_channels.pas',
  scssdk_eut2 in '..\..\Headers\eurotrucks2\scssdk_eut2.pas',
  scssdk_telemetry_eut2 in '..\..\Headers\eurotrucks2\scssdk_telemetry_eut2.pas',
  scssdk_ats in '..\..\Headers\amtrucks\scssdk_ats.pas',
  scssdk_telemetry_ats in '..\..\Headers\amtrucks\scssdk_telemetry_ats.pas';

begin
  WriteLn('HT_SCS_TELEMETRY_DEV');
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.
