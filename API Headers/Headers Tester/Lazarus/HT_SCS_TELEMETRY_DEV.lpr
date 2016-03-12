program HT_SCS_TELEMETRY_DEV;

{$mode objfpc}{$H+}

uses
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel,
  scssdk_telemetry,
  scssdk_telemetry_common_configs,
  scssdk_telemetry_common_channels,
  scssdk_telemetry_trailer_common_channels,
  scssdk_telemetry_truck_common_channels,
  scssdk_eut2,
  scssdk_telemetry_eut2;

begin
  WriteLn('HT_SCS_TELEMETRY_DEV');
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.

