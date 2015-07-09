{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program SCSTelemetry_BinaryLogger;

{$mode objfpc}{$H+}
{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  SysUtils,
  Windows,

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
  TelemetryStreaming,
  TelemetryVersionObjects,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryRecipientBinder,

  TelemetryLogText,
  TelemetryLogBinary,
  TelemetryLogBinaryParser;

var
  ErrorCode:    Integer;
  OutFileName:  String;
  Converter:    TTelemetryLogBinaryToTextConverter;

begin
WriteLn('================================');
WriteLn('        Telemetry Library       ');
WriteLn('  Binary to Text log Converter  ');
WriteLn('          Version 1.0           ');
WriteLn('================================');
WriteLn;
If ParamCount > 0 then
  try
    WriteLn('Creating converter object...');
    WriteLn;
    WriteLn('Input binary-log file:');
    WriteLn(ParamStr(1));
    If ParamCount > 1 then OutFileName := ParamStr(2)
      else OutFileName := '';
    Converter := TTelemetryLogBinaryToTextConverter.Create(ParamStr(1),OutFileName);
    try
      WriteLn;
      WriteLn('Output text-log file:');
      WriteLn(Converter.OutFileName);
      WriteLn;
      WriteLn('Running conversion...');
      Converter.Convert;
      WriteLn;
      WriteLn('Conversion complete.');
    finally
      WriteLn;
      WriteLn('Freeing converter object.');
      Converter.Free;
      OutFileName := '';
    end;
  except
    on E: Exception do
      begin
        WriteLn('Exception occured, program terminated (' + E.Message + ').');
        Write('Press enter to end the program...'); ReadLn;
      end
    else
      ErrorCode := GetLastError;
      WriteLn;
      If ErrorCode <> 0 then
        WriteLn('Fatal error occured, program terminated (0x' +
        IntToHex(ErrorCode,8) + ', "' + SysErrorMessage(ErrorCode) + '").')
      else
        WriteLn('Unknown error occured, program terminated.');
      Write('Press enter to end the program...'); ReadLn;
  end
else
  begin
    WriteLn('Program use:');
    WriteLn;
    WriteLn('  LogConverter.exe bin_file_name [out_file_name]');
    WriteLn;
    Write('Press enter to end the program...'); ReadLn;
  end;
end.

