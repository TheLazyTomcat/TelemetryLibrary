{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program SCSTelemetry_BinaryLogger;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Windows,

  SCS_Telemetry_Condensed,

  TelemetryRecipient,

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

