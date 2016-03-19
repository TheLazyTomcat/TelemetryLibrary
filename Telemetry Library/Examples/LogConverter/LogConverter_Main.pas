{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit LogConverter_Main;

interface

procedure Main;

implementation

uses
{$IFDEF FPC}
  Windows,
{$ENDIF}
  SysUtils,
  TelemetryLogBinaryParser
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  , LazUTF8
{$IFEND};

//------------------------------------------------------------------------------  

procedure Main;
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
    If ParamCount > 1 then
    {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
      OutFileName := SysToUTF8(ParamStr(2));
    {$ELSE}
      OutFileName := ParamStr(2)
    {$IFEND}
    else
      OutFileName := '';
  {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
    Converter := TTelemetryLogBinaryToTextConverter.Create(SysToUTF8(ParamStr(1)),OutFileName);
  {$ELSE}
    Converter := TTelemetryLogBinaryToTextConverter.Create(ParamStr(1),OutFileName);
  {$IFEND}
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
    end;
  except
    on E: Exception do
      begin
        WriteLn(Format('Exception occured, program terminated (%s).',[E.Message]));
        Write('Press enter to end the program...'); ReadLn;
      end
    else
      ErrorCode := GetLastError;
      WriteLn;
      If ErrorCode <> 0 then
        WriteLn(Format('Fatal error occured, program terminated (0x%.8x, "%s").',[ErrorCode,SysErrorMessage(ErrorCode)]))
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
end;

end.
