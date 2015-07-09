{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program LogConverter;

{$APPTYPE CONSOLE}
{$INCLUDE '..\..\..\Source\Telemetry_defs.inc'}

uses
  FastMM4         in '..\..\..\..\Libs\FastMM\Prg\FastMM4.pas',
  FastMM4Messages in '..\..\..\..\Libs\FastMM\Prg\FastMM4Messages.pas',
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
  TelemetryStreaming        in '..\..\..\Source\TelemetryStreaming.pas',
  TelemetryVersionObjects   in '..\..\..\Source\TelemetryVersionObjects.pas',
  TelemetryInfoProvider     in '..\..\..\Source\TelemetryInfoProvider.pas',
  TelemetryRecipient        in '..\..\..\Source\TelemetryRecipient.pas',
  TelemetryRecipientBinder  in '..\..\..\Source\TelemetryRecipientBinder.pas',
  
  TelemetryLogText          in '..\..\..\Source\Log\TelemetryLogText.pas',
  TelemetryLogBinary        in '..\..\..\Source\Log\TelemetryLogBinary.pas',
  TelemetryLogBinaryParser  in '..\..\..\Source\Log\TelemetryLogBinaryParser.pas';

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

