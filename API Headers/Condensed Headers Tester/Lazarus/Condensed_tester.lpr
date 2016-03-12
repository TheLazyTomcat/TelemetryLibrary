program Condensed_tester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SCS_Telemetry_Condensed
  { you can add units after this };

begin
  WriteLn('Condensed Tester');
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.

