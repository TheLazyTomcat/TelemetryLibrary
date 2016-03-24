{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program Condensed_tester;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SCS_Telemetry_Condensed in '..\..\Condensed Headers\SCS_Telemetry_Condensed.pas';

begin
  WriteLn('Condensed Tester');
  WriteLn;
  Write('Press enter to end...'); ReadLn;
end.
