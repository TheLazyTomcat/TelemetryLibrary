{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program LogConverter;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  LogConverter_Main in '..\LogConverter_Main.pas';

begin
LogConverter_Main.Main;
end.

