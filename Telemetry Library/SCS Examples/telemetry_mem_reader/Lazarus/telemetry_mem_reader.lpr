{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program telemetry_mem_reader;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, ImgDraw, MainForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Telemetry Mem Reader';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.Run;
end.
