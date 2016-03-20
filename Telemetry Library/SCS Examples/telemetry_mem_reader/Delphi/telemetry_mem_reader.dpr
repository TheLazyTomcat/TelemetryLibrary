{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program telemetry_mem_reader;

uses
  FastMM4,

  Forms,
  MainForm in '..\MainForm.pas' {fMainForm},
  ImgDraw in '..\ImgDraw.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Telemetry Mem Reader';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.Run;
end.
