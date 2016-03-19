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
