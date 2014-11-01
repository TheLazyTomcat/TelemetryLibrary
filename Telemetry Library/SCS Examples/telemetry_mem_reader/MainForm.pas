unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  TelemetrySCS_Examples_telemetry_mem, ImgDraw;

type
  TfMainForm = class(TForm)
    tmrReadTimer: TTimer;
    stbStatusBar: TStatusBar;
    imgMainImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrReadTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    Reader: TSCSExm_TelemetryMem_Reader;
    Drawer: TImgDrawer;
    procedure ClearShownData;
    procedure UpdateShownData;
  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.dfm}

procedure TfMainForm.ClearShownData;
begin
imgMainImage.Picture.Assign(Drawer.ImgNoData);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.UpdateShownData;
begin
Reader.RetrieveCurrentState;
Drawer.Update(Reader.StoredState);
imgMainImage.Picture.Assign(Drawer.ImgOutput);
end;

//==============================================================================

procedure TfMainForm.FormCreate(Sender: TObject);
begin
Reader := TSCSExm_TelemetryMem_Reader.Create;
Drawer := TImgDrawer.Create(imgMainImage.Width,imgMainImage.Height,Self.Font);
tmrReadTimer.OnTimer(nil);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
Drawer.Free;
Reader.Free;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.tmrReadTimerTimer(Sender: TObject);
begin
If Reader.Initialized then UpdateShownData
  else
    begin
      If Reader.Initialize then UpdateShownData
        else ClearShownData;
    end;
end;

end.
