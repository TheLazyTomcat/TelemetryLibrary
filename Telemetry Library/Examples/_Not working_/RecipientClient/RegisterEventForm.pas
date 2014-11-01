unit RegisterEventForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmRegisterEvent = class(TForm)
    lblKnownEvents: TLabel;
    lbKnownEvents:  TListBox;
    lblManualEntry: TLabel;
    seManualEntry:  TSpinEdit;
    btnOK:          TButton;
    btnCancel:      TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbKnownEventsDblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);    
  private
    fLastSelectedIndex: Integer;
  public
    { Public declarations }
  end;

var
  frmRegisterEvent: TfrmRegisterEvent;

implementation

uses
  MainForm;

{$R *.dfm}

procedure TfrmRegisterEvent.FormShow(Sender: TObject);
var
  i:  Integer;
begin
lbKnownEvents.Items.Clear;
with MainForm.Client.TelemetryInfoProvider do
  For i := 0 to (KnownEvents.Count - 1) do
    lbKnownEvents.Items.Add('0x' + IntToHex(KnownEvents[i].Event,8) +
                            ' - ' + EventGetName(KnownEvents[i].Event));
If lbKnownEvents.Items.Count > fLastSelectedIndex then
  lbKnownEvents.ItemIndex := fLastSelectedIndex;
seManualEntry.Value := -1;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterEvent.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
fLastSelectedIndex := lbKnownEvents.ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterEvent.lbKnownEventsDblClick(Sender: TObject);
begin
btnOK.OnClick(nil);
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterEvent.btnOKClick(Sender: TObject);
begin
If (lbKnownEvents.ItemIndex < 0) and (seManualEntry.Value < 0) then
  ShowMessage('Select one of known events or select event number larger or equal to 0.')
else
  begin
    If seManualEntry.Value >= 0 then
      MainForm.Client.EventRegister(seManualEntry.Value)
    else
      MainForm.Client.EventRegister(MainForm.Client.TelemetryInfoProvider.KnownEvents[lbKnownEvents.ItemIndex].Event);
    Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterEvent.btnCancelClick(Sender: TObject);
begin
Close;
end;

end.
