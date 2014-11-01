unit RegisterChannelForm;

interface

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_channel;
{$ENDIF}

type
  TfrmRegisterChannel = class(TForm)
    lblKnownChannels: TLabel;
    lbKnownChannels:  TListBox;
    cbManualEntry:    TCheckBox;
    eChannelName:     TEdit;
    lblIndex:         TLabel;
    seIndex:          TSpinEdit;
    lblValueType:     TLabel;
    cmbValueType:     TComboBox;
    cbEachFrame:      TCheckBox;
    cbNoValue:        TCheckBox;
    btnOK:            TButton;
    btnCancel:        TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbKnownChannelsClick(Sender: TObject);
    procedure lbKnownChannelsDblClick(Sender: TObject);
    procedure cbManualEntryClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);    
    procedure btnCancelClick(Sender: TObject);
  private
    fLastSelectedIndex: Integer;
  public
    { Public declarations }
  end;

var
  frmRegisterChannel: TfrmRegisterChannel;

implementation

uses
  MainForm,
  TelemetryLists,
  TelemetryRecipientAux;

{$R *.dfm}

procedure TfrmRegisterChannel.FormShow(Sender: TObject);
var
  i:        Integer;
  TempStr:  String;
begin
lbKnownChannels.Items.Clear;
with MainForm.Client.TelemetryInfoProvider do
  For i := 0 to (KnownChannels.Count - 1) do
    begin
      If KnownChannels[i].Indexed then
        TempStr := KnownChannels[i].Name + '[]'
      else
        TempStr := KnownChannels[i].Name;
      lbKnownChannels.Items.Add(TempStr);
    end;
If lbKnownChannels.Items.Count > fLastSelectedIndex then
  lbKnownChannels.ItemIndex := fLastSelectedIndex;
cbManualEntry.Checked := False;
cbEachFrame.Checked := False;
cbNoValue.Checked := False;
lbKnownChannels.OnClick(nil);
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
fLastSelectedIndex := lbKnownChannels.ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.lbKnownChannelsClick(Sender: TObject);
var
  KnownChannel: TKnownChannel;
begin
If lbKnownChannels.ItemIndex >= 0 then
  begin
    cmbValueType.Items.Clear;
    KnownChannel := Client.TelemetryInfoProvider.KnownChannels[lbKnownChannels.ItemIndex];
    If KnownChannel.PrimaryType <> SCS_VALUE_TYPE_INVALID then
      cmbValueType.Items.Add(SCSValueTypeToStr(KnownChannel.PrimaryType));
    If KnownChannel.SecondaryType <> SCS_VALUE_TYPE_INVALID then
      cmbValueType.Items.Add(SCSValueTypeToStr(KnownChannel.SecondaryType));
    If KnownChannel.TertiaryType <> SCS_VALUE_TYPE_INVALID then
      cmbValueType.Items.Add(SCSValueTypeToStr(KnownChannel.TertiaryType));
    If cmbValueType.Items.Count > 0 then cmbValueType.ItemIndex := 0;
    If not KnownChannel.Indexed then seIndex.Value := Integer(SCS_U32_NIL);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.lbKnownChannelsDblClick(Sender: TObject);
begin
btnOK.OnClick(nil);
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.cbManualEntryClick(Sender: TObject);
var
  i:  Integer;
begin
lblKnownChannels.Enabled := not cbManualEntry.Checked;
lbKnownChannels.Enabled := not cbManualEntry.Checked;
eChannelName.Enabled := cbManualEntry.Checked;
If cbManualEntry.Checked then
  begin
    cmbValueType.Items.Clear;
    For i := Low(cSCSValueTypeIdentifiers) to High(cSCSValueTypeIdentifiers) do
      cmbValueType.Items.Add(cSCSValueTypeIdentifiers[i]);
    If cmbValueType.Items.Count > 0 then cmbValueType.ItemIndex := 0;   
  end
else lbKnownChannels.OnClick(nil);
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.btnOKClick(Sender: TObject);
var
  Flags:        scs_u32_t;
  KnownChannel: TKnownChannel;
begin
Flags := SCS_TELEMETRY_CHANNEL_FLAG_none;
If cbEachFrame.Checked then Flags := Flags or SCS_TELEMETRY_CHANNEL_FLAG_each_frame;
If cbNoValue.Checked then Flags := Flags or SCS_TELEMETRY_CHANNEL_FLAG_no_value;
If cbManualEntry.Checked then
  begin
    If eChannelName.Text <> '' then
      begin
        MainForm.Client.ChannelRegister(eChannelName.Text,
                                        seIndex.Value,
                                        SCSValueTypeFromStr(cmbValueType.Text),
                                        Flags);
        Close;
      end
    else
      ShowMessage('Enter non-empty string as a name for registered channel.')
  end
else
  begin
    If lbKnownChannels.ItemIndex < 0 then
      ShowMessage('Select one of known channels.')
    else
      begin
        KnownChannel := MainForm.Client.TelemetryInfoProvider.KnownChannels[lbKnownChannels.ItemIndex];
        MainForm.Client.ChannelRegister(KnownChannel.Name,
                                        seIndex.Value,
                                        SCSValueTypeFromStr(cmbValueType.Text),
                                        Flags);
        Close;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmRegisterChannel.btnCancelClick(Sender: TObject);
begin
Close;
end;

end.
