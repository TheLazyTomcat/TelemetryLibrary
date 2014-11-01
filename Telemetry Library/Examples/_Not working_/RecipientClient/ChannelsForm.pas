unit ChannelsForm;

interface

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Spin, ExtCtrls,
  TelemetryCommon,
  TelemetryLists,
  TelemetryNetClientRecipient,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value;
{$ENDIF}


type
  TBufferedChannelValue = record
    Name:           String;
    ID:             TChannelID;
    Index:          scs_u32_t;
    Value:          scs_value_localized_t;
    ValueConfigID:  TConfigID;    
    Changed:        Boolean;
  end;
  PBufferedChannelValue = ^TBufferedChannelValue;

  TChannelsValuesBuffer = class(TCustomTelemetryList)
  private
    fUpdated: Boolean;
    Function GetStoredValue(Index: Integer): TBufferedChannelValue;
  public
    procedure Clear; override;
    Function IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; virtual;
    Function Synchronize(TelemetryClient: TTelemetryNetClientRecipient): Integer; virtual;
    Function Remove(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t): Integer; virtual;
    procedure Delete(Index: Integer);
    Function ChangeValue(ID: TChannelID; Index: scs_u32_t; NewValue: p_scs_value_t): Integer; virtual;
    property ChannelsValues[Index: Integer]: TBufferedChannelValue read GetStoredValue; default;
  published
    property Updated: Boolean read fUpdated write fUpdated;
  end;

//------------------------------------------------------------------------------

  TfrmChannelsForm = class(TForm)
    sgRegisteredChannels: TStringGrid;
    lblUpdateInterval:    TLabel;
    seUpdateInterval:     TSpinEdit;
    tmrUpdateTimer:       TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure seUpdateIntervalChange(Sender: TObject);
    procedure tmrUpdateTimerTimer(Sender: TObject);
  private
    procedure InitChannelsTab;
  public
    BufferedChannelsValues: TChannelsValuesBuffer;
  end;

var
  frmChannelsForm: TfrmChannelsForm;

implementation

uses
  MainForm,
  TelemetryRecipient,
  TelemetryRecipientAux;

{$R *.dfm}

Function TChannelsValuesBuffer.GetStoredValue(Index: Integer): TBufferedChannelValue;
begin
If (Index >= 0) and (Index < Count) then
  Result := PBufferedChannelValue(PtrGetItem(Index))^
else
  raise Exception.Create('TChannelsValuesBuffer.GetStoredValue(Index): Index (' +
                         IntToStr(Index) + ') out of bounds.');
end;

//------------------------------------------------------------------------------

procedure TChannelsValuesBuffer.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PBufferedChannelValue(PtrGetItem(i)));
Updated := True;
inherited;
end;

//------------------------------------------------------------------------------

Function TChannelsValuesBuffer.IndexOf(ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
var
  i:        Integer;
  TempItem: PBufferedChannelValue;
begin
Result := -1;
For i := 0 to (Count - 1) do
  begin
    TempItem := PBufferedChannelValue(PtrGetItem(i));
    If (TempItem^.ID = ID) and (TempItem^.Index = Index) and
      ((TempItem^.Value.ValueType = ValueType) or (TempItem^.Value.ValueType = SCS_VALUE_TYPE_INVALID)) then
      begin
        Result := i;
        Break;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TChannelsValuesBuffer.Synchronize(TelemetryClient: TTelemetryNetClientRecipient): Integer;
var
  i:          Integer;
  RegChannel: TChannelInfo;
  NewItem:    PBufferedChannelValue;
begin
BeginUpdate;
try
  // Remove not registered channels.
  For i := (Count - 1) downto 0 do
    with PBufferedChannelValue(PtrGetItem(i))^ do
      If TelemetryClient.RegisteredChannels.IndexOf(ID,Index,Value.ValueType) < 0 then
        begin
          Delete(i);
          Updated := True;
        end;
  // Add new channels, if there are any.
  For i := 0 to (TelemetryClient.RegisteredChannels.Count - 1) do
    begin
      RegChannel := TelemetryClient.RegisteredChannels[i];
      If IndexOf(RegChannel.ID,RegChannel.Index,RegChannel.ValueType) < 0 then
        begin
          New(NewItem);
          NewItem^.Name := RegChannel.Name;
          NewItem^.ID := RegChannel.ID;
          NewItem^.Index := RegChannel.Index;
          NewItem^.Value.ValueType := SCS_VALUE_TYPE_INVALID;
          NewItem^.Changed := True;
          If PtrAdd(NewItem) < 0 then Dispose(NewItem)
            else Updated := True;
        end;
    end;
finally
  EndUpdate;
end;
Result := Count;
end;

//------------------------------------------------------------------------------

Function TChannelsValuesBuffer.Remove(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t): Integer;
begin
Result := IndexOf(GetItemID(Name),Index,ValueType);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TChannelsValuesBuffer.Delete(Index: Integer);
begin
If (Index >= 0) and (Index < Count) then
  begin
    Dispose(PBufferedChannelValue(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise Exception.Create('TChannelsValuesBuffer.Delete(Index): Index (' +
                         IntToStr(Index) + ') out of bounds.');
end;

//------------------------------------------------------------------------------

Function TChannelsValuesBuffer.ChangeValue(ID: TChannelID; Index: scs_u32_t; NewValue: p_scs_value_t): Integer;
begin
Result := -1;
If Assigned(NewValue) then
  begin
    Result := IndexOf(ID,Index,NewValue^._type);
    If Result >= 0 then
      with PBufferedChannelValue(PtrGetItem(Result))^ do
        begin
          Value := scs_value_localized(NewValue^);
          Changed := True;
        end;
  end;
end;

//==============================================================================

procedure TfrmChannelsForm.FormCreate(Sender: TObject);
begin
sgRegisteredChannels.DoubleBuffered := True;
BufferedChannelsValues := TChannelsValuesBuffer.Create;
InitChannelsTab;
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.FormShow(Sender: TObject);
begin
If Assigned(tmrUpdateTimer.OnTimer) then tmrUpdateTimer.OnTimer(nil);
tmrUpdateTimer.Interval := seUpdateInterval.Value;
tmrUpdateTimer.Enabled := True;    
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.FormResize(Sender: TObject);
begin
sgRegisteredChannels.ColWidths[1] := sgRegisteredChannels.Width -
                                     (sgRegisteredChannels.ColWidths[0] + 30);
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
tmrUpdateTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.FormDestroy(Sender: TObject);
begin
BufferedChannelsValues.Free;
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.seUpdateIntervalChange(Sender: TObject);
begin
tmrUpdateTimer.Interval := seUpdateInterval.Value;
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.tmrUpdateTimerTimer(Sender: TObject);
var
  i:          Integer;
  RegChannel: TChannelInfo;
  BuffValue:  TBufferedChannelValue;
begin
If BufferedChannelsValues.Updated then
  begin
    Caption := 'Channels [' + IntToStr(MainForm.Client.RegisteredChannels.Count) + ']';
    If BufferedChannelsValues.Count <= 0 then InitChannelsTab
      else sgRegisteredChannels.RowCount := BufferedChannelsValues.Count + 1;
    For i := 0 to (BufferedChannelsValues.Count - 1) do
      begin
        RegChannel := Client.RegisteredChannels[i];
        If RegChannel.Index <> SCS_U32_NIL then
          frmChannelsForm.sgRegisteredChannels.Cells[0,i + 1] := RegChannel.Name + '[' + IntToStr(RegChannel.Index) + ']'
        else
          frmChannelsForm.sgRegisteredChannels.Cells[0,i + 1] := RegChannel.Name;
      end;
    BufferedChannelsValues.Updated := False;
  end;
For i := 0 to (BufferedChannelsValues.Count - 1) do
  begin
    BuffValue := BufferedChannelsValues[i];
    If BuffValue.Changed then
      frmChannelsForm.sgRegisteredChannels.Cells[1,i + 1] := SCSValueLocalizedToStr(BuffValue.Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmChannelsForm.InitChannelsTab;
begin
sgRegisteredChannels.ColCount := 2;
sgRegisteredChannels.RowCount := 2;
sgRegisteredChannels.FixedRows := 1;
sgRegisteredChannels.DefaultRowHeight := 16;
sgRegisteredChannels.Cells[0,0] := 'Channel name';
sgRegisteredChannels.Cells[1,0] := 'Channel value';
sgRegisteredChannels.Cells[0,1] := '';
sgRegisteredChannels.Cells[1,1] := '';
end;

end.
