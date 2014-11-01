unit MainForm;

interface

{$INCLUDE '..\..\Telemetry\Telemetry_defs.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Buttons, ExtCtrls, StdCtrls, Spin, XPMan, ScktComp,
  TelemetryNetClientRecipient,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

type
  TfrmMainForm = class(TForm)
    lblEvents: TLabel;
    lbEvents: TListBox;
    meActionsLog: TMemo;
    lblActionsLog: TLabel;
    leAddress: TLabeledEdit;
    sePort: TSpinEdit;
    lblPort: TLabel;
    btnConnect: TButton;
    lblChannels: TLabel;
    lbChannels: TListBox;
    btnEventRegister: TButton;
    btnEventUnregister: TButton;
    bvlSplit: TBevel;
    stbStatusBar: TStatusBar;
    btnChannelRegister: TButton;
    btnChannelUnregister: TButton;
    oXPManifest: TXPManifest;
    lblItemNormal: TLabel;
    shpItemNormal: TShape;
    shpItemRegistered: TShape;
    shpItemUnknown: TShape;
    lblItemRegistered: TLabel;
    lblItemUnknown: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure lbEventsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbChannelsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnEventRegisterClick(Sender: TObject);
    procedure btnEventUnregisterClick(Sender: TObject);
    procedure lbEventsDblClick(Sender: TObject);
    procedure btnChannelRegisterClick(Sender: TObject);
    procedure btnChannelUnregisterClick(Sender: TObject);
    procedure lbChannelsDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ClearGroups;
    procedure EnableControls(Enable: Boolean);
    procedure OnConnecting(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnConnected(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnConnectionComplete(Sender: TObject);
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure OnKnownEventsChange(Sender: TObject);
    procedure OnRegisteredEventsChange(Sender: TObject);
    procedure OnKnownChannelsChange(Sender: TObject);
    procedure OnRegisteredChannelsChange(Sender: TObject);
    procedure OnEventRegister(Sender: TObject; Event: scs_event_t; Result: scs_result_t);
    procedure OnEventUnregister(Sender: TObject; Event: scs_event_t; Result: scs_result_t);
    procedure OnChannelRegister(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; Result: scs_result_t);
    procedure OnChannelUnregister(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Result: scs_result_t);
  end;

var
  frmMainForm:  TfrmMainForm;
  Client:       TTelemetryNetClientRecipient = nil;
  Connected:    Boolean = False;

implementation

{$R *.dfm}

uses
  Types, StrUtils,
  TelemetryLists,
  TelemetryRecipientAux;

const
  BgColorNormal             = clWindow;
  BgColorNormalSelected     = clSilver;
  BgColorRegistered         = $00a9ffd2;
  BgColorRegisteredSelected = $0000d567;
  BgColorUnknown            = $0088ffff;
  BgColorUnknownSelected    = $0000cccc;

  cMaxChannelIndex = 7;

{===============================================================================
     TfrmMainForm // Public methods
===============================================================================}

procedure TfrmMainForm.ClearGroups;
begin
lbEvents.Items.Clear;
meActionsLog.Lines.Clear;
lbChannels.Items.Clear;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.EnableControls(Enable: Boolean);
begin
lblEvents.Enabled := Enable;
lbEvents.Enabled := Enable;
btnEventRegister.Enabled := Enable;
btnEventUnregister.Enabled := Enable;
lblActionsLog.Enabled := Enable;
meActionsLog.Enabled := Enable;
lblChannels.Enabled := Enable;
lbChannels.Enabled := Enable;
btnChannelRegister.Enabled := Enable;
btnChannelUnregister.Enabled := Enable;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnConnecting(Sender: TObject; Socket: TCustomWinSocket);
begin
stbStatusBar.Panels.Items[0].Text := 'Connecting to ' +
  Client.ServerAddress + ' : ' + IntToStr(Client.ServerPort);
btnConnect.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnConnected(Sender: TObject; Socket: TCustomWinSocket);
begin
Connected := True;
stbStatusBar.Panels.Items[0].Text := 'Connected to server (' +
  Client.ServerAddress + ' : ' + IntToStr(Client.ServerPort) + ')';
btnConnect.Caption := 'Disconnect from server';
btnConnect.Enabled := True;
btnConnect.Tag := 1;
ClearGroups;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnConnectionComplete(Sender: TObject);
begin
EnableControls(True);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
Connected := False;
stbStatusBar.Panels.Items[0].Text := 'Not connected';
btnConnect.Caption := 'Connect to server';
btnConnect.Enabled := True;
btnConnect.Tag := 0;
ClearGroups;
EnableControls(False);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
If ErrorEvent = eeConnect then
  begin
    stbStatusBar.Panels.Items[0].Text := 'Could not connect to server ' + Client.ServerAddress +
      ' : ' + IntToStr(Client.ServerPort) + ' (0x' + IntToHex(ErrorCode,8) + ')';
    btnConnect.Caption := 'Connect to server';
    btnConnect.Enabled := True;
    btnConnect.Tag := 0;
    ClearGroups;
    EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnKnownEventsChange(Sender: TObject);
var
  i:  Integer;
begin
If Connected then
  begin
    If Client.TelemetryInfoProvider.KnownEvents.Count > lbEvents.Count then
      For i := 1 to (Client.TelemetryInfoProvider.KnownEvents.Count - lbEvents.Count) do
        lbEvents.Items.Add('_');
    If Client.TelemetryInfoProvider.KnownEvents.Count < lbEvents.Count then
      For i := 1 to (lbEvents.Count - Client.TelemetryInfoProvider.KnownEvents.Count) do
        lbEvents.Items.Delete(Pred(lbEvents.Items.Count));
    For i := 0 to (Client.TelemetryInfoProvider.KnownEvents.Count - 1) do
      lbEvents.Items.Objects[i] := Pointer(Client.TelemetryInfoProvider.KnownEvents[i].Event);
    OnRegisteredEventsChange(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnRegisteredEventsChange(Sender: TObject);
var
  i,j:  Integer;
  Listed: Boolean;
begin
For i := (lbEvents.Items.Count - 1) downto (Client.TelemetryInfoProvider.KnownEvents.Count) do
  If not Client.EventRegistered(scs_event_t(lbEvents.Items.Objects[i])) then
    lbEvents.Items.Delete(i);
For i := 0 to (Client.RegisteredEvents.Count - 1) do
  If Client.TelemetryInfoProvider.KnownEvents.IndexOf(Client.RegisteredEvents[i].Event) < 0 then
    begin
      Listed := False;
      For j := 0 to (lbEvents.Items.Count - 1) do
        If scs_event_t(lbEvents.Items.Objects[j]) = Client.RegisteredEvents[i].Event then
          begin
            Listed := True;
            Break;
          end;
      If not Listed then
        lbEvents.Items.AddObject('%',Pointer(Client.RegisteredEvents[i].Event));
    end;
lbEvents.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnKnownChannelsChange(Sender: TObject);
var
  i:  Integer;
begin
If Connected then
  begin
    If Client.TelemetryInfoProvider.KnownChannels.Count > lbChannels.Count then
      For i := 1 to (Client.TelemetryInfoProvider.KnownChannels.Count - lbChannels.Count) do
        lbChannels.Items.Add('_');
    If Client.TelemetryInfoProvider.KnownChannels.Count < lbChannels.Count then
      For i := 1 to (lbChannels.Count - Client.TelemetryInfoProvider.KnownChannels.Count) do
        lbChannels.Items.Delete(Pred(lbChannels.Items.Count));
    For i := 0 to (Client.TelemetryInfoProvider.KnownChannels.Count - 1) do
      lbChannels.Items.Objects[i] := Pointer(Client.TelemetryInfoProvider.KnownChannels[i].ID);
    OnRegisteredChannelsChange(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnRegisteredChannelsChange(Sender: TObject);
var
  i,j:  Integer;
  Listed: Boolean;
begin
For i := (lbChannels.Items.Count - 1) downto (Client.TelemetryInfoProvider.KnownChannels.Count) do
  If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[i])) < 0 then
    lbChannels.Items.Delete(i);
For i := 0 to (Client.RegisteredChannels.Count - 1) do
  If Client.TelemetryInfoProvider.KnownChannels.IndexOf(Client.RegisteredChannels[i].ID) < 0 then
    begin
      Listed := False;
      For j := 0 to (lbChannels.Items.Count - 1) do
        If TChannelID(lbChannels.Items.Objects[j]) = Client.RegisteredChannels[i].ID then
          begin
            Listed := True;
            Break;
          end;
      If not Listed then
        lbChannels.Items.AddObject('%',Pointer(Client.RegisteredChannels[i].ID));
    end;
lbChannels.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnEventRegister(Sender: TObject; Event: scs_event_t; Result: scs_result_t);
begin
If Result = SCS_RESULT_ok then
  meActionsLog.Lines.Add('>> Event registration successful: (0x' +
    IntToHex(Event,8)+ ') ' + Client.TelemetryInfoProvider.EventGetName(Event))
else
  meActionsLog.Lines.Add('>> Event registration unsuccessful (' + IntToStr(Result) +
    '): (0x' + IntToHex(Event,8)+ ') ' + Client.TelemetryInfoProvider.EventGetName(Event));
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnEventUnregister(Sender: TObject; Event: scs_event_t; Result: scs_result_t);
begin
If Result = SCS_RESULT_ok then
  meActionsLog.Lines.Add('>> Event unregistration successful: (0x' +
    IntToHex(Event,8)+ ') ' + Client.TelemetryInfoProvider.EventGetName(Event))
else
  meActionsLog.Lines.Add('>> Event unregistration unsuccessful (' + IntToStr(Result) +
    '): (0x' + IntToHex(Event,8)+ ') ' + Client.TelemetryInfoProvider.EventGetName(Event));
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnChannelRegister(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; Result: scs_result_t);
var
  TempStr:  String;
begin
If Index = SCS_U32_NIL then TempStr := ' '
  else TempStr := '[' + IntToStr(Index) + '] ';
If Result = SCS_RESULT_ok then
  meActionsLog.Lines.Add('>> Channel registration successful: (0x' + IntToHex(Client.TelemetryInfoProvider.KnownChannels.ChannelNameToID(Name),8) +
    ') ' + Name + TempStr + SCSValueTypeToStr(ValueType) + ' (' + IntToHex(Flags,8) + ')')
else
  meActionsLog.Lines.Add('>> Channel registration unsuccessful: (0x' + IntToHex(Client.TelemetryInfoProvider.KnownChannels.ChannelNameToID(Name),8) +
    ') ' + Name + TempStr + SCSValueTypeToStr(ValueType) + ' (' + IntToHex(Flags,8) + ')');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnChannelUnregister(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Result: scs_result_t);
var
  TempStr:  String;
begin
If Index = SCS_U32_NIL then TempStr := ' '
  else TempStr := '[' + IntToStr(Index) + '] ';
If Result = SCS_RESULT_ok then
  meActionsLog.Lines.Add('>> Channel unregistration successful: (0x' + IntToHex(Client.TelemetryInfoProvider.KnownChannels.ChannelNameToID(Name),8) +
    ') ' + Name + TempStr + SCSValueTypeToStr(ValueType))
else
  meActionsLog.Lines.Add('>> Channel unregistration unsuccessful: (0x' + IntToHex(Client.TelemetryInfoProvider.KnownChannels.ChannelNameToID(Name),8) +
    ') ' + Name + TempStr + SCSValueTypeToStr(ValueType));
end;


{===============================================================================
     TfrmMainForm // Events handlers
===============================================================================}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
meActionsLog.DoubleBuffered := True;
stbStatusBar.DoubleBuffered := True;
// Initialize client.
Client := TTelemetryNetClientRecipient.Create;
Client.Password := 'password';
// Initialize connection parameters.
leAddress.Text := Client.ServerAddress;
sePort.Value := Client.ServerPort;
// Clear & Enable/Disable controls.
ClearGroups;
EnableControls(False);
// Assign client events handlers.
Client.OnConnecting := OnConnecting;
Client.OnConnect := OnConnected;
Client.OnConnectionComplete := OnConnectionComplete;
Client.OnDisconnect := OnDisconnect;
Client.OnError := OnSocketError;
Client.TelemetryInfoProvider.KnownEvents.OnChange := OnKnownEventsChange;
Client.RegisteredEvents.OnChange := OnRegisteredEventsChange;
Client.TelemetryInfoProvider.KnownChannels.OnChange := OnKnownChannelsChange;
Client.RegisteredChannels.OnChange := OnRegisteredChannelsChange;
Client.OnEventUnregister := OnEventUnregister;
Client.OnEventRegister := OnEventRegister;
Client.OnChannelUnregister := OnChannelUnregister;
Client.OnChannelRegister := OnChannelRegister;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Client.Disconnect;
// Deassigning event handlers.
Client.OnConnecting := nil;
Client.OnConnect := nil;
Client.OnConnectionComplete := nil;
Client.OnDisconnect := nil;
Client.OnError := nil;
Client.RegisteredEvents.OnChange := nil;
Client.RegisteredChannels.OnChange := nil;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
Client.Free;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnConnectClick(Sender: TObject);
begin
case btnConnect.Tag of
  0:  Client.Connect(leAddress.Text,sePort.Value);
  1:  Client.Disconnect;
end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.lbEventsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Text:             String;
  BgColor:          TColor;
  BgColorSelected:  TColor;
begin
with lbEvents.Canvas do
  begin 
    // Get colors
    If Index < Client.TelemetryInfoProvider.KnownEvents.Count then
      begin
        If Client.EventRegistered(Client.TelemetryInfoProvider.KnownEvents[Index].Event) then
          begin
            BgColor := BgColorRegistered;
            BgColorSelected := BgColorRegisteredSelected;
          end
        else
          begin
            BgColor := BgColorNormal;
            BgColorSelected := BgColorNormalSelected;
          end;
      end
    else
      begin
        BgColor := BgColorUnknown;
        BgColorSelected := BgColorUnknownSelected;
      end;

    // Draw background
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    Brush.Color := BgColor;
    FillRect(Rect);

    // Get item text
    If Index < Client.TelemetryInfoProvider.KnownEvents.Count then
      Text := '(0x' + IntToHex(Client.TelemetryInfoProvider.KnownEvents[Index].Event,8) + ') ' +
              Client.TelemetryInfoProvider.EventGetName(Client.TelemetryInfoProvider.KnownEvents[Index].Event)
    else
      Text := '(0x' + IntToHex(scs_event_t(lbEvents.Items.Objects[Index]),8) + ') ' +
              Client.TelemetryInfoProvider.EventGetName(scs_event_t(lbEvents.Items.Objects[Index]));

    // Draw text
    Font := lbEvents.Font;
    Pen.Style := psClear;
    Pen.Color := Font.Color;
    Brush.Style := bsClear;
    TextOut(Rect.Left + 5,Rect.Top  + (Rect.Bottom - Rect.Top - TextHeight(Text)) div 2,Text);
    
    // Draw selection markers
    If odSelected	in State then
      begin
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := BgColorSelected;
        Rectangle(Rect.Left,Rect.Top,Rect.Left + 5,Rect.Bottom);
        Rectangle(Rect.Right - 5,Rect.Top,Rect.Right,Rect.Bottom);
      end;
    // Draw focus outline
    If odFocused in State then
      begin
        Pen.Style := psDot;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(Rect);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.lbEventsDblClick(Sender: TObject);
begin
If lbEvents.ItemIndex >= 0 then
  begin
    If Client.EventRegistered(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex])) then
      btnEventUnregister.OnClick(nil)
    else
      btnEventRegister.OnClick(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnEventRegisterClick(Sender: TObject);
begin
If lbEvents.ItemIndex >= 0 then
  begin
    If not Client.EventRegistered(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex])) then
      begin
        Client.EventRegister(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex]));
        meActionsLog.Lines.Add('<< Registering event: (0x' +
          IntToHex(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex]),8)+ ') ' +
          Client.TelemetryInfoProvider.EventGetName(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex])));
      end
    else
      Showmessage('Select event is already registered.');
  end
else
  Showmessage('Select event you want to register from list.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnEventUnregisterClick(Sender: TObject);
begin
If lbEvents.ItemIndex >= 0 then
  begin
    If Client.EventRegistered(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex])) then
      begin
        Client.EventUnregister(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex]));
        meActionsLog.Lines.Add('<< Unregistering event: (0x' +
          IntToHex(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex]),8)+ ') ' +
          Client.TelemetryInfoProvider.EventGetName(scs_event_t(lbEvents.Items.Objects[lbEvents.ItemIndex])));
      end
    else
      Showmessage('Select event is not registered.');
  end
else
  Showmessage('Select event you want to unregister from list.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.lbChannelsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Text:             String;
  Indices:          String;
  ValueType:        String;
  BgColor:          TColor;
  BgColorSelected:  TColor;
  ChIndex:          Integer;

  Function GetIndicesOfRegisteredChannel(ChannelID: TChannelID): String;
  var
    i:  Integer;
  begin
    Result := '[';
    For i := 0 to (Client.RegisteredChannels.Count - 1) do
      If Client.RegisteredChannels[i].ID = ChannelID then
        If Client.RegisteredChannels[i].Index <> SCS_U32_NIL then
          Result := Result + IntToStr(Client.RegisteredChannels[i].Index) + ',';
    If AnsiEndsText(',',Result) then Result := Copy(Result,1,Length(Result) - 1);
    Result := Result + ']';
    If Result = '[]' then Result := '';
  end;

begin
with lbChannels.Canvas do
  begin
    // Get colors
    If Index < Client.TelemetryInfoProvider.KnownChannels.Count then
      begin
        If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[Index])) >= 0 then
          begin
            BgColor := BgColorRegistered;
            BgColorSelected := BgColorRegisteredSelected;
          end
        else
          begin
            BgColor := BgColorNormal;
            BgColorSelected := BgColorNormalSelected;
          end;
      end
    else
      begin
        BgColor := BgColorUnknown;
        BgColorSelected := BgColorUnknownSelected;
      end;

    // Draw background
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    Brush.Color := BgColor;
    FillRect(Rect);

    // Get texts
    If Index < Client.TelemetryInfoProvider.KnownChannels.Count then
      begin
        Text := Client.TelemetryInfoProvider.KnownChannels[Index].Name;
        ValueType := SCSValueTypeToStr(Client.TelemetryInfoProvider.KnownChannels[Index].PrimaryType);
        If Client.TelemetryInfoProvider.KnownChannels[Index].Indexed then
          begin
            If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[Index])) >= 0 then
              Indices := GetIndicesOfRegisteredChannel(TChannelID(lbChannels.Items.Objects[Index]))
            else
             Indices := '[]';
          end
        else Indices := '';
      end
    else
      begin
        Text := IntToHex(TChannelID(lbChannels.Items.Objects[Index]),8);
        ChIndex := Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[Index]));
        If ChIndex >= 0 then
          ValueType := SCSValueTypeToStr(Client.RegisteredChannels[ChIndex].ValueType)
        else
          ValueType := '';
        Indices := GetIndicesOfRegisteredChannel(TChannelID(lbChannels.Items.Objects[Index]));
      end;

    // Draw texts
    Font := lbChannels.Font;
    Pen.Style := psClear;
    Pen.Color := Font.Color;
    Brush.Style := bsClear;
    TextOut(Rect.Left + 5,Rect.Top  + (Rect.Bottom - Rect.Top - TextHeight(Text)) div 2,Text);
    TextOut(Rect.Right - 100,Rect.Top  + (Rect.Bottom - Rect.Top - TextHeight(Text)) div 2,ValueType);
    TextOut(Rect.Right - 300,Rect.Top  + (Rect.Bottom - Rect.Top - TextHeight(Text)) div 2,Indices);

    // Draw selection markers
    If odSelected	in State then
      begin
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := BgColorSelected;
        Rectangle(Rect.Left,Rect.Top,Rect.Left + 5,Rect.Bottom);
        Rectangle(Rect.Right - 5,Rect.Top,Rect.Right,Rect.Bottom);
      end;

    // Draw focus outline
    If odFocused in State then
      begin
        Pen.Style := psDot;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(Rect);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnChannelRegisterClick(Sender: TObject);
var
  Index:  Integer;
  i:      Integer;
begin
If lbChannels.ItemIndex >= 0 then
  begin
    If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex])) < 0 then
      begin
        Index := Client.TelemetryInfoProvider.KnownChannels.IndexOf(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex]));
        If Index >= 0 then
          If Client.TelemetryInfoProvider.KnownChannels[Index].Indexed then
            begin
              For i := 0 to cMaxChannelIndex do
                begin
                  Client.ChannelRegister(Client.TelemetryInfoProvider.KnownChannels[Index].Name,i,
                                         Client.TelemetryInfoProvider.KnownChannels[Index].PrimaryType);
                  meActionsLog.Lines.Add('<< Registering channel: (0x' +
                    IntToHex(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex]),8)+ ') ' +
                    Client.TelemetryInfoProvider.KnownChannels[Index].Name + '[' + IntToStr(i) + '] ' +
                    SCSValueTypeToStr(Client.TelemetryInfoProvider.KnownChannels[Index].PrimaryType));
                end;
            end
          else
            begin
              Client.ChannelRegister(Client.TelemetryInfoProvider.KnownChannels[Index].Name,SCS_U32_NIL,
                                     Client.TelemetryInfoProvider.KnownChannels[Index].PrimaryType);
              meActionsLog.Lines.Add('<< Registering channel: (0x' +
                IntToHex(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex]),8)+ ') ' +
                Client.TelemetryInfoProvider.KnownChannels[Index].Name + ' ' +
                SCSValueTypeToStr(Client.TelemetryInfoProvider.KnownChannels[Index].PrimaryType));
            end;
      end
    else
      Showmessage('Select channel is already registered.');
  end
else
  Showmessage('Select channel you want to register from list.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnChannelUnregisterClick(Sender: TObject);
var
  i:        Integer;
  TempStr:  String;
begin
If lbChannels.ItemIndex >= 0 then
  begin
    If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex])) >= 0 then
      begin
        For i := (Client.RegisteredChannels.Count - 1) downto 0 do
          If Client.RegisteredChannels[i].ID = TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex]) then
            begin
              Client.ChannelUnregister(Client.RegisteredChannels[i].Name,
                                       Client.RegisteredChannels[i].Index,
                                       Client.RegisteredChannels[i].ValueType);
              If Client.RegisteredChannels[i].Index <> SCS_U32_NIL then
                TempStr := '[' + IntToStr(Client.RegisteredChannels[i].Index) + '] '
              else
                TempStr := ' ';
              meActionsLog.Lines.Add('<< Unregistering channel: (0x' +
                IntToHex(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex]),8)+ ') ' +
                Client.RegisteredChannels[i].Name + TempStr +
                SCSValueTypeToStr(Client.RegisteredChannels[i].ValueType));
            end;
      end
    else
      Showmessage('Select channel is not registered.');
  end
else
  Showmessage('Select channel you want to unregister from list.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.lbChannelsDblClick(Sender: TObject);
begin
If lbChannels.ItemIndex >= 0 then
  begin
    If Client.RegisteredChannels.IndexOf(TChannelID(lbChannels.Items.Objects[lbChannels.ItemIndex])) >= 0 then
      btnChannelUnregister.OnClick(nil)
    else
      btnChannelRegister.OnClick(nil);
  end;
end;

end.
