unit MainForm;

interface

{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, Buttons, ScktComp,
  TelemetryLists,
  TelemetryIDs,
  TelemetryNetPackets,
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
    leAddress:                TLabeledEdit;
    lblPort:                  TLabel;
    sePort:                   TSpinEdit;
    btnConnect:               TButton;
    bvlConnectSeparator:      TBevel;

    grbTelemetryInformation:  TGroupBox;
    lblTelemetryC:            TLabel;
    lblServerC:               TLabel;
    lblTelemetry:             TLabel;
    lblGame:                  TLabel;
    lblServer:                TLabel;

    grbGameLog:               TGroupBox;
    lblLogType:               TLabel;
    cmbLogType:               TComboBox;
    leLogText:                TLabeledEdit;
    btnSendLogText:           TBitBtn;

    grbRegisteredEvents:      TGroupBox;
    lbRegisteredEvents:       TListBox;
    btnRegisterEvent:         TButton;
    btnUnregisterEvent:       TButton;
    btnRegisterAllEvents:     TButton;
    btnUnregisterAllEvents:   TButton;

    grbRegisteredChannels:    TGroupBox;
    lbRegisteredChannels:     TListBox;
    btnRegisterChannel:       TButton;
    btnUnregisterChannel:     TButton;
    btnRegisterAllChannels:   TButton;
    btnUnregisterAllChannels: TButton;
    btnOpenChannelsWnd:       TButton;
    tmrRegChannelsUpdate:     TTimer;

    grbPacketsLog:            TGroupBox;
    mePacketsLog:             TMemo;
    cbShowChannelsPackets:    TCheckBox;

    grbEventsLog:             TGroupBox;
    meEventsLog:              TMemo;
    cbLogEventsData:          TCheckBox;

    stbStatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendLogTextClick(Sender: TObject);
    procedure btnRegisterEventClick(Sender: TObject);
    procedure btnUnregisterEventClick(Sender: TObject);
    procedure btnRegisterAllEventsClick(Sender: TObject);
    procedure btnUnregisterAllEventsClick(Sender: TObject);
    procedure btnRegisterChannelClick(Sender: TObject);
    procedure btnUnregisterChannelClick(Sender: TObject);
    procedure btnRegisterAllChannelsClick(Sender: TObject);
    procedure btnUnregisterAllChannelsClick(Sender: TObject);
    procedure btnOpenChannelsWndClick(Sender: TObject);
    procedure tmrRegChannelsUpdateTimer(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ClearGroups;
    procedure EnableGroups(Enable: Boolean);
    procedure OnConnecting(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnConnected(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnConnectionComplete(Sender: TObject);
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure OnPacketReceived(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
    procedure OnPacketSend(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
    procedure OnEvent(Sender: TObject; Event: scs_event_t; Data: Pointer);
    procedure OnChannel(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
    procedure OnRegisteredEventsChange(Sender: TObject);
    procedure OnRegisteredChannelsChange(Sender: TObject);
  end;

var
  frmMainForm: TfrmMainForm;
  // Main client object.
  Client: TTelemetryNetClientRecipient = nil;

implementation

uses
  RegisterEventForm, RegisterChannelForm, ChannelsForm,
  TelemetryRecipient,
  TelemetryRecipientAux,
  TelemetryNetPacketsResolving;

{$R *.dfm}

{===============================================================================
     TfrmMainForm // Public methods
===============================================================================}

procedure TfrmMainForm.ClearGroups;
begin
// Information group.
lblTelemetry.Caption := '';
lblServer.Caption := '';
lblGame.Caption := '';
// Game log group.
cmbLogType.ItemIndex := 0;
leLogText.Text := '';
// Registered events group.
grbRegisteredEvents.Caption := 'Registered events [0]';
lbRegisteredEvents.Items.Clear;
// Registered channels group.
grbRegisteredChannels.Caption := 'Registered channels [0]';
lbRegisteredChannels.Items.Clear;
// Packets log group.
mePacketsLog.Lines.Clear;
cbShowChannelsPackets.Checked := False;
// Events log group.
meEventsLog.Lines.Clear;
cbLogEventsData.Checked := False;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.EnableGroups(Enable: Boolean);
var
  i,j:  Integer;
begin
For i := 0 to (ComponentCount - 1) do
  If Components[i] is TGroupBox then
    begin
      For j := 0 to (TGroupBox(Components[i]).ControlCount - 1) do
        TGroupBox(Components[i]).Controls[j].Enabled := Enable;
      TGroupBox(Components[i]).Enabled := Enable;
    end;
tmrRegChannelsUpdate.Enabled := Enable;
If Assigned(tmrRegChannelsUpdate.OnTimer) then tmrRegChannelsUpdate.OnTimer(nil);
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
lblServer.Caption := IntToStr(HiWord(Client.ServerVersion)) + '.' +
                     IntToStr(LoWord(Client.ServerVersion));
lblTelemetry.Caption := SCSGetVersionAsString(Client.TelemetryVersion);
lblGame.Caption := Client.GameName + ' (' + Client.GameID + ' ' +
                   SCSGetVersionAsString(Client.GameVersion) + ')';
EnableGroups(True);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
stbStatusBar.Panels.Items[0].Text := 'Not connected';
btnConnect.Caption := 'Connect to server';
btnConnect.Enabled := True;
btnConnect.Tag := 0;
ClearGroups;
EnableGroups(False);
frmChannelsForm.BufferedChannelsValues.Clear;
frmChannelsForm.Close;
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
    EnableGroups(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnPacketReceived(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
var
  currPtr:  pointer;
  tempstr:  String;
begin
If not ((GetPacketHeader(Packet).PacketID = TN_PACKET_CHANNEL_CHANNEL) or
        (GetPacketHeader(Packet).PacketID = TN_PACKET_CHANNEL_CHANNEL_BUFFERED)) or
        cbShowChannelsPackets.Checked then
  mePacketsLog.Lines.Add('-> id: 0x' + IntToHex(GetPacketHeader(Packet).PacketID,8) +
                         ' (' + IntToStr(Packet.Size) + 'B)');
If GetPacketHeader(Packet).PacketID = TN_PACKET_ERROR then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    TempStr := IntToHex(Ptr_ReadoutInteger(CurrPtr),8);
    TempStr := TempStr + ' ' + DateTimeToStr(Ptr_ReadoutDouble(CurrPtr));
    TempStr := TempStr + ' ' + IntToStr(Ptr_ReadoutInteger(CurrPtr));
    TempStr := TempStr + ' ' + IntToStr(Ptr_ReadoutInteger(CurrPtr));
    meEventsLog.Lines.Add(TempStr);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnPacketSend(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
begin
mePacketsLog.Lines.Add('<- id: 0x' + IntToHex(GetPacketHeader(Packet).PacketID,8) +
                       ' (' + IntToStr(Packet.Size) + 'B)');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnEvent(Sender: TObject; Event: scs_event_t; Data: Pointer);
var
  TempStr:  String;
begin
If cbLogEventsData.Checked then
  TempStr := Client.EventGetDataAsString(Event,Data);
If TempStr <> '' then TempStr := sLineBreak + TempStr;
TempStr := '> Event 0x' + IntToHex(Event,8) + ' - ' +
            Client.TelemetryInfoProvider.EventGetName(Event) + TempStr;
meEventsLog.Lines.Add(TempStr);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnChannel(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
frmChannelsForm.BufferedChannelsValues.ChangeValue(ID,Index,Value);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnRegisteredEventsChange(Sender: TObject);
var
  i:  Integer;
begin
grbRegisteredEvents.Caption := 'Registered events [' + IntToStr(Client.RegisteredEvents.Count) + ']';
If lbRegisteredEvents.Items.Count > Client.RegisteredEvents.Count then
  For i := (lbRegisteredEvents.Items.Count - 1) downto Client.RegisteredEvents.Count do
    lbRegisteredEvents.Items.Delete(i);
If lbRegisteredEvents.Items.Count < Client.RegisteredEvents.Count then
  For i := 1 to (Client.RegisteredEvents.Count - lbRegisteredEvents.Items.Count) do
    lbRegisteredEvents.Items.Add('');
For i := 0 to (lbRegisteredEvents.Items.Count - 1) do
  lbRegisteredEvents.Items[i] := '0x' + IntToHex(Client.RegisteredEvents[i].Event,8) +
    ' - ' + Client.TelemetryInfoProvider.EventGetName(Client.RegisteredEvents[i].Event);
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.OnRegisteredChannelsChange(Sender: TObject);
begin
tmrRegChannelsUpdate.Tag := 1;
frmChannelsForm.BufferedChannelsValues.Synchronize(Client);
end;

{===============================================================================
     TfrmMainForm // Events handlers
===============================================================================}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
mePacketsLog.DoubleBuffered := True;
meEventsLog.DoubleBuffered := True;
stbStatusBar.DoubleBuffered := True;
// Initialize client.
Client := TTelemetryNetClientRecipient.Create;
Client.Password := 'password';
// Initialize connection parameters.
leAddress.Text := Client.ServerAddress;
sePort.Value := Client.ServerPort;
// Clear & Enable/Disable controls.
ClearGroups;
EnableGroups(False);
// Assign client events handlers.
Client.OnConnecting := OnConnecting;
Client.OnConnect := OnConnected;
Client.OnConnectionComplete := OnConnectionComplete;
Client.OnDisconnect := OnDisconnect;
Client.OnError := OnSocketError;
Client.OnPacketReceived := OnPacketReceived;
Client.OnPacketSend := OnPacketSend;
Client.OnEvent := OnEvent;
Client.OnChannel := OnChannel;
Client.RegisteredEvents.OnChange := OnRegisteredEventsChange;
Client.RegisteredChannels.OnChange := OnRegisteredChannelsChange;
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
Client.OnPacketReceived := nil;
Client.OnPacketSend := nil;
Client.OnEvent := nil;
Client.OnChannel := nil;
Client.RegisteredEvents.OnChange := nil;
Client.RegisteredChannels.OnChange := nil;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
Client.Free;
end;

{===============================================================================
     Connection group // Events handlers and Client events handlers
===============================================================================}

procedure TfrmMainForm.btnConnectClick(Sender: TObject);
begin
case btnConnect.Tag of
  0:  Client.Connect(leAddress.Text,sePort.Value);
  1:  Client.Disconnect;
end;
end;

{===============================================================================
     Game log group // Events handlers
===============================================================================}

procedure TfrmMainForm.btnSendLogTextClick(Sender: TObject);
begin
Client.Log(scs_log_type_t(cmbLogType.ItemIndex),leLogText.Text);
end;

{===============================================================================
     Registered events group // Events handlers
===============================================================================}

procedure TfrmMainForm.btnRegisterEventClick(Sender: TObject);
begin
frmRegisterEvent.ShowModal;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnUnregisterEventClick(Sender: TObject);
begin
If lbRegisteredEvents.ItemIndex >= 0 then
  begin
    If Client.RegisteredEvents.Count > lbRegisteredEvents.ItemIndex then
      Client.EventUnregister(Client.RegisteredEvents[lbRegisteredEvents.ItemIndex].Event);
  end
else
  If lbRegisteredEvents.Items.Count > 0 then
    ShowMessage('Select event you want to unregister from list of registered events.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnRegisterAllEventsClick(Sender: TObject);
begin
Client.EventRegisterAll;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnUnregisterAllEventsClick(Sender: TObject);
begin
Client.EventUnregisterAll;
end;

{===============================================================================
     Registered channels group // Events handlers
===============================================================================}

procedure TfrmMainForm.btnRegisterChannelClick(Sender: TObject);
begin
frmRegisterChannel.ShowModal;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnUnregisterChannelClick(Sender: TObject);
var
  RegChannel: TChannelInfo;
begin
If lbRegisteredChannels.ItemIndex >= 0 then
  begin
    If Client.RegisteredChannels.Count > lbRegisteredChannels.ItemIndex then
      begin
        RegChannel := Client.RegisteredChannels[lbRegisteredChannels.ItemIndex];
        Client.ChannelUnregister(RegChannel.Name,RegChannel.Index,RegChannel.ValueType);
      end;
  end
else
  If lbRegisteredChannels.Items.Count > 0 then
    ShowMessage('Select channel you want to unregister from list of registered channels.');
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnRegisterAllChannelsClick(Sender: TObject);
begin
Client.ChannelRegisterAll;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnUnregisterAllChannelsClick(Sender: TObject);
begin
Client.ChannelUnregisterAll;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.btnOpenChannelsWndClick(Sender: TObject);
begin
frmChannelsForm.Show;
end;

//------------------------------------------------------------------------------

procedure TfrmMainForm.tmrRegChannelsUpdateTimer(Sender: TObject);
var
  i:          Integer;
  TempStr:    String;
  RegChannel: TChannelInfo;
begin
If tmrRegChannelsUpdate.Tag <> 0 then
  begin
    tmrRegChannelsUpdate.Tag := 0;
    grbRegisteredChannels.Caption := 'Registered channels [' + IntToStr(Client.RegisteredChannels.Count) + ']';
    If lbRegisteredChannels.Items.Count > Client.RegisteredChannels.Count then
      For i := (lbRegisteredChannels.Items.Count - 1) downto Client.RegisteredChannels.Count do
        lbRegisteredChannels.Items.Delete(i);
    If lbRegisteredChannels.Items.Count < Client.RegisteredChannels.Count then
      For i := 1 to (Client.RegisteredChannels.Count - lbRegisteredChannels.Items.Count) do
        lbRegisteredChannels.Items.Add('');
    For i := 0 to (Client.RegisteredChannels.Count - 1) do
      begin
        RegChannel := Client.RegisteredChannels[i];
        If RegChannel.Index <> SCS_U32_NIL then
          TempStr := RegChannel.Name + '[' + IntToStr(RegChannel.Index) + ']'
        else
          TempStr := RegChannel.Name;
        lbRegisteredChannels.Items[i] := '(' + IntToHex(RegChannel.Flags,8) + ')' + TempStr +
                                         ' (' + SCSValueTypeToStr(RegChannel.ValueType) + ')';
      end;
  end;
end;

end.
