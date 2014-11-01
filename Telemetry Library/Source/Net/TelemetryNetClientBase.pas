unit TelemetryNetClientBase;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  Classes, ScktComp,
  TelemetryVersionObjects,
  TelemetryNetCommon,
  TelemetryNetLists,
  TelemetryNetPackets,
  TelemetryNetSockets,
{$IFDEF Documentation}
  TelemetryNetPacketsBuilding,
  TelemetryNetPacketsResolving,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk;
{$ENDIF}

type
  TTelemetryNetClientBase = class(TTelemetryVersionObject)
  private
    fClientSocket:          TTelemetryNetClientSocket;

    fServerVersion:         LongWord;
    fTelemetryVersion:      scs_u32_t;    { SCS_U32_NIL             }
    fGameName:              String;       { '' (empty string)       }
    fGameID:                String;       { '' (empty string)       }
    fGameVersion:           scs_u32_t;    { SCS_U32_NIL             }

    fPassword:              String;       { '' (empty string)       }

    fCompletionCounter:     Integer;
    fConnectionComplete:    Boolean;

    // Forwarded ServerSocket events.
    fOnConnect:             TSocketNotifyEvent;
    fOnConnectionComplete:  TNotifyEvent;
    fOnDisconnect:          TSocketNotifyEvent;
    fOnError:               TSocketErrorEvent;
    fOnRead:                TSocketNotifyEvent;
    fOnWrite:               TSocketNotifyEvent;

    // Forwarded client socket events.
    fOnPacketDrop:          TPacketNotifyEvent;
    fOnOverflowClear:       TSocketNotifyEvent;
    fOnPacketReceived:      TPacketNotifyEvent;

    // Events called when data are sent.
    fOnPacketSending:       TPacketNotifyEvent;
    fOnPacketSend:          TPacketNotifyEvent;

    // Client object events.
    fOnServerSettings:      TNotifyEvent;
    fOnDestroy:             TNotifyEvent;

    // Remote settings.
    rsSrvSendEvents:            Boolean;
    rsSrvSendChannels:          Boolean;
    rsSrvSendConfigs:           Boolean;
    rsSrvActiveMode:            Boolean;
    rsSrvBufferChannels:        Boolean;
    rsSrvSendFrameEvents:       Boolean;
    rsSrvSaveSettingsToFile:    Boolean;
    rsRecKeepUtilityEvents:     Boolean;
    rsRecStoreConfigurations:   Boolean;
    rsRecManageIndexedChannels: Boolean;
    rsRecStoreChannelValues:    Boolean;
    rsLstListMode:              TAddressListMode;
    rsLstListFileName:          String;

    // Getters and setters.
    Function GetConnectedState: Boolean;
    Function GetServerAddress: String;
    Function GetServerPort: Word;

    procedure SetPassword(Value: String);

    Function GetOnLookup: TSocketNotifyEvent;
    procedure SetOnLookup(Value: TSocketNotifyEvent);
    Function GetOnConnecting: TSocketNotifyEvent;
    procedure SetOnConnecting(Value: TSocketNotifyEvent);

    procedure SetSrvSendEvents(Value: Boolean);
    procedure SetSrvSendChannels(Value: Boolean);
    procedure SetSrvSendConfigs(Value: Boolean);
    procedure SetSrvActiveMode(Value: Boolean);
    procedure SetSrvBufferChannels(Value: Boolean);
    procedure SetSrvSendFrameEvents(Value: Boolean);
    procedure SetSrvSaveSettingsToFile(Value: Boolean);
    procedure SetRecKeepUtilityEvents(Value: Boolean);
    procedure SetRecStoreConfigurations(Value: Boolean);
    procedure SetRecManageIndexedChannels(Value: Boolean);
    procedure SetLstListMode(Value: TAddressListMode);
    procedure SetLstListFileName(Value: String);
  protected
    fCompletionCount: Integer;
    procedure UtilityObjectsInitialize; virtual; abstract;
    procedure UtilityObjectsFinalize; virtual; abstract;
    procedure UtilityObjectsClear; virtual; abstract;
    procedure UtilityObjectsFill; virtual; abstract;
    // Client socket events handlers.
    procedure OnConnectHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnDisconnectHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnErrorHandler(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual;
    procedure OnReadHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnWriteHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    // Incoming and outgoing buffers events handlers.
    procedure OnOverflowClearHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnPacketDropHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer); virtual;
    procedure OnPacketHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer); virtual;

    // Packet groups processing
    Function ProcessPacketGroup_COMMON(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_ADMIN(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_SUPERADMIN(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_EVENT_KNOWN(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_CHANNEL_KNOWN(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_CONFIG_KNOWN(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_EVENT(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_CHANNEL(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_CONFIG(Packet: TPacketBuffer): Boolean; virtual;
    Function ProcessPacketGroup_LOG(Packet: TPacketBuffer): Boolean; virtual;

    // Packet processing
    {$DEFINE Declaration_part}
    {$INCLUDE '.\INC\TTelemetryNetClientBase.ProcessPacket_TN_PACKET.pas'}
    {$UNDEF Declaration_part}
    procedure IncreaseCompletionCounter; virtual;
  public
    class Function HighestSupportedServerVersion: LongWord; virtual;
    class Function SupportsServerVersion(aServerVersion: LongWord): Boolean; virtual;

    constructor Create;
    destructor Destroy; override;

    Function Connect(ServerAddress: String = def_ServerAddress; ServerPort: Word = def_ServerPort): Boolean; virtual;
    Function Disconnect(Reason: TDisconnectReason = drClientDisconnected): Boolean; virtual;

    procedure Send(Packet: TPacketBuffer; FreePacket: Boolean); virtual;
    procedure GetServerSettings(ParameterID: TParameterID = pidAll); virtual;
  published
    property Connected: Boolean read GetConnectedState;
    property ServerAddress: String read GetServerAddress;
    property ServerPort: Word read GetServerPort;

    property ServerVersion: LongWord read fServerVersion;
    property TelemetryVersion: scs_u32_t read fTelemetryVersion;
    property GameName: String read fGameName;
    property GameID: String read fGameID;
    property GameVersion: scs_u32_t read fGameVersion;

    property Password: String read fPassword write SetPassword;
    property ConnectionComplete: Boolean read fConnectionComplete;

    property OnLookup: TSocketNotifyEvent read GetOnLookup write SetOnLookup;
    property OnConnecting: TSocketNotifyEvent read GetOnConnecting write SetOnConnecting;
    property OnConnect: TSocketNotifyEvent read fOnConnect write fOnConnect;
    property OnConnectionComplete: TNotifyEvent read fOnConnectionComplete write fOnConnectionComplete;
    property OnDisconnect: TSocketNotifyEvent read fOnDisconnect write fOnDisconnect;
    property OnError: TSocketErrorEvent read fOnError write fOnError;
    property OnRead: TSocketNotifyEvent read fOnRead write fOnRead;
    property OnWrite: TSocketNotifyEvent read fOnWrite write fOnWrite;

    property OnPacketDrop: TPacketNotifyEvent read fOnPacketDrop write fOnPacketDrop;
    property OnOverflowClear: TSocketNotifyEvent read fOnOverflowClear write fOnOverflowClear;
    property OnPacketReceived: TPacketNotifyEvent read fOnPacketReceived write fOnPacketReceived;

    property OnPacketSending: TPacketNotifyEvent read fOnPacketSending write fOnPacketSending;
    property OnPacketSend: TPacketNotifyEvent read fOnPacketSend write fOnPacketSend;

    property OnServerSettings: TNotifyEvent read fOnServerSettings write fOnServerSettings;

    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;

    property RemoteSettings_SrvSendEvents: Boolean read rsSrvSendEvents write SetSrvSendEvents;
    property RemoteSettings_SrvSendChannels: Boolean read rsSrvSendChannels write SetSrvSendChannels;
    property RemoteSettings_SrvSendConfigs: Boolean read rsSrvSendConfigs write SetSrvSendConfigs;
    property RemoteSettings_SrvActiveMode: Boolean read rsSrvActiveMode write SetSrvActiveMode;
    property RemoteSettings_SrvBufferChannels: Boolean read rsSrvBufferChannels write SetSrvBufferChannels;
    property RemoteSettings_SrvSendFrameEvents: Boolean read rsSrvSendFrameEvents write SetSrvSendFrameEvents;
    property RemoteSettings_SrvSaveSettingsToFile: Boolean read rsSrvSaveSettingsToFile write SetSrvSaveSettingsToFile;
    property RemoteSettings_RecKeepUtilityEvents: Boolean read rsRecKeepUtilityEvents write SetRecKeepUtilityEvents;
    property RemoteSettings_RecStoreConfigurations: Boolean read rsRecStoreConfigurations write SetRecStoreConfigurations;
    property RemoteSettings_RecManageIndexedChannels: Boolean read rsRecManageIndexedChannels write SetRecManageIndexedChannels;
    property RemoteSettings_LstListMode: TAddressListMode read rsLstListMode write SetLstListMode;
    property RemoteSettings_LstListFileName: String read rsLstListFileName write SetLstListFileName;
  end;

implementation

uses
  SysUtils,
  TelemetrynetPacketsBuilding,
  TelemetrynetPacketsResolving;

const
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Remember to update:'}
{$ENDIF}
(*
TN_PACKET_VERSION
TN_PACKET_TELEMETRY_VERSION
TN_PACKET_PARAM * 13
*)
  cBaseCompletionCount = 15;

  cSupportedServerVersions: Array[0..0] of LongWord = ($00010000 {1.0});

{==============================================================================}
{    TTelemetryNetClientBase // Private methods                                }
{==============================================================================}

Function TTelemetryNetClientBase.GetConnectedState: Boolean;
begin
Result := fClientSocket.Socket.Connected;
end;

Function TTelemetryNetClientBase.GetServerAddress: String;
begin
Result := fClientSocket.Address;
end;

Function TTelemetryNetClientBase.GetServerPort: Word;
begin
Result := fClientSocket.Port;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.SetPassword(Value: String);
begin
If not AnsiSameStr(fPassword,Value) then
  begin
    Disconnect(drPasswordChange);
    fPassword := Value;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetClientBase.GetOnLookup: TSocketNotifyEvent;
begin
Result := fClientSocket.OnLookup;
end;

procedure TTelemetryNetClientBase.SetOnLookup(Value: TSocketNotifyEvent);
begin
fClientSocket.OnLookup := Value;
end;

Function TTelemetryNetClientBase.GetOnConnecting: TSocketNotifyEvent;
begin
Result := fClientSocket.OnConnecting;
end;

procedure TTelemetryNetClientBase.SetOnConnecting(Value: TSocketNotifyEvent);
begin
fClientSocket.OnConnecting := Value;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.SetSrvSendEvents(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvSendEvents,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvSendChannels(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvSendChannels,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvSendConfigs(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvSendConfigs,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvActiveMode(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvActiveMode,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvBufferChannels(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvBufferChannels,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvSendFrameEvents(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvSendFrameEvents,Value),True);
end;

procedure TTelemetryNetClientBase.SetSrvSaveSettingsToFile(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidSrvSaveSettingsToFile,Value),True);
end;

procedure TTelemetryNetClientBase.SetRecKeepUtilityEvents(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidRecKeepUtilityEvents,Value),True);
end;

procedure TTelemetryNetClientBase.SetRecStoreConfigurations(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidRecStoreConfigurations,Value),True);
end;

procedure TTelemetryNetClientBase.SetRecManageIndexedChannels(Value: Boolean);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidRecManageIndexedChannels,Value),True);
end;

procedure TTelemetryNetClientBase.SetLstListMode(Value: TAddressListMode);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidLstListMode,Integer(Value)),True);
end;

procedure TTelemetryNetClientBase.SetLstListFileName(Value: String);
begin
Send(BuildPacket_TN_PACKET_ADMIN_PARAM_SET(pidLstListFileName,Value),True);
end;

{==============================================================================}
{    TTelemetryNetClientBase // Protected methods                              }
{==============================================================================}

procedure TTelemetryNetClientBase.OnConnectHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
fConnectionComplete := False;
fCompletionCounter := 0;
// Clear incoming and outgoing buffer/stream for new connection.
TNCWinSocket(Socket).IncomingStream.Clear;
TNCWinSocket(Socket).CircularPacketsBuffer.Clear;
If Assigned(fOnConnect) then fOnConnect(Self,Socket);
Send(BuildPacket_TN_PACKET_HI(fPassword),True);
end;

procedure TTelemetryNetClientBase.OnDisconnectHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
// Read and write any remaining data.
OnWriteHandler(Sender,Socket);
OnReadHandler(Sender,Socket);
If Assigned(fOnDisconnect) then fOnDisconnect(Self,Socket);
fConnectionComplete := False;
fCompletionCounter := 0;
end;

procedure TTelemetryNetClientBase.OnErrorHandler(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
If Assigned(fOnError) then fOnError(Self,Socket,ErrorEvent,ErrorCode);
case ErrorEvent of
  eeSend:       TNCWinSocket(Socket).CircularPacketsBuffer.Clear;
  eeReceive:    TNCWinSocket(Socket).IncomingStream.Clear;
  eeConnect,
  eeDisconnect,
  eeAccept,
  eeGeneral:;   // No action defined, yet.
end;
// Stop error propagation.
ErrorCode := 0;
end;

procedure TTelemetryNetClientBase.OnReadHandler(Sender: TObject; Socket: TCustomWinSocket);
var
  SmallBuffer:  Array[0..cLargeBufferThreshold - 1] of Byte;
  LargeBuffer:  Pointer;
  ReadLength:   Integer;
  AllocSize:    Integer;
begin
If Assigned(fOnRead) then fOnRead(Self,Socket);
ReadLength := Socket.ReceiveLength;
If ReadLength > 0 then
  begin
    If ReadLength > cLargeBufferThreshold then
      begin
        // Large buffer must be allocated.
        AllocSize := ReadLength;
        GetMem(LargeBuffer,AllocSize);
        try
          ReadLength := Socket.ReceiveBuf(LargeBuffer^,ReadLength);
          If ReadLength > 0 then
            TNCWinSocket(Socket).IncomingStream.WriteIncomingBuffer(LargeBuffer^,ReadLength);
        finally
          FreeMem(LargeBuffer,AllocSize);
        end;
      end
    else
      begin
        // Small buffer can be used.
        ReadLength := Socket.ReceiveBuf(SmallBuffer,ReadLength);
        If ReadLength > 0 then
          TNCWinSocket(Socket).IncomingStream.WriteIncomingBuffer(SmallBuffer,ReadLength);
      end;
  end;
end;

procedure TTelemetryNetClientBase.OnWriteHandler(Sender: TObject; Socket: TCustomWinSocket);
var
  TempPacket: TPacketBuffer;
begin
While TNCWinSocket(Socket).CircularPacketsBuffer.ContainsPacket do
  begin
    TempPacket := TNCWinSocket(Socket).CircularPacketsBuffer.PeekPacket;
    If Socket.SendBuf(TempPacket.Data^,TempPacket.Size) < TempPacket.Size then Break
      else TNCWinSocket(Socket).CircularPacketsBuffer.RemovePacket;
  end;
If Assigned(fOnWrite) then fOnWrite(Self,Socket);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.OnOverflowClearHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
If Assigned(fOnOverflowClear) then fOnOverflowClear(Sender,Socket);
end;

procedure TTelemetryNetClientBase.OnPacketDropHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
begin
If Assigned(fOnPacketDrop) then fOnPacketDrop(Sender,Socket,Packet);
end;

procedure TTelemetryNetClientBase.OnPacketHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
begin
If Assigned(fOnPacketReceived) then fOnPacketReceived(Self,Socket,Packet);
try
  case GetPacketIDPrefix(GetPacketHeader(Packet).PacketID) of
    TN_PREFIX_COMMON:         ProcessPacketGroup_COMMON(Packet);
    TN_PREFIX_ADMIN:          ProcessPacketGroup_ADMIN(Packet);
    TN_PREFIX_SUPERADMIN:     ProcessPacketGroup_SUPERADMIN(Packet);
    TN_PREFIX_EVENT_KNOWN:    ProcessPacketGroup_EVENT_KNOWN(Packet);
    TN_PREFIX_CHANNEL_KNOWN:  ProcessPacketGroup_CHANNEL_KNOWN(Packet);
    TN_PREFIX_CONFIG_KNOWN:   ProcessPacketGroup_CONFIG_KNOWN(Packet);
    TN_PREFIX_EVENT:          ProcessPacketGroup_EVENT(Packet);
    TN_PREFIX_CHANNEL:        ProcessPacketGroup_CHANNEL(Packet);
    TN_PREFIX_CONFIG:         ProcessPacketGroup_CONFIG(Packet);
    TN_PREFIX_LOG:            ProcessPacketGroup_LOG(Packet);
  else
    Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  end;
except;
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petErrorReadingPacket,GetLastError),True);
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetClientBase.ProcessPacketGroup_COMMON(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_PING:                       ProcessPacket_TN_PACKET_PING(Packet);
  TN_PACKET_PING_RESPONSE:              ProcessPacket_TN_PACKET_PING_RESPONSE(Packet);
  TN_PACKET_HI:                         ProcessPacket_TN_PACKET_HI(Packet);
  TN_PACKET_VERSION:                    ProcessPacket_TN_PACKET_VERSION(Packet);
  TN_PACKET_TELEMETRY_VERSION_GET:      ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet);
  TN_PACKET_TELEMETRY_VERSION:          ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet);
  TN_PACKET_READY:                      ProcessPacket_TN_PACKET_READY(Packet);
  TN_PACKET_PARAM_GET:                  ProcessPacket_TN_PACKET_PARAM_GET(Packet);
  TN_PACKET_PARAM:                      ProcessPacket_TN_PACKET_PARAM(Packet);
  TN_PACKET_BYE:                        ProcessPacket_TN_PACKET_BYE(Packet);
  TN_PACKET_MESSAGE:                    ProcessPacket_TN_PACKET_MESSAGE(Packet);
  TN_PACKET_PACKET:                     ProcessPacket_TN_PACKET_PACKET(Packet);
  TN_PACKET_RIGHTS_ADMIN_REQUEST:       ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet);
  TN_PACKET_RIGHTS_ADMIN:               ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet);
  TN_PACKET_ERROR:                      ProcessPacket_TN_PACKET_ERROR(Packet);
  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST:  ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet);
  TN_PACKET_RIGHTS_SUPERADMIN:          ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_ADMIN(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_ADMIN_CLIENT_CONNECTING:    ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet);
  TN_PACKET_ADMIN_CLIENT_REJECTED:      ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet);
  TN_PACKET_ADMIN_CLIENT_CONNECTED:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet);
  TN_PACKET_ADMIN_CLIENT_DISCONNECTED:  ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet);
  TN_PACKET_ADMIN_CLIENT_CHANGE:        ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet);
  TN_PACKET_ADMIN_CLIENT_OPERATION:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet);
  TN_PACKET_ADMIN_CLIENT_COUNT_GET:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet);
  TN_PACKET_ADMIN_CLIENT_COUNT:         ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet);
  TN_PACKET_ADMIN_CLIENT_INDEX_GET:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet);
  TN_PACKET_ADMIN_CLIENT_INDEX:         ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet);
  TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET: ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet);
  TN_PACKET_ADMIN_CLIENT_ALL_GET:       ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet);
  TN_PACKET_ADMIN_CLIENT_ALL:           ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet);
  TN_PACKET_ADMIN_LIST_COUNT_GET:       ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet);
  TN_PACKET_ADMIN_LIST_COUNT:           ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet);
  TN_PACKET_ADMIN_LIST_INDEX_GET:       ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet);
  TN_PACKET_ADMIN_LIST_INDEX:           ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet);
  TN_PACKET_ADMIN_LIST_INDEX_ALL_GET:   ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet);
  TN_PACKET_ADMIN_LIST_ALL_GET:         ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet);
  TN_PACKET_ADMIN_LIST_ALL:             ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet);
  TN_PACKET_ADMIN_LIST_CLEAR:           ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet);
  TN_PACKET_ADMIN_LIST_ADD:             ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet);
  TN_PACKET_ADMIN_LIST_REMOVE:          ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet);
  TN_PACKET_ADMIN_LIST_DELETE:          ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet);
  TN_PACKET_ADMIN_LIST_RELOAD:          ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet);
  TN_PACKET_ADMIN_LIST_SAVE:            ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet);
  TN_PACKET_ADMIN_LIST_CHANGE:          ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet);
  TN_PACKET_ADMIN_SEND_MESSAGE:         ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet);
  TN_PACKET_ADMIN_PASSWORD:             ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet);
  TN_PACKET_ADMIN_CHANGE_PASSWORD:      ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet);
  TN_PACKET_ADMIN_PARAM_SET:            ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_SUPERADMIN(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD:        ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet);
  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD: ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_EVENT_KNOWN(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_EVENT_KNOWN_COUNT_GET:      ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet);
  TN_PACKET_EVENT_KNOWN_COUNT:          ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet);
  TN_PACKET_EVENT_KNOWN_INDEX_GET:      ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet);
  TN_PACKET_EVENT_KNOWN_INDEX:          ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet);
  TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet);
  TN_PACKET_EVENT_KNOWN_ALL_GET:        ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet);
  TN_PACKET_EVENT_KNOWN_ALL:            ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_CHANNEL_KNOWN(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CHANNEL_KNOWN_COUNT_GET:      ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet);
  TN_PACKET_CHANNEL_KNOWN_COUNT:          ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet);
  TN_PACKET_CHANNEL_KNOWN_INDEX_GET:      ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet);
  TN_PACKET_CHANNEL_KNOWN_INDEX:          ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet);
  TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet);
  TN_PACKET_CHANNEL_KNOWN_ALL_GET:        ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet);
  TN_PACKET_CHANNEL_KNOWN_ALL:            ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_CONFIG_KNOWN(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CONFIG_KNOWN_COUNT_GET:     ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet);
  TN_PACKET_CONFIG_KNOWN_COUNT:         ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet);
  TN_PACKET_CONFIG_KNOWN_INDEX_GET:     ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet);
  TN_PACKET_CONFIG_KNOWN_INDEX:         ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet);
  TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET: ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet);
  TN_PACKET_CONFIG_KNOWN_ALL_GET:       ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet);
  TN_PACKET_CONFIG_KNOWN_ALL:           ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_EVENT(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_EVENT_REG:                      ProcessPacket_TN_PACKET_EVENT_REG(Packet);
  TN_PACKET_EVENT_REG_ALL:                  ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet);
  TN_PACKET_EVENT_UNREG:                    ProcessPacket_TN_PACKET_EVENT_UNREG(Packet);
  TN_PACKET_EVENT_UNREG_ALL:                ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet);
  TN_PACKET_EVENT_EVENT:                    ProcessPacket_TN_PACKET_EVENT_EVENT(Packet);
  TN_PACKET_EVENT_REGISTERED:               ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet);
  TN_PACKET_EVENT_REGISTERED_COUNT_GET:     ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet);
  TN_PACKET_EVENT_REGISTERED_COUNT:         ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet);
  TN_PACKET_EVENT_REGISTERED_INDEX_GET:     ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet);
  TN_PACKET_EVENT_REGISTERED_INDEX:         ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet);
  TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet);
  TN_PACKET_EVENT_REGISTERED_ALL_GET:       ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet);
  TN_PACKET_EVENT_REGISTERED_ALL:           ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_CHANNEL(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CHANNEL_REG:                      ProcessPacket_TN_PACKET_CHANNEL_REG(Packet);
  TN_PACKET_CHANNEL_REG_ALL:                  ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet);
  TN_PACKET_CHANNEL_UNREG:                    ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet);
  TN_PACKET_CHANNEL_UNREG_ALL:                ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet);
  TN_PACKET_CHANNEL_CHANNEL:                  ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet);
  TN_PACKET_CHANNEL_CHANNEL_BUFFERED:         ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet);
  TN_PACKET_CHANNEL_REGISTERED:               ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet);
  TN_PACKET_CHANNEL_REGISTERED_COUNT_GET:     ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet);
  TN_PACKET_CHANNEL_REGISTERED_COUNT:         ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet);
  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET:     ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet);
  TN_PACKET_CHANNEL_REGISTERED_INDEX:         ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet);
  TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet);
  TN_PACKET_CHANNEL_REGISTERED_ALL_GET:       ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet);
  TN_PACKET_CHANNEL_REGISTERED_ALL:           ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet);
  TN_PACKET_CHANNEL_STORED_SEND_ALL:          ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_CONFIG(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CONFIG_CONFIG:                ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet);
  TN_PACKET_CONFIG_STORED:                ProcessPacket_TN_PACKET_CONFIG_STORED(Packet);
  TN_PACKET_CONFIG_STORED_COUNT_GET:      ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet);
  TN_PACKET_CONFIG_STORED_COUNT:          ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet);
  TN_PACKET_CONFIG_STORED_INDEX_GET:      ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet);
  TN_PACKET_CONFIG_STORED_INDEX:          ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet);
  TN_PACKET_CONFIG_STORED_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet);
  TN_PACKET_CONFIG_STORED_ALL_GET:        ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet);
  TN_PACKET_CONFIG_STORED_ALL:            ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

Function TTelemetryNetClientBase.ProcessPacketGroup_LOG(Packet: TPacketBuffer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_LOG_LOG:  ProcessPacket_TN_PACKET_LOG_LOG(Packet);
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

{$DEFINE Implementation_part}
{$INCLUDE '.\INC\TTelemetryNetClientBase.ProcessPacket_TN_PACKET.pas'}
{$UNDEF Implementation_part}

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.IncreaseCompletionCounter;
begin
Inc(fCompletionCounter);
If (fCompletionCounter >= fCompletionCount) and not ConnectionComplete then
  begin
    fConnectionComplete := True;
    If Assigned(fOnConnectionComplete) then fOnConnectionComplete(Self);
  end;
end;

{==============================================================================}
{    TTelemetryNetClientBase // Public methods                                 }
{==============================================================================}

class Function TTelemetryNetClientBase.HighestSupportedServerVersion: LongWord;
begin
Result := cSupportedServerVersions[High(cSupportedServerVersions)];
end;

class Function TTelemetryNetClientBase.SupportsServerVersion(aServerVersion: LongWord): Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(cSupportedServerVersions) to High(cSupportedServerVersions) do
  If cSupportedServerVersions[i] = aServerVersion then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

constructor TTelemetryNetClientBase.Create;
begin
inherited Create;
UtilityObjectsInitialize;
// Create and prepare client socket.
fClientSocket := TTelemetryNetClientSocket.Create(nil);
fClientSocket.Address := def_ServerAddress;
fClientSocket.Port := def_ServerPort;  
fClientSocket.OnConnect := OnConnectHandler;
fClientSocket.OnDisconnect := OnDisconnectHandler;
fClientSocket.OnError := OnErrorHandler;
fClientSocket.OnRead := OnReadHandler;
fClientSocket.OnWrite := OnWriteHandler;
with TNCWinSocket(fClientSocket.Socket) do
  begin
    CircularPacketsBuffer.OnPacketDrop := OnPacketDropHandler;
    IncomingStream.OnOverflowClear := OnOverflowClearHandler;
    IncomingStream.OnPacket := OnPacketHandler;
  end;
// Fields initialization.
fServerVersion := 0;
fTelemetryVersion := SCS_U32_NIL;
fGameName := '';
fGameID := '';
fGameVersion := SCS_U32_NIL;
Password := '';
fCompletionCount := cBaseCompletionCount;
fCompletionCounter := 0;
fConnectionComplete := False;
end;

destructor TTelemetryNetClientBase.Destroy;
begin
If Assigned(fOnDestroy) then fOnDestroy(Self);
// Close connection and free socket.
Disconnect(drClientTerminated);
fClientSocket.Free;
UtilityObjectsFinalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetClientBase.Connect(ServerAddress: String = def_ServerAddress; ServerPort: Word = def_ServerPort): Boolean;
begin
If not fClientSocket.Socket.Connected then
  begin
    fClientSocket.Address := ServerAddress;
    fClientSocket.Port := ServerPort;
    fClientSocket.Open;
    Result := True;
  end
else Result := False;
end;

Function TTelemetryNetClientBase.Disconnect(Reason: TDisconnectReason = drClientDisconnected): Boolean;
begin
If fClientSocket.Socket.Connected then
  begin
    Send(BuildPacket_TN_PACKET_BYE(Reason),True);
    fClientSocket.Close;
    UtilityObjectsClear;
    // Clear server info.
    fServerVersion := 0;
    fTelemetryVersion := SCS_U32_NIL;
    fGameName := '';
    fGameID := '';
    fGameVersion := SCS_U32_NIL;    
    Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.Send(Packet: TPacketBuffer; FreePacket: Boolean);

  procedure BufferPacket(aPacket: TPacketBuffer; var aFreePacket: Boolean);
  begin
    TNCWinSocket(fClientsocket.Socket).CircularPacketsBuffer.AddPacket(aPacket,not aFreePacket);
    aFreePacket := False;
  end;

begin
If Assigned(fOnPacketSending) then fOnPacketSending(Self,fClientsocket.Socket,Packet);
OnWriteHandler(nil,fClientsocket.Socket);
If not TNCWinSocket(fClientsocket.Socket).CircularPacketsBuffer.ContainsPacket then
  begin
    If fClientsocket.Socket.SendBuf(Packet.Data^,Packet.Size) < Packet.Size then
      BufferPacket(Packet,FreePacket)
    else
      If Assigned(fOnPacketSend) then fOnPacketSend(Self,fClientsocket.Socket,Packet);
  end
else BufferPacket(Packet,FreePacket);
If FreePacket then FreeMem(Packet.Data,Packet.Size);
end;

procedure TTelemetryNetClientBase.GetServerSettings(ParameterID: TParameterID = pidAll);
begin
Send(BuildPacket_TN_PACKET_PARAM_GET(ParameterID),True);
end;

end.
