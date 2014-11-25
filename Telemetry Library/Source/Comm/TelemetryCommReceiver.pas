unit TelemetryCommReceiver;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryRecipient,
  TelemetryCommCommon,
  TelemetryCommPackets,
  TelemetryCommCircularBuffers,
  TelemetryCommPacketsBuilder,
  TelemetryCommPacketsResolving,
  TelemetryCommRemoteRecipient,
  TelemetryCommTransmitter,
{$IFDEF Documentation}
  TelemetryConversions,
  TelemetryLists,
  TelemetryStreaming,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}


{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryCommReceiver                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommReceiver // Class declaration                                }
{==============================================================================}
type
  TTelemetryCommReceiver = class(TObject)
  private
    fRecipient:           TTelemetryRecipient;
    fPacketsBuilder:      TTelemetryCommPacketsBuilder;
    fTransmitter:         TTelemetryCommTransmitter;
    fDefferedOperations:  TDefferedOperationsBuffer;
    fProtocolVersion:     TProtocolVersion;
    fOnUnprocessedPacket: TPacketNotifyEvent;
    fOnFatalError:        TPacketNotifyEvent;
  protected
    Function ProcessPacketGroup_COMMON(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_KNOWN_EVENTS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_KNOWN_CHANNELS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_KNOWN_CONFIGS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    Function ProcessPacketGroup_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_COMMON packets                                              }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_PING packet.}
    procedure ProcessPacket_TC_PACKET_PING(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_PING_RESPONSE packet.}
    procedure ProcessPacket_TC_PACKET_PING_RESPONSE(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_PROTOCOL_VERSION_GET packet.}
    procedure ProcessPacket_TC_PACKET_PROTOCOL_VERSION_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_PROTOCOL_VERSION packet.}
    procedure ProcessPacket_TC_PACKET_PROTOCOL_VERSION(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_TELEMETRY_INFO_GET packet.}
    procedure ProcessPacket_TC_PACKET_TELEMETRY_INFO_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_TELEMETRY_INFO packet.}
    procedure ProcessPacket_TC_PACKET_TELEMETRY_INFO(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_PACKET packet.}
    procedure ProcessPacket_TC_PACKET_PACKET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_ERROR packet.}
    procedure ProcessPacket_TC_PACKET_ERROR(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_DEFFERED packet.}
    procedure ProcessPacket_TC_PACKET_DEFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_KNOWN_EVENTS packets                                        }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_KNOWN_EVENTS_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_ALL packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_KNOWN_CHANNELS packets                                      }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_KNOWN_CHANNELS_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CHANNELS_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CHANNELS_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CHANNELS_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CHANNELS_ALL packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_KNOWN_CONFIGS packets                                       }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_KNOWN_CONFIGS_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CONFIGS_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CONFIGS_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CONFIGS_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_KNOWN_CONFIGS_ALL packet.}
    procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_EVENT packets                                               }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_EVENT_REGISTER packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTER_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTER_ALL packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_UNREGISTER packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_UNREGISTER_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_UNREGISTER_BY_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_UNREGISTER_ALL packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_EVENT packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_INDEX_GET packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_EVENT_REGISTERED_ALL packet.}
    procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_CHANNEL packets                                             }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_CHANNEL_REGISTER packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTER_BY_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTER_BY_NAME packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTER_ALL packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_UNREGISTER packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_UNREGISTER_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_UNREGISTER_BY_NAME packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_UNREGISTER_ALL packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_CHANNEL packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_CHANNEL_BUFFERED packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX_GET packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_REGISTERED_ALL packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CHANNEL_STORED_SEND_ALL packet.}
    procedure ProcessPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_CONFIG packets                                              }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_CONFIG_CONFIG packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_COUNT_GET packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_COUNT packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_INDEX_GE packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_INDEX packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_INDEX_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_ALL_GET packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {Method processing TC_PACKET_CONFIG_STORED_ALL packet.}
    procedure ProcessPacket_TC_PACKET_CONFIG_STORED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    {--------------------------------------------------------------------------}
    {    TC_PREFIX_CONFIG packets                                              }
    {--------------------------------------------------------------------------}
    {Method processing TC_PACKET_LOG_LOG packet.}
    procedure ProcessPacket_TC_PACKET_LOG_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer); virtual; abstract;
    procedure ExecuteDefferedOperations(Sender: TObject; Event: scs_event_t; UserData: Pointer); virtual; abstract;
  public
    constructor Create(Recipient: TTelemetryRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder; Transmitter: TTelemetryCommTransmitter; ProtocolVersion: TProtocolVersion);
    destructor Destroy; override;
    Function ProcessPacket(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean; virtual;
  published
    property Recipient: TTelemetryRecipient read fRecipient;
    property PacketsBuilder: TTelemetryCommPacketsBuilder read fPacketsBuilder;
    property Transmitter: TTelemetryCommTransmitter read fTransmitter;
    property DefferedOperations: TDefferedOperationsBuffer read fDefferedOperations;
    property ProtocolVersion: TProtocolVersion read fProtocolVersion;
    property OnUnprocessedPacket: TPacketNotifyEvent read fOnUnprocessedPacket write fOnUnprocessedPacket;
    property OnFatalError: TPacketNotifyEvent read fOnFatalError write fOnFatalError;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryCommServerReceiver                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommServerReceiver // Class declaration                          }
{==============================================================================}
  TTelemetryCommServerReceiver = class(TTelemetryCommReceiver)
  protected
    {$DEFINE DeclarationPart}
      {$INCLUDE '.\Inc\TTelemetryCommServerReceiver.ProcessPacket.pas'}
    {$UNDEF DeclarationPart}
    procedure ExecuteDefferedOperations(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryCommClientReceiver                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommClientReceiver // Class declaration                          }
{==============================================================================}
  TTelemetryCommClientReceiver = class(TTelemetryCommReceiver)
  protected
    {$DEFINE DeclarationPart}
      {$INCLUDE '.\Inc\TTelemetryCommClientReceiver.ProcessPacket.pas'}
    {$UNDEF DeclarationPart}
    procedure ExecuteDefferedOperations(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
  public
    constructor Create(Recipient: TTelemetryCommRemoteRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder; Transmitter: TTelemetryCommTransmitter; ProtocolVersion: TProtocolVersion);
  end;

implementation

uses
  SysUtils,
  TelemetryConversions, TelemetryLists, TelemetryStreaming;

{==============================================================================}
{------------------------------------------------------------------------------}
{                            TTelemetryCommReceiver                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommReceiver // Class implementation                             }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommReceiver // Protected methods                                }
{------------------------------------------------------------------------------}

Function TTelemetryCommReceiver.ProcessPacketGroup_COMMON(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_PING:                 ProcessPacket_TC_PACKET_PING(Packet,ConnectionData);
  TC_PACKET_PING_RESPONSE:        ProcessPacket_TC_PACKET_PING_RESPONSE(Packet,ConnectionData);
  TC_PACKET_PROTOCOL_VERSION_GET: ProcessPacket_TC_PACKET_PROTOCOL_VERSION_GET(Packet,ConnectionData);
  TC_PACKET_PROTOCOL_VERSION:     ProcessPacket_TC_PACKET_PROTOCOL_VERSION(Packet,ConnectionData);
  TC_PACKET_TELEMETRY_INFO_GET:   ProcessPacket_TC_PACKET_TELEMETRY_INFO_GET(Packet,ConnectionData);
  TC_PACKET_TELEMETRY_INFO:       ProcessPacket_TC_PACKET_TELEMETRY_INFO(Packet,ConnectionData);
  TC_PACKET_PACKET:               ProcessPacket_TC_PACKET_PACKET(Packet,ConnectionData);
  TC_PACKET_ERROR:                ProcessPacket_TC_PACKET_ERROR(Packet,ConnectionData);
  TC_PACKET_DEFFERED:             ProcessPacket_TC_PACKET_DEFFERED(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_KNOWN_EVENTS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_KNOWN_EVENTS_COUNT_GET:     ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_COUNT:         ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_INDEX_GET:     ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_INDEX:         ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET: ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_ALL_GET:       ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_EVENTS_ALL:           ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_KNOWN_CHANNELS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_KNOWN_CHANNELS_COUNT_GET:     ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_COUNT:         ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_INDEX_GET:     ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_INDEX:         ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET: ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_ALL_GET:       ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CHANNELS_ALL:           ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_KNOWN_CONFIGS(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_KNOWN_CONFIGS_COUNT_GET:      ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_COUNT:          ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_INDEX_GET:      ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_INDEX:          ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET:  ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_ALL_GET:        ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET(Packet,ConnectionData);
  TC_PACKET_KNOWN_CONFIGS_ALL:            ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_EVENT_REGISTER:                 ProcessPacket_TC_PACKET_EVENT_REGISTER(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTER_BY_INDEX:        ProcessPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTER_ALL:             ProcessPacket_TC_PACKET_EVENT_REGISTER_ALL(Packet,ConnectionData);
  TC_PACKET_EVENT_UNREGISTER:               ProcessPacket_TC_PACKET_EVENT_UNREGISTER(Packet,ConnectionData);
  TC_PACKET_EVENT_UNREGISTER_BY_INDEX:      ProcessPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(Packet,ConnectionData);
  TC_PACKET_EVENT_UNREGISTER_ALL:           ProcessPacket_TC_PACKET_EVENT_UNREGISTER_ALL(Packet,ConnectionData);
  TC_PACKET_EVENT_EVENT:                    ProcessPacket_TC_PACKET_EVENT_EVENT(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED:               ProcessPacket_TC_PACKET_EVENT_REGISTERED(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_COUNT_GET:     ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_COUNT:         ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_INDEX_GET:     ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_INDEX:         ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_ALL_GET:       ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET(Packet,ConnectionData);
  TC_PACKET_EVENT_REGISTERED_ALL:           ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_CHANNEL_REGISTER:                 ProcessPacket_TC_PACKET_CHANNEL_REGISTER(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTER_BY_INDEX:        ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTER_BY_NAME:         ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTER_ALL:             ProcessPacket_TC_PACKET_CHANNEL_REGISTER_ALL(Packet,ConnectionData);
  TC_PACKET_CHANNEL_UNREGISTER:               ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER(Packet,ConnectionData);
  TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX:      ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(Packet,ConnectionData);
  TC_PACKET_CHANNEL_UNREGISTER_BY_NAME:       ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(Packet,ConnectionData);
  TC_PACKET_CHANNEL_UNREGISTER_ALL:           ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL(Packet,ConnectionData);
  TC_PACKET_CHANNEL_CHANNEL:                  ProcessPacket_TC_PACKET_CHANNEL_CHANNEL(Packet,ConnectionData);
  TC_PACKET_CHANNEL_CHANNEL_BUFFERED:         ProcessPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED:               ProcessPacket_TC_PACKET_CHANNEL_REGISTERED(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_COUNT_GET:     ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_COUNT:         ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_INDEX_GET:     ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_INDEX:         ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_ALL_GET:       ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet,ConnectionData);
  TC_PACKET_CHANNEL_REGISTERED_ALL:           ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(Packet,ConnectionData);
  TC_PACKET_CHANNEL_STORED_SEND_ALL:          ProcessPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_CONFIG_CONFIG:                ProcessPacket_TC_PACKET_CONFIG_CONFIG(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED:                ProcessPacket_TC_PACKET_CONFIG_STORED(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_COUNT_GET:      ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT_GET(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_COUNT:          ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_INDEX_GET:      ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_INDEX:          ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_INDEX_ALL_GET:  ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_ALL_GET:        ProcessPacket_TC_PACKET_CONFIG_STORED_ALL_GET(Packet,ConnectionData);
  TC_PACKET_CONFIG_STORED_ALL:            ProcessPacket_TC_PACKET_CONFIG_STORED_ALL(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacketGroup_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TC_PACKET_LOG_LOG:  ProcessPacket_TC_PACKET_LOG_LOG(Packet,ConnectionData);
else
  If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
  Result := False;
end;
end;

{------------------------------------------------------------------------------}
{   TTelemetryCommReceiver // Public methods                                   }
{------------------------------------------------------------------------------}

constructor TTelemetryCommReceiver.Create(Recipient: TTelemetryRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder; Transmitter: TTelemetryCommTransmitter; ProtocolVersion: TProtocolVersion);
begin
inherited Create;
If not Assigned(Recipient) then raise Exception.Create('TTelemetryCommReceiver.Create: Recipient not assigned.');
If not Assigned(PacketsBuilder) then raise Exception.Create('TTelemetryCommReceiver.Create: Packets builder not assigned.');
If not Assigned(Transmitter) then raise Exception.Create('TTelemetryCommReceiver.Create: Transmitter not assigned.');
fDefferedOperations := TDefferedOperationsBuffer.Create;
fProtocolVersion := ProtocolVersion;
fTransmitter.ExecuteDefferedOperations := ExecuteDefferedOperations;
end;

//------------------------------------------------------------------------------

destructor TTelemetryCommReceiver.Destroy;
begin
fTransmitter.ExecuteDefferedOperations := nil;
fDefferedOperations.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommReceiver.ProcessPacket(var Packet: TPacketBuffer; ConnectionData: Pointer): Boolean;
begin
try
  case GetPacketIDPrefix(GetPacketHeader(Packet).PacketID) of
    TC_PREFIX_COMMON:         Result := ProcessPacketGroup_COMMON(Packet,ConnectionData);
    TC_PREFIX_KNOWN_EVENTS:   Result := ProcessPacketGroup_KNOWN_EVENTS(Packet,ConnectionData);
    TC_PREFIX_KNOWN_CHANNELS: Result := ProcessPacketGroup_KNOWN_CHANNELS(Packet,ConnectionData);
    TC_PREFIX_KNOWN_CONFIGS:  Result := ProcessPacketGroup_KNOWN_CONFIGS(Packet,ConnectionData);
    TC_PREFIX_EVENT:          Result := ProcessPacketGroup_EVENT(Packet,ConnectionData);
    TC_PREFIX_CHANNEL:        Result := ProcessPacketGroup_CHANNEL(Packet,ConnectionData);
    TC_PREFIX_CONFIG:         Result := ProcessPacketGroup_CONFIG(Packet,ConnectionData);
    TC_PREFIX_LOG:            Result := ProcessPacketGroup_LOG(Packet,ConnectionData);
  else
    If Assigned(fOnUnprocessedPacket) then fOnUnprocessedPacket(Self,Packet,ConnectionData);
    Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnknownPacket),ConnectionData);
    Result := False;
  end;
except
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petErrorReadingPacket,GetLastError),ConnectionData);
  Result := False;
end;
Packet.AllocInfo.CanBeFreed := True;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryCommServerReceiver                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommServerReceiver // Class implementation                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommServerReceiver // Protected methods                          }
{------------------------------------------------------------------------------}

{$DEFINE ImplementationPart}
  {$INCLUDE '.\Inc\TTelemetryCommServerReceiver.ProcessPacket.pas'}
{$UNDEF ImplementationPart}

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ExecuteDefferedOperations(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  DefferedOperation:  TDefferedOperation;
  TempEvent:          scs_event_t;
begin
while DefferedOperations.ContainsOperation do
  begin
    DefferedOperation := DefferedOperations.PeekOperation;
    // As this method is called in event callback, it is posssible there is
    // collision with buffered operation - more precisely, an event cannot be
    // registered in its own callback - if that is the case, delete operation
    // (if that event was called - therefore is already registered - then
    // there is no need to register it again).
    If DefferedOperation.OperationType in [dotEventReg,dotEventRegByIndex] then
      begin
        TempEvent := DefferedOperation.EventParams.Event;
        If DefferedOperation.OperationType = dotEventRegByIndex then
          If (Integer(TempEvent) >= 0) and (Integer(TempEvent) < Recipient.TelemetryInfoProvider.KnownEvents.Count) then
            TempEvent := Recipient.TelemetryInfoProvider.KnownEvents[Integer(TempEvent)].Event;
        If TempEvent = Event then
          begin
            fDefferedOperations.RemoveOperation;
            Continue;
          end;
      end;
    case DefferedOperation.OperationType of
{--- ---                                                                --- ---}
      dotEventReg:
        with DefferedOperation.EventParams do
          If not Recipient.EventRegister(Event) then
            Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTER(
              Event,Recipient.LastTelemetryResult),DefferedOperation.ConnectionData);
{--- ---                                                                --- ---}
      dotEventUnreg:
        with DefferedOperation.EventParams do
          If not Recipient.EventUnregister(Event) then
            Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_UNREGISTER(
              Event,Recipient.LastTelemetryResult),DefferedOperation.ConnectionData);
{--- ---                                                                --- ---}
      dotEventRegByIndex:
        Recipient.EventRegisterByIndex(Integer(DefferedOperation.EventParams.Event));
{--- ---                                                                --- ---}
      dotEventUnregIndex:
        Recipient.EventUnregisterIndex(Integer(DefferedOperation.EventParams.Event));
{--- ---                                                                --- ---}
      dotEventUnregByIndex:
        Recipient.EventUnregisterByIndex(Integer(DefferedOperation.EventParams.Event));
{--- ---                                                                --- ---}
      dotEventRegAll:
        Recipient.EventRegisterAll;
{--- ---                                                                --- ---}
      dotEventUnregAll:
        Recipient.EventUnregisterAll;
{--- ---                                                                --- ---}
      dotChannelReg:
        with DefferedOperation.ChannelParams do
          If not Recipient.ChannelRegister(Name,Index,ValueType,Flags) then
            Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER(
              Name,Index,ValueType,Flags,Recipient.LastTelemetryResult),DefferedOperation.ConnectionData);
{--- ---                                                                --- ---}
      dotChannelUnreg:
        with DefferedOperation.ChannelParams do
          If not Recipient.ChannelUnregister(Name,Index,ValueType) then
            Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER(
              Name,Index,ValueType,Recipient.LastTelemetryResult),DefferedOperation.ConnectionData);
{--- ---                                                                --- ---}
      dotChannelRegAll:
        with DefferedOperation.ChannelsRegAllParams do
          Recipient.ChannelRegisterAll(RegPrimaryType,SecondarySelectionMask);
{--- ---                                                                --- ---}
      dotChannelUnregAll:
        Recipient.ChannelUnregisterAll;
{--- ---                                                                --- ---}
      dotChannelRegByIndex:
        Recipient.ChannelRegisterByIndex(Integer(DefferedOperation.ChannelParams.Index));
{--- ---                                                                --- ---}
      dotChannelUnregIndex:
        Recipient.ChannelUnregisterIndex(Integer(DefferedOperation.ChannelParams.Index));
{--- ---                                                                --- ---}
      dotChannelUnregByIndex:
        Recipient.ChannelUnregisterByIndex(Integer(DefferedOperation.ChannelParams.Index));
{--- ---                                                                --- ---}
      dotChannelRegByName:
        Recipient.ChannelRegisterByName(DefferedOperation.ChannelParams.Name);
{--- ---                                                                --- ---}
      dotChannelUnregByName:
        Recipient.ChannelUnregisterByName(DefferedOperation.ChannelParams.Name);
{--- ---                                                                --- ---}      
    else
      {dotNone} // No action.
    end;
    DefferedOperations.RemoveOperation;
  end;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryCommClientReceiver                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommClientReceiver // Class implementation                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommClientReceiver // Protected methods                          }
{------------------------------------------------------------------------------}

{$DEFINE ImplementationPart}
  {$INCLUDE '.\Inc\TTelemetryCommClientReceiver.ProcessPacket.pas'}
{$UNDEF ImplementationPart}

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ExecuteDefferedOperations(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
DefferedOperations.Clear;
end;

{------------------------------------------------------------------------------}
{   TTelemetryCommClientReceiver // Public methods                             }
{------------------------------------------------------------------------------}

constructor TTelemetryCommClientReceiver.Create(Recipient: TTelemetryCommRemoteRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder; Transmitter: TTelemetryCommTransmitter; ProtocolVersion: TProtocolVersion);
begin
inherited Create(Recipient,PacketsBuilder,Transmitter,ProtocolVersion);
end;

end.
