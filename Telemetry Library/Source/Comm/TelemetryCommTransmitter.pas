{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit TelemetryCommTransmitter;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryCommon,
  TelemetryIDs,
  TelemetryRecipient,
  TelemetryRecipientBinder,
  TelemetryCommCommon,
  TelemetryCommPackets,
  TelemetryCommCircularBuffers,
  TelemetryCommPacketsBuilder,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryCommTransmitter                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommTransmitter // Class declaration                             }
{==============================================================================}
type
  TTelemetryCommTransmitter = class(TTelemetryRecipientBinder)
  private
    fBufferedChannels:          TCircularChannelsBuffer;
    fPacketsBuilder:            TTelemetryCommPacketsBuilder;
    fSendEvents:                Boolean;
    fSendChannels:              Boolean;
    fSendConfigs:               Boolean;
    fBufferChannels:            Boolean;
    fSendFrameEvents:           Boolean;
    fOnChannelBuffering:        TChannelEvent;
    fOnSendingPacket:           TPacketNotifyEvent;
    fExecuteDefferedOperations: TEventRegisterEvent;
    Function GetBufferedChannelsCount: Integer;
    Function GetActiveMode: Boolean;
    procedure SetActiveMode(Value: Boolean);
    procedure SetBufferChannels(Value: Boolean);
  protected
  public
    constructor Create(Recipient: TTelemetryRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder);
    destructor Destroy; override;
    procedure SendBufferedChannels; virtual; abstract;
    procedure SendStoredConfigs(OnePacket: Boolean; ConnectionData: Pointer); virtual; abstract;
    procedure SendStoredChannelsValues(ConnectionData: Pointer); virtual; abstract;
    procedure SendPacket(Packet: TPacketBuffer; ConnectionData: Pointer); virtual;
  published
    property BufferedChannels: TCircularChannelsBuffer read fBufferedChannels;
    property PacketsBuilder: TTelemetryCommPacketsBuilder read fPacketsBuilder;
    property BufferedChannelsCount: Integer read GetBufferedChannelsCount;
    property SendEvents: Boolean read fSendEvents write fSendEvents;
    property SendChannels: Boolean read fSendChannels write fSendChannels;
    property SendConfigs: Boolean read fSendConfigs write fSendConfigs;
    property ActiveMode: Boolean read GetActiveMode write SetActiveMode;
    property BufferChannels: Boolean read fBufferChannels write SetBufferChannels;
    property SendFrameEvents: Boolean read fSendFrameEvents write fSendFrameEvents;
    property OnChannelBuffering: TChannelEvent read fOnChannelBuffering write fOnChannelBuffering;
    property OnSendingPacket: TPacketNotifyEvent read fOnSendingPacket write fOnSendingPacket;
    property ExecuteDefferedOperations: TEventRegisterEvent read fExecuteDefferedOperations write fExecuteDefferedOperations;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommServerTransmitter                       }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommServerTransmitter // Class declaration                       }
{==============================================================================}
  TTelemetryCommServerTransmitter = class(TTelemetryCommTransmitter)
  public
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); override;
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
    procedure SendBufferedChannels; override;
    procedure SendStoredConfigs(OnePacket: Boolean; ConnectionData: Pointer); override;
    procedure SendStoredChannelsValues(ConnectionData: Pointer); override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommClientTransmitter                       }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommClientTransmitter // Class declaration                       }
{==============================================================================}
  TTelemetryCommClientTransmitter = class(TTelemetryCommTransmitter)
  public
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); override;
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); override;
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
    procedure SendBufferedChannels; override;
    procedure SendStoredConfigs(OnePacket: Boolean; ConnectionData: Pointer); override;
    procedure SendStoredChannelsValues(ConnectionData: Pointer); override;
  end;


implementation

uses
  SysUtils;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryCommTransmitter                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommTransmitter // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommTransmitter // Private methods                               }
{------------------------------------------------------------------------------}

Function TTelemetryCommTransmitter.GetBufferedChannelsCount: Integer;
begin
Result := fBufferedChannels.Count;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommTransmitter.GetActiveMode: Boolean;
begin
Result := fSendEvents and fSendChannels and fSendConfigs;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommTransmitter.SetActiveMode(Value: Boolean);
begin
fSendEvents := Value;
fSendChannels := Value;
fSendConfigs := Value;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommTransmitter.SetBufferChannels(Value: Boolean);
begin
If fBufferChannels <> Value then
  begin
    If not Value then SendBufferedChannels;
    fBufferChannels := Value;
  end;
end;

{------------------------------------------------------------------------------}
{   TTelemetryCommTransmitter // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryCommTransmitter.Create(Recipient: TTelemetryRecipient; PacketsBuilder: TTelemetryCommPacketsBuilder);
begin
If not Assigned(Recipient) then
  raise Exception.Create('TTelemetryCommTransmitter.Create: Recipient not assigned.');
inherited Create(Recipient);
If not Assigned(PacketsBuilder) then
  raise Exception.Create('TTelemetryCommTransmitter.Create: Packets builder not assigned.');
fPacketsBuilder := PacketsBuilder;
fBufferedChannels := TCircularChannelsBuffer.Create;
end;

//------------------------------------------------------------------------------

destructor TTelemetryCommTransmitter.Destroy;
begin
fBufferedChannels.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommTransmitter.SendPacket(Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If Assigned(fOnSendingPacket) then fOnSendingPacket(Self,Packet,ConnectionData);
If Packet.AllocInfo.CanBeFreed then fPacketsBuilder.FreePacket(Packet);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommServerTransmitter                       }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommServerTransmitter // Class implementation                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommServerTransmitter // Public methods                          }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerTransmitter.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_LOG_LOG(LogType,LogText),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTER(Event,SCS_RESULT_ok),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_UNREGISTER(Event,SCS_RESULT_ok),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
begin
If SendEvents then
  begin
    If Event in [SCS_TELEMETRY_EVENT_frame_start,SCS_TELEMETRY_EVENT_frame_end] then
      begin
        If SendFrameEvents then
          SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_EVENT(Event,Data),cSendToAll);
      end
    else SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_EVENT(Event,Data),cSendToAll);
  end;
If BufferChannels then SendBufferedChannels;
If Assigned(ExecuteDefferedOperations) then ExecuteDefferedOperations(Self,Event,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER(Name,Index,ValueType,Flags,SCS_RESULT_ok),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER(Name,Index,ValueType,SCS_RESULT_ok),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
begin
If BufferChannels then
  begin
    BufferedChannels.AddChannel(Name,ID,Index,Value);
    If Assigned(OnChannelBuffering) then OnChannelBuffering(Sender,Name,ID,Index,Value,nil);
  end
else
  If SendChannels then SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL(Name,ID,Index,Value),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If SendConfigs then
  SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_CONFIG(Name,ID,Index,Value),cSendToAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.SendBufferedChannels;
begin
If BufferedChannels.ContainsChannel then
  begin
    If SendChannels then
      SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(BufferedChannels),cSendToAll);
    BufferedChannels.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.SendStoredConfigs(OnePacket: Boolean; ConnectionData: Pointer);
var
  i:  Integer;
begin
If OnePacket then
  SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL(Recipient),ConnectionData)
else
  For i := 0 to Pred(Recipient.StoredConfigs.Count) do
    SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX(Recipient,i),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerTransmitter.SendStoredChannelsValues(ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.StoredChannelsValues.Count) do
  SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL(Recipient.StoredChannelsValues[i]),ConnectionData);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommClientTransmitter                       }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommClientTransmitter // Class implementation                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommClientTransmitter // Public methods                          }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientTransmitter.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.SendBufferedChannels;
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.SendStoredConfigs(OnePacket: Boolean; ConnectionData: Pointer);
begin
Exit;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientTransmitter.SendStoredChannelsValues(ConnectionData: Pointer);
begin
Exit;
end;

end.
