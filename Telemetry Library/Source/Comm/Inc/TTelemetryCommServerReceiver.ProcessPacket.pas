{$IFDEF DeclarationPart}
{------------------------------------------------------------------------------}
{   TC_PREFIX_COMMON packets                                                   }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_PING packet.}
procedure ProcessPacket_TC_PACKET_PING(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_PING_RESPONSE packet.}
procedure ProcessPacket_TC_PACKET_PING_RESPONSE(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_PROTOCOL_VERSION_GET packet.}
procedure ProcessPacket_TC_PACKET_PROTOCOL_VERSION_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_PROTOCOL_VERSION packet.}
procedure ProcessPacket_TC_PACKET_PROTOCOL_VERSION(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_TELEMETRY_INFO_GET packet.}
procedure ProcessPacket_TC_PACKET_TELEMETRY_INFO_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_TELEMETRY_INFO packet.}
procedure ProcessPacket_TC_PACKET_TELEMETRY_INFO(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_PACKET packet.}
procedure ProcessPacket_TC_PACKET_PACKET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_ERROR packet.}
procedure ProcessPacket_TC_PACKET_ERROR(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_DEFFERED packet.}
procedure ProcessPacket_TC_PACKET_DEFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_EVENTS packets                                             }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_KNOWN_EVENTS_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_COUNT packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_INDEX packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_ALL packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CHANNELS packets                                           }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_KNOWN_CHANNELS_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CHANNELS_COUNT packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CHANNELS_INDEX packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CHANNELS_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CHANNELS_ALL packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CONFIGS packets                                            }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_KNOWN_CONFIGS_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CONFIGS_COUNT packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CONFIGS_INDEX packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CONFIGS_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_KNOWN_CONFIGS_ALL packet.}
procedure ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_EVENT packets                                                    }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_EVENT_REGISTER packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTER_INDEX packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTER_ALL packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_UNREGISTER packet.}
procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_UNREGISTER_INDEX packet.}
procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_UNREGISTER_BY_INDEX packet.}
procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_UNREGISTER_ALL packet.}
procedure ProcessPacket_TC_PACKET_EVENT_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_EVENT packet.}
procedure ProcessPacket_TC_PACKET_EVENT_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_COUNT packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_INDEX_GET packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_INDEX packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_EVENT_REGISTERED_ALL packet.}
procedure ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_CHANNEL packets                                                  }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_CHANNEL_REGISTER packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTER_BY_INDEX packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTER_BY_NAME packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTER_ALL packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_UNREGISTER packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_UNREGISTER_INDEX packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_UNREGISTER_BY_NAME packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_UNREGISTER_ALL packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_CHANNEL packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_CHANNEL_BUFFERED packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_COUNT packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX_GET packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_REGISTERED_ALL packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CHANNEL_STORED_SEND_ALL packet.}
procedure ProcessPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_CONFIG packets                                                   }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_CONFIG_CONFIG packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_COUNT_GET packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_COUNT packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_INDEX_GE packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_INDEX packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_ALL_GET packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{Method processing TC_PACKET_CONFIG_STORED_ALL packet.}
procedure ProcessPacket_TC_PACKET_CONFIG_STORED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer); override;

{------------------------------------------------------------------------------}
{   TC_PREFIX_LOG packets                                                      }
{------------------------------------------------------------------------------}
{Method processing TC_PACKET_LOG_LOG packet.}
procedure ProcessPacket_TC_PACKET_LOG_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer); override;
{$ENDIF}

{$IFDEF ImplementationPart}
{------------------------------------------------------------------------------}
{   TC_PREFIX_COMMON packets                                                   }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_PING(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_PING_RESPONSE(GetPacketHeader(Packet).TimeStamp),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_PING_RESPONSE(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_PROTOCOL_VERSION_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_PROTOCOL_VERSION(ProtocolVersion),ConnectionData);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_PROTOCOL_VERSION(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If TProtocolVersion(Payload_ReadoutInteger(Packet)) <> ProtocolVersion then
  If Assigned(OnFatalError) then OnFatalError(Self,Packet,ConnectionData);
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_TELEMETRY_INFO_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_TELEMETRY_INFO(Recipient),ConnectionData);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_TELEMETRY_INFO(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_PACKET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_ERROR(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior - but error should not be reported.
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_DEFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_EVENTS packets                                             }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT(Recipient.TelemetryInfoProvider.KnownEvents.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX(Recipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.TelemetryInfoProvider.KnownEvents.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX(Recipient.TelemetryInfoProvider,i),ConnectionData)
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL(Recipient.TelemetryInfoProvider),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CHANNELS packets                                           }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(Recipient.TelemetryInfoProvider.KnownChannels.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(Recipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.TelemetryInfoProvider.KnownChannels.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(Recipient.TelemetryInfoProvider,i),ConnectionData)
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL(Recipient.TelemetryInfoProvider),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CONFIGS packets                                            }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(Recipient.TelemetryInfoProvider.KnownConfigs.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(Recipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.TelemetryInfoProvider.KnownConfigs.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(Recipient.TelemetryInfoProvider,i),ConnectionData)
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL(Recipient.TelemetryInfoProvider),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_EVENT packets                                                    }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  DefferedOperations.AddOperation(ConnectionData,dotEventReg,Payload_ReadoutInteger(Packet))
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Index:  Integer;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Index := Payload_ReadoutInteger(Packet);
    If (Index >= 0) and (Index < Recipient.TelemetryInfoProvider.KnownEvents.Count) then
      DefferedOperations.AddOperation(ConnectionData,dotEventRegByIndex,Index)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
DefferedOperations.AddOperation(ConnectionData,dotEventRegAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  DefferedOperations.AddOperation(ConnectionData,dotEventUnreg,Payload_ReadoutInteger(Packet))
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
   DefferedOperations.AddOperation(ConnectionData,dotEventUnregIndex,Payload_ReadoutInteger(Packet))
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Index:  Integer;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Index := Payload_ReadoutInteger(Packet);
    If (Index >= 0) and (Index < Recipient.TelemetryInfoProvider.KnownEvents.Count) then
      DefferedOperations.AddOperation(ConnectionData,dotEventUnregByIndex,Index)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
DefferedOperations.AddOperation(ConnectionData,dotEventUnregAll);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Registered: Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Registered := Recipient.EventRegistered(Payload_ReadoutInteger(Packet));
    PacketsBuilder.ReusePacket(Packet,TC_PACKET_EVENT_REGISTERED,SizeOf(scs_event_t),SizeOf(Boolean),@Registered);
    Transmitter.SendPacket(Packet,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT(Recipient.RegisteredEvents.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX(Recipient,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.RegisteredEvents.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX(Recipient,i),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL(Recipient),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_CHANNEL packets                                                  }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr,True);
        Index := Ptr_ReadoutInteger(CurrPtr,True);
        ValueType := Ptr_ReadoutInteger(CurrPtr,True);
        Flags := Ptr_ReadoutInteger(CurrPtr,True);
        DefferedOperations.AddOperation(ConnectionData,dotChannelReg,Name,Index,ValueType,Flags);
      end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Index:  Integer;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Index := Payload_ReadoutInteger(Packet);
    If (Index >= 0) and (Index < Recipient.TelemetryInfoProvider.KnownChannels.Count) then
      DefferedOperations.AddOperation(ConnectionData,dotChannelRegByIndex,'',Index,SCS_VALUE_TYPE_INVALID)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Name: TelemetryString;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Name := Payload_ReadoutString(Packet);
    If Name <> '' then
      DefferedOperations.AddOperation(ConnectionData,dotChannelRegByName,Name,SCS_U32_NIL,SCS_VALUE_TYPE_INVALID)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  RegInfo:  TChannelRegisterAllOperationInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    RegInfo.RegPrimaryType := Ptr_ReadoutBoolean(CurrPtr,True);
    RegInfo.SecondarySelectionMask := Ptr_ReadoutInteger(CurrPtr,True);
    DefferedOperations.AddOperation(ConnectionData,dotChannelRegAll,RegInfo.RegPrimaryType,RegInfo.SecondarySelectionMask);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr,True);
        Index := Ptr_ReadoutInteger(CurrPtr,True);
        ValueType := Ptr_ReadoutInteger(CurrPtr,True);
        DefferedOperations.AddOperation(ConnectionData,dotChannelUnreg,Name,Index,ValueType,Flags);
      end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  DefferedOperations.AddOperation(ConnectionData,dotChannelUnregIndex,'',Payload_ReadoutInteger(Packet),SCS_VALUE_TYPE_INVALID)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Index:  Integer;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Index := Payload_ReadoutInteger(Packet);
    If (Index >= 0) and (Index < Recipient.TelemetryInfoProvider.KnownChannels.Count) then
      DefferedOperations.AddOperation(ConnectionData,dotChannelUnregByIndex,'',Index,SCS_VALUE_TYPE_INVALID)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  Name: TelemetryString;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    Name := Payload_ReadoutString(Packet);
    If Name <> '' then
      DefferedOperations.AddOperation(ConnectionData,dotChannelUnregByName,Name,SCS_U32_NIL,SCS_VALUE_TYPE_INVALID)
    else
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petWrongData),ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  DefferedOperations.AddOperation(ConnectionData,dotChannelUnregAll)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Channel:    TChannelInfo;
  Registered: Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        Registered := Recipient.ChannelRegistered(Name,Index,ValueType);
      end;
    PacketsBuilder.ReusePacket(Packet,TC_PACKET_CHANNEL_REGISTERED,CurrPtr,SizeOf(Boolean),@Registered);
    Transmitter.SendPacket(Packet,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(Recipient.RegisteredChannels.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(Recipient,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.RegisteredChannels.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(Recipient,i),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(Recipient),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.StoredChannelsValues.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL(Recipient.StoredChannelsValues[i]),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_CONFIG packets                                                   }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Config:   TStoredConfig;
  Stored:   Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Config do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        Stored := Recipient.ConfigStored(Name,Index);
      end;
    PacketsBuilder.ReusePacket(Packet,TC_PACKET_CONFIG_STORED,CurrPtr,SizeOf(Boolean),@Stored);
    Transmitter.SendPacket(Packet,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_COUNT(Recipient.StoredConfigs.Count),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX(Recipient,Payload_ReadoutInteger(Packet)),ConnectionData)
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  i:  Integer;
begin
For i := 0 to Pred(Recipient.RegisteredChannels.Count) do
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX(Recipient,i),ConnectionData)
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL(Recipient),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_LOG packets                                                      }
{------------------------------------------------------------------------------}

procedure TTelemetryCommServerReceiver.ProcessPacket_TC_PACKET_LOG_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  LogType:  scs_log_type_t;
  LogText:  String;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    LogType := Ptr_ReadoutInteger(CurrPtr);
    LogText := TelemetryStringDecode(Ptr_ReadoutString(CurrPtr));
    Recipient.Log(LogType,LogText);    
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;
{$ENDIF}
