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

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_PING(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_PING_RESPONSE(GetPacketHeader(Packet).TimeStamp),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_PING_RESPONSE(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_PROTOCOL_VERSION_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_PROTOCOL_VERSION(ProtocolVersion),ConnectionData);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_PROTOCOL_VERSION(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If TProtocolVersion(Payload_ReadoutInteger(Packet)) <> ProtocolVersion then
  If Assigned(OnFatalError) then OnFatalError(Self,Packet,ConnectionData);
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_TELEMETRY_INFO_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_TELEMETRY_INFO(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_PACKET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_ERROR(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior - but error should not be reported.
end;

  
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_DEFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUndefinedBehaviour),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_EVENTS packets                                             }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.TelemetryInfoProvider.KnownEvents.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Index:      Integer;
  KnownEvent: TKnownEvent;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_KnownEvent(CurrPtr,KnownEvent,True);
        If Index < Recipient.TelemetryInfoProvider.KnownEvents.Count then
          Recipient.TelemetryInfoProvider.KnownEvents.ReplaceIndex(Index,KnownEvent.Event,KnownEvent.Name,KnownEvent.Valid,KnownEvent.Utility)
        else
          Recipient.TelemetryInfoProvider.KnownEvents.Add(KnownEvent.Event,KnownEvent.Name,KnownEvent.Valid,KnownEvent.Utility);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_EVENTS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  i,Count:    Integer;
  KnownEvent: TKnownEvent;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.TelemetryInfoProvider.KnownEvents.BeginUpdate;
    try
      Recipient.TelemetryInfoProvider.KnownEvents.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_KnownEvent(CurrPtr,KnownEvent,True);
          Recipient.TelemetryInfoProvider.KnownEvents.Add(KnownEvent.Event,KnownEvent.Name,KnownEvent.Valid,KnownEvent.Utility);
        end;
    finally
      Recipient.TelemetryInfoProvider.KnownEvents.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CHANNELS packets                                           }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.TelemetryInfoProvider.KnownChannels.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  Index:        Integer;
  KnownChannel: TKnownChannel;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_KnownChannel(CurrPtr,KnownChannel,True);
        If Index < Recipient.TelemetryInfoProvider.KnownChannels.Count then
          Recipient.TelemetryInfoProvider.KnownChannels.ReplaceIndex(Index,KnownChannel.Name,KnownChannel.PrimaryType,
            KnownChannel.SecondaryType,KnownChannel.TertiaryType,KnownChannel.Indexed,KnownChannel.IndexConfig,KnownChannel.MaxIndex)
        else
          Recipient.TelemetryInfoProvider.KnownChannels.Add(KnownChannel.Name,KnownChannel.PrimaryType,
            KnownChannel.SecondaryType,KnownChannel.TertiaryType,KnownChannel.Indexed,KnownChannel.IndexConfig,KnownChannel.MaxIndex);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CHANNELS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  i,Count:      Integer;
  KnownChannel: TKnownChannel;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.TelemetryInfoProvider.KnownChannels.BeginUpdate;
    try
      Recipient.TelemetryInfoProvider.KnownChannels.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_KnownChannel(CurrPtr,KnownChannel,True);
          Recipient.TelemetryInfoProvider.KnownChannels.Add(KnownChannel.Name,KnownChannel.PrimaryType,
            KnownChannel.SecondaryType,KnownChannel.TertiaryType,KnownChannel.Indexed,KnownChannel.IndexConfig,KnownChannel.MaxIndex);
        end;
    finally
      Recipient.TelemetryInfoProvider.KnownChannels.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_KNOWN_CONFIGS packets                                            }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.TelemetryInfoProvider.KnownConfigs.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  Index:        Integer;
  KnownConfig:  TKnownConfig;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_KnownConfig(CurrPtr,KnownConfig,True);
        If Index < Recipient.TelemetryInfoProvider.KnownConfigs.Count then
          Recipient.TelemetryInfoProvider.KnownConfigs.ReplaceIndex(Index,KnownConfig.Name,KnownConfig.ValueType,KnownConfig.Indexed,KnownConfig.Binded)
        else
          Recipient.TelemetryInfoProvider.KnownConfigs.Add(KnownConfig.Name,KnownConfig.ValueType,KnownConfig.Indexed,KnownConfig.Binded);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_KNOWN_CONFIGS_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  i,Count:      Integer;
  KnownConfig:  TKnownConfig;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.TelemetryInfoProvider.KnownConfigs.BeginUpdate;
    try
      Recipient.TelemetryInfoProvider.KnownConfigs.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_KnownConfig(CurrPtr,KnownConfig,True);
          Recipient.TelemetryInfoProvider.KnownConfigs.Add(KnownConfig.Name,KnownConfig.ValueType,KnownConfig.Indexed,KnownConfig.Binded);
        end;
    finally
      Recipient.TelemetryInfoProvider.KnownConfigs.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_EVENT packets                                                    }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  ResultCode: scs_result_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If (ResultCode = SCS_RESULT_ok) and not Recipient.EventRegistered(Event) then
      Recipient.RegisteredEvents.Add(Event,Recipient.TelemetryInfoProvider.KnownEvents.IsUtility(Event));
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnEventRegisterResult(Self,Event,ResultCode);      
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  ResultCode: scs_result_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnEventUnregisterResult(Self,Event,ResultCode);
    If (ResultCode = SCS_RESULT_ok) then Recipient.RegisteredEvents.Remove(Event);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_EVENT(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Event:    scs_event_t;
  DataConf: scs_telemetry_configuration_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    case Event of
      SCS_TELEMETRY_EVENT_frame_start:
        Recipient.DoOnEvent(Self,Event,CurrPtr,nil);
      SCS_TELEMETRY_EVENT_configuration:
        begin
          Ptr_Read_scs_telemetry_configuration(CurrPtr,DataConf,True,True,False);
          try
            Recipient.DoOnEvent(Self,Event,@DataConf,nil);
          finally
            scs_telemetry_configuration_free(DataConf,True);
          end;
        end
    else
      Recipient.DoOnEvent(Self,Event,nil,nil);
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  Registered: Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    Registered := Ptr_ReadoutBoolean(CurrPtr,True);
    If Registered <> Recipient.EventRegistered(Event) then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET,ConnectionData);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnEventRegistered(Self,Event,Registered);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.RegisteredEvents.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:          Pointer;
  Index:            Integer;
  RegisteredEvent:  TEventInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_EventInfo(CurrPtr,RegisteredEvent,True);
        If Index < Recipient.RegisteredEvents.Count then
          begin
            Recipient.RegisteredEvents.Contexts[Index]^.EventInfo := RegisteredEvent;
            Recipient.RegisteredEvents.DoChange;
          end
        else
          Recipient.RegisteredEvents.Add(RegisteredEvent.Event,RegisteredEvent.Utility);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_EVENT_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:          Pointer;
  i,Count:          Integer;
  RegisteredEvent:  TEventInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.RegisteredEvents.BeginUpdate;
    try
      Recipient.RegisteredEvents.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_EventInfo(CurrPtr,RegisteredEvent,True);
          Recipient.RegisteredEvents.Add(RegisteredEvent.Event,RegisteredEvent.Utility);
        end;
    finally
      Recipient.RegisteredEvents.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_CHANNEL packets                                                  }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Channel:    TChannelInfo;
  ResultCode: scs_result_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Channel.Name := Ptr_ReadoutString(CurrPtr,True);
    Channel.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Channel.ValueType := Ptr_ReadoutInteger(CurrPtr,True);
    Channel.Flags := Ptr_ReadoutInteger(CurrPtr,True);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If (ResultCode = SCS_RESULT_ok) and not Recipient.ChannelRegistered(Channel.Name,Channel.Index,Channel.ValueType) then
      Recipient.RegisteredChannels.Add(Channel.Name,Channel.Index,Channel.ValueType,Channel.Flags,
        Recipient.TelemetryInfoProvider.KnownChannels.ChannelIndexConfigID(Channel.Name));
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnChannelRegisterResult(Self,Channel.Name,Channel.Index,Channel.ValueType,Channel.Flags,ResultCode);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Channel:    TChannelInfo;
  ResultCode: scs_result_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Channel.Name := Ptr_ReadoutString(CurrPtr,True);
    Channel.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Channel.ValueType := Ptr_ReadoutInteger(CurrPtr,True);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnChannelUnregisterResult(Self,Channel.Name,Channel.Index,Channel.ValueType,ResultCode);
    If (ResultCode = SCS_RESULT_ok) then
      Recipient.RegisteredChannels.Remove(Channel.Name,Channel.Index,Channel.ValueType);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_CHANNEL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
  Value:    scs_value_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Channel.Name := Ptr_ReadoutString(CurrPtr,True);
    Channel.ID := Ptr_ReadoutInteger(CurrPtr,True);
    Channel.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Ptr_Read_scs_value(CurrPtr,Value,True,True);
    try
      If Value._type = SCS_VALUE_TYPE_INVALID then
        Recipient.DoOnChannel(Self,Channel.Name,Channel.ID,Channel.Index,nil,nil)
      else
        Recipient.DoOnChannel(Self,Channel.Name,Channel.ID,Channel.Index,@Value,nil);
    finally
      scs_value_free(Value);
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  i,Count:  Integer;
  Channel:  TChannelInfo;
  Value:    scs_value_t;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnBufferedChannels(Self,Count,CurrPtr);
    For i := 1 to Count do
      begin
        Channel.Name := Ptr_ReadoutString(CurrPtr,True);
        Channel.ID := Ptr_ReadoutInteger(CurrPtr,True);
        Channel.Index := Ptr_ReadoutInteger(CurrPtr,True);
        Ptr_Read_scs_value(CurrPtr,Value,True,True);
        try
          If Value._type = SCS_VALUE_TYPE_INVALID then
            Recipient.DoOnChannel(Self,Channel.Name,Channel.ID,Channel.Index,nil,nil)
          else
            Recipient.DoOnChannel(Self,Channel.Name,Channel.ID,Channel.Index,@Value,nil);
        finally
          scs_value_free(Value);
        end;
      end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:    Pointer;
  Channel:    TChannelInfo;
  Registered: Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Channel.Name := Ptr_ReadoutString(CurrPtr,True);
    Channel.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Channel.ValueType := Ptr_ReadoutInteger(CurrPtr,True);
    Registered := Ptr_ReadoutBoolean(CurrPtr,True);
    If Registered <> Recipient.ChannelRegistered(Channel.Name,Channel.Index,Channel.ValueType) then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET,ConnectionData);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnChannelRegistered(Self,Channel.Name,Channel.Index,Channel.ValueType,Registered);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.RegisteredChannels.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:            Pointer;
  Index:              Integer;
  RegisteredChannel:  TChannelInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_ChannelInfo(CurrPtr,RegisteredChannel,True);
        If Index < Recipient.RegisteredChannels.Count then
          begin
            Recipient.RegisteredChannels.Contexts[Index]^.ChannelInfo := RegisteredChannel;
            Recipient.RegisteredChannels.DoChange;
          end
        else
          Recipient.RegisteredChannels.Add(RegisteredChannel.Name,RegisteredChannel.Index,RegisteredChannel.ValueType,
                                           RegisteredChannel.Flags,RegisteredChannel.IndexConfigID);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:            Pointer;
  i,Count:            Integer;
  RegisteredChannel:  TChannelInfo;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.RegisteredChannels.BeginUpdate;
    try
      Recipient.RegisteredChannels.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_ChannelInfo(CurrPtr,RegisteredChannel,True);
          Recipient.RegisteredChannels.Add(RegisteredChannel.Name,RegisteredChannel.Index,
            RegisteredChannel.ValueType,RegisteredChannel.Flags,RegisteredChannel.IndexConfigID);
        end;
    finally
      Recipient.RegisteredChannels.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_CONFIG packets                                                   }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_CONFIG(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Config:   TStoredConfig;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Config.Name := Ptr_ReadoutString(CurrPtr,True);
    Config.ID := Ptr_ReadoutInteger(CurrPtr,True);
    Config.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Ptr_Read_scs_value_localized(CurrPtr,Config.Value,True,True);
    If Recipient.StoredConfigs.ChangeConfigValue(Config.ID,Config.Index,Config.Value) < 0 then
      Recipient.StoredConfigs.Add(Config.Name,Config.Index,Config.Value,Config.Binded);
    Recipient.DoOnConfig(Self,Config.Name,Config.ID,Config.Index,Config.Value);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  Config:   TStoredConfig;
  Stored:   Boolean;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Config.Name := Ptr_ReadoutString(CurrPtr,True);
    Config.Index := Ptr_ReadoutInteger(CurrPtr,True);
    Stored := Ptr_ReadoutBoolean(CurrPtr,True);
    If Stored <> Recipient.ConfigStored(Config.Name,Config.Index) then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET,ConnectionData);
    If Recipient is TTelemetryCommRemoteRecipient then
      TTelemetryCommRemoteRecipient(Recipient).DoOnConfigStored(Self,Config.Name,Config.Index,Stored);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;
//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_COUNT(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> Recipient.StoredConfigs.Count then
      Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET,ConnectionData)
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  Index:        Integer;
  StoredConfig: TStoredConfig;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr,True);
    If Index >= 0 then
      begin
        Ptr_Read_StoredConfig(CurrPtr,StoredConfig,True);
        If Index < Recipient.StoredConfigs.Count then
          begin
            Recipient.StoredConfigs.Pointers[Index]^ := StoredConfig;
            Recipient.StoredConfigs.DoChange;
          end
        else Recipient.StoredConfigs.Add(StoredConfig.Name,StoredConfig.Index,StoredConfig.Value,StoredConfig.Binded);
      end
    else Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET,ConnectionData);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_ALL_GET(var Packet: TPacketBuffer; ConnectionData: Pointer);
begin
// No defined behavior, this packet is not expected to be received.
Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petUnexpectedPacket),ConnectionData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_CONFIG_STORED_ALL(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:      Pointer;
  i,Count:      Integer;
  StoredConfig: TStoredConfig;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr,True);
    Recipient.StoredConfigs.BeginUpdate;
    try
      Recipient.StoredConfigs.Clear;
      For i := 1 to Count do
        begin
          Ptr_Read_StoredConfig(CurrPtr,StoredConfig,True);
          Recipient.StoredConfigs.Add(StoredConfig.Name,StoredConfig.Index,StoredConfig.Value,StoredConfig.Binded);
        end;
    finally
      Recipient.StoredConfigs.EndUpdate;
    end;
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;


{------------------------------------------------------------------------------}
{   TC_PREFIX_LOG packets                                                      }
{------------------------------------------------------------------------------}

procedure TTelemetryCommClientReceiver.ProcessPacket_TC_PACKET_LOG_LOG(var Packet: TPacketBuffer; ConnectionData: Pointer);
var
  CurrPtr:  Pointer;
  LogType:  scs_log_type_t;
  LogText:  String;
begin
If PacketsBuilder.CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    LogType := Ptr_ReadoutInteger(CurrPtr,True);
    LogText := TelemetryStringDecode(Ptr_ReadoutString(CurrPtr,True));
    Recipient.DoOnLog(Self,LogType,LogText);
  end
else
  Transmitter.SendPacket(PacketsBuilder.BuildPacket_TC_PACKET_ERROR(Packet,petPacketTooSmall),ConnectionData);
end;
{$ENDIF}
