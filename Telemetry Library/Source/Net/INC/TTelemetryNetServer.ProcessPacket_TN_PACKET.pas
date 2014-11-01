{$IFDEF Declaration_part}
// General communication packets.
{ Method processing TN_PACKET_PING packet.}
procedure ProcessPacket_TN_PACKET_PING(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_PING_RESPONSE packet.}
procedure ProcessPacket_TN_PACKET_PING_RESPONSE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_HI packet.}
procedure ProcessPacket_TN_PACKET_HI(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_VERSION packet.}
procedure ProcessPacket_TN_PACKET_VERSION(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_TELEMETRY_VERSION_GET packet.}
procedure ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_TELEMETRY_VERSION packet.}
procedure ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_READY packet.}
procedure ProcessPacket_TN_PACKET_READY(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_PARAM_GET packet.}
procedure ProcessPacket_TN_PACKET_PARAM_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_PARAM packet.}
procedure ProcessPacket_TN_PACKET_PARAM(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_BYE packet.}
procedure ProcessPacket_TN_PACKET_BYE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_MESSAGE packet.}
procedure ProcessPacket_TN_PACKET_MESSAGE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_PACKET packet.}
procedure ProcessPacket_TN_PACKET_PACKET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_RIGHTS_ADMIN_REQUEST packet.}
procedure ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_RIGHTS_ADMIN packet.}
procedure ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ERROR packet.}
procedure ProcessPacket_TN_PACKET_ERROR(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_RIGHTS_SUPERADMIN_REQUEST packet.}
procedure ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_RIGHTS_SUPERADMIN packet.}
procedure ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Admin level communication packets.
{ Method processing TN_PACKET_ADMIN_CLIENT_CONNECTING packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_REJECTED packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_CONNECTED packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_DISCONNECTED packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_CHANGE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_OPERATION packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_COUNT packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_INDEX packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CLIENT_ALL packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_COUNT packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_INDEX packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_ALL packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_CLEAR packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_ADD packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_REMOVE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_DELETE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_RELOAD packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_SAVE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_LIST_CHANGE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_SEND_MESSAGE packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_PASSWORD packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_CHANGE_PASSWORD packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_ADMIN_PARAM_SET packet.}
procedure ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Superadmin level communication packets.
{ Method processing TN_PACKET_SUPERADMIN_ADMIN_PASSWORD packet.}
procedure ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD packet.}
procedure ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Known events packets.
{ Method processing TN_PACKET_EVENT_KNOWN_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_COUNT packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_INDEX packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_KNOWN_ALL packet.}
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Known channels packets.
{ Method processing TN_PACKET_CHANNEL_KNOWN_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_COUNT packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_INDEX packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_KNOWN_ALL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Known configs packets.
{ Method processing TN_PACKET_CONFIG_KNOWN_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_COUNT packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_INDEX packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_KNOWN_ALL packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Events communication packets.
{ Method processing TN_PACKET_EVENT_REG packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REG_ALL packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_UNREG packet.}
procedure ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_UNREG_ALL packet.}
procedure ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_EVENT packet.}
procedure ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_COUNT packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_INDEX packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_EVENT_REGISTERED_ALL packet.}
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Channels communication packets.
{ Method processing TN_PACKET_CHANNEL_REG packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REG_ALL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_UNREG packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_UNREG_ALL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_CHANNEL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_CHANNEL_BUFFERED packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_COUNT packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_INDEX packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_REGISTERED_ALL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CHANNEL_STORED_SEND_ALL packet.}
procedure ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Configs communication packets.
{ Method processing TN_PACKET_CONFIG_CONFIG packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_COUNT_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_COUNT packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_INDEX_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_INDEX packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_INDEX_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_ALL_GET packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;
{ Method processing TN_PACKET_CONFIG_STORED_ALL packet.}
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

// Game log communication packets.
{ Method processing TN_PACKET_LOG_LOG packet.}
procedure ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer; Socket: TCustomWinSocket); virtual;

{$ENDIF}

{$IFDEF Implementation_part}
{-------------------------------------------------------------------------------
***  General communication packets  ********************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_PING(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_PING_RESPONSE,True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_PING_RESPONSE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_HI structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_HI(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  HashType: THashType;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    HashType := THashType(Ptr_ReadoutInteger(CurrPtr));
    If IsTextHash(Password,HashType,CurrPtr) then
      begin
        TNSCWinSocket(Socket).WaitingForPassword := False;
        SendTo(Socket,BuildPacket_TN_PACKET_VERSION(ServerVersion),True)
      end
    else DisconnectClient(Socket,drWrongPassword);
  end
else
  begin
    SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
    DisconnectClient(Socket,drWrongPassword);
  end;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_VERSION structure:
//  begin
//    server version    4B  (unsigned 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_VERSION(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_TELEMETRY_VERSION(fTelemetryRecipient),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_TELEMETRY_VERSION structure:
//  begin
//    telemetry version 4B      (scs_u32_t)
//    game ID           String  (variable size)
//    game version      4B      (scs_u32_t)
//    game name         String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_READY(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
TNSCWinSocket(Socket).ReadyToWork := True;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM_GET structure:
//  begin
//    parameter id    4B  (TParameterID)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_PARAM_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendServerSettings(TParameterID(Payload_ReadoutInteger(Packet)),Socket)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM structure:
//  begin
//    parameter id    4B  (TParameterID)
//    data            []  (variable size, can have zero size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_PARAM(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_BYE structure:
//  begin
//    disconnection reason  4B  (TDisconnectReason)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_BYE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior - but error should not be reported.
end;

//------------------------------------------------------------------------------

//  TN_PACKET_MESSAGE structure:
//  begin
//    message text    String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_MESSAGE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PACKET structure:
//  begin
//    payload   []  (data carried by packet, can be empty)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_PACKET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_ADMIN_REQUEST structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  HashType: THashType;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    HashType := THashType(Ptr_ReadoutInteger(CurrPtr));
    TNSCWinSocket(Socket).AdminRights := IsTextHash(AdminPassword,HashType,CurrPtr);
    SendTo(Socket,BuildPacket_TN_PACKET_RIGHTS_ADMIN(TNSCWinSocket(Socket).AdminRights),True);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_ADMIN structure:
//  begin
//    granted   1B    (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Ptr_WriteBoolean(CurrPtr,TNSCWinSocket(Socket).AdminRights);
    SendTo(Socket,Packet,False);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ERROR structure:
//  begin
//    packet id           4B  (unsigned 32bit integer)
//    packet time stamp   8B  (TDateTime)
//    error type          4B  (TPacketErrorType)
//    error code          4B  (unsigned 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ERROR(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior - but error should not be reported.
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  HashType: THashType;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    HashType := THashType(Ptr_ReadoutInteger(CurrPtr));
    TNSCWinSocket(Socket).SuperAdminRights := IsTextHash(SuperAdminPassword,HashType,CurrPtr);
    SendTo(Socket,BuildPacket_TN_PACKET_RIGHTS_SUPERADMIN(TNSCWinSocket(Socket).SuperAdminRights),True);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  PACKET_RIGHTS_SUPERADMIN structure:
//  begin
//    granted   1B    (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Ptr_WriteBoolean(CurrPtr,TNSCWinSocket(Socket).SuperAdminRights);
    SendTo(Socket,Packet,False);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

{-------------------------------------------------------------------------------
***  Admin level communication packets  ****************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_ADMIN_CLIENT_CONNECTING structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_REJECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//    in list       1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_CONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_DISCONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_CHANGE structure:
//  begin
//    client id             16B     (TGUID)
//    client IP             String  (variable size)
//    client port           4B      (unsigned 32bit integer)
//    waiting for password  1B      (boolean)
//    ready to work         1B      (boolean)
//    admin rights          1B      (boolean)
//    super-admin rights    1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_OPERATION structure:
//  begin
//    client id   16B     (TGUID)
//    operation    4B     (TClientOperation)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:    Pointer;
  ClientGUID: TGUID;
  Client:     TNSCWinSocket;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Ptr_ReadBuffer(CurrPTr,ClientGUID,SizeOf(ClientGUID));
    Client := GetClient(ClientGUID);
    If Assigned(Client) then
      begin
        case TClientOperation(Ptr_ReadoutInteger(CurrPtr)) of
          copDisconnect:  DisconnectClient(Client,drDisconnectedByAdmin);
          copMakeAdmin:   begin
                            Client.AdminRights := True;
                            SendTo(Socket,BuildPacket_TN_PACKET_RIGHTS_ADMIN(True),True);
                          end;
          copStripAdmin:  begin
                            Client.AdminRights := False;
                            SendTo(Socket,BuildPacket_TN_PACKET_RIGHTS_ADMIN(False),True);
                          end;
        end;
      end
    else
      SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownClient),True);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_CLIENT_COUNT(fServerSocket.Socket.ActiveConnections),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX(fServerSocket,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_INDEX structure:
//  begin
//    index                 4B      (signed 32bit integer)
//    client id             16B     (TGUID)
//    client IP             String  (variable size)
//    client port           4B      (unsigned 32bit integer)
//    waiting for password  1B      (boolean)
//    ready to work         1B      (boolean)
//    admin rights          1B      (boolean)
//    super-admin rights    1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fServerSocket.Socket.ActiveConnections - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_CLIENT_INDEX(fServerSocket,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_CLIENT_ALL(fServerSocket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      client id             16B     (TGUID)
//      client IP             String  (variable size)
//      client port           4B      (unsigned 32bit integer)
//      waiting for password  1B      (boolean)
//      ready to work         1B      (boolean)
//      admin rights          1B      (boolean)
//      super-admin rights    1B      (boolean)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_LIST_COUNT(fAddressList.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_LIST_INDEX(fAddressList,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX structure:
//  begin
//    index   4B      (signed 32bit integer)
//    item    String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fAddressList.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_LIST_INDEX(fAddressList,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_LIST_ALL(fAddressList),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ALL structure:
//  begin
//    count     4B        (signed 32bit integer, number of "item" fields)
//    item      String[]  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
fAddressList.Clear;
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ADD structure:
//  begin
//    item  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  fAddressList.Add(Payload_ReadoutString(Packet))
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_REMOVE structure:
//  begin
//    item  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  Index: Integer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    Index := fAddressList.IndexOf(Payload_ReadoutString(Packet));
    If Index >= 0 then fAddressList.Delete(Index);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_DELETE structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  Index: Integer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    Index := Payload_ReadoutInteger(Packet);
    If (Index >= 0) and (Index < fAddressList.Count) then
      fAddressList.Delete(Index)
    else
      SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petGeneric),True);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If SaveSettingsToFile then fAddressList.LoadFromFile;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If SaveSettingsToFile then fAddressList.SaveToFile;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_SEND_MESSAGE structure:
//  begin
//    client    16B     (TGUID)
//    message   String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:    Pointer;
  ClientGUID: TGUID;
  Client:     TNSCWinSocket;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Ptr_ReadBuffer(CurrPTr,ClientGUID,SizeOf(ClientGUID));
    Client := GetClient(ClientGUID);
    If Assigned(Client) then
      SendTo(Client,BuildPacket_TN_PACKET_MESSAGE(Ptr_ReadoutString(CurrPtr)),True)
    else
      SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownClient),True);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_ADMIN_PASSWORD(Password),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CHANGE_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  Password := Payload_ReadoutString(Packet)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PARAM_SET structure:
//  begin
//    parameter id    4B  (TParameterID)
//    value           []  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    case TParameterID(Ptr_ReadoutInteger(CurrPtr)) of
      pidSrvSendEvents:
        SendEvents := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendChannels:
        SendChannels := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendConfigs:
        SendConfigs := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvActiveMode:
        ActiveMode := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvBufferChannels:
        BufferChannels := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendFrameEvents:
        SendFrameEvents := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSaveSettingsToFile:
        begin
          SaveSettingsToFile := Ptr_ReadoutBoolean(CurrPtr);
          SendServerSettings(pidSrvSaveSettingsToFile);
        end;
      pidRecKeepUtilityEvents:
        begin
          fTelemetryRecipient.KeepUtilityEvents := Ptr_ReadoutBoolean(CurrPtr);
          SendServerSettings(pidRecKeepUtilityEvents);
        end;
      pidRecStoreConfigurations:
        begin
          fTelemetryRecipient.StoreConfigurations := Ptr_ReadoutBoolean(CurrPtr);
          SendServerSettings(pidRecStoreConfigurations);
        end;
      pidRecManageIndexedChannels:
        begin
          fTelemetryRecipient.ManageIndexedChannels := Ptr_ReadoutBoolean(CurrPtr);
          SendServerSettings(pidRecManageIndexedChannels);
        end;
      pidRecStoreChannelValues:
        begin
          fTelemetryRecipient.StoreChannelsValues := Ptr_ReadoutBoolean(CurrPtr);
          SendServerSettings(pidRecStoreChannelValues);
        end;
      pidLstListMode:
        begin
          fAddressList.ListMode := TAddressListMode(Ptr_ReadoutInteger(CurrPtr));
          SendServerSettings(pidLstListMode);
        end;
      pidLstListFileName:
        begin
          fAddressList.FileName := Ptr_ReadoutString(CurrPtr);
          SendServerSettings(pidLstListFileName);
        end;
    else
      SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownParameter),True);
    end;
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;


{-------------------------------------------------------------------------------
***  Superadmin level communication packets  ***********************************
-------------------------------------------------------------------------------}

//  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(AdminPassword),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  AdminPassword := Payload_ReadoutString(Packet)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;


{-------------------------------------------------------------------------------
***  Known events packets  *****************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_EVENT_KNOWN_COUNT(fTelemetryRecipient.TelemetryInfoProvider.KnownEvents.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_INDEX structure:
//  begin
//    index   4B      (signed 32bit integer)
//    event   4B      (scs_event_t)
//    name    String  (variable size)
//    valid   1B      (boolean)
//    utility 1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.TelemetryInfoProvider.KnownEvents.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_EVENT_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_EVENT_KNOWN_ALL(fTelemetryRecipient.TelemetryInfoProvider),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      event     4B      (scs_event_t)
//      name      String  (variable size)
//      valid     1B      (boolean)
//      utility   1B      (boolean)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Known channels packets  ***************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(fTelemetryRecipient.TelemetryInfoProvider.KnownChannels.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_INDEX structure:
//  begin
//    index             4B      (signed 32bit integer)
//    name              String  (variable size)
//    id                4B      (TChannelID)
//    primary type      4B      (scs_value_type_t)
//    secondary type    4B      (scs_value_type_t)
//    tertiary type     4B      (scs_value_type_t)
//    indexed           1B      (boolean)
//    index config      String  (variable size)
//    index config id   4B      (TItemID)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.TelemetryInfoProvider.KnownChannels.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL(fTelemetryRecipient.TelemetryInfoProvider),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      name:             String  (variable size)
//      id                4B      (TChannelID)
//      primary type      4B      (scs_value_type_t)
//      secondary type    4B      (scs_value_type_t)
//      tertiary type     4B      (scs_value_type_t)
//      indexed:          1B      (boolean)
//      index config      String  (variable size)
//      index config id   4B      (TItemID)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Known configs packets  ****************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_KNOWN_COUNT(fTelemetryRecipient.TelemetryInfoProvider.KnownConfigs.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_INDEX structure:
//  begin
//    index         4B      (signed 32bit integer)
//    name          String  (variable size)
//    id            4B      (TConfigID)
//    value type    4B      (scs_value_type_t)
//    indexed       1B      (boolean)
//    binded        1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.TelemetryInfoProvider.KnownConfigs.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_KNOWN_INDEX(fTelemetryRecipient.TelemetryInfoProvider,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL(fTelemetryRecipient.TelemetryInfoProvider),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_ALL structure:
//  begin
//    count     4B  (signed 32bit integer, number of info substructures)
//    info      substructure[]
//    begin
//      name:         String  (variable size)
//      id            4B      (TConfigID)
//      value type:   4B      (scs_value_type_t)
//      indexed:      1B      (boolean)
//      binded:       1B      (boolean)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Events communication packets  *********************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_EVENT_REG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  fDefferedOperations.AddOperation(Socket,dotEventRegister,Payload_ReadoutInteger(Packet))
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
fDefferedOperations.AddOperation(Socket,dotEventRegisterAll);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_UNREG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  fDefferedOperations.AddOperation(Socket,dotEventUnregister,Payload_ReadoutInteger(Packet))
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
fDefferedOperations.AddOperation(Socket,dotEventUnregisterAll);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_EVENT structure:
//  begin
//    event       4B  (scs_event_t)
//    data size   4B  (unsigned 32bit integer)
//    data        []  (variable size, can be 0)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED structure:
//  begin
//    event       4B  (scs_event_t)
//    registered  1B  (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  Event:    scs_event_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    Ptr_WriteBoolean(CurrPtr,fTelemetryRecipient.EventRegistered(Event));
    SendTo(Socket,Packet,False);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_EVENT_REGISTERED_COUNT(fTelemetryRecipient.RegisteredEvents.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX(fTelemetryRecipient,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX structure:
//  begin
//    index   4B  (signed 32bit integer)
//    event   4B  (scs_event_t)
//    utility 1B  (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.RegisteredEvents.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_EVENT_REGISTERED_INDEX(fTelemetryRecipient,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL(fTelemetryRecipient),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_ALL structure:
//  begin
//    count     4B    (signed 32bit integer, number of info substructures)
//    info  substructure[]
//    begin
//      event     4B    (scs_event_t)
//      utility   1B    (boolean)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Channels communication packets  *******************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_CHANNEL_REG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    flags         4B      (scs_u32_t)
//    result        4B      (scs_result_t)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        Flags := Ptr_ReadoutInteger(CurrPtr);
        fDefferedOperations.AddOperation(Socket,dotChannelRegister,Name,Index,ValueType,Flags);
      end;
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REG_ALL structure:
//  begin
//    primary types     1B  (boolean)
//    secondary types   1B  (boolean)
//    tertiary types    1B  (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  RegInfo:  TChannelRegisterAllOperationInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with RegInfo do
      begin
        RegisterPrimaryTypes := Ptr_ReadoutBoolean(CurrPtr);
        RegisterSecondaryTypes := Ptr_ReadoutBoolean(CurrPtr);
        RegisterTertiaryTypes := Ptr_ReadoutBoolean(CurrPtr);
        fDefferedOperations.AddOperation(Socket,dotChannelRegisterAll,RegisterPrimaryTypes,
                                         RegisterSecondaryTypes,RegisterTertiaryTypes);
      end;
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_UNREG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    result        4B      (scs_result_t)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        fDefferedOperations.AddOperation(Socket,dotChannelUnregister,Name,Index,ValueType);
      end;
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
fDefferedOperations.AddOperation(Socket,dotChannelUnregisterAll);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_CHANNEL structure:
//  begin
//    name        String  (variable size)
//    id          4B      (TChannelID)
//    index       4B      (scs_u32_t)
//    value type  4B      (scs_value_type_t)
//    value       []      (variable size data, actual size depends on value type)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_CHANNEL_BUFFERED structure:
//  begin
//    count   4B  (signed 32bit integer, number of info substructures)
//    info    substructure[]
//    begin
//      name        String  (variable size)
//      id          4B      (TChannelID)
//      index       4B      (scs_u32_t)
//      value type  4B      (scs_value_type_t)
//      value       []      (variable size data, actual size depends on value type)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    registered    1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  Channel:  TChannelInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Channel do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        Ptr_WriteBoolean(CurrPtr,fTelemetryRecipient.ChannelRegistered(Name,Index,ValueType));
      end;
    SendTo(Socket,Packet,False);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(fTelemetryRecipient.RegisteredChannels.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(fTelemetryRecipient,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_INDEX structure:
//  begin
//    list index        4B      (signed 32bit integer)
//    name              String  (variable size)
//    id                4B      (TChannelID)
//    index             4B      (scs_u32_t)
//    value type        4B      (scs_value_type_t)
//    flags             4B      (scs_u32_t)
//    index config id   4B      (TItemID)
//    value config id   4B      (TItemID)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.RegisteredChannels.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(fTelemetryRecipient,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(fTelemetryRecipient),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_ALL structure:
//  begin
//    count       4B    (signed 32bit integer, number of info substructures)
//    info        substructure[]
//    begin
//      name              String  (variable size)
//      id                4B      (TChannelID)
//      index             4B      (scs_u32_t)
//      value type        4B      (scs_value_type_t)
//      flags             4B      (scs_u32_t)
//      index config id   4B      (TItemID)
//      value config id   4B      (TItemID)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.StoredChannelsValues.Count - 1) do
  with fTelemetryRecipient.StoredChannelsValues[i] do
    SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name,ID,Index,Value),True);
end;


{-------------------------------------------------------------------------------
***  Configs communication packets  ********************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_CONFIG_CONFIG structure:
//  begin
//    name        String  (variable size)
//    id          4B      (TConfigID)
//    index       4B      (scs_u32_t)
//    value type  4B      (scs_value_type_t)
//    value       []      (variable size data, actual size depends on value type)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED structure:
//  begin
//    name      String  (variable size)
//    index     4B      (scs_u32_t)
//    stored    1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  Config:   TStoredConfig;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with Config do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        Ptr_WriteBoolean(CurrPtr,fTelemetryRecipient.ConfigStored(Name,Index));
      end;
    SendTo(Socket,Packet,False);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(fTelemetryRecipient.StoredConfigs.Count),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
If CheckPacketPayloadSize(Packet) then
  SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_STORED_INDEX(fTelemetryRecipient,Payload_ReadoutInteger(Packet)),True)
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_INDEX structure:
//  begin
//    list index    4B      (signed 32bit integer)
//    name          String  (variable size)
//    id            4B      (TConfigID)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    value         []B     (variable size)
//    binded        1B      (boolean)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  i:  Integer;
begin
For i := 0 to (fTelemetryRecipient.StoredConfigs.Count - 1) do
  SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_STORED_INDEX(fTelemetryRecipient,i),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
SendTo(Socket,BuildPacket_TN_PACKET_CONFIG_STORED_ALL(fTelemetryRecipient),True);
end;

//  TN_PACKET_CONFIG_STORED_ALL structure:
//  begin
//    count       4B    (signed 32bit integer, number of info substructures)
//    info        substructure[]
//    begin
//      name          String  (variable size)
//      id            4B      (TConfigID)
//      index         4B      (scs_u32_t)
//      value type    4B      (scs_value_type_t)
//      value         []B     (variable size)
//      binded        1B      (boolean)
//    end;
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer; Socket: TCustomWinSocket);
begin
// No defined behavior, this packet is not expected to be received.
SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Game log communication packets  *******************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_LOG_LOG structure:
//  begin
//    log type  4B      (scs_log_type_t)
//    log text  String  (variable size)
//  end;
procedure TTelemetryNetServer.ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer; Socket: TCustomWinSocket);
var
  CurrPtr:  Pointer;
  LogType:  scs_log_type_t;
  LogText:  String;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    LogType := Ptr_ReadoutInteger(CurrPtr);
    LogText := Ptr_ReadoutString(CurrPtr);
    fTelemetryRecipient.Log(LogType,LogText);
  end
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;
{$ENDIF}
