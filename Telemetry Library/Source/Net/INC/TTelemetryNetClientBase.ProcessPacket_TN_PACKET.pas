{$IFDEF Declaration_part}
// General communication packets.
procedure ProcessPacket_TN_PACKET_PING(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_PING_RESPONSE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_HI(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_VERSION(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_READY(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_PARAM_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_PARAM(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_BYE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_MESSAGE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_PACKET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ERROR(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet: TPacketBuffer); virtual;

// Admin level communication packets.
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet: TPacketBuffer); virtual;

// Superadmin level communication packets.
procedure ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet: TPacketBuffer); virtual;

// Known events packets.
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer); virtual;

// Known channels packets.
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer); virtual;

// Known configs packets.
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer); virtual;

// Events communication packets.
procedure ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer); virtual;

// Channels communication packets.
procedure ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet: TPacketBuffer); virtual;

// Configs communication packets.
procedure ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet: TPacketBuffer); virtual;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer); virtual;

// Game log communication packets.
procedure ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer); virtual;
{$ENDIF}

//==============================================================================

{$IFDEF Implementation_part}
{-------------------------------------------------------------------------------
***  General communication packets  ********************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_PING(Packet: TPacketBuffer);
begin
Send(BuildPacket_TN_PACKET_PING_RESPONSE,True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_PING_RESPONSE(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_HI structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_HI(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_VERSION structure:
//  begin
//    server version    4B  (unsigned 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_VERSION(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    fServerVersion := Payload_ReadoutInteger(Packet);
    If not SupportsServerVersion(fServerVersion) then Disconnect(drServerUnsupported)
      else
        begin
          Send(BuildPacket_TN_PACKET_TELEMETRY_VERSION_GET,True);
          IncreaseCompletionCounter;
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_TELEMETRY_VERSION structure:
//  begin
//    telemetry version 4B      (scs_u32_t)
//    game ID           String  (variable size)
//    game version      4B      (scs_u32_t)
//    game name         String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet: TPacketBuffer);
var
  CurrPtr: Pointer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    fTelemetryVersion := Ptr_ReadoutInteger(CurrPtr);
    fGameID := Ptr_ReadoutString(CurrPtr);
    fGameVersion := Ptr_ReadoutInteger(CurrPtr);
    fGameName := Ptr_ReadoutString(CurrPtr);
    If SupportsTelemetryAndGameVersion(fTelemetryVersion,scs_string_t(fGameID),fGameVersion) then
      begin
        Send(BuildPacket_TN_PACKET_READY,True);
        UtilityObjectsFill;
        GetServerSettings;
        IncreaseCompletionCounter;
      end
    else Disconnect(drTelemetryUnsupported);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_READY(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM_GET structure:
//  begin
//    parameter id    4B  (TParameterID)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_PARAM_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PARAM structure:
//  begin
//    parameter id    4B  (TParameterID)
//    data            []  (variable size, can have zero size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_PARAM(Packet: TPacketBuffer);
var
  CurrPtr:  Pointer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    case TParameterID(Ptr_ReadoutInteger(CurrPtr)) of
      pidSrvSendEvents:             rsSrvSendEvents := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendChannels:           rsSrvSendChannels := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendConfigs:            rsSrvSendConfigs := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvActiveMode:             rsSrvActiveMode := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvBufferChannels:         rsSrvBufferChannels := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSendFrameEvents:        rsSrvSendFrameEvents := Ptr_ReadoutBoolean(CurrPtr);
      pidSrvSaveSettingsToFile:     rsSrvSaveSettingsToFile := Ptr_ReadoutBoolean(CurrPtr);
      pidRecKeepUtilityEvents:      rsRecKeepUtilityEvents := Ptr_ReadoutBoolean(CurrPtr);
      pidRecStoreConfigurations:    rsRecStoreConfigurations := Ptr_ReadoutBoolean(CurrPtr);
      pidRecManageIndexedChannels:  rsRecManageIndexedChannels := Ptr_ReadoutBoolean(CurrPtr);
      pidRecStoreChannelValues:     rsRecStoreChannelValues := Ptr_ReadoutBoolean(CurrPtr);
      pidLstListMode:               rsLstListMode := TAddressListMode(Ptr_ReadoutInteger(CurrPtr));
      pidLstListFileName:           rsLstListFileName := Ptr_ReadoutString(CurrPtr);
    else
      Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnknownParameter),True);
    end;
    If Assigned(fOnServerSettings) then fOnServerSettings(Self);
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_BYE structure:
//  begin
//    disconnection reason  4B  (TDisconnectReason)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_BYE(Packet: TPacketBuffer);
begin
// No defined behavior - but error should not be reported.
end;

//------------------------------------------------------------------------------

//  TN_PACKET_MESSAGE structure:
//  begin
//    message text    String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_MESSAGE(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_PACKET structure:
//  begin
//    payload   []  (data carried by packet, can be empty)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_PACKET(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_ADMIN_REQUEST structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_ADMIN structure:
//  begin
//    granted   1B    (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ERROR structure:
//  begin
//    packet id           4B  (unsigned 32bit integer)
//    packet time stamp   8B  (TDateTime)
//    error type          4B  (TPacketErrorType)
//    error code          4B  (unsigned 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ERROR(Packet: TPacketBuffer);
begin
// No defined behavior - but error should not be reported.
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST structure:
//  begin
//    hash type       4B  (THashType)
//    password hash   []  (password hash)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_RIGHTS_SUPERADMIN structure:
//  begin
//    granted   1B    (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_REJECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//    in list       1B      (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_CONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_DISCONNECTED structure:
//  begin
//    client id     16B     (TGUID)
//    client IP     String  (variable size)
//    client port   4B      (unsigned 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_OPERATION structure:
//  begin
//    client id   16B     (TGUID)
//    operation    4B     (TClientOperation)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CLIENT_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_INDEX structure:
//  begin
//    index   4B      (signed 32bit integer)
//    item    String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ALL structure:
//  begin
//    count     4B        (signed 32bit integer, number of "item" fields)
//    item      String[]  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_ADD structure:
//  begin
//    item  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_REMOVE structure:
//  begin
//    item  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_LIST_DELETE structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_SEND_MESSAGE structure:
//  begin
//    client    16B     (TGUID)
//    message   String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_CHANGE_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_ADMIN_PARAM_SET structure:
//  begin
//    parameter id    4B  (TParameterID)
//    value           []  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Superadmin level communication packets  ***********************************
-------------------------------------------------------------------------------}

//  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD structure:
//  begin
//    password  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;


{-------------------------------------------------------------------------------
***  Known events packets  *****************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;


{-------------------------------------------------------------------------------
***  Known channels packets  ***************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;


{-------------------------------------------------------------------------------
***  Known configs packets  ****************************************************
-------------------------------------------------------------------------------}

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_KNOWN_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;


{-------------------------------------------------------------------------------
***  Events communication packets  *********************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_EVENT_REG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_UNREG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_EVENT structure:
//  begin
//    event       4B  (scs_event_t)
//    data size   4B  (unsigned 32bit integer)
//    data        []  (variable size, can be 0)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED structure:
//  begin
//    event       4B  (scs_event_t)
//    registered  1B  (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX structure:
//  begin
//    index   4B  (signed 32bit integer)
//    event   4B  (scs_event_t)
//    utility 1B  (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REG_ALL structure:
//  begin
//    primary types     1B  (boolean)
//    secondary types   1B  (boolean)
//    tertiary types    1B  (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_UNREG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    result        4B      (scs_result_t)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    registered    1B      (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
//    end;
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED structure:
//  begin
//    name      String  (variable size)
//    index     4B      (scs_u32_t)
//    stored    1B      (boolean)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_INDEX_GET structure:
//  begin
//    index   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet: TPacketBuffer);
begin
// No defined behavior, this packet is not expected to be received.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUnexpectedPacket),True);
end;

//------------------------------------------------------------------------------

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
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;

{-------------------------------------------------------------------------------
***  Game log communication packets  *******************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_LOG_LOG structure:
//  begin
//    log type  4B      (scs_log_type_t)
//    log text  String  (variable size)
//  end;
procedure TTelemetryNetClientBase.ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;
{$ENDIF}
