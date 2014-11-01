{$IFDEF Declaration_part}
// Known events packets.

procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer); override;

// Known channels packets.
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer); override;

// Known configs packets.
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer); override;

// Events communication packets.
procedure ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer); override;

// Channels communication packets.
procedure ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer); override;

// Configs communication packets.
procedure ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer); override;
procedure ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer); override;

// Game log communication packets.
procedure ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer); override;
{$ENDIF}

//==============================================================================

{$IFDEF Implementation_part}
{-------------------------------------------------------------------------------
***  Known events packets  *****************************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_EVENT_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fTelemetryInfoProvider.KnownEvents.Count then
      Send(BuildPacket_TN_PACKET_EVENT_KNOWN_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Index:      Integer;
  KnownEvent: TKnownEvent;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr);
    If Index >= 0 then
      If Index < fTelemetryInfoProvider.KnownEvents.Count then
        begin
          with fTelemetryInfoProvider.KnownEvents.Pointers[Index]^ do
            begin
              Event := Ptr_ReadoutInteger(CurrPtr);
              Name := Ptr_ReadoutString(CurrPtr);
              Valid := Ptr_ReadoutBoolean(CurrPtr);
              Utility := Ptr_ReadoutBoolean(CurrPtr);
            end;
          fTelemetryInfoProvider.KnownEvents.DoChange;
        end
      else
        begin
          If Index = fTelemetryInfoProvider.KnownEvents.Count then
            begin
              with KnownEvent do
                begin
                  Event := Ptr_ReadoutInteger(CurrPtr);
                  Name := Ptr_ReadoutString(CurrPtr);
                  Valid := Ptr_ReadoutBoolean(CurrPtr);
                  Utility := Ptr_ReadoutBoolean(CurrPtr);
                  fTelemetryInfoProvider.KnownEvents.Add(Event,Name,Valid,Utility);
                end;
            end
          else Send(BuildPacket_TN_PACKET_EVENT_KNOWN_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Count:      Integer;
  i:          Integer;
  KnownEvent: TKnownEvent;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fTelemetryInfoProvider.KnownEvents.BeginUpdate;
    try
      fTelemetryInfoProvider.KnownEvents.Clear;
      For i := 1 to Count do
        with KnownEvent do
          begin
            Event := Ptr_ReadoutInteger(CurrPtr);
            Name := Ptr_ReadoutString(CurrPtr);
            Valid := Ptr_ReadoutBoolean(CurrPtr);
            Utility := Ptr_ReadoutBoolean(CurrPtr);
            fTelemetryInfoProvider.KnownEvents.Add(Event,Name,Valid,Utility);
          end;
    finally
      fTelemetryInfoProvider.KnownEvents.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;


{-------------------------------------------------------------------------------
***  Known channels packets  ***************************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_CHANNEL_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fTelemetryInfoProvider.KnownChannels.Count then
      Send(BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Index:        Integer;
  KnownChannel: TKnownChannel;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr);
    If Index >= 0 then
      If Index < fTelemetryInfoProvider.KnownChannels.Count then
        begin
          with fTelemetryInfoProvider.KnownChannels.Pointers[Index]^ do
            begin
              Name := Ptr_ReadoutString(CurrPtr);
              ID := Ptr_ReadoutInteger(CurrPtr);
              PrimaryType := Ptr_ReadoutInteger(CurrPtr);
              SecondaryType := Ptr_ReadoutInteger(CurrPtr);
              TertiaryType:= Ptr_ReadoutInteger(CurrPtr);
              Indexed := Ptr_ReadoutBoolean(CurrPtr);
              IndexConfig := Ptr_ReadoutString(CurrPtr);
              IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
            end;
          fTelemetryInfoProvider.KnownChannels.DoChange;
        end
      else
        begin
          If Index = fTelemetryInfoProvider.KnownChannels.Count then
            begin
              with KnownChannel do
                begin
                  Name := Ptr_ReadoutString(CurrPtr);
                  ID := Ptr_ReadoutInteger(CurrPtr);
                  PrimaryType := Ptr_ReadoutInteger(CurrPtr);
                  SecondaryType := Ptr_ReadoutInteger(CurrPtr);
                  TertiaryType:= Ptr_ReadoutInteger(CurrPtr);
                  Indexed := Ptr_ReadoutBoolean(CurrPtr);
                  IndexConfig := Ptr_ReadoutString(CurrPtr);
                  IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
                  fTelemetryInfoProvider.KnownChannels.Add(Name,PrimaryType,
                    SecondaryType,TertiaryType,Indexed,IndexConfig);
                end;
            end
          else Send(BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Count:        Integer;
  i:            Integer;
  KnownChannel: TKnownChannel;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fTelemetryInfoProvider.KnownChannels.BeginUpdate;
    try
      fTelemetryInfoProvider.KnownChannels.Clear;
      For i := 1 to Count do
        with KnownChannel do
          begin
            Name := Ptr_ReadoutString(CurrPtr);
            ID := Ptr_ReadoutInteger(CurrPtr);
            PrimaryType := Ptr_ReadoutInteger(CurrPtr);
            SecondaryType := Ptr_ReadoutInteger(CurrPtr);
            TertiaryType:= Ptr_ReadoutInteger(CurrPtr);
            Indexed := Ptr_ReadoutBoolean(CurrPtr);
            IndexConfig := Ptr_ReadoutString(CurrPtr);
            IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
            fTelemetryInfoProvider.KnownChannels.Add(Name,PrimaryType,
              SecondaryType,TertiaryType,Indexed,IndexConfig);
          end;
    finally
      fTelemetryInfoProvider.KnownChannels.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;


{-------------------------------------------------------------------------------
***  Known configs packets  ****************************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_CONFIG_KNOWN_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fTelemetryInfoProvider.KnownConfigs.Count then
      Send(BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Index:        Integer;
  KnownConfig:  TKnownConfig;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Index := Ptr_ReadoutInteger(CurrPtr);
    If Index >= 0 then
      If Index < fTelemetryInfoProvider.KnownConfigs.Count then
        begin
          with fTelemetryInfoProvider.KnownConfigs.Pointers[Index]^ do
            begin
              Name := Ptr_ReadoutString(CurrPtr);
              ID := Ptr_ReadoutInteger(CurrPtr);
              ValueType := Ptr_ReadoutInteger(CurrPtr);
              Indexed := Ptr_ReadoutBoolean(CurrPtr);
              Binded := Ptr_ReadoutBoolean(CurrPtr);
            end;
          fTelemetryInfoProvider.KnownConfigs.DoChange;
        end
      else
        begin
          If Index = fTelemetryInfoProvider.KnownConfigs.Count then
            begin
              with KnownConfig do
                begin
                  Name := Ptr_ReadoutString(CurrPtr);
                  ID := Ptr_ReadoutInteger(CurrPtr);
                  ValueType := Ptr_ReadoutInteger(CurrPtr);
                  Indexed := Ptr_ReadoutBoolean(CurrPtr);
                  Binded := Ptr_ReadoutBoolean(CurrPtr);
                  fTelemetryInfoProvider.KnownConfigs.Add(Name,ValueType,Indexed,Binded);
                end;
            end
          else Send(BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Count:        Integer;
  i:            Integer;
  KnownConfig:  TKnownConfig;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fTelemetryInfoProvider.KnownConfigs.BeginUpdate;
    try
      fTelemetryInfoProvider.KnownConfigs.Clear;    
      For i := 1 to Count do
        with KnownConfig do
          begin
            Name := Ptr_ReadoutString(CurrPtr);
            ID := Ptr_ReadoutInteger(CurrPtr);
            ValueType := Ptr_ReadoutInteger(CurrPtr);
            Indexed := Ptr_ReadoutBoolean(CurrPtr);
            Binded := Ptr_ReadoutBoolean(CurrPtr);
            fTelemetryInfoProvider.KnownConfigs.Add(Name,ValueType,Indexed,Binded);
          end;
    finally
      fTelemetryInfoProvider.KnownConfigs.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;


{-------------------------------------------------------------------------------
***  Events communication packets  *********************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_EVENT_REG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_REG(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  ResultCode: scs_result_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If Assigned(fOnEventRegister) then fOnEventRegister(Self,Event,ResultCode);
    If (ResultCode = SCS_RESULT_ok) and not EventRegistered(Event) then
      fRegisteredEvents.Add(Event,fTelemetryInfoProvider.KnownEvents.IsUtility(Event));
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_UNREG structure:
//  begin
//    event   4B  (scs_event_t)
//    result  4B  (scs_result_t)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_UNREG(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  ResultCode: scs_result_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    ResultCode := Ptr_ReadoutInteger(CurrPtr);
    If Assigned(fOnEventUnregister) then fOnEventUnregister(Self,Event,ResultCode);
    If ResultCode = SCS_RESULT_ok then fRegisteredEvents.Remove(Event);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_EVENT structure:
//  begin
//    event       4B  (scs_event_t)
//    data size   4B  (unsigned 32bit integer)
//    data        []  (variable size, can be 0)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_EVENT(Packet: TPacketBuffer);
var
  CurrPtr:  Pointer;
  Event:    scs_event_t;
  DataSize: Integer;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    DataSize := Ptr_ReadoutInteger(CurrPtr);
    If (Event = SCS_TELEMETRY_EVENT_configuration) and ParseConfigurationEvents then
      ProcessConfigurationEvent(CurrPtr);
    If DataSize <= 0 then CurrPtr := nil;
    If Assigned(fOnEvent) then fOnEvent(Self,Event,CurrPtr);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED structure:
//  begin
//    event       4B  (scs_event_t)
//    registered  1B  (boolean)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Event:      scs_event_t;
  Registered: Boolean;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Event := Ptr_ReadoutInteger(CurrPtr);
    Registered := Ptr_ReadoutBoolean(CurrPtr);
    If Registered <> EventRegistered(Event) then
      Send(BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET,True);
    If Assigned(fOnEventRegistered) then fOnEventRegistered(Self,Event,Registered);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fRegisteredEvents.Count then
      Send(BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_EVENT_REGISTERED_INDEX structure:
//  begin
//    index   4B  (signed 32bit integer)
//    event   4B  (scs_event_t)
//    utility 1B  (boolean)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  ListIndex:  Integer;
  EventInfo:  TEventInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    ListIndex := Ptr_ReadoutInteger(CurrPtr);
    If ListIndex >= 0 then
      If ListIndex < fRegisteredEvents.Count then
        begin
          with fRegisteredEvents.Contexts[ListIndex]^ do
            begin
              EventInfo.Event := Ptr_ReadoutInteger(CurrPtr);
              EventInfo.Utility := Ptr_ReadoutBoolean(CurrPtr);
            end;
          fRegisteredEvents.DoChange;
        end
      else
        begin
          If ListIndex = fRegisteredEvents.Count then
            begin
              with EventInfo do
                begin
                  Event := Ptr_ReadoutInteger(CurrPtr);
                  Utility := Ptr_ReadoutBoolean(CurrPtr);
                  fRegisteredEvents.Add(Event,Utility);
                end;
            end
          else Send(BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Count:      Integer;
  i:          Integer;
  EventInfo:  TEventInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fRegisteredEvents.BeginUpdate;
    try
      fRegisteredEvents.Clear;
      For i := 1 to Count do
        with EventInfo do
          begin
            Event := Ptr_ReadoutInteger(CurrPtr);
            Utility := Ptr_ReadoutBoolean(CurrPtr);
            fRegisteredEvents.Add(Event,Utility);
          end;
    finally
      fRegisteredEvents.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_REG(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
  ResultCode:   scs_result_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with ChannelInfo do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        Flags := Ptr_ReadoutInteger(CurrPtr);
        ResultCode := Ptr_ReadoutInteger(CurrPtr);
        If Assigned(fOnChannelRegister) then
          fOnChannelRegister(Self,Name,Index,ValueType,Flags,ResultCode);
        If (ResultCode = SCS_RESULT_ok) and not ChannelRegistered(Name,Index,ValueType) then
          fRegisteredChannels.Add(Name,Index,ValueType,Flags,
            fTelemetryInfoProvider.KnownChannels.ChannelIndexConfigID(Name));
      end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_UNREG structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    result        4B      (scs_result_t)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
  ResultCode:   scs_result_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with ChannelInfo do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        ResultCode := Ptr_ReadoutInteger(CurrPtr);
        If Assigned(fOnChannelUnregister) then fOnChannelUnregister(Self,Name,Index,ValueType,ResultCode);
        If ResultCode = SCS_RESULT_ok then fRegisteredChannels.Remove(Name,Index,ValueType)
      end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
  TempString:   String;
  TempValue:    scs_value_t;
  TempValuePtr: p_scs_value_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Assigned(fOnChannel) then
      with ChannelInfo do
        begin
          CurrPtr := GetPayloadAddress(Packet);
          Name := Ptr_ReadoutString(CurrPtr);
          ID := Ptr_ReadoutInteger(CurrPtr);
          Index := Ptr_ReadoutInteger(CurrPtr);
          TempValue._type := Ptr_ReadoutInteger(CurrPtr);
          case TempValue._type of
            SCS_VALUE_TYPE_string:
              begin
                TempString := Ptr_ReadoutString(CurrPtr);
                TempValue.value_string.value := TelemetryStringToAPIString(TempString);
              end;
          else
            Ptr_ReadBuffer(CurrPtr,TempValue,SizeOf(TempValue));
          end;
          If TempValue._type = SCS_VALUE_TYPE_INVALID then TempValuePtr := nil
            else TempValuePtr := @TempValue;
          fOnChannel(Self,Name,ID,Index,TempValuePtr);
          APIStringFree(TempValue.value_string.value);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Count:        Integer;
  i:            Integer;
  ChannelInfo:  TChannelInfo;
  TempString:   String;
  TempValue:    scs_value_t;
  TempValuePtr: p_scs_value_t;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    If Assigned(fOnBufferedChannels) then fOnBufferedChannels(Self,Count,CurrPtr)
      else If Assigned(fOnChannel) then
        For i := 1 to Count do
          with ChannelInfo do
            begin
              Name := Ptr_ReadoutString(CurrPtr);
              ID := Ptr_ReadoutInteger(CurrPtr);
              Index := Ptr_ReadoutInteger(CurrPtr);
              TempValue._type := Ptr_ReadoutInteger(CurrPtr);
              case TempValue._type of
                SCS_VALUE_TYPE_string:
                  begin
                    TempString := Ptr_ReadoutString(CurrPtr);
                    TempValue.value_string.value := TelemetryStringtoAPIString(TempString);
                  end;
              else
                Ptr_ReadBuffer(CurrPtr,TempValue,SizeOf(TempValue));
              end;
              If TempValue._type = SCS_VALUE_TYPE_INVALID then TempValuePtr := nil
                else TempValuePtr := @TempValue;
              fOnChannel(Self,Name,ID,Index,TempValuePtr);
              APIStringFree(TempValue.value_string.value);
            end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED structure:
//  begin
//    name          String  (variable size)
//    index         4B      (scs_u32_t)
//    value type    4B      (scs_value_type_t)
//    registered    1B      (boolean)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ChannelInfo:  TChannelInfo;
  Registered:   Boolean;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with ChannelInfo do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        ValueType := Ptr_ReadoutInteger(CurrPtr);
        Registered := Ptr_ReadoutBoolean(CurrPtr);
        If Registered <> ChannelRegistered(Name,Index,ValueType) then
          Send(BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET,True);
        If Assigned(fOnChannelRegistered) then fOnChannelRegistered(Self,Name,Index,ValueType,Registered);
      end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CHANNEL_REGISTERED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fRegisteredChannels.Count then
      Send(BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ListIndex:    Integer;
  ChannelInfo:  TChannelInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    ListIndex := Ptr_ReadoutInteger(CurrPtr);
    If ListIndex >= 0 then
      If ListIndex < fRegisteredChannels.Count then
        begin
          with fRegisteredChannels.Contexts[ListIndex]^ do
            begin
              ChannelInfo.Name := Ptr_ReadoutString(CurrPtr);
              ChannelInfo.ID := Ptr_ReadoutInteger(CurrPtr);
              ChannelInfo.Index := Ptr_ReadoutInteger(CurrPtr);
              ChannelInfo.ValueType := Ptr_ReadoutInteger(CurrPtr);
              ChannelInfo.Flags := Ptr_ReadoutInteger(CurrPtr);
              ChannelInfo.IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
            end;
          fRegisteredChannels.DoChange;
        end
      else
        begin
          If ListIndex = fRegisteredChannels.Count then
            begin
              with ChannelInfo do
                begin
                  Name := Ptr_ReadoutString(CurrPtr);
                  ID := Ptr_ReadoutInteger(CurrPtr);
                  Index := Ptr_ReadoutInteger(CurrPtr);
                  ValueType := Ptr_ReadoutInteger(CurrPtr);
                  Flags := Ptr_ReadoutInteger(CurrPtr);
                  IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
                  fRegisteredChannels.Add(Name,Index,ValueType,Flags,IndexConfigID);
                end;
            end
          else Send(BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  Count:        Integer;
  i:            Integer;
  ChannelInfo:  TChannelInfo;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fRegisteredChannels.BeginUpdate;
    try
      fRegisteredChannels.Clear;
      For i := 1 to Count do
        with ChannelInfo do
          begin
            Name := Ptr_ReadoutString(CurrPtr);
            ID := Ptr_ReadoutInteger(CurrPtr);
            Index := Ptr_ReadoutInteger(CurrPtr);
            ValueType := Ptr_ReadoutInteger(CurrPtr);
            Flags := Ptr_ReadoutInteger(CurrPtr);
            IndexConfigID := Ptr_ReadoutInteger(CurrPtr);
            fRegisteredChannels.Add(Name,Index,ValueType,Flags,IndexConfigID);
          end;
    finally
      fRegisteredChannels.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet: TPacketBuffer);
var
  CurrPtr:      Pointer;
  ConfigInfo:   TStoredConfig;
begin
If CheckPacketPayloadSize(Packet) then
  If not ParseConfigurationEvents then
    begin
      CurrPtr := GetPayloadAddress(Packet);
      with ConfigInfo do
        begin
          Name := Ptr_ReadoutString(CurrPtr);
          ID := Ptr_ReadoutInteger(CurrPtr);
          Index := Ptr_ReadoutInteger(CurrPtr);
          Value.ValueType := Ptr_ReadoutInteger(CurrPtr);
          case Value.ValueType of
            SCS_VALUE_TYPE_string:
              Value.StringData := Ptr_ReadoutString(CurrPtr);
          else
            Ptr_ReadBuffer(CurrPtr,Value.BinaryData,SizeOf(Value.BinaryData));
          end;
          If fStoredConfigs.ChangeConfigValue(ID,Index,Value) < 0 then
            fStoredConfigs.Add(Name,Index,Value,fTelemetryInfoProvider.KnownConfigs.IsBinded(Name));
          If Assigned(fOnConfig) then fOnConfig(Self,Name,ID,Index,Value);
        end;
    end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED structure:
//  begin
//    name      String  (variable size)
//    index     4B      (scs_u32_t)
//    stored    1B      (boolean)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_STORED(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  ConfigInfo: TStoredConfig;
  Stored:     Boolean;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    with ConfigInfo do
      begin
        Name := Ptr_ReadoutString(CurrPtr);
        Index := Ptr_ReadoutInteger(CurrPtr);
        Stored := Ptr_ReadoutBoolean(CurrPtr);
        If Stored <> ConfigStored(Name,Index) then
          Send(BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET,True);
        If Assigned(fOnConfigStored) then fOnConfigStored(Self,Name,Index,Stored);
      end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

//------------------------------------------------------------------------------

//  TN_PACKET_CONFIG_STORED_COUNT structure:
//  begin
//    count   4B  (signed 32bit integer)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet: TPacketBuffer);
begin
If CheckPacketPayloadSize(Packet) then
  begin
    If Payload_ReadoutInteger(Packet) <> fStoredConfigs.Count then
      Send(BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET,True);
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  ListIndex:  Integer;
  ConfigInfo: TStoredConfig;

  Function ReadoutValue(var SourcePtr: Pointer): scs_value_localized_t;
  begin
    Result.ValueType := Ptr_ReadoutInteger(SourcePtr);
    case Result.ValueType of
      SCS_VALUE_TYPE_string:
        Result.StringData := Ptr_ReadoutString(SourcePtr);
    else
      Ptr_ReadBuffer(SourcePtr,Result.BinaryData,SizeOf(Result.BinaryData));
    end;
  end;

begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    ListIndex := Ptr_ReadoutInteger(CurrPtr);
    If ListIndex >= 0 then
      If ListIndex < fStoredConfigs.Count then
        begin
          with fStoredConfigs.Pointers[ListIndex]^ do
            begin
              Name := Ptr_ReadoutString(CurrPtr);
              ID := Ptr_ReadoutInteger(CurrPtr);
              Index := Ptr_ReadoutInteger(CurrPtr);
              Value := ReadoutValue(CurrPtr);
              Binded := Ptr_ReadoutBoolean(CurrPtr);
            end;
          fStoredConfigs.DoChange;
        end
      else
        begin
          If ListIndex = fStoredConfigs.Count then
            begin
              with ConfigInfo do
                begin
                  Name := Ptr_ReadoutString(CurrPtr);
                  ID := Ptr_ReadoutInteger(CurrPtr);
                  Index := Ptr_ReadoutInteger(CurrPtr);
                  Value := ReadoutValue(CurrPtr);
                  Binded := Ptr_ReadoutBoolean(CurrPtr);
                  If not ConfigStored(Name,Index) then fStoredConfigs.Add(Name,Index,Value,Binded)
                    else Send(BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET,True);
                end;
            end
          else Send(BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET,True);
        end;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
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
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet: TPacketBuffer);
var
  CurrPtr:    Pointer;
  Count:      Integer;
  i:          Integer;
  ConfigInfo: TStoredConfig;
begin
If CheckPacketPayloadSize(Packet) then
  begin
    CurrPtr := GetPayloadAddress(Packet);
    Count := Ptr_ReadoutInteger(CurrPtr);
    fStoredConfigs.BeginUpdate;
    try
      fStoredConfigs.Clear;
      For i := 1 to Count do
        with ConfigInfo do
          begin
            Name := Ptr_ReadoutString(CurrPtr);
            ID := Ptr_ReadoutInteger(CurrPtr);
            Index := Ptr_ReadoutInteger(CurrPtr);
            Value.ValueType := Ptr_ReadoutInteger(CurrPtr);
            case Value.ValueType of
              SCS_VALUE_TYPE_string:
                Value.StringData := Ptr_ReadoutString(CurrPtr);
            else
              Ptr_ReadBuffer(CurrPtr,Value.BinaryData,SizeOf(Value.BinaryData));
            end;
            Binded := Ptr_ReadoutBoolean(CurrPtr);
            fStoredConfigs.Add(Name,Index,Value,Binded)
          end;
    finally
      fStoredConfigs.EndUpdate;
    end;
    IncreaseCompletionCounter;
  end
else
  Send(BuildPacket_TN_PACKET_ERROR(Packet,petPacketTooSmall),True);
end;

{-------------------------------------------------------------------------------
***  Game log communication packets  *******************************************
-------------------------------------------------------------------------------}

//  TN_PACKET_LOG_LOG structure:
//  begin
//    log type  4B      (scs_log_type_t)
//    log text  String  (variable size)
//  end;
procedure TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET_LOG_LOG(Packet: TPacketBuffer);
begin
// No defined behavior.
Send(BuildPacket_TN_PACKET_ERROR(Packet,petUndefinedBehaviour),True);
end;
{$ENDIF}
