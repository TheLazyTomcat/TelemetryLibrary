unit TelemetryNetClientRecipient;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryNetPackets,
  TelemetryNetClientBase,
{$IFDEF Documentation}
  TelemetryRecipientAux,
  TelemetryNetPacketsBuilding,
  TelemetryNetPacketsResolving,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}

type
  TEventRegisteredEvent = procedure(Sender: TObject; Event: scs_event_t; Registered: Boolean) of object;
  TEventRegisterResultEvent = procedure(Sender: TObject; Event: scs_event_t; Result: scs_result_t) of object;
  TChannelRegisteredEvent = procedure(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean) of Object;
  TChannelRegisterResultEvent = procedure(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; Result: scs_result_t) of Object;
  TChannelUnregisterResultEvent = procedure(Sender: TObject; Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Result: scs_result_t) of Object;
  TBufferedChannelsEvent = procedure(Sender: TObject; Count: Integer; Channels: Pointer) of object;
  TConfigStoredEvent = procedure(Sender: TObject; Name: String; Index: scs_u32_t; Stored: Boolean) of object;

  TTelemetryNetClientRecipient = class(TTelemetryNetClientBase)
  private
    fTelemetryInfoProvider: TTelemetryInfoProvider;
    fRegisteredEvents:      TRegisteredEventsList;
    fRegisteredChannels:    TRegisteredChannelsList;
    fStoredConfigs:         TStoredConfigsList;

    // Client settings.
    fParseConfigEvents:     Boolean;

    // API comunnication events.
    fOnLog:                 TLogEvent;
    fOnEventRegistered:     TEventRegisteredEvent;
    fOnEventRegister:       TEventRegisterResultEvent;
    fOnEventUnregister:     TEventRegisterResultEvent;
    fOnEvent:               TEventEvent;
    fOnChannelRegistered:   TChannelRegisteredEvent;
    fOnChannelRegister:     TChannelRegisterResultEvent;
    fOnChannelUnregister:   TChannelUnregisterResultEvent;
    fOnChannel:             TChannelEvent;
    fOnBufferedChannels:    TBufferedChannelsEvent;
    fOnConfigStored:        TConfigStoredEvent;
    fOnConfig:              TConfigEvent;
  protected
    procedure UtilityObjectsInitialize; override;
    procedure UtilityObjectsFinalize; override;
    procedure UtilityObjectsClear; override;
    procedure UtilityObjectsFill; override;
    // Packet processing
    {$DEFINE Declaration_part}
    {$INCLUDE '.\INC\TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET.pas'}
    {$UNDEF Declaration_part}
    procedure ProcessConfigurationEvent(Data: Pointer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FillTelemetryInfoProvider; virtual;
    procedure FillRegisteredItems; virtual;
    procedure GetAllStoredChannelValues; virtual;

    // Recipient control.
    procedure Log(LogType: scs_log_type_t; LogText: String); overload; virtual;
    procedure Log(LogText: String); overload; virtual;

    Function EventRegistered(Event: scs_event_t): Boolean; virtual;
    procedure EventRegistered_Async(Event: scs_event_t); virtual;
    procedure EventRegister(Event: scs_event_t); virtual;
    procedure EventUnregister(Event: scs_event_t); virtual;
    procedure EventRegisterAll; virtual;
    procedure EventUnregisterAll; virtual;

    Function ChannelRegistered(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean; virtual;
    procedure ChannelRegistered_Async(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
    procedure ChannelRegister(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none); virtual;
    procedure ChannelUnregister(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
    procedure ChannelRegisterAll(RegPrimaryTypes: Boolean = True; RegSecondaryTypes: Boolean = False; RegTertiaryTypes: Boolean = False); virtual;
    procedure ChannelUnregisterAll; virtual;

    Function ConfigStored(Name: String; Index: scs_u32_t = SCS_U32_NIL): Boolean; virtual;
    procedure ConfigStored_Async(Name: String; Index: scs_u32_t = SCS_U32_NIL); virtual;

    // Methods to convert received data to text.
    Function EventGetDataAsString(Event: scs_event_t; Data: Pointer; TypeName: Boolean = False): String; virtual;
    Function ChannelGetValueAsString(Value: p_scs_value_t; TypeName: Boolean = False): String; virtual;    
  published
    property TelemetryInfoProvider: TTelemetryInfoProvider read fTelemetryInfoProvider;
    property RegisteredEvents: TRegisteredEventsList read fRegisteredEvents;
    property RegisteredChannels: TRegisteredChannelsList read fRegisteredChannels;
    property StoredConfigs: TStoredConfigsList read fStoredConfigs;

    property ParseConfigurationEvents: Boolean read fParseConfigEvents write fParseConfigEvents;

    property OnLog: TLogEvent read fOnLog write fOnLog;
    property OnEventRegistered: TEventRegisteredEvent read fOnEventRegistered write fOnEventRegistered;
    property OnEventRegister: TEventRegisterResultEvent read fOnEventRegister write fOnEventRegister;
    property OnEventUnregister: TEventRegisterResultEvent read fOnEventUnregister write fOnEventUnregister;
    property OnEvent: TEventEvent read fOnEvent write fOnEvent;
    property OnChannelRegistered: TChannelRegisteredEvent read fOnChannelRegistered write fOnChannelRegistered;
    property OnChannelRegister: TChannelRegisterResultEvent read fOnChannelRegister write fOnChannelRegister;
    property OnChannelUnregister: TChannelUnregisterResultEvent read fOnChannelUnregister write fOnChannelUnregister;
    property OnChannel: TChannelEvent read fOnChannel write fOnChannel;
    property OnBufferedChannels: TBufferedChannelsEvent read fOnBufferedChannels write fOnBufferedChannels;
    property OnConfigStored: TConfigStoredEvent read fOnConfigStored write fOnConfigStored;
    property OnConfig: TConfigEvent read fOnConfig write fOnConfig;
  end;

implementation

uses
  TelemetryRecipientAux,
  TelemetryNetPacketsBuilding,
  TelemetryNetPacketsResolving;

const
  def_ParseConfigEvents = False;

{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Remember to update:'}
{$ENDIF}
(*
TN_PACKET_EVENT_KNOWN_ALL
TN_PACKET_CHANNEL_KNOWN_ALL
TN_PACKET_CONFIG_KNOWN_ALL
TN_PACKET_EVENT_REGISTERED_ALL
TN_PACKET_CHANNEL_REGISTERED_ALL
TN_PACKET_CONFIG_STORED_ALL
*)
  cCompletionCountInc = 6;

  cSupportedServerVersions: Array[0..0] of LongWord = ($00010000 {1.0});

{===============================================================================
     TTelemetryNetClient // Protected methods
===============================================================================}

procedure TTelemetryNetClientRecipient.UtilityObjectsInitialize;
begin
fTelemetryInfoProvider := TTelemetryInfoProvider.Create;
fRegisteredEvents := TRegisteredEventsList.Create;
fRegisteredChannels := TRegisteredChannelsList.Create;
fStoredConfigs := TStoredConfigsList.Create;
end;

procedure TTelemetryNetClientRecipient.UtilityObjectsFinalize;
begin
fStoredConfigs.Free;
fRegisteredChannels.Free;
fRegisteredEvents.Free;
fTelemetryInfoProvider.Free;
end;

procedure TTelemetryNetClientRecipient.UtilityObjectsClear;
begin
fStoredConfigs.Clear;
fRegisteredChannels.Clear;
fRegisteredEvents.Clear;
fTelemetryInfoProvider.Clear;
end;

procedure TTelemetryNetClientRecipient.UtilityObjectsFill;
begin
FillTelemetryInfoProvider;
FillRegisteredItems;
GetAllStoredChannelValues;
end;

//------------------------------------------------------------------------------

{$DEFINE Implementation_part}
{$INCLUDE '.\INC\TTelemetryNetClientRecipient.ProcessPacket_TN_PACKET.pas'}
{$UNDEF Implementation_part}

//------------------------------------------------------------------------------

procedure TTelemetryNetClientRecipient.ProcessConfigurationEvent(Data: Pointer);
var
  ConfigID:       String;
  TempConfig:     TStoredConfig;
  ConfigFullName: String;
begin
ConfigID := Ptr_ReadoutString(Data);
fStoredConfigs.BeginUpdate;
try
  with TempConfig do
    while Ptr_ReadString(Data,String(Name)) > SizeOfPacketString do
      begin
        Index := Ptr_ReadoutInteger(Data);
        Value.ValueType := Ptr_ReadoutInteger(Data);
        case Value.ValueType of
          SCS_VALUE_TYPE_string:  Value.StringData := Ptr_ReadoutString(Data);
        else
          Ptr_ReadBuffer(Data,Value.BinaryData,SizeOf(Value.BinaryData));
        end;
        ConfigFullName := ConfigID + cConfigFieldsSeparator + Name;
        If fStoredConfigs.ChangeConfigValue(ConfigFullName,Index,Value) < 0 then
          fStoredConfigs.Add(ConfigFullName,Index,Value,fTelemetryInfoProvider.KnownConfigs.IsBinded(Name));
  end;
finally
  fStoredConfigs.EndUpdate;
end;
end;

{===============================================================================
     TTelemetryNetClient // Public methods
===============================================================================}

constructor TTelemetryNetClientRecipient.Create;
begin
inherited Create;
ParseConfigurationEvents := def_ParseConfigEvents;
Inc(fCompletionCount,cCompletionCountInc);
end;

destructor TTelemetryNetClientRecipient.Destroy;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientRecipient.FillTelemetryInfoProvider;
begin
Send(BuildPacket_TN_PACKET_EVENT_KNOWN_ALL_GET,True);
Send(BuildPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET,True);
Send(BuildPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET,True);
end;

procedure TTelemetryNetClientRecipient.FillRegisteredItems;
begin
Send(BuildPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET,True);
Send(BuildPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET,True);
Send(BuildPacket_TN_PACKET_CONFIG_STORED_ALL_GET,True);
end;

procedure TTelemetryNetClientRecipient.GetAllStoredChannelValues;
begin
Send(BuildPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL,True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientRecipient.Log(LogType: scs_log_type_t; LogText: String);
begin
Send(BuildPacket_TN_PACKET_LOG_LOG(LogType,LogText),True);
end;

procedure TTelemetryNetClientRecipient.Log(LogText: String);
begin
Log(SCS_LOG_TYPE_message,LogText);
end;

Function TTelemetryNetClientRecipient.EventRegistered(Event: scs_event_t): Boolean;
begin
Result := fRegisteredEvents.IndexOf(Event) >= 0;
end;

procedure TTelemetryNetClientRecipient.EventRegistered_Async(Event: scs_event_t);
begin
Send(BuildPacket_TN_PACKET_EVENT_REGISTERED(Event,False),True);
end;

procedure TTelemetryNetClientRecipient.EventRegister(Event: scs_event_t);
begin
Send(BuildPacket_TN_PACKET_EVENT_REG(Event,SCS_RESULT_ok),True);
end;

procedure TTelemetryNetClientRecipient.EventUnregister(Event: scs_event_t);
begin
Send(BuildPacket_TN_PACKET_EVENT_UNREG(Event,SCS_RESULT_ok),True);
end;

procedure TTelemetryNetClientRecipient.EventRegisterAll;
begin
Send(BuildPacket_TN_PACKET_EVENT_REG_ALL,True);
end;

procedure TTelemetryNetClientRecipient.EventUnregisterAll;
begin
Send(BuildPacket_TN_PACKET_EVENT_UNREG_ALL,True);
end;

Function TTelemetryNetClientRecipient.ChannelRegistered(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean;
begin
Result := fRegisteredChannels.IndexOf(Name,Index,ValueType) >= 0;
end;

procedure TTelemetryNetClientRecipient.ChannelRegistered_Async(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
Send(BuildPacket_TN_PACKET_CHANNEL_REGISTERED(Name,Index,ValueType,False),True);
end;

procedure TTelemetryNetClientRecipient.ChannelRegister(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none);
begin
Send(BuildPacket_TN_PACKET_CHANNEL_REG(Name,Index,ValueType,Flags,SCS_RESULT_ok),True);
end;

procedure TTelemetryNetClientRecipient.ChannelUnregister(Name: String; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
Send(BuildPacket_TN_PACKET_CHANNEL_UNREG(Name,Index,ValueType,SCS_RESULT_ok),True);
end;

procedure TTelemetryNetClientRecipient.ChannelRegisterAll(RegPrimaryTypes: Boolean = True; RegSecondaryTypes: Boolean = False; RegTertiaryTypes: Boolean = False);
begin
Send(BuildPacket_TN_PACKET_CHANNEL_REG_ALL(RegPrimaryTypes,RegSecondaryTypes,RegTertiaryTypes),True);
end;

procedure TTelemetryNetClientRecipient.ChannelUnregisterAll;
begin
Send(BuildPacket_TN_PACKET_CHANNEL_UNREG_ALL,True);
end;

Function TTelemetryNetClientRecipient.ConfigStored(Name: String; Index: scs_u32_t = SCS_U32_NIL): Boolean;
begin
Result := fStoredConfigs.IndexOf(Name,Index) >= 0;
end;

procedure TTelemetryNetClientRecipient.ConfigStored_Async(Name: String; Index: scs_u32_t = SCS_U32_NIL);
begin
Send(BuildPacket_TN_PACKET_CONFIG_STORED(Name,Index,False),True);
end;

//------------------------------------------------------------------------------

Function TTelemetryNetClientRecipient.EventGetDataAsString(Event: scs_event_t; Data: Pointer; TypeName: Boolean = False): String;
begin
If Assigned(Data) then
  case Event of
    SCS_TELEMETRY_EVENT_frame_start:
      Result := TelemetryEventFrameStartToStr(p_scs_telemetry_frame_start_t(Data)^);
    SCS_TELEMETRY_EVENT_configuration:
      Result := PacketEventConfigurationToStr(Data,TypeName);
  else
    Result := '';
  end;
end;

Function TTelemetryNetClientRecipient.ChannelGetValueAsString(Value: p_scs_value_t; TypeName: Boolean = False): String;
begin
If Assigned(Value) then Result := SCSValueToStr(Value^,TypeName)
  else Result := '';
end;

end.

