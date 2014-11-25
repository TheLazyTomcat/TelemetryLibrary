{@html(<hr>)
@abstract(Contains class designed to provide simple way to create complete
          packets.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-16)
@lastmod(2014-11-25)

  @bold(@NoAutoLink(TelemetryCommPacketsBuilder))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryCommPacketsBuilder class (see class declaration
  for details).

  Last change:  2014-11-25

  Change List:@unorderedList(
    @item(2014-05-16 - First stable version.)
    @item(2014-11-02 - Small implementation changes.)
    @item(2014-11-05 - Type of parameters @code(PacketID) and @code(NewPacketID)
                       changed to TPacketID in following methods:@unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryCommPacketsBuilder.PreparePacket)
                         @item(TTelemetryCommPacketsBuilder.BuildPacket)
                         @item(TTelemetryCommPacketsBuilder.MinimalPacketPayloadSize)
                         @item(TTelemetryCommPacketsBuilder.ReusePacket)))
    @item(2014-11-05 - Type of parameters @code(ValueSize), @code(DataOffset)
                       and @code(DataSize) changed from signed to unsigned
                       integer in following methods:@unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryCommPacketsBuilder.BuildPacket)
                         @item(TTelemetryCommPacketsBuilder.ReusePacket)))
    @item(2014-11-05 - Type of result changed from signed to unsigned integer
                       for method
                       TTelemetryCommPacketsBuilder.MinimalPacketPayloadSize)
    @item(2014-11-25 - Changed implementation and resulting packet layout of
                       method TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER_ALL
                       due to a new system of storing and passing secondary
                       types of channel value. Implementation of method
                       TTelemetryCommPacketsBuilder.MinimalPacketPayloadSize
                       also slightly changed to mirror new structure of packet
                       TC_PACKET_CHANNEL_REGISTER_ALL))



@html(<hr>)}
unit TelemetryCommPacketsBuilder;

{$INCLUDE '..\Telemetry_defs.inc'}

interface

uses
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryCommCommon,
  TelemetryCommPackets,
  TelemetryCommCircularBuffers,
  TelemetryCommPacketsAllocator,
{$IFDEF Documentation}
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
{                         TTelemetryCommPacketsBuilder                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommPacketsBuilder // Class declaration                          }
{==============================================================================}
{
  @abstract(Class providing methods that creates complete initialized predefined
            packets and few more methods intended for packets resolving.)
  There are two main function groups and few other (not grouped) functions.
  Main groups are:
@unorderedList(
  @item(@code(@noAutoLink(BuildPacket)) functions - Set of functions providing
        ready-to-be-sent packet buffers. Use them to create packets with zero or
        one parameter (refer to individual functions for details).)
  @item(@code(BuildPacket_TC_PACKET_*) functions - These functions are designed
        to provide complete, ready-to-be-sent, packets of specific packet ID.
        Each function identifier starts with @code(BuildPacket_) followed by
        appropriate packet ID identifier (packet of this ID is returned by such
        function).@br
        Each function has such parameters that are required to build appropriate
        packet (paramerers for individual functions are not documented as they
        should be self-explanatory). Refer to packet definition or function
        implementation for details.))

  @member(PreparePacket
    Method used for packet preparation.@br
    It expects the field @code(Size) in @code(Packet) parameter to be set to
    size the packet will have (if it is set to value lower than is required for
    given packet ID, then an exception is raised). @code(Data) field of
    @code(Packet) parameter must not be initialized, since it will be
    initialized (allocated) in this method (to size passed in
    @code(Packet.Size)). Method also fills all information in packet header.

    @param Packet    Packet that has to be initialized in this routine.
    @param PacketID  ID of initialized packet.

    @returns(Pointer that points to a beginning of packet payload (when packet
    has no payload, it points to the end of the packet).))

  @member(BuildPacket_TC_PACKET_PING
    Creates and returns complete TC_PACKET_PING packet.)

  @member(BuildPacket_TC_PACKET_PING_RESPONSE
    Creates and returns complete TC_PACKET_PING_RESPONSE packet.)

  @member(BuildPacket_TC_PACKET_PROTOCOL_VERSION_GET
    Creates and returns complete TC_PACKET_PROTOCOL_VERSION_GET packet.)

  @member(BuildPacket_TC_PACKET_PROTOCOL_VERSION
    Creates and returns complete TC_PACKET_PROTOCOL_VERSION packet.)

  @member(BuildPacket_TC_PACKET_TELEMETRY_INFO_GET
    Creates and returns complete TC_PACKET_TELEMETRY_INFO_GET packet.)

  @member(BuildPacket_TC_PACKET_TELEMETRY_INFO
    Creates and returns complete TC_PACKET_TELEMETRY_INFO packet.)

  @member(BuildPacket_TC_PACKET_PACKET
    Creates and returns complete TC_PACKET_PACKET packet.)

  @member(BuildPacket_TC_PACKET_ERROR
    Creates and returns complete TC_PACKET_ERROR packet.)

  @member(BuildPacket_TC_PACKET_DEFFERED
    Creates and returns complete TC_PACKET_DEFFERED packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_COUNT packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_INDEX packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL
    Creates and returns complete TC_PACKET_KNOWN_EVENTS_ALL packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_COUNT packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_INDEX packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL
    Creates and returns complete TC_PACKET_KNOWN_CHANNELS_ALL packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_COUNT packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_INDEX packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL
    Creates and returns complete TC_PACKET_KNOWN_CONFIGS_ALL packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTER
    Creates and returns complete TC_PACKET_EVENT_REGISTER packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX
    Creates and returns complete TC_PACKET_EVENT_REGISTER_BY_INDEX packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTER_ALL
    Creates and returns complete TC_PACKET_EVENT_REGISTER_ALL packet.)

  @member(BuildPacket_TC_PACKET_EVENT_UNREGISTER
    Creates and returns complete TC_PACKET_EVENT_UNREGISTER packet.)

  @member(BuildPacket_TC_PACKET_EVENT_UNREGISTER_INDEX
    Creates and returns complete TC_PACKET_EVENT_UNREGISTER_INDEX packet.)

  @member(BuildPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX
    Creates and returns complete TC_PACKET_EVENT_UNREGISTER_BY_INDEX packet.)

  @member(BuildPacket_TC_PACKET_EVENT_UNREGISTER_ALL
    Creates and returns complete TC_PACKET_EVENT_UNREGISTER_ALL packet.)

  @member(BuildPacket_TC_PACKET_EVENT_EVENT
    Creates and returns complete TC_PACKET_EVENT_EVENT packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED
    Creates and returns complete TC_PACKET_EVENT_REGISTERED packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_COUNT packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_INDEX packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL
    Creates and returns complete TC_PACKET_EVENT_REGISTERED_ALL packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTER
    Creates and returns complete TC_PACKET_CHANNEL_REGISTER packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX
    Creates and returns complete TC_PACKET_CHANNEL_REGISTER_BY_INDEX packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME
    Creates and returns complete TC_PACKET_CHANNEL_REGISTER_BY_NAME packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTER_ALL
    Creates and returns complete TC_PACKET_CHANNEL_REGISTER_ALL packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_UNREGISTER
    Creates and returns complete TC_PACKET_CHANNEL_UNREGISTER packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX
    Creates and returns complete TC_PACKET_CHANNEL_UNREGISTER_INDEX packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX
    Creates and returns complete TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME
    Creates and returns complete TC_PACKET_CHANNEL_UNREGISTER_BY_NAME packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL
    Creates and returns complete TC_PACKET_CHANNEL_UNREGISTER_ALL packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED
    Creates and returns complete TC_PACKET_CHANNEL_CHANNEL_BUFFERED packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_COUNT packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_INDEX packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL
    Creates and returns complete TC_PACKET_CHANNEL_REGISTERED_ALL packet.)

  @member(BuildPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL
    Creates and returns complete TC_PACKET_CHANNEL_STORED_SEND_ALL packet.)  

  @member(BuildPacket_TC_PACKET_CONFIG_CONFIG
    Creates and returns complete TC_PACKET_CONFIG_CONFIG packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED
    Creates and returns complete TC_PACKET_CONFIG_STORED packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_COUNT_GET
    Creates and returns complete TC_PACKET_CONFIG_STORED_COUNT_GET packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_COUNT
    Creates and returns complete TC_PACKET_CONFIG_STORED_COUNT packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_GET
    Creates and returns complete TC_PACKET_CONFIG_STORED_INDEX_GET packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_INDEX
    Creates and returns complete TC_PACKET_CONFIG_STORED_INDEX packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET
    Creates and returns complete TC_PACKET_CONFIG_STORED_INDEX_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET
    Creates and returns complete TC_PACKET_CONFIG_STORED_ALL_GET packet.)

  @member(BuildPacket_TC_PACKET_CONFIG_STORED_ALL
    Creates and returns complete TC_PACKET_CONFIG_STORED_ALL packet.)

  @member(BuildPacket_TC_PACKET_LOG_LOG
    Creates and returns complete TC_PACKET_LOG_LOG packet.)

  @member(MinimalPacketPayloadSize
    Returns minimal packet payload size.@br
    Each packets payload must have minimal size given by its structure. This
    method returns this size for given packet ID - if unknown packet ID is
    passed, an exception is raised.@br
    This method should be used to check received packet before further
    processing.@br
    For actual values, refer to implementation.

    @param PacketID ID of packet for which the minimum size is required.

    @returns Minimal size of payload for given packet ID.)

  @member(CheckPacketPayloadSize
    Checks whether passed packet payload is large enough for its ID.

    @param Packet Packet whose payload size will be checked.

    @returns(@True when packet payload is large enough, @false when it is too
             small.))
}
type
  TTelemetryCommPacketsBuilder = class(TTelemetryCommPacketsAllocator)
  public
    Function PreparePacket(var Packet: TPacketBuffer; PacketID: TPacketID): Pointer; virtual;
    {
      Returns complete initialized no-payload packet of given ID.

      @param PacketID ID of created packet.

      @returns Initialized ready-to-be-sent packet.
    }
    Function BuildPacket(PacketID: TPacketID): TPacketBuffer; overload; virtual;
    {
      Creates complete initialized packet of given ID with payload consisting of
      passed 32bit integer value.

      @param PacketID ID of created packet.
      @param Value    Packet payload.

      @returns Initialized ready-to-be-sent packet.
    }
    Function BuildPacket(PacketID: TPacketID; Value: Integer): TPacketBuffer; overload; virtual;
    {
      Creates complete initialized packet of given ID with payload consisting of
      passed string value.

      @param PacketID ID of created packet.
      @param Value    Packet payload.

      @returns Initialized ready-to-be-sent packet.
    }
    Function BuildPacket(PacketID: TPacketID; Value: UTF8String): TPacketBuffer; overload; virtual;
    {
      Creates complete initialized packet of given ID with payload containign
      untyped buffer.

      @param PacketID  ID of created packet.
      @param Value     Untyped buffer to be stored in packet payload.
      @param ValueSize Size of passed buffer.

      @returns Initialized ready-to-be-sent packet.
    }
    Function BuildPacket(PacketID: TPacketID; const Value; ValueSize: LongWord): TPacketBuffer; overload; virtual;

    Function BuildPacket_TC_PACKET_PING: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_PING_RESPONSE(TimeStamp: TPacketHeaderTime): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_PROTOCOL_VERSION_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_PROTOCOL_VERSION(ProtocolVersion: LongWord): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_TELEMETRY_INFO_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_TELEMETRY_INFO(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_PACKET(Payload: Pointer; PayloadSize: LongWord): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_ERROR(ErroneousPacket: TPacketBuffer; ErrorType: TPacketErrorType; ErrorCode: LongWord = 0): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_DEFFERED(DefferedPacket: TPacketBuffer; DefferedOperationType: TDefferedOperationType): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_EVENT_REGISTER(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTER_ALL: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_UNREGISTER(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_UNREGISTER_ALL: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_EVENT(Event: scs_event_t; Data: Pointer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED(Event: scs_event_t; Registered: Boolean): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_CHANNEL_REGISTER(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; ResultCode: scs_result_t): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(Name: TelemetryString): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTER_ALL(RegPrimaryType: Boolean; SecondarySelectionMask: Longword): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_UNREGISTER(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; ResultCode: scs_result_t): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(Name: TelemetryString): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL: TPacketBuffer; virtual;
    {Creates and returns complete TC_PACKET_CHANNEL_CHANNEL packet.}
    Function BuildPacket_TC_PACKET_CHANNEL_CHANNEL(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): TPacketBuffer; overload; virtual;
    {Creates and returns complete TC_PACKET_CHANNEL_CHANNEL packet.}
    Function BuildPacket_TC_PACKET_CHANNEL_CHANNEL(StoredChannel: TStoredChannel): TPacketBuffer; overload; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(BufferedChannels: TCircularChannelsBuffer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL: TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_CONFIG_CONFIG(Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED(Name: TelemetryString; Index: scs_u32_t; Stored: Boolean): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_COUNT_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_COUNT(Count: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET: TPacketBuffer; virtual;
    Function BuildPacket_TC_PACKET_CONFIG_STORED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer; virtual;

    Function BuildPacket_TC_PACKET_LOG_LOG(LogType: scs_log_type_t; LogText: String): TPacketBuffer; virtual;
    
    Function MinimalPacketPayloadSize(PacketID: TPacketID): LongWord; virtual;
    Function CheckPacketPayloadSize(Packet: TPacketBuffer): Boolean; virtual;
    {
      Use to change packets ID and reuse it.

      @param Packet      Packet that will be reused.
      @param NewPacketID New packet ID to be written to reused packet.
    }
    procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID); overload; virtual;
    {
      Use to change packets ID and store new data to it.@br
      Data are store at position given by offset from packet payload beginning.
      When data would not fit into packet, an exception is raised.

      @param Packet      Packet to be changed.
      @param NewPacketID New packet ID to be written to reused packet.
      @param(DataOffset  Position offset from payload beginning where to write
                         data.)
      @param DataSize    Size of data to be written.
      @param Data        Data to be writen to packet.
    }
    procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID; DataOffset, DataSize: LongWord; Data: Pointer); overload; virtual;
    {
      Use to change packets ID and store new data to it.@br
      Data are store at given address. When data would not fit into packet or
      address is outside of packet memory image, an exception is raised.

      @param Packet      Packet to be changed.
      @param NewPacketID New packet ID to be written to reused packet.
      @param Address     Adress at which the data will be written.
      @param DataSize    Size of data to be written.
      @param Data        Data to be writen to packet.
    }
    procedure ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID; Address: Pointer; DataSize: LongWord; Data: Pointer); overload; virtual;
  end;

implementation

uses
  Windows, SysUtils, Classes,
  TelemetryStreaming;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryCommPacketsBuilder                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommPacketsBuilder // Class implementation                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommPacketsBuilder // Public methods                             }
{------------------------------------------------------------------------------}

Function TTelemetryCommPacketsBuilder.PreparePacket(var Packet: TPacketBuffer; PacketID: TPacketID): Pointer;
begin
If Packet.Size >= MinimalPacketPayloadSize(PacketID) then
  begin
    AllocatePacket(Packet);
    PPacketHeader(Packet.Data)^.Signature := TC_PACKET_SIGNATURE;
    PPacketHeader(Packet.Data)^.PacketID := PacketID;
    PPacketHeader(Packet.Data)^.TimeStamp := DateTimeToTimeStamp(Now);
    PPacketHeader(Packet.Data)^.PayloadSize := Packet.Size - SizeOf(TPacketHeader);
    Result := Pointer(PtrUInt(Packet.Data) + SizeOf(TPacketHeader));
  end
else
  raise Exception.Create('TTelemetryCommPacketsBuilder.PreparePacket: Packet size too small (' + IntToStr(Packet.Size) + ').');
end;

//==============================================================================

Function TTelemetryCommPacketsBuilder.BuildPacket(PacketID: TPacketID): TPacketBuffer;
begin
Result.Size := SizeOf(TPacketHeader);
PreparePacket(Result,PacketID);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket(PacketID: TPacketID; Value: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Value);
CurrPtr := PreparePacket(Result,PacketID);
Ptr_WriteInteger(CurrPtr,Value);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket(PacketID: TPacketID; Value: UTF8String): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOfString(Value);
CurrPtr := PreparePacket(Result,PacketID);
Ptr_WriteString(CurrPtr,Value);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket(PacketID: TPacketID; const Value; ValueSize: LongWord): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + ValueSize;
CurrPtr := PreparePacket(Result,PacketID);
Ptr_WriteBuffer(CurrPtr,Value,ValueSize);
end;

//==============================================================================

{------------------------------------------------------------------------------}
{   Common                                                                     }
{------------------------------------------------------------------------------}

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_PING: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_PING);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Timestamp   8 bytes    TPacketHeaderTime
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_PING_RESPONSE(TimeStamp: TPacketHeaderTime): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_PING_RESPONSE,TimeStamp,SizeOf(TPacketHeaderTime));
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_PROTOCOL_VERSION_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_PROTOCOL_VERSION_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Protocol version    4 bytes    TProtocolVersion
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_PROTOCOL_VERSION(ProtocolVersion: TProtocolVersion): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_PROTOCOL_VERSION,ProtocolVersion);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_TELEMETRY_INFO_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_TELEMETRY_INFO_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Telemetry version   4 bytes     scs_u32_t
    Game ID             variable    String
    Game version        4 bytes     scs_u32_t
    Game name           variable    String
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_TELEMETRY_INFO(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_u32_t){Telemetry version} +
               SizeOfString(TelemetryRecipient.GameID){Game ID} +
               SizeOf(scs_u32_t){Game version} +
               SizeOfString(TelemetryRecipient.GameName){Game name};
CurrPtr := PreparePacket(Result,TC_PACKET_TELEMETRY_INFO);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.TelemetryVersion);
Ptr_WriteString(CurrPtr,TelemetryRecipient.GameID);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.GameVersion);
Ptr_WriteString(CurrPtr,TelemetryRecipient.GameName);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Payload data   variable    not specified
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_PACKET(Payload: Pointer; PayloadSize: LongWord): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_PACKET,Payload^,PayloadSize);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Packet ID          4 bytes    32b uint
    Packet timestamp   8 bytes    TPacketHeaderTime
    Error type         4 bytes    TPacketErrorType
    Error code         4 bytes    32b uint
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_ERROR(ErroneousPacket: TPacketBuffer; ErrorType: TPacketErrorType; ErrorCode: LongWord = 0): TPacketBuffer;
var
  CurrPtr:                Pointer;
  ErroneousPacketHeader:  TPacketHeader;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(LongWord){Packet ID} +
               SizeOf(TPacketHeaderTime){Packet time stamp} +
               SizeOf(Integer){Error type} +
               SizeOf(LongWord){Error code};
CurrPtr := PreparePacket(Result,TC_PACKET_ERROR);
ErroneousPacketHeader := GetPacketHeader(ErroneousPacket);
Ptr_WriteInteger(CurrPtr,ErroneousPacketHeader.PacketID);
Ptr_WriteBuffer(CurrPtr,ErroneousPacketHeader.TimeStamp,SizeOf(TPacketHeaderTime));
Ptr_WriteInteger(CurrPtr,Integer(ErrorType));
Ptr_WriteInteger(CurrPtr,ErrorCode);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Packet ID          4 bytes    32b uint
    Packet timestamp   8 bytes    TPacketHeaderTime
    Operation type     4 bytes    TDefferedOperationType
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_DEFFERED(DefferedPacket: TPacketBuffer; DefferedOperationType: TDefferedOperationType): TPacketBuffer;
var
  CurrPtr:              Pointer;
  DefferedPacketHeader: TPacketHeader;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(LongWord){Packet ID} +
               SizeOf(TPacketHeaderTime){Packet time stamp} +
               SizeOf(Integer){Operation type};
CurrPtr := PreparePacket(Result,TC_PACKET_DEFFERED);
DefferedPacketHeader := GetPacketHeader(DefferedPacket);
Ptr_WriteInteger(CurrPtr,DefferedPacketHeader.PacketID);
Ptr_WriteBuffer(CurrPtr,DefferedPacketHeader.TimeStamp,SizeOf(TPacketHeaderTime));
Ptr_WriteInteger(CurrPtr,Integer(DefferedOperationType));
end;

{------------------------------------------------------------------------------}
{   Known events                                                               }
{------------------------------------------------------------------------------}

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_EVENTS_COUNT_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_EVENTS_COUNT,Count);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_EVENTS_COUNT,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index       4 bytes    32b int
    Event       4 bytes    scs_event_t
    Name        variable   String
    Valid       1 byte     Boolean
    Utility     1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownEvents.Count) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownEvent(TelemetryInfoProvider.KnownEvents[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_EVENTS_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_KnownEvent(CurrPtr,TelemetryInfoProvider.KnownEvents[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownEvent(cEmptyKnownEvent);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_EVENTS_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_Write_KnownEvent(CurrPtr,cEmptyKnownEvent,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_EVENTS_ALL_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count     4 bytes    32b int
    Info[]    vyriable   array of Info substructures

  Info substructure:

    Event     4 bytes    scs_event_t
    Name      variable   String
    Valid     1 byte     Boolean
    Utility   1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_EVENTS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size for all known events.
For i := 0 to Pred(TelemetryInfoProvider.KnownEvents.Count) do
  Inc(Result.Size,Size_KnownEvent(TelemetryInfoProvider.KnownEvents[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_EVENTS_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownEvents.Count);
// Fill info about all known events.
For i := 0 to Pred(TelemetryInfoProvider.KnownEvents.Count) do
  Ptr_Write_KnownEvent(CurrPtr,TelemetryInfoProvider.KnownEvents[i],Result.Size);
end;

{------------------------------------------------------------------------------}
{   Known channels                                                             }
{------------------------------------------------------------------------------}

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CHANNELS_COUNT_GET)
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CHANNELS_COUNT,Count)
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CHANNELS_INDEX_GET,Index)
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index             4 bytes    32b int
    Name              variable   String
    ID                4 bytes    TChannelID
    Primary type      4 bytes    scs_value_type_t
    Secondary type    4 bytes    scs_value_type_t
    Tertiary type     4 bytes    scs_value_type_t
    Indexed           1 byte     Boolean
    Index config      variable   String
    Index config id   4 bytes    TItemID
    Maximum index     4 bytes    scs_u32_t
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownChannels.Count) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownChannel(TelemetryInfoProvider.KnownChannels[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CHANNELS_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_KnownChannel(CurrPtr,TelemetryInfoProvider.KnownChannels[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownChannel(cEmptyKnownChannel);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CHANNELS_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_Write_KnownChannel(CurrPtr,cEmptyKnownChannel,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET)
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CHANNELS_ALL_GET)
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count             4 bytes    32b int
    Info[]            variable   array of Info substructures

  Info substructure:

    Name              variable   String
    ID                4 bytes    TChannelID
    Primary type      4 bytes    scs_value_type_t
    Secondary type    4 bytes    scs_value_type_t
    Tertiary type     4 bytes    scs_value_type_t
    Indexed:          1 byte     Boolean
    Index config      variable   String
    Index config id   4 bytes    TItemID
    Maximum index     4 bytes    scs_u32_t
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CHANNELS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size for all known channels.
For i := 0 to Pred(TelemetryInfoProvider.KnownChannels.Count) do
  Inc(Result.Size,Size_KnownChannel(TelemetryInfoProvider.KnownChannels[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CHANNELS_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownChannels.Count);
// Fill info about all known channels.
For i := 0 to Pred(TelemetryInfoProvider.KnownChannels.Count) do
  Ptr_Write_KnownChannel(CurrPtr,TelemetryInfoProvider.KnownChannels[i],Result.Size);
end;

{------------------------------------------------------------------------------}
{   Known configs                                                              }
{------------------------------------------------------------------------------}

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CONFIGS_COUNT_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CONFIGS_COUNT,Count);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CONFIGS_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index         4 bytes    32b int
    Name          variable   String
    ID            4 bytes    TConfigID
    Value type    4 bytes    scs_value_type_t
    Indexed       1 byte     Boolean
    Binded        1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX(TelemetryInfoProvider: TTelemetryInfoProvider; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If (Index >= 0) and (Index < TelemetryInfoProvider.KnownConfigs.Count) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownConfig(TelemetryInfoProvider.KnownConfigs[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CONFIGS_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_KnownConfig(CurrPtr,TelemetryInfoProvider.KnownConfigs[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_KnownConfig(cEmptyKnownConfig);
    CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CONFIGS_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_Write_KnownConfig(CurrPtr,cEmptyKnownConfig,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_KNOWN_CONFIGS_ALL_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count         4 bytes    32b int
    Info[]        variable   array of Info substructures

  Info substructure:

    Name          variable   String
    ID            4 bytes    TConfigID
    Value type    4 bytes    scs_value_type_t
    Indexed       1 byte     Boolean
    Binded        1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_KNOWN_CONFIGS_ALL(TelemetryInfoProvider: TTelemetryInfoProvider): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size for all known configs.
For i := 0 to Pred(TelemetryInfoProvider.KnownConfigs.Count) do
  Inc(Result.Size,Size_KnownConfig(TelemetryInfoProvider.KnownConfigs[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_KNOWN_CONFIGS_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryInfoProvider.KnownConfigs.Count);
// Fill info about all known configs.
For i := 0 to Pred(TelemetryInfoProvider.KnownConfigs.Count) do
  Ptr_Write_KnownConfig(CurrPtr,TelemetryInfoProvider.KnownConfigs[i],Result.Size);
end;

{------------------------------------------------------------------------------}
{   Events                                                                     }
{------------------------------------------------------------------------------}

{ Payload structure:

    Event     4 bytes    scs_event_t
    Result    4 bytes    scs_result_t
}
Function TTelemetryCommPacketsBuilder.BuildPAcket_TC_PACKET_EVENT_REGISTER(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){Event} +
               SizeOf(scs_result_t){Result};
CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_REGISTER);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTER_BY_INDEX(Index: Integer): TPacketBuffer;

begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTER_BY_INDEX,Index);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTER_ALL: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTER_ALL);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Event     4 bytes    scs_event_t
    Result    4 bytes    scs_result_t
}
Function TTelemetryCommPacketsBuilder.BuildPAcket_TC_PACKET_EVENT_UNREGISTER(Event: scs_event_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){Event} +
               SizeOf(scs_result_t){Result};
CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_UNREGISTER);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_UNREGISTER_INDEX(Index: Integer): TPacketBuffer;

begin
Result := BuildPacket(TC_PACKET_EVENT_UNREGISTER_INDEX,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_UNREGISTER_BY_INDEX(Index: Integer): TPacketBuffer;

begin
Result := BuildPacket(TC_PACKET_EVENT_UNREGISTER_BY_INDEX,Index);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_UNREGISTER_ALL: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_UNREGISTER_ALL);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Event   4 bytes    scs_event_t
    Data    variable
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_EVENT(Event: scs_event_t; Data: Pointer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(scs_event_t){Event};
case Event of
  SCS_TELEMETRY_EVENT_frame_start:
    begin
      Inc(Result.Size,SizeOf(scs_telemetry_frame_start_t));
      CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_EVENT);
      Ptr_WriteInteger(CurrPtr,Event);
      Ptr_WriteBuffer(CurrPtr,Data^,SizeOf(scs_telemetry_frame_start_t));
    end;
  SCS_TELEMETRY_EVENT_configuration:
    begin
      Inc(Result.Size,Size_scs_telemetry_configuration(p_scs_telemetry_configuration_t(Data)^,True,False));
      CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_EVENT);
      Ptr_WriteInteger(CurrPtr,Event);
      Ptr_Write_scs_telemetry_configuration(CurrPtr,p_scs_telemetry_configuration_t(Data)^,Result.Size,True,True,False);
    end;
else
  CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_EVENT);
  Ptr_WriteInteger(CurrPtr,Event);
end;
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Event        4 bytes    scs_event_t
    Registered   1 bytes    Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED(Event: scs_event_t; Registered: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_event_t){Event} +
               SizeOf(Boolean){Registered};
CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_REGISTERED);
Ptr_WriteInteger(CurrPtr,Event);
Ptr_WriteBoolean(CurrPtr,Registered);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTERED_COUNT_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTERED_COUNT,Count);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTERED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index      4 bytes    32b int
    Event      4 bytes    scs_event_t
    Utility    1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If (Index >= 0) and (Index < TelemetryRecipient.RegisteredEvents.Count) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_EventInfo(TelemetryRecipient.RegisteredEvents[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_EventInfo(CurrPtr,TelemetryRecipient.RegisteredEvents[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Index} +
                   Size_EventInfo(cEmptyEventInfo);
    CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_EventInfo(CurrPtr,cEmptyEventInfo,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_EVENT_REGISTERED_ALL_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count     4 bytes    32b int
    Info[]    variable   array of Info substructures

  Info substructure:

    Event     4 bytes    scs_event_t
    Utility   1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_EVENT_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size to store all registered events.
For i := 0 to Pred(TelemetryRecipient.RegisteredEvents.Count) do
  Inc(Result.Size,Size_EventInfo(TelemetryRecipient.RegisteredEvents[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_EVENT_REGISTERED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.RegisteredEvents.Count);
// Fill info about all registered events.
For i := 0 to Pred(TelemetryRecipient.RegisteredEvents.Count) do
  Ptr_Write_EventInfo(CurrPtr,TelemetryRecipient.RegisteredEvents[i],Result.Size);
end;

{------------------------------------------------------------------------------}
{   Channels                                                                   }
{------------------------------------------------------------------------------}

{ Payload structure:

    Name         variable    String
    Index        4 bytes     scs_u32_t
    Value type   4 bytes     scs_value_type_t
    Flags        4 bytes     scs_u32_t
    Result       4 bytes     scs_result_t
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(scs_u32_t){Index} +
               SizeOf(scs_value_type_t){Value type} +
               SizeOf(scs_u32_t){Flags} +
               SizeOf(scs_result_t){Result};
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTER);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteInteger(CurrPtr,Flags);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes      32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_INDEX(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTER_BY_INDEX,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name      variable      String 
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER_BY_NAME(Name: TelemetryString): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTER_BY_NAME,Name);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Primary type      1 byte     Boolean
    Secondary types   4 byte     32b uint
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTER_ALL(RegPrimaryType: Boolean; SecondarySelectionMask: Longword): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               3 * SizeOf(Boolean){Primary,Secondary,Tertiary types};
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTER_ALL);
Ptr_WriteBoolean(CurrPtr,RegPrimaryType);
Ptr_WriteInteger(CurrPtr,SecondarySelectionMask);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name         variable    String
    Index        4 bytes     scs_u32_t
    Value type   4 bytes     scs_value_type_t
    Result       4 bytes     scs_result_t
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; ResultCode: scs_result_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(scs_u32_t){Index} +
               SizeOf(scs_value_type_t){Value type} +
               SizeOf(scs_result_t){Result};
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_UNREGISTER);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteInteger(CurrPtr,ResultCode);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes      32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_INDEX(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_UNREGISTER_INDEX,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index     4 bytes      32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name      variable      String 
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_BY_NAME(Name: TelemetryString): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_UNREGISTER_BY_NAME,Name);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_UNREGISTER_ALL: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_UNREGISTER_ALL);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name      variable    String
    ID        4 bytes     TChannelID
    Index     4 bytes     scs_u32_t
    Value     variable
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL(Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(TChannelID){ID} +
               SizeOf(scs_u32_t){Index} +
               Size_scs_value(Value^,True);
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_CHANNEL);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,ID);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_Write_scs_value(CurrPtr,Value^,Result.Size,True,True);
end;

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL(StoredChannel: TStoredChannel): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + Size_StoredChannel(StoredChannel);
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_CHANNEL);
Ptr_Write_StoredChannel(CurrPtr,StoredChannel,Result.Size,True);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count       4 bytes    32b int
    Info[]      variable   array of Info substructures

  Info substructure:
    Name        variable   String
    ID          4 bytes    TChannelID
    Index       4 bytes    scs_u32_t
    Value       variable
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_CHANNEL_BUFFERED(BufferedChannels: TCircularChannelsBuffer): TPacketBuffer;
var
  TraversalInfo:  TCircularBufferTraversalInfo;
  ChannelInfo:    TStoredChannel;
  CurrPtr:        Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get size of all buffered channels.
If BufferedChannels.TraverseFirst(TraversalInfo) then
  repeat
    ChannelInfo := PStoredChannel(TraversalInfo.Item)^;
    Inc(Result.Size,SizeOfString(ChannelInfo.Name){Name} +
                    SizeOf(TChannelID){ID} +
                    SizeOf(scs_u32_t){Index} +
                    Size_scs_value_localized(ChannelInfo.Value,True));
  until not BufferedChannels.TraverseNext(TraversalInfo);
BufferedChannels.TraverseClose(TraversalInfo);
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_CHANNEL_BUFFERED);
Ptr_WriteInteger(CurrPtr,BufferedChannels.Count);
while BufferedChannels.ContainsChannel do
  begin
    ChannelInfo := BufferedChannels.PeekChannel;
    Ptr_WriteString(CurrPtr,ChannelInfo.Name);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.ID);
    Ptr_WriteInteger(CurrPtr,ChannelInfo.Index);
    Ptr_Write_scs_value_localized(CurrPtr,ChannelInfo.Value,Result.Size,True,True);
    BufferedChannels.RemoveChannel;
  end;
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name          variable   String
    Index         4 bytes    scs_u32_t
    Value type    4 bytes    scs_value_type_t
    Registered    1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(scs_u32_t){Index} +
               SizeOf(scs_value_type_t){Value type} +
               SizeOf(boolean){Registered};
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTERED);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteInteger(CurrPtr,ValueType);
Ptr_WriteBoolean(CurrPtr,Registered);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTERED_COUNT_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTERED_COUNT,Count);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTERED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    List index        4 bytes    32b int
    Name              variable   String
    ID                4 bytes    TChannelID
    Index             4 bytes    scs_u32_t
    Value type        4 bytes    scs_value_type_t
    Flags             4 bytes    scs_u32_t
    Index config ID   4 bytes    TItemID
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
If (Index >= 0) and (Index < TelemetryRecipient.RegisteredChannels.Count) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){List index} +
                   Size_ChannelInfo(TelemetryRecipient.RegisteredChannels[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_ChannelInfo(CurrPtr,TelemetryRecipient.RegisteredChannels[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){List index} +
                   Size_ChannelInfo(cEmptyChannelInfo);
    CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTERED_INDEX);
    Ptr_WriteInteger(CurrPtr,-1);
    Ptr_Write_ChannelInfo(CurrPtr,cEmptyChannelInfo,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_REGISTERED_ALL_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count             4 bytes    32b int
    Info[]            variable   array of Info substructures

  Info substructure:
    Name              variable   String
    ID                4 bytes    TChannelID
    Index             4 bytes    scs_u32_t
    Value type        4 bytes    scs_value_type_t
    Flags             4 bytes    scs_u32_t
    Index config ID   4 bytes    TItemID
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_REGISTERED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size to store all registered channels.
For i := 0 to Pred(TelemetryRecipient.RegisteredChannels.Count) do
  Inc(Result.Size,Size_ChannelInfo(TelemetryRecipient.RegisteredChannels[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_CHANNEL_REGISTERED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.RegisteredChannels.Count);
// Fill info about all registered channels.
For i := 0 to Pred(TelemetryRecipient.RegisteredChannels.Count) do
  Ptr_Write_ChannelInfo(CurrPtr,TelemetryRecipient.RegisteredChannels[i],Result.Size);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CHANNEL_STORED_SEND_ALL: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CHANNEL_STORED_SEND_ALL);
end;

{------------------------------------------------------------------------------}
{   Configs                                                                    }
{------------------------------------------------------------------------------}

{ Payload structure:

    Name     variable   String
    ID       4 bytes    TConfigID
    Index    4 bytes    scs_u32_t
    Value    variable
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_CONFIG(Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(TConfigID){ID} +
               SizeOf(scs_u32_t){Index} +
               Size_scs_value_localized(Value,True);
CurrPtr := PreparePacket(Result,TC_PACKET_CONFIG_CONFIG);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,ID);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_Write_scs_value_localized(CurrPtr,Value,Result.Size,True,True);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Name      variable   String
    Index     4 bytes    scs_u32_t
    Stored    1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED(Name: TelemetryString; Index: scs_u32_t; Stored: Boolean): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOfString(Name){Name} +
               SizeOf(scs_u32_t){Index} +
               SizeOf(Boolean){Stored};
CurrPtr := PreparePacket(Result,TC_PACKET_CONFIG_STORED);
Ptr_WriteString(CurrPtr,Name);
Ptr_WriteInteger(CurrPtr,Index);
Ptr_WriteBoolean(CurrPtr,Stored);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_COUNT_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CONFIG_STORED_COUNT_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_COUNT(Count: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CONFIG_STORED_COUNT,Count);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Index   4 bytes     32b int
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_GET(Index: Integer): TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CONFIG_STORED_INDEX_GET,Index);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    List index    4 bytes    32b int
    Name          variable   String
    ID            4 bytes    TConfigID
    Index         4 bytes    scs_u32_t
    Value         variable
    Binded        1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX(TelemetryRecipient: TTelemetryRecipient; Index: Integer): TPacketBuffer;
var
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(Integer){list index} +
               SizeOf(TConfigID){id} +
               SizeOf(scs_u32_t){index} +
               SizeOf(scs_value_type_t){value type} +
               SizeOf(Boolean){binded};
If (Index >= 0) and (Index < TelemetryRecipient.StoredConfigs.Count - 1) then
  begin
    // Index is valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){List index} +
                   Size_StoredConfig(TelemetryRecipient.StoredConfigs[Index]);
    CurrPtr := PreparePacket(Result,TC_PACKET_CONFIG_STORED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_StoredConfig(CurrPtr,TelemetryRecipient.StoredConfigs[Index],Result.Size);
  end
else
  begin
    // Index is NOT valid.
    Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){List index} +
                   Size_StoredConfig(cEmptyStoredConfig);
    CurrPtr := PreparePacket(Result,TC_PACKET_CONFIG_STORED_INDEX);
    Ptr_WriteInteger(CurrPtr,Index);
    Ptr_Write_StoredConfig(CurrPtr,cEmptyStoredConfig,Result.Size);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_INDEX_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CONFIG_STORED_INDEX_ALL_GET);
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL_GET: TPacketBuffer;
begin
Result := BuildPacket(TC_PACKET_CONFIG_STORED_ALL_GET);
end;

//------------------------------------------------------------------------------

{ Payload structure:

    Count         4 bytes    32b int
    Info[]        variable   array of Info substructures

  Info substructure:

    Name          variable   String
    ID            4 bytes    TConfigID
    Index         4 bytes    scs_u32_t
    Value type    4 bytes    scs_value_type_t
    Value         variable
    Binded        1 byte     Boolean
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_CONFIG_STORED_ALL(TelemetryRecipient: TTelemetryRecipient): TPacketBuffer;
var
  i:        Integer;
  CurrPtr:  Pointer;
begin
Result.Size := SizeOf(TPacketHeader) + SizeOf(Integer){Count};
// Get required size for all stored configs.
For i := 0 to Pred(TelemetryRecipient.StoredConfigs.Count) do
  Inc(Result.Size,Size_StoredConfig(TelemetryRecipient.StoredConfigs[i]));
CurrPtr := PreparePacket(Result,TC_PACKET_CONFIG_STORED_ALL);
Ptr_WriteInteger(CurrPtr,TelemetryRecipient.StoredConfigs.Count);
// Write all stored configs.
For i := 0 to Pred(TelemetryRecipient.StoredConfigs.Count) do
  Ptr_Write_StoredConfig(CurrPtr,TelemetryRecipient.StoredConfigs[i],Result.Size);
end;

{------------------------------------------------------------------------------}
{   Log                                                                        }
{------------------------------------------------------------------------------}

{ Payload structure:

    Log type   4 bytes    scs_log_type_t
    Log text   variable   String
}
Function TTelemetryCommPacketsBuilder.BuildPacket_TC_PACKET_LOG_LOG(LogType: scs_log_type_t; LogText: String): TPacketBuffer;
var
  CurrPtr:  Pointer;
  TempStr:  TelemetryString;
begin
TempStr := TelemetryStringEncode(LogText);
Result.Size := SizeOf(TPacketHeader) +
               SizeOf(scs_log_type_t){Log type} +
               SizeOfString(TempStr){Log text};
CurrPtr := PreparePacket(Result,TC_PACKET_LOG_LOG);
Ptr_WriteInteger(CurrPtr,LogType);
Ptr_WriteString(CurrPtr,TempStr);
end;

//==============================================================================

Function TTelemetryCommPacketsBuilder.MinimalPacketPayloadSize(PacketID: TPacketID): LongWord;
begin
case PacketID of
  // TC_PREFIX_COMMON prefix.
  TC_PACKET_PING:                             Result := 0;
  TC_PACKET_PING_RESPONSE:                    Result := SizeOf(TPacketHeaderTime);
  TC_PACKET_PROTOCOL_VERSION_GET:             Result := 0;
  TC_PACKET_PROTOCOL_VERSION:                 Result := SizeOf(TProtocolVersion);
  TC_PACKET_TELEMETRY_INFO_GET:               Result := 0;
  TC_PACKET_TELEMETRY_INFO:                   Result := 2 * SizeOf(scs_u32_t) + 2 * SizeOfString;
  TC_PACKET_PACKET:                           Result := 0;
  TC_PACKET_ERROR:                            Result := 3 * SizeOf(Integer) + SizeOf(TPacketHeaderTime);
  TC_PACKET_DEFFERED:                         Result := 2 * SizeOf(Integer) + SizeOf(TPacketHeaderTime);

  // TC_PREFIX_KNOWN_EVENTS prefix.
  TC_PACKET_KNOWN_EVENTS_COUNT_GET:           Result := 0;
  TC_PACKET_KNOWN_EVENTS_COUNT:               Result := SizeOf(Integer);
  TC_PACKET_KNOWN_EVENTS_INDEX_GET:           Result := SizeOf(Integer);
  TC_PACKET_KNOWN_EVENTS_INDEX:               Result := SizeOf(Integer) + SizeOf(scs_event_t) + SizeOfString + 2 * SizeOf(Boolean);
  TC_PACKET_KNOWN_EVENTS_INDEX_ALL_GET:       Result := 0;
  TC_PACKET_KNOWN_EVENTS_ALL_GET:             Result := 0;
  TC_PACKET_KNOWN_EVENTS_ALL:                 Result := SizeOf(Integer);

  // TC_PREFIX_KNOWN_CHANNELS prefix.
  TC_PACKET_KNOWN_CHANNELS_COUNT_GET:         Result := 0;
  TC_PACKET_KNOWN_CHANNELS_COUNT:             Result := SizeOf(Integer);
  TC_PACKET_KNOWN_CHANNELS_INDEX_GET:         Result := SizeOf(Integer);
  TC_PACKET_KNOWN_CHANNELS_INDEX:             Result := SizeOf(Integer) + 2 * SizeOfString + SizeOf(TChannelID) + 3 * SizeOf(scs_value_type_t) + SizeOf(Boolean) + SizeOf(TItemID);
  TC_PACKET_KNOWN_CHANNELS_INDEX_ALL_GET:     Result := 0;
  TC_PACKET_KNOWN_CHANNELS_ALL_GET:           Result := 0;
  TC_PACKET_KNOWN_CHANNELS_ALL:               Result := SizeOf(Integer);

  // TC_PREFIX_KNOWN_CONFIGS prefix.
  TC_PACKET_KNOWN_CONFIGS_COUNT_GET:          Result := 0;
  TC_PACKET_KNOWN_CONFIGS_COUNT:              Result := SizeOf(Integer);
  TC_PACKET_KNOWN_CONFIGS_INDEX_GET:          Result := SizeOf(Integer);
  TC_PACKET_KNOWN_CONFIGS_INDEX:              Result := SizeOf(Integer) + SizeOfString + SizeOf(TConfigID) + SizeOf(scs_value_type_t) + 2 * SizeOf(Boolean);
  TC_PACKET_KNOWN_CONFIGS_INDEX_ALL_GET:      Result := 0;
  TC_PACKET_KNOWN_CONFIGS_ALL_GET:            Result := 0;
  TC_PACKET_KNOWN_CONFIGS_ALL:                Result := SizeOf(Integer);

  // TC_PREFIX_EVENT prefix.
  TC_PACKET_EVENT_REGISTER:                   Result := SizeOf(scs_event_t) + SizeOf(scs_result_t);
  TC_PACKET_EVENT_REGISTER_BY_INDEX:          Result := SizeOf(Integer);
  TC_PACKET_EVENT_REGISTER_ALL:               Result := 0;
  TC_PACKET_EVENT_UNREGISTER:                 Result := SizeOf(scs_event_t) + SizeOf(scs_result_t);
  TC_PACKET_EVENT_UNREGISTER_BY_INDEX:        Result := SizeOf(Integer);
  TC_PACKET_EVENT_UNREGISTER_ALL:             Result := 0;
  TC_PACKET_EVENT_EVENT:                      Result := SizeOf(scs_event_t);
  TC_PACKET_EVENT_REGISTERED:                 Result := SizeOf(scs_event_t) + SizeOf(Boolean);
  TC_PACKET_EVENT_REGISTERED_COUNT_GET:       Result := 0;
  TC_PACKET_EVENT_REGISTERED_COUNT:           Result := SizeOf(Integer);
  TC_PACKET_EVENT_REGISTERED_INDEX_GET:       Result := SizeOf(Integer);
  TC_PACKET_EVENT_REGISTERED_INDEX:           Result := SizeOf(Integer) + SizeOf(scs_event_t) + SizeOf(Boolean);
  TC_PACKET_EVENT_REGISTERED_INDEX_ALL_GET:   Result := 0;
  TC_PACKET_EVENT_REGISTERED_ALL_GET:         Result := 0;
  TC_PACKET_EVENT_REGISTERED_ALL:             Result := SizeOf(Integer);

  // TC_PREFIX_CHANNEL prefix.
  TC_PACKET_CHANNEL_REGISTER:                 Result := SizeOfString + 2 * SizeOf(scs_u32_t) + SizeOf(scs_value_type_t) + SizeOf(scs_result_t);
  TC_PACKET_CHANNEL_REGISTER_BY_INDEX:        Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_REGISTER_BY_NAME:         Result := SizeOfString;
  TC_PACKET_CHANNEL_REGISTER_ALL:             Result := SizeOf(Boolean) + SizeOf(LongWord);
  TC_PACKET_CHANNEL_UNREGISTER:               Result := SizeOfString + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t) + SizeOf(scs_result_t);
  TC_PACKET_CHANNEL_UNREGISTER_BY_INDEX:      Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_UNREGISTER_BY_NAME:       Result := SizeOfString;
  TC_PACKET_CHANNEL_UNREGISTER_ALL:           Result := 0;
  TC_PACKET_CHANNEL_CHANNEL:                  Result := Succ(SizeOfString + SizeOf(TChannelID) + SizeOf(scs_u32_t));
  TC_PACKET_CHANNEL_CHANNEL_BUFFERED:         Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_REGISTERED:               Result := SizeOfString + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t) + SizeOf(Boolean);
  TC_PACKET_CHANNEL_REGISTERED_COUNT_GET:     Result := 0;
  TC_PACKET_CHANNEL_REGISTERED_COUNT:         Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_REGISTERED_INDEX_GET:     Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_REGISTERED_INDEX:         Result := SizeOf(Integer) + SizeOfString + SizeOf(TChannelID) + 2 * SizeOf(scs_u32_t) + SizeOf(scs_value_type_t) + SizeOf(TItemID);
  TC_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: Result := 0;
  TC_PACKET_CHANNEL_REGISTERED_ALL_GET:       Result := 0;
  TC_PACKET_CHANNEL_REGISTERED_ALL:           Result := SizeOf(Integer);
  TC_PACKET_CHANNEL_STORED_SEND_ALL:          Result := 0;

  // TC_PREFIX_CONFIG prefix.
  TC_PACKET_CONFIG_CONFIG:                    Result := Succ(SizeOfString + SizeOf(TConfigID) + SizeOf(scs_u32_t));
  TC_PACKET_CONFIG_STORED:                    Result := SizeOfString + SizeOf(scs_u32_t) + SizeOf(Boolean);
  TC_PACKET_CONFIG_STORED_COUNT_GET:          Result := 0;
  TC_PACKET_CONFIG_STORED_COUNT:              Result := SizeOf(Integer);
  TC_PACKET_CONFIG_STORED_INDEX_GET:          Result := SizeOf(Integer);
  TC_PACKET_CONFIG_STORED_INDEX:              Result := Succ(SizeOf(Integer) + SizeOfString + SizeOf(TConfigID) + SizeOf(scs_u32_t) + SizeOf(Boolean));
  TC_PACKET_CONFIG_STORED_INDEX_ALL_GET:      Result := 0;
  TC_PACKET_CONFIG_STORED_ALL_GET:            Result := 0;
  TC_PACKET_CONFIG_STORED_ALL:                Result := SizeOf(Integer);

  // TC_PREFIX_LOG prefix.
  TC_PACKET_LOG_LOG:  Result := SizeOf(scs_log_type_t) + SizeOfString;
else
  raise Exception.Create('TTelemetryCommPacketsBuilder.MinimalPacketPayloadSize: Unknown packet (0x' + IntToHex(PacketID,8) + ').');
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryCommPacketsBuilder.CheckPacketPayloadSize(Packet: TPacketBuffer): Boolean;
begin
Result := GetPacketHeader(Packet).PayloadSize >= MinimalPacketPayloadSize(GetPacketHeader(Packet).PacketID);
end;

//==============================================================================

procedure TTelemetryCommPacketsBuilder.ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID);
begin
PPacketHeader(Packet.Data)^.PacketID := NewPacketID;
PPacketHeader(Packet.Data)^.TimeStamp := DateTimeToTimeStamp(Now);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommPacketsBuilder.ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID; DataOffset, DataSize: LongWord; Data: Pointer);
begin
If (DataOffset + DataSize) <= GetPacketHeader(Packet).PayloadSize then
  begin
    PPacketHeader(Packet.Data)^.PacketID := NewPacketID;
    PPacketHeader(Packet.Data)^.TimeStamp := DateTimeToTimeStamp(Now);
    CopyMemory(Pointer(PtrUInt(GetPayloadAddress(Packet)) + DataOffset),Data,DataSize);
  end
else raise Exception.Create('TelemetryNetPacketsResolving.ReusePacket(Offset): Data cannot fit into reused packet.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommPacketsBuilder.ReusePacket(var Packet: TPacketBuffer; NewPacketID: TPacketID; Address: Pointer; DataSize: LongWord; Data: Pointer);
begin
If (PtrUInt(Address) >= PtrUInt(GetPayloadAddress(Packet))) and
   (PtrUInt(Address) <= (PtrUInt(GetPayloadAddress(Packet)) + GetPacketHeader(Packet).PayloadSize)) then
  begin
    If (PtrUInt(Address) - PtrUInt(Packet.Data) + DataSize) <= GetPacketHeader(Packet).PayloadSize then
      begin
        PPacketHeader(Packet.Data)^.PacketID := NewPacketID;
        PPacketHeader(Packet.Data)^.TimeStamp := DateTimeToTimeStamp(Now);
        CopyMemory(Address,Data,DataSize);
      end
    else raise Exception.Create('TelemetryNetPacketsResolving.ReusePacket(Address): Data cannot fit into reused packet.');
  end
else raise Exception.Create('TelemetryNetPacketsResolving.ReusePacket(Address): Address is outside of allowed boundary.');
end;

end.
