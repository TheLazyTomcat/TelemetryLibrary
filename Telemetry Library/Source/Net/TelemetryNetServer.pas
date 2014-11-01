{*******************************************************************************
@abstract(Telemetry network server class.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-13)
@lastmod(2013-10-13)

  TelemetryNetServer

  ©František Milt, all rights reserved.

  This unit contains TTelemetryNetServer class used as telemetry server (see
  class declaration for details).

  Included files:@preformatted(
    .\INC\TTelemetryNetServer.ProcessPacket_TN_PACKET.pas)

  Last change:  2013-10-13

  Change List:@unorderedList(
    @item(2013-10-13  - First stable version.))

*******************************************************************************}
unit TelemetryNetServer;

interface

{$INCLUDE '..\Telemetry_defs.inc'}
uses
  Classes, WinSock, ScktComp,
  TelemetryCommon,
  TelemetryIDs,
  TelemetryVersionObjects,
  TelemetryLists,
  TelemetryRecipient,
  TelemetryNetCommon,
  TelemetryNetLists,
  TelemetryNetCircularBuffers,
  TelemetryNetSockets,
  TelemetryNetPackets,
{$IFDEF Documentation}
  TelemetryNetHashing,
  TelemetryNetPacketsBuilding,
  TelemetryNetPacketsResolving,
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
{                             TTelemetryNetServer                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TTelemetryNetServer // Class declaration                                  }
{==============================================================================}
{
  @abstract(Telemetry server class.)
  Telemetry server runs inside of telemetry plugin (library) and is intended
  to provide communication interface between client application(s) and telemetry
  API (with help of telemetry recipient). It is designed using windows sockets
  and thus can communicate through any network supporting TCP/IP protocol
  (Ethernet, Internet, ...).@br
  Communication between server and client(s) is realized using variable length
  packets - see unit TelemetryNetPackets for details.

  @bold(Note:) event hadlers dont have documented parameters, instead a link
  to event they are handling is provided where possible. Refer to this link for
  required informations.    

@member(fAddressList See AddressList property.)

@member(fServerSocket
    Main object used for communication. It is delphi-specific socket wrapper.@br
    Methods of this object are used to send and receive data to/from connection.
    Also, some events of this object are handled by designated server methods
    (OnClientConnectHandler, OnClientDisconnectHandler, OnClientErrorHandler,
    OnClientReadHandler, OnClientWriteHandler, OnGetSocketHandler).@br
    @bold(Note:) - internal socket endpoint objects are of specialized class
    (see OnGetSocketHandler method).)

@member(fBufferedChannels
    When BufferChannels property is set to @true, any channel received from
    telemetry recipient is stored in this circular buffer and sent later in one
    large packet when any telemetry event is received. It is there to minimize
    flood of TN_PACKET_CHANNEL_CHANNEL packets that will otherwise occur (at
    extreme cases, over 15000 packets can be sent in one second).)

@member(fTelemetryRecipient See TelemetryRecipient property.)

@member(fDefferedOperations
    Some operations (e.g. event registration) cannot be executed any time, but
    only in telemetry callbacks. But from nature of network comunnication, they
    can be requested pretty much any time. When request for such operation
    arriwes, it is stored with its parameters into this object. Later, when
    these operations can be executed (generally when telemetry event is
    received), they are performed and removed from buffer.@br
    This system requires that telemetry events are regularly received. Therefore
    it does not work if no event is registered - but, in this system, no event
    can be registered outside of event callback. To resolve this problem,
    telemetry recipient must be created with
    @link(TTelemetryRecipient.KeepUtilityEvents KeepUtilityEvents) property set
    to @true (default settings).)

@member(fPassword See Password property.)

@member(fAdminPassword See AdminPassword property.)

@member(fSuperAdminPassword See SuperAdminPassword property.)

@member(fSendEvents See SendEvents property.)

@member(fSendChannels See SendChannels property.)

@member(fSendConfigs See SendConfigs property.)

@member(fBufferChannels See BufferChannels property.)

@member(fSendFrameEvents See SendFrameEvents property.)

@member(fSaveSettingsToFile See SaveSettingsToFile property.)

@member(fOnClientConnect Holds reference to OnClientConnect event handler.)

@member(fOnClientDisconnect Holds reference to OnClientDisconnect event
    handler.)

@member(fOnClientError Holds reference to OnClientError event handler.)

@member(fOnClientRead Holds reference to OnClientRead event handler.)

@member(fOnClientWrite Holds reference to OnClientWrite event handler.)

@member(fOnGetSocket Holds reference to OnGetSocket event handler.)

@member(fOnStatusChange Holds reference to OnStatusChange event handler.)

@member(fOnPacketDrop Holds reference to OnPacketDrop event handler.)

@member(fOnOverflowClear Holds reference to OnOverflowClear event handler.)

@member(fOnPacketReceived Holds reference to OnPacketReceived event handler.)

@member(fOnPacketSending Holds reference to OnPacketSending event handler.)

@member(fOnPacketSend Holds reference to OnPacketSend event handler.)

@member(fOnLog Holds reference to OnLog event handler.)

@member(fOnEventRegister Holds reference to OnEventRegister event handler.)

@member(fOnEventUnregister Holds reference to OnEventUnregister event handler.)

@member(fOnEvent Holds reference to OnEvent event handler.)

@member(fOnChannelRegister Holds reference to OnChannelRegister event handler.)

@member(fOnChannelUnregister Holds reference to OnChannelUnregister event
    handler.)

@member(fOnChannel Holds reference to OnChannel event handler.)

@member(fOnChannelBuffering Holds reference to OnChannelBuffering event
    handler.)

@member(fOnConfig Holds reference to OnConfig event handler.)

@member(fOnDestroy Holds reference to OnDestroy event handler.)

@member(GetConnectedClientsCount
    Getter for ConnectedClientsCount property.
    @returns Number of connected clients.)

@member(GetListenPort
    Getter for Port property.
    @returns Port at which the server is listening.)

@member(GetBufferedChannelsCount
    Getter for BufferedChannelsCount property.
    @returns Number of channels buffered.)

@member(GetDefferedOperationsCount
    Getter for DefferedOperationsCount.
    @returns Number of deffered operations.)

@member(GetServerVersion
    Getter for ServerVersion property.@br
    Server version is not stored in object instance, instead it is a global
    constant defined in implemetation section named cCurrentServerVersion.
    @returns Server version.)

@member(SetPassword
    Setter for Password property.@br
    When @noAutoLink(password) is changed, all clients get disconnected with
    reason set to @code(drPasswordChange).
    @param Value New value to be stored in fPassword field.)

@member(SetAdminPassword
    Setter for AdminPassword property.@br
    When @noAutoLink(password) is changed, all administrators are stripped of
    their admin rights.
    @param Value New value to be stored in fAdminPassword field.)

@member(SetSuperAdminPassword
    Setter for SuperAdminPassword property.@br
    When @noAutoLink(password) is changed, all super-administrators are stripped
    of their super-admin rights.
    @param Value New value to be stored in fSuperAdminPassword field.)

@member(SetSendEvents
    Setter for SendEvents property.
    @param Value New value to be stored in fSendEvents field.)

@member(SetSendChannels
    Setter for SendChannels property.
    @param Value New value to be stored in fSendChannels field.)

@member(SetSendConfigs
    Setter for SendConfigs property.
    @param Value New value to be stored in fSendConfigs field.)

@member(GetActiveMode
    Getter for ActiveMode property.
    @returns(@True when SendEvents, SendChannels and SendConfigs are all @true,
             @false otherwise.))

@member(SetActiveMode
    Setter for ActiveMode property.@br
    @param(Value Value to be stored in fSendEvents, fSendChannels and
                 fSendConfigs fields.))

@member(SetBufferChannels
    Setter for BufferChannels property.@br
    When new value is @false, then before new value is actually stored all
    buffered channels are sent.
    @param Value New value to be stored in fBufferChannels field.)

@member(SetSendFrameEvents
    Setter for SendFrameEvents property.
    @param Value New value to be stored in fSendFrameEvents field.)

@member(GetOnAccept
    Getter for OnAccept property.
    @returns Handler of fServerSocker.OnAccept event.)

@member(SetOnAccept
    Setter for OnAccept property.
    @param Value New handler for fServerSocker.OnAccept event. Can be @nil.)

@member(GetOnListen
    Getter for OnListen property.
    @returns Handler of fServerSocker.OnListen event.)

@member(SetOnListen
    Setter for OnListen property.
    @param Value New handler for fServerSocker.OnListen event. Can be @nil.)

@member(GetOnGetThread
    Getter for OnGetThread property.
    @returns Handler of fServerSocker.OnGetThread event.)

@member(SetOnGetThread
    Setter for OnGetThread property.
    @param Value New handler for fServerSocker.OnGetThread event. Can be @nil.)

@member(GetOnThreadEnd
    Getter for OnThreadEnd property.
    @returns Handler of fServerSocker.OnThreadEnd event.)

@member(SetOnThreadEnd
    Setter for OnThreadEnd property.
    @param Value New handler for fServerSocker.OnThreadEnd event. Can be @nil.)

@member(GetOnThreadStart
    Getter for OnThreadStart property.
    @returns Handler of fServerSocker.OnThreadStart event.)

@member(SetOnThreadStart
    Setter for OnThreadStart property.
    @param(Value New handler for fServerSocker.OnThreadStart event. Can be
    @nil.))



@member(OnAddressListChangeHandler
    Handler for fAddressList.OnChange event.@br
    At the moment, only thing it does is informing connected administrators
    about changes in address list (item addition, removal, ...).)

@member(OnClientConnectHandler
    Handler for fServerSocket.OnClientConnect event.@br
    It handles connection af every client, for example informs administrators or
    checks whether client can actually connect (depending on its presence in
    address list).)

@member(OnClientDisconnectHandler
    Handler for fServerSocket.OnClientDisconnect event.@br
    Clears incoming and outgoing buffers.)

@member(OnClientErrorHandler
    Handler for fServerSocket.OnClientError envet.@br
    Set ErrorCode to zero to stop further error propagation (prevents exception
    raising).)

@member(OnClientReadHandler
    Handler for fServerSocket.OnClientRead event.@br
    Manages data retrieval for all individual endpoint sockets. Received data
    are read to buffer and then passed to incoming stream object belinging to
    appropriate endpoint socket object. Stream object then handles this data and
    further process then to get valid packets.)

@member(OnClientWriteHandler
    Handler for fServerSocket.OnClientWrite event.@br
    Called when client endpoint socket is ready to send data. Thes method peeks
    and tries to write packets stored in outgoing buffer belonging to calling
    socket. When buffered packet is successfuly written, it is removed from
    buffer and next stored packet is attempted to be written until all stored
    packkets are send (written) or socket signals that it cannot send more
    data.)

@member(OnGetSocketHandler
    Handler for fServerSocket.OnGetSocket event.@br
    Called when server is creating endpoint socket for connecting client. This
    method ensures the newly created endpoint socket will be of
    TTelemetryNetServerClientWinSocket class and assigns its event handlers.)


@member(OnStatusChangeHandler
    Handler for client @link(TTelemetryNetServerClientWinSocket.OnStatusChange
    OnStatusChange) event. It is assigned as handler of mentioned event for all
    endpoint client sockets.@br
    Called when client socket changes its status.)

@member(OnPacketDropHandler
    Handler for client's outgoing buffer
    @link(TCircularPacketsBuffer.OnPacketDrop OnPacketDrop) event. Assigned as
    handler for all clients.@br
    Called when client's outgoing buffer must drop old packet to make room for
    a new one.)

@member(OnOverflowClearHandler
    Handler for clients's incoming stream @link(TIncomingStream.OnOverflowClear
    OnOverflowClear) event. Assigned as handler for all clients.@br
    Called just before client's incoming stream buffer clears itself.)

@member(OnPacketHandler
    Handler for client's incoming stream @link(TIncomingStream.OnPacket
    OnPacket) event. Assigned as handler for all clients.@br
    Called when client's incoming stream passes new incoming packet. This method
    takes passed packet, look on its ID and, depending on packet ID prefix,
    passes it to one of packet group processing methods (only when client
    endpoint can receive such packet, refer to method implementation for
    details).)

@member(OnLogHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnLog OnLog)
    event.@br
    Called when recipient attempts to write to game log.)

@member(OnEventRegisterHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnEventRegister
    OnEventRegister) event.@br
    Called when recipient successfuly registers telemetry event.)

@member(OnEventUnregisterHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnEventUnregister
    OnEventUnregister) event.@br
    Called when recipient successfuly unregisters telemetry event.)

@member(OnEventHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnEvent OnEvent)
    event.@br
    Called when recipient receives an event from telemetry API. This method
    processes passed event data and, depending on server settings, send it to
    clients.)

@member(OnChannelRegisterHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnChannelRegister
    OnChannelRegister) event.@br
    Called when recipient successfuly registers telemetry channel.)

@member(OnChannelUnregisterHandler
    Handler for telemetry recipient
    @link(TTelemetryRecipient.OnChannelUnregister OnChannelUnregister) event.@br
    Called when recipient successfuly unregisters telemetry channel.)

@member(OnChannelHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnChannel
    OnChannel) event.@br
    Called when recipient receives channel from telemetry API. This method
    processes passed channel and, depending on server settings, send it to
    clients or buffers it.)

@member(OnConfigHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnConfig OnConfig)
    event.@br
    Called when recipient parses config. This method processes passed
    configuration and, depending on server settings, send it to clients.)

@member(OnDestroyHandler
    Handler for telemetry recipient @link(TTelemetryRecipient.OnDestroy
    OnDestroy) event.@br
    Called when recipient is destroyed (freed). When this method is called, it
    forces server instance to destroy itself, as server cannot run without
    recipient.)

  Methods starting with @code(ProcessPacketGroup_) and @code(ProcessPacket_) all
  have following two parameters (they are therefore not documented for each
  individual method):
  @unorderedList(
    @itemSpacing(Compact)
    @item(@bold(Packet) - Packet that has to be processed by given method.)
    @item(@bold(Socket) - Endpoint client socket that has received processed
          packet. It is used when processed packet requires a respond to be sent
          back to the sender.))

@member(ProcessPacketGroup_COMMON
    Process packets with ID prefix TN_PREFIX_COMMON.
    @returns @True when packet is successfuly processed, @false otherwise.)
  
@member(ProcessPacketGroup_ADMIN
    Process packets with ID prefix TN_PREFIX_ADMIN.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_SUPERADMIN
    Process packets with ID prefix TN_PREFIX_SUPERADMIN.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_EVENT_KNOWN
    Process packets with ID prefix TN_PREFIX_EVENT_KNOWN.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_CHANNEL_KNOWN
    Process packets with ID prefix TN_PREFIX_CHANNEL_KNOWN.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_CONFIG_KNOWN
    Process packets with ID prefix TN_PREFIX_CONFIG_KNOWN.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_EVENT
    Process packets with ID prefix TN_PREFIX_EVENT.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_CHANNEL
    Process packets with ID prefix TN_PREFIX_CHANNEL.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_CONFIG
    Process packets with ID prefix TN_PREFIX_CONFIG.
    @returns @True when packet is successfuly processed, @false otherwise.)

@member(ProcessPacketGroup_LOG
    Process packets with ID prefix TN_PREFIX_LOG.
    @returns @True when packet is successfuly processed, @false otherwise.)

  All @code(ProcessPacket_*) methods are intended to process individual packet.
  Which packet they process can be differentiated by method name. Name of each
  method contains full packet ID identifier.@br
  For details about individual method behavior, you have to refer to method
  implementation (complete documentation would be too extensive).

@member(GetClient
    Searches through connected clients for one with given ID.

    @param ClientID ID of requested client.
    @returns Reference to endpoint socket object for requested client, @nil when
             client is not found (is not connected).)

@member(SendBufferedChannels
    Sends all buffered channels and clears buffer (see fBufferedChannels for
    details).)

@member(ExecuteDefferedOperations
    Executes all deffered operations. They are executed in the same order as
    they arrived. See fDefferedOperations for details.

    @param(CallingEvent Telemetry event that has induced call to this method.
                        For details, refer to method implementation.))

@member(SendTo
    Sends passed packet to given client.

    @param(Socket     Endpoint client socket to which the packet will be send.
                      Must not be @nil.)
    @param Packet     Packet to be sent.
    @param(FreePacket When set to @true, then packet (packet memory image)
                      should be freed after packet is sent. When packet gets
                      buffered, and this parameter is @true, then only reference
                      is buffered and packet is not freed (it is freed later
                      when removed from buffer).@br
                      When @false and packet must be buffered, then packet is
                      copied in buffer.))

@member(SendToAll
    Sends passed packet to all connected clients.

    @param Packet     Packet to be sent.
    @param(FreePacket When set to @true, then packet (packet memory image)
                      should be freed after packet is sent.))


@member(SendServerSettings
    Sends server settings value(s) to client(s).

    @param(ParameterID ID of settings parameter to be sent. When set to
                       @code(pidAll), then all known parameters are sent.)
    @param(Socket      Endpoint socket of client to which the parameter should
                       be sent. When set to @nil, the parameter is sent to all
                       connected clients.))

@member(DisconnectClient
    Disconnects selected client.

    @param Socket Endpoint socket of client that will be disconnected.
    @param(Reason Reason why the client will be disconnected. See
                  TDisconnectReason for possible values.))

@member(DisconnectAllClients
    Disconnects all client of selected criteria.

    @param(Reason           Reason why the client will be disconnected. See
                            TDisconnectReason for possible values.)
    @param OnlyAdmins       Only administrators get disconnected.
    @param OnlySuperAdmins  Only super-administrators get disconnected.)

@member(LoadSettings
    Loads server settings from file. See SaveSettingsToFile property for
    details.

    @returns @True when loaded successfuly, @false otherwise (e.g. when settings
             file does not exists).)

@member(SaveSettings
    Save server settings to file. See SaveSettingsToFile property for details.

    @returns @True when saved successfuly, @false otherwise.)




@member(Create
    Object constructor.@br
    Telemetry and game versions with which the passed recipient was created are
    checked, and when server does not support these wersions, an exception is
    raised.

    @param aTelemetryRecipient Pre-created telemetry recipient.
    @param(Port                Port at which the server will listen for incoming
                               connections.))

@member(Destroy
    Object destructor.@br
    All clients are forced to disconnect, utility objects are freed and server
    settings are saved (when SaveSettingsToFile property is set to @true).)



@member(AddressList
    List used to store addresses and check whether client with particular
    address can, or cannot, depending on list mode, connect to the server.)

@member(ConnectedClientsCount
    Number of connected clients.)

@member(ListenPort
    Port at which the server listens for incomming connections. It is set in
    constructor and cannot be changed later.)

@member(BufferedChannelsCount
    Number of buffered channels. See fBufferedChannels object for details.)

@member(TelemetryRecipient
    Reference to telemetry recipient object. This object must be created outside
    of server (before the server is created), because it is passed to server
    constructor.@br
    Server binds recipients @link(TTelemetryRecipient.OnDestroy OnDestroy)
    event (in fact, server binds all recipient events), and when recipient is
    destroyed, server destroys itself too.)

@member(DefferedOperationsCount
    Munber of deffered operations stored in deffered operations buffer. See
    fDefferedOperations for details.)

@member(ServerVersion
    Version of created server. High word of this value is major version and low
    word is minor version.)

@member(Password
    Login @noAutoLink(password). All clients must log in with this
    @noAutoLink(password) when connecting, otherwise they are rejected and
    immediately disconnected.)

@member(AdminPassword
    Each client, when requesting administrative rigts, must demonstrate it knows
    this @noAutoLink(password), otherwise the admin rights are not granted.)

@member(SuperAdminPassword
    Each client, when requesting superadministrator rigts, must demonstrate it
    knows this @noAutoLink(password), otherwise the superadmin rights are not
    granted.)

@member(SendEvents
    Determines whether server should send telemetry events
    (TN_PACKET_EVENT_EVENT packets). When set to @true, events are sent.)

@member(SendChannels
    Determines whether server should send telemetry channels
    (TN_PACKET_CHANNEL_CHANNEL and TN_PACKET_CHANNEL_CHANNEL_BUFFERED packets).
    When set to @true, channels are sent.)

@member(SendConfigs
    Determines whether server should send configurations
    (TN_PACKET_CONFIG_CONFIG packets). When set to @true, configs are sent.)
    
@member(ActiveMode
    When server is in active mode, its sends all telemetry events, channels and
    configs. It is a composite property depending on SendEvents, SendChannels
    and SendConfigs properties. Active mode is @true only when all componnets
    are @true. When you set new value to this property, all components are set
    to his value too.)

@member(BufferChannels
    When set to @true, channels received from telemetry recipient are buffered
    and sent later in one large packet, otherwise they are sent as they arrive
    (see fBufferedChannels).)

@member(SendFrameEvents
    When set to @true, telemetry events SCS_TELEMETRY_EVENT_frame_start and
    SCS_TELEMETRY_EVENT_frame_end are sent normally, but when set to @false,
    these events are not sent to clients (but are processed as usual).@br
    It is there to minimize unnecessary traffic.)

@member(SaveSettingsToFile
    When @true, server settings are saved to configuration file on server
    destruction. Also, address list is saved.@br
    This file is located in the same folder as executable/library in which the
    server runs. At the moment, file name is "TelemetryNetServer.ini", but can
    be changed in future versions (name of file with address list can be set by
    user, but by default is "address.list.txt").)

@member(OnAccept
    Forwarded @noAutoLink(OnAccept) event of fServerSocket object.)

@member(OnListen
    Forwarded @noAutoLink(OnListen) event of fServerSocket object.)

@member(OnClientConnect
    Event called whenever new clients connects. Is called before actual client
    connection is processed, meaning client can be forced to disconnect right
    after this event.@br
    Called from fServerSocket.OnClientConnect event @link(OnClientConnectHandler
    handler method).)

@member(OnClientDisconnect
    Event called when any client disconnects (before actual disconnection is
    perfomed).@br
    Called from fServerSocket.OnClientDisconnect event
    @link(OnClientDisconnectHandler handler method).)

@member(OnClientError
    Event called whenever any error propagates itself from fServerSocket. This
    event is called @bold(before) the error is handled by server.@br
    Called from fServerSocket.OnClientError event @link(OnClientErrorHandler
    handler method).)

@member(OnClientRead
    Event called when data are read from client socket. Called before actual
    reading takes place.@br
    Called from fServerSocket.fOnClientRead event @link(OnClientReadHandler
    handler method).)

@member(OnClientWrite
    Event called when data are written to client socket. Called after data are
    written.@br
    Called from fServerSocket.fOnClientWrite event @link(OnClientWriteHandler
    handler method).)

@member(OnGetSocket
    Event called when new endpoint socket is required (when client is
    connecting).@br
    Called form fServerSocket.fOnGetSocket event @link(OnGetSocketHandler
    handler method).@br
    @bold(Warning!) - when this event is assigned, handler is not executed.
    So when you assign this event, you are responsible to to take all necessary
    actions (creating endpoint socket object, assigning event handlers, etc.).)

@member(OnGetThread
    Forwarded @noAutoLink(OnGetThread) event of fServerSocket object.)

@member(OnThreadEnd
    Forwarded @noAutoLink(OnThreadEnd) event of fServerSocket object.)

@member(OnThreadStart
    Forwarded @noAutoLink(OnThreadStart) event of fServerSocket object.)

@member(OnStatusChange
    Forwarded client @link(TTelemetryNetServerClientWinSocket.OnStatusChange
    OnStatusChange) event.@br
    Called when client socket changes its status (e.g. client is granted
    administrative rights), after processing original event.@br
    Paramaneter "Self" contains reference to client's endpoint socket object.)

@member(OnPacketDrop
    Forwarded client's outgoing buffer @link(TCircularPacketsBuffer.OnPacketDrop
    OnPacketDrop) event.@br
    Called when client's outgoing buffer must drop old packet to make room for
    a new one.@br
    Parameter "Self" contains reference to client's outgoing buffer object
    (TCircularPacketsBuffer class), "Socket" is client endpoint socket object
    and "Packet" is packet packet that will be dropped.)

@member(OnOverflowClear
    Forwarded client's incoming stream @link(TIncomingStream.OnOverflowClear
    OnOverflowClear) event.@br
    Called just before client's incoming stream buffer clears itself.@br
    Parameter "Self" contains reference to client's incoming stream.)

@member(OnPacketReceived
    Forwarded client's incoming stream @link(TIncomingStream.OnPacket OnPacket)
    event.@br
    Called when client's incoming stream passes new incoming packet, before this
    packet is processed by server. Called only when client endpoint socket
    object can receive passed packet.@br
    Parameter "Self" contains reference to client's incoming stream.)

@member(OnPacketSending
    Event called when server is about to send packet.@br
    @bold(Note:) Packet is not guaranteed to be actually sent, it can buffered
    when endpoint cannot send it immediately.)

@member(OnPacketSend
    Event called when server successfuly send packet.)

@member(OnLog
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnLog OnLog)
    event.@br
    Called when recipient writes to game log.)

@member(OnEventRegister
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnEventRegister
    OnEventRegister) event.@br
    Called when recipient successfuly registers new telemetry event.)

@member(OnEventUnregister
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnEventUnregister
    OnEventUnregister) event.@br
    Called when recipient successfuly unregisters registered telemetry event.)

@member(OnEvent
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnEvent OnEvent)
    event.@br
    Called when recipient passes telemetry event to server, after server
    processes and sends this event to clients.)

@member(OnChannelRegister
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnChannelRegister
    OnChannelRegisterr) event.@br
    Called when recipient successfuly registers new telemetry channel.)

@member(OnChannelUnregister
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnChannelUnregister
    OnChannelUnregisterr) event.@br
    Called when recipient successfuly unregisters registered telemetry channel.)

@member(OnChannel
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnChannel OnChannel)
    event.@br
    Called when recipient passes telemetry channel to server, after server
    processes and sends or buffers this channel.)

@member(OnChannelBuffering
    Called immediately after server buffers channel passed from telemetry
    recipient (see BufferChannels property).)

@member(OnConfig
    Forwarded telemetry recipient @link(TTelemetryRecipient.OnConfig OnConfig)
    event.@br
    Called when recipient passes new or changed configuration value to server.)

@member(OnDestroy
    Called when instance of server is destroyed, at the beginning of
    destructor.)
}
type
  TTelemetryNetServer = class(TTelemetryVersionObject)
  private
    fAddressList:         TAddressList;
    fServerSocket:        TServerSocket;
    fBufferedChannels:    TCircularChannelsBuffer;
    fTelemetryRecipient:  TTelemetryRecipient;
    fDefferedOperations:  TDefferedOperationsBuffer;
    fPassword:            String;
    fAdminPassword:       String;
    fSuperAdminPassword:  String;
    fSendEvents:          Boolean;
    fSendChannels:        Boolean;
    fSendConfigs:         Boolean;
    fBufferChannels:      Boolean;
    fSendFrameEvents:     Boolean;
    fSaveSettingsToFile:  Boolean;
    fOnClientConnect:     TSocketNotifyEvent;
    fOnClientDisconnect:  TSocketNotifyEvent;
    fOnClientError:       TSocketErrorEvent;
    fOnClientRead:        TSocketNotifyEvent;
    fOnClientWrite:       TSocketNotifyEvent;
    fOnGetSocket:         TGetSocketEvent;
    fOnStatusChange:      TNotifyEvent;
    fOnPacketDrop:        TPacketNotifyEvent;
    fOnOverflowClear:     TSocketNotifyEvent;    
    fOnPacketReceived:    TPacketNotifyEvent;
    fOnPacketSending:     TPacketNotifyEvent;
    fOnPacketSend:        TPacketNotifyEvent;
    fOnLog:               TLogEvent;
    fOnEventRegister:     TEventRegisterEvent;
    fOnEventUnregister:   TEventRegisterEvent;
    fOnEvent:             TEventEvent;
    fOnChannelRegister:   TChannelRegisterEvent;
    fOnChannelUnregister: TChannelUnregisterEvent;
    fOnChannel:           TChannelEvent;
    fOnChannelBuffering:  TChannelEvent;
    fOnConfig:            TConfigEvent;
    fOnDestroy:           TNotifyEvent;
    Function GetConnectedClientsCount: Integer;
    Function GetListenPort: Word;
    Function GetBufferedChannelsCount: Integer;
    Function GetDefferedOperationsCount: Integer;
    Function GetServerVersion: LongWord;
    procedure SetPassword(Value: String);
    procedure SetAdminPassword(Value: String);
    procedure SetSuperAdminPassword(Value: String);
    procedure SetSendEvents(Value: Boolean);
    procedure SetSendChannels(Value: Boolean);
    procedure SetSendConfigs(Value: Boolean);
    Function GetActiveMode: Boolean;
    procedure SetActiveMode(Value: Boolean);
    procedure SetBufferChannels(Value: Boolean);
    procedure SetSendFrameEvents(Value: Boolean);
    Function GetOnAccept: TSocketNotifyEvent;
    procedure SetOnAccept(Value: TSocketNotifyEvent);
    Function GetOnListen: TSocketNotifyEvent;
    procedure SetOnListen(Value: TSocketNotifyEvent);
    Function GetOnGetThread: TGetThreadEvent;
    procedure SetOnGetThread(Value: TGetThreadEvent);
    Function GetOnThreadEnd: TThreadNotifyEvent;
    procedure SetOnThreadEnd(Value: TThreadNotifyEvent);
    Function GetOnThreadStart: TThreadNotifyEvent;
    procedure SetOnThreadStart(Value: TThreadNotifyEvent);
  protected
    procedure OnAddressListChangeHandler(Sender: TObject); virtual; 
    procedure OnClientConnectHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnClientDisconnectHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnClientErrorHandler(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual;
    procedure OnClientReadHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnClientWriteHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;
    procedure OnGetSocketHandler(Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket); virtual;
    procedure OnStatusChangeHandler(Sender: TObject); virtual;
    procedure OnPacketDropHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer); virtual;
    procedure OnOverflowClearHandler(Sender: TObject; Socket: TCustomWinSocket); virtual;    
    procedure OnPacketHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer); virtual;
    procedure OnLogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); virtual;
    procedure OnEventRegisterHandler(Sender: TObject; Event: scs_event_t); virtual;
    procedure OnEventUnregisterHandler(Sender: TObject; Event: scs_event_t); virtual;
    procedure OnEventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer); virtual;
    procedure OnChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
    procedure OnChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
    procedure OnChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
    procedure OnConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
    procedure OnDestroyHandler(Sender: TObject); virtual;
    Function ProcessPacketGroup_COMMON(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_ADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_SUPERADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_EVENT_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_CHANNEL_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_CONFIG_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_EVENT(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_CHANNEL(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_CONFIG(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    Function ProcessPacketGroup_LOG(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean; virtual;
    // Methods processing specific received packets.
    {$DEFINE Declaration_part}
    {$INCLUDE '.\INC\TTelemetryNetServer.ProcessPacket_TN_PACKET.pas'}
    {$UNDEF Declaration_part}
    Function GetClient(ClientID: TGUID): TNSCWinSocket; virtual;
    procedure SendBufferedChannels; virtual;
    procedure ExecuteDefferedOperations(CallingEvent: scs_event_t = SCS_TELEMETRY_EVENT_invalid ); virtual;
    procedure SendTo(Socket: TCustomWinSocket; Packet: TPacketBuffer; FreePacket: Boolean); virtual;
    procedure SendToAll(Packet: TPacketBuffer; FreePacket: Boolean); virtual;
    procedure SendServerSettings(ParameterID: TParameterID = pidAll; Socket: TCustomWinSocket = nil); virtual;
    procedure DisconnectClient(Socket: TCustomWinSocket; Reason: TDisconnectReason = drGeneral); virtual;
    procedure DisconnectAllClients(Reason: TDisconnectReason = drGeneral; OnlyAdmins: Boolean = False; OnlySuperAdmins: Boolean = False); virtual;
    Function LoadSettings: Boolean; virtual;
    Function SaveSettings: Boolean; virtual;
  public
    constructor Create(aTelemetryRecipient: TTelemetryRecipient; Port: Word = def_ServerPort);
    destructor Destroy; override;
  published
    property AddressList: TAddressList read fAddressList;
    property ConnectedClientsCount: Integer read GetConnectedClientsCount;
    property ListenPort: Word read GetListenPort;
    property BufferedChannelsCount: Integer read GetBufferedChannelsCount;
    property TelemetryRecipient: TTelemetryRecipient read fTelemetryRecipient;
    property DefferedOperationsCount: Integer read GetDefferedOperationsCount;
    property ServerVersion: LongWord read GetServerVersion;
    property Password: String read fPassword write SetPassword;
    property AdminPassword: String read fAdminPassword write SetAdminPassword;
    property SuperAdminPassword: String read fSuperadminPassword write SetSuperadminPassword;
    property SendEvents: Boolean read fSendEvents write SetSendEvents;
    property SendChannels: Boolean read fSendChannels write SetSendChannels;
    property SendConfigs: Boolean read fSendConfigs write SetSendConfigs;
    property ActiveMode: Boolean read GetActiveMode write SetActiveMode;
    property BufferChannels: Boolean read fBufferChannels write SetBufferChannels;
    property SendFrameEvents: Boolean read fSendFrameEvents write SetSendFrameEvents;
    property SaveSettingsToFile: Boolean read fSaveSettingsToFile write fSaveSettingsToFile;
    property OnAccept: TSocketNotifyEvent read GetOnAccept write SetOnAccept;
    property OnListen: TSocketNotifyEvent read GetOnListen write SetOnListen;
    property OnClientConnect: TSocketNotifyEvent read fOnClientConnect write fOnClientConnect;
    property OnClientDisconnect: TSocketNotifyEvent read fOnClientDisconnect write fOnClientDisconnect;
    property OnClientError: TSocketErrorEvent read fOnClientError write fOnClientError;
    property OnClientRead: TSocketNotifyEvent read fOnClientRead write fOnClientRead;
    property OnClientWrite: TSocketNotifyEvent read fOnClientWrite write fOnClientWrite;
    property OnGetSocket: TGetSocketEvent read fOnGetSocket write fOnGetSocket;
    property OnGetThread: TGetThreadEvent read GetOnGetThread write SetOnGetThread;
    property OnThreadEnd: TThreadNotifyEvent read GetOnThreadEnd write SetOnThreadEnd;
    property OnThreadStart: TThreadNotifyEvent read GetOnThreadStart write SetOnThreadStart;
    property OnStatusChange: TNotifyEvent read fOnStatusChange write fOnStatusChange;
    property OnPacketDrop: TPacketNotifyEvent read fOnPacketDrop write fOnPacketDrop;
    property OnOverflowClear: TSocketNotifyEvent read fOnOverflowClear write fOnOverflowClear;
    property OnPacketReceived: TPacketNotifyEvent read fOnPacketReceived write fOnPacketReceived;
    property OnPacketSending: TPacketNotifyEvent read fOnPacketSending write fOnPacketSending;
    property OnPacketSend: TPacketNotifyEvent read fOnPacketSend write fOnPacketSend;
    property OnLog: TLogEvent read fOnLog write fOnLog;
    property OnEventRegister: TEventRegisterEvent read fOnEventRegister write fOnEventRegister;
    property OnEventUnregister: TEventRegisterEvent read fOnEventUnregister write fOnEventUnregister;
    property OnEvent: TEventEvent read fOnEvent write fOnEvent;
    property OnChannelRegister: TChannelRegisterEvent read fOnChannelRegister write fOnChannelRegister;
    property OnChannelUnregister: TChannelUnregisterEvent read fOnChannelUnregister write fOnChannelUnregister;
    property OnChannel: TChannelEvent read fOnChannel write fOnChannel;
    property OnChannelBuffering: TChannelEvent read fOnChannelBuffering write fOnChannelBuffering;
    property OnConfig: TConfigEvent read fOnConfig write fOnConfig;
    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
  end;


implementation

uses
  SysUtils, IniFiles,
  TelemetryNetHashing,
  TelemetryNetPacketsBuilding,
  TelemetryNetPacketsResolving;

{==============================================================================}
{    TTelemetryNetServer // Implementation                                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TTelemetryNetServer // Constants                                          }
{------------------------------------------------------------------------------}

const
  // Default (initial) values for selected server properties.
  def_Password           = 'password';
  def_AdminPassword      = 'admin.password';
  def_SuperAdminPassword = 'superadmin.password';
  def_SendEvents         = True;
  def_SendChannels       = True;
  def_SendConfigs        = True;
  def_BufferChannels     = True;
  def_SendFrameEvents    = False;
  def_SaveSettingsToFile = False;

  // Current server version. Remember to change it when server implementation
  // is changed.
  cCurrentServerVersion = $00010000;  {1.0}

  // Name of file to which server will save its settings.
  cSettingsFileName     = 'TelemetryNetServer.ini';



{------------------------------------------------------------------------------}
{    TTelemetryNetServer // Private methods                                    }
{------------------------------------------------------------------------------}

Function TTelemetryNetServer.GetConnectedClientsCount: Integer;
begin
Result := fServerSocket.Socket.ActiveConnections;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetListenPort: Word;
begin
Result := fServerSocket.Port;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetBufferedChannelsCount: Integer;
begin
Result := fBufferedChannels.Count;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetDefferedOperationsCount: Integer;
begin
Result := fDefferedOperations.Count;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetServerVersion: LongWord;
begin
Result := cCurrentServerVersion;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetPassword(Value: String);
begin
If not AnsiSameStr(fPassword,Value) then
  begin
    DisconnectAllClients(drPasswordChange);
    fPassword := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetAdminPassword(Value: String);
begin
If not AnsiSameStr(fAdminPassword,Value) then
  begin
    DisconnectAllClients(drPasswordChange,True);
    fAdminPassword := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetSuperAdminPassword(Value: String);
begin
If not AnsiSameStr(fSuperAdminPassword,Value) then
  begin
    DisconnectAllClients(drPasswordChange,False,True);
    fSuperAdminPassword := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetSendEvents(Value: Boolean);
begin
fSendEvents := Value;
SendServerSettings(pidSrvSendEvents);
SendServerSettings(pidSrvActiveMode);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetSendChannels(Value: Boolean);
begin
fSendChannels := Value;
SendServerSettings(pidSrvSendChannels);
SendServerSettings(pidSrvActiveMode);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetSendConfigs(Value: Boolean);
begin
fSendConfigs := Value;
SendServerSettings(pidSrvSendConfigs);
SendServerSettings(pidSrvActiveMode);
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetActiveMode: Boolean;
begin
Result := fSendEvents and fSendChannels and fSendConfigs;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetActiveMode(Value: Boolean);
begin
fSendEvents := Value;
fSendChannels := Value;
fSendConfigs := Value;
SendServerSettings(pidSrvActiveMode);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetBufferChannels(Value: Boolean);
begin
If fBufferChannels <> Value then
  begin
    If not Value then SendBufferedChannels;
    fBufferChannels := Value;
  end;
SendServerSettings(pidSrvBufferChannels);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetSendFrameEvents(Value: Boolean);
begin
fSendFrameEvents := Value;
SendServerSettings(pidSrvSendFrameEvents);
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetOnAccept: TSocketNotifyEvent;
begin
Result := fServerSocket.OnAccept;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetOnAccept(Value: TSocketNotifyEvent);
begin
fServerSocket.OnAccept := Value;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetOnListen: TSocketNotifyEvent;
begin
Result := fServerSocket.OnListen;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetOnListen(Value: TSocketNotifyEvent);
begin
fServerSocket.OnListen := Value;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetOnGetThread: TGetThreadEvent;
begin
Result := fServerSocket.OnGetThread;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetOnGetThread(Value: TGetThreadEvent);
begin
fServerSocket.OnGetThread := Value;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetOnThreadEnd: TThreadNotifyEvent;
begin
Result := fServerSocket.OnThreadEnd;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetOnThreadEnd(Value: TThreadNotifyEvent);
begin
fServerSocket.OnThreadEnd := Value;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetOnThreadStart: TThreadNotifyEvent;
begin
Result := fServerSocket.OnThreadStart;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SetOnThreadStart(Value: TThreadNotifyEvent);
begin
fServerSocket.OnThreadStart := Value;
end;


{------------------------------------------------------------------------------}
{    TTelemetryNetServer // Protected methods                                  }
{------------------------------------------------------------------------------}

procedure TTelemetryNetServer.OnAddressListChangeHandler(Sender: TObject);
begin
SendToAll(BuildPacket_TN_PACKET_ADMIN_LIST_CHANGE,True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnClientConnectHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
SendToAll(BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(TNSCWinSocket(Socket)),True);
If Assigned(fOnClientConnect) then fOnClientConnect(Self,Socket);
If not fAddressList.ClientCanConnect(Socket.RemoteAddress) then
  begin
    SendToAll(BuildPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(TNSCWinSocket(Socket),
                fAddressList.ClientIsInList(Socket.RemoteAddress)),True);
    DisconnectClient(Socket,drNotAllowed);
  end
else
  SendToAll(BuildPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(TNSCWinSocket(Socket)),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnClientDisconnectHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
If Assigned(fOnClientDisconnect) then fOnClientDisconnect(Self,Socket);
// Read and write any remaining data.
OnClientWriteHandler(Sender,Socket);
OnClientReadHandler(Sender,Socket);
SendToAll(BuildPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(TNSCWinSocket(Socket)),True);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnClientErrorHandler(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
If Assigned(fOnClientError) then fOnClientError(Self,Socket,ErrorEvent,ErrorCode);
case ErrorEvent of
  eeSend:       TNSCWinSocket(Socket).CircularPacketsBuffer.Clear;
  eeReceive:    TNSCWinSocket(Socket).IncomingStream.Clear;
  eeConnect,
  eeDisconnect,
  eeAccept,
  eeGeneral:;   // No action defined, yet.
end;
// Stop error propagation.
ErrorCode := 0;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnClientReadHandler(Sender: TObject; Socket: TCustomWinSocket);
var
  SmallBuffer:  Array[0..cLargeBufferThreshold - 1] of Byte;
  LargeBuffer:  Pointer;
  ReadLength:   Integer;
  AllocSize:    Integer;
begin
If Assigned(fOnClientRead) then fOnClientRead(Self,Socket);
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
            TNSCWinSocket(Socket).IncomingStream.WriteIncomingBuffer(LargeBuffer^,ReadLength);
        finally
          FreeMem(LargeBuffer,AllocSize);
        end;
      end
    else
      begin
        // Small buffer can be used.
        ReadLength := Socket.ReceiveBuf(SmallBuffer,ReadLength);
        If ReadLength > 0 then
          TNSCWinSocket(Socket).IncomingStream.WriteIncomingBuffer(SmallBuffer,ReadLength);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnClientWriteHandler(Sender: TObject; Socket: TCustomWinSocket);
var
  TempPacket: TPacketBuffer;
begin
While TNSCWinSocket(Socket).CircularPacketsBuffer.ContainsPacket do
  begin
    TempPacket := TNSCWinSocket(Socket).CircularPacketsBuffer.PeekPacket;
    If Socket.SendBuf(TempPacket.Data^,TempPacket.Size) < TempPacket.Size then Break
      else TNSCWinSocket(Socket).CircularPacketsBuffer.RemovePacket;
  end;
If Assigned(fOnClientWrite) then fOnClientWrite(Self,Socket);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnGetSocketHandler(Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket);
begin
If Assigned(fOnGetSocket) then fOnGetSocket(Sender,Socket,ClientSocket)
else
  begin
    ClientSocket := TNSCWinSocket.Create(Socket,TServerWinSocket(Sender));
    TNSCWinSocket(ClientSocket).OnStatusChange := OnStatusChangeHandler;
    TNSCWinSocket(ClientSocket).CircularPacketsBuffer.OnPacketDrop := OnPacketDropHandler;
    TNSCWinSocket(ClientSocket).IncomingStream.OnOverflowClear := OnOverflowClearHandler;
    TNSCWinSocket(ClientSocket).IncomingStream.OnPacket := OnPacketHandler;
    If Assigned(TNSCWinSocket(ClientSocket).OnSocketEvent) then
      TNSCWinSocket(ClientSocket).OnSocketEvent(Self,ClientSocket,seConnect);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnStatusChangeHandler(Sender: TObject);
begin
If Sender is TTelemetryNetServerClientWinSocket then
  SendToAll(BuildPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(TNSCWinSocket(Sender)),True);
If Assigned(fOnStatusChange) then fOnStatusChange(Sender);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnPacketDropHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
begin
If Assigned(fOnPacketDrop) then fOnPacketDrop(Sender,Socket,Packet);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnOverflowClearHandler(Sender: TObject; Socket: TCustomWinSocket);
begin
If Assigned(fOnOverflowClear) then fOnOverflowClear(Sender,Socket);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnPacketHandler(Sender: TObject; Socket: TCustomWinSocket; Packet: TPacketBuffer);
var
  Success: Boolean;
begin
If TNSCWinSocket(Socket).CanReceivePacket(Packet) then
  begin
    If Assigned(fOnPacketReceived) then fOnPacketReceived(Sender,Socket,Packet);
    try
      case GetPacketIDPrefix(GetPacketHeader(Packet).PacketID) of
        TN_PREFIX_COMMON:         Success := ProcessPacketGroup_COMMON(Packet,Socket);
        TN_PREFIX_ADMIN:          Success := ProcessPacketGroup_ADMIN(Packet,Socket);
        TN_PREFIX_SUPERADMIN:     Success := ProcessPacketGroup_SUPERADMIN(Packet,Socket);
        TN_PREFIX_EVENT_KNOWN:    Success := ProcessPacketGroup_EVENT_KNOWN(Packet,Socket);
        TN_PREFIX_CHANNEL_KNOWN:  Success := ProcessPacketGroup_CHANNEL_KNOWN(Packet,Socket);
        TN_PREFIX_CONFIG_KNOWN:   Success := ProcessPacketGroup_CONFIG_KNOWN(Packet,Socket);
        TN_PREFIX_EVENT:          Success := ProcessPacketGroup_EVENT(Packet,Socket);
        TN_PREFIX_CHANNEL:        Success := ProcessPacketGroup_CHANNEL(Packet,Socket);
        TN_PREFIX_CONFIG:         Success := ProcessPacketGroup_CONFIG(Packet,Socket);
        TN_PREFIX_LOG:            Success := ProcessPacketGroup_LOG(Packet,Socket);
      else
        SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
        Success := False;
      end;
    except
      SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petErrorReadingPacket,GetLastError),True);
      Success := False;
    end;
  end
else Success := False;
If not Success then
  begin
    If TNSCWinSocket(Socket).WaitingForPassword then DisconnectClient(Socket,drWrongPassword);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnLogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
If fServerSocket.Socket.ActiveConnections > 0 then
  SendToAll(BuildPacket_TN_PACKET_LOG_LOG(LogType,LogText),True);
If Assigned(fOnLog) then fOnLog(Self,LogType,LogText);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnEventRegisterHandler(Sender: TObject; Event: scs_event_t);
begin
If fServerSocket.Socket.ActiveConnections > 0 then
  SendToAll(BuildPacket_TN_PACKET_EVENT_REG(Event,SCS_RESULT_ok),True);
If Assigned(fOnEventRegister) then fOnEventRegister(Self,Event);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnEventUnregisterHandler(Sender: TObject; Event: scs_event_t);
begin
If fServerSocket.Socket.ActiveConnections > 0 then
  SendToAll(BuildPacket_TN_PACKET_EVENT_UNREG(Event,SCS_RESULT_ok),True);
If Assigned(fOnEventUnregister) then fOnEventUnregister(Self,Event);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnEventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer);
var
  DataSize:       Integer;
  CanSendPacket:  Boolean;
begin
If (fServerSocket.Socket.ActiveConnections > 0) and SendEvents then
  begin
      DataSize := 0;
      case Event of
        SCS_TELEMETRY_EVENT_frame_start:
          begin
            DataSize := SizeOf(scs_telemetry_frame_start_t);
            CanSendPacket := SendFrameEvents;
          end;
        SCS_TELEMETRY_EVENT_frame_end:
          CanSendPacket := SendFrameEvents;
        SCS_TELEMETRY_EVENT_configuration:
          begin
            SendToAll(BuildPacket_TN_PACKET_EVENT_EVENT(Event,p_scs_telemetry_configuration_t(Data)),True);
            CanSendPacket := False;
          end;
      else
       {SCS_TELEMETRY_EVENT_invalid,
        SCS_TELEMETRY_EVENT_paused,
        SCS_TELEMETRY_EVENT_started:}
        CanSendPacket := True;
      end;
    If CanSendPacket then
      SendToAll(BuildPacket_TN_PACKET_EVENT_EVENT(Event,DataSize,Data),True);
  end;
If BufferChannels then SendBufferedChannels;
ExecuteDefferedOperations(Event);
If Assigned(fOnEvent) then fOnEvent(Self,Event,Data);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
If fServerSocket.Socket.ActiveConnections > 0 then
  SendToAll(BuildPacket_TN_PACKET_CHANNEL_REG(Name,Index,ValueType,Flags,SCS_RESULT_ok),True);
If Assigned(fOnChannelRegister) then fOnChannelRegister(Self,Name,ID,Index,ValueType,Flags);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
If fServerSocket.Socket.ActiveConnections > 0 then
  SendToAll(BuildPacket_TN_PACKET_CHANNEL_UNREG(Name,Index,ValueType,SCS_RESULT_ok),True);
If Assigned(fOnChannelUnregister) then fOnChannelUnregister(Self,Name,ID,Index,ValueType);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If (fServerSocket.Socket.ActiveConnections > 0) then
  begin
    If BufferChannels then
      begin
        // Channels are buffered, store data.
        fBufferedChannels.AddChannel(Name,ID,Index,Value);
        If Assigned(fOnChannelBuffering) then fOnChannelBuffering(Sender,Name,ID,Index,Value);
      end
    else
      // Channels are not buffered, send data.
      If SendChannels then
        SendToAll(BuildPacket_TN_PACKET_CHANNEL_CHANNEL(Name,ID,Index,Value),True);
  end;
If Assigned(fOnChannel) then fOnChannel(Self,Name,ID,Index,Value);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If (fServerSocket.Socket.ActiveConnections > 0) and SendConfigs then
  SendToAll(BuildPacket_TN_PACKET_CONFIG_CONFIG(Name,ID,Index,Value),True);
If Assigned(fOnConfig) then fOnConfig(Sender,Name,ID,Index,Value);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.OnDestroyHandler(Sender: TObject);
begin
Self.Free;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_COMMON(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_PING:                       ProcessPacket_TN_PACKET_PING(Packet,Socket);
  TN_PACKET_PING_RESPONSE:              ProcessPacket_TN_PACKET_PING_RESPONSE(Packet,Socket);
  TN_PACKET_HI:                         ProcessPacket_TN_PACKET_HI(Packet,Socket);
  TN_PACKET_VERSION:                    ProcessPacket_TN_PACKET_VERSION(Packet,Socket);
  TN_PACKET_TELEMETRY_VERSION_GET:      ProcessPacket_TN_PACKET_TELEMETRY_VERSION_GET(Packet,Socket);
  TN_PACKET_TELEMETRY_VERSION:          ProcessPacket_TN_PACKET_TELEMETRY_VERSION(Packet,Socket);
  TN_PACKET_READY:                      ProcessPacket_TN_PACKET_READY(Packet,Socket);
  TN_PACKET_PARAM_GET:                  ProcessPacket_TN_PACKET_PARAM_GET(Packet,Socket);
  TN_PACKET_PARAM:                      ProcessPacket_TN_PACKET_PARAM(Packet,Socket);
  TN_PACKET_BYE:                        ProcessPacket_TN_PACKET_BYE(Packet,Socket);
  TN_PACKET_MESSAGE:                    ProcessPacket_TN_PACKET_MESSAGE(Packet,Socket);
  TN_PACKET_PACKET:                     ProcessPacket_TN_PACKET_PACKET(Packet,Socket);
  TN_PACKET_RIGHTS_ADMIN_REQUEST:       ProcessPacket_TN_PACKET_RIGHTS_ADMIN_REQUEST(Packet,Socket);
  TN_PACKET_RIGHTS_ADMIN:               ProcessPacket_TN_PACKET_RIGHTS_ADMIN(Packet,Socket);
  TN_PACKET_ERROR:                      ProcessPacket_TN_PACKET_ERROR(Packet,Socket);
  TN_PACKET_RIGHTS_SUPERADMIN_REQUEST:  ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN_REQUEST(Packet,Socket);
  TN_PACKET_RIGHTS_SUPERADMIN:          ProcessPacket_TN_PACKET_RIGHTS_SUPERADMIN(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_ADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_ADMIN_CLIENT_CONNECTING:    ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTING(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_REJECTED:      ProcessPacket_TN_PACKET_ADMIN_CLIENT_REJECTED(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_CONNECTED:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_CONNECTED(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_DISCONNECTED:  ProcessPacket_TN_PACKET_ADMIN_CLIENT_DISCONNECTED(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_CHANGE:        ProcessPacket_TN_PACKET_ADMIN_CLIENT_CHANGE(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_OPERATION:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_OPERATION(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_COUNT_GET:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT_GET(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_COUNT:         ProcessPacket_TN_PACKET_ADMIN_CLIENT_COUNT(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_INDEX_GET:     ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_GET(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_INDEX:         ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET: ProcessPacket_TN_PACKET_ADMIN_CLIENT_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_ALL_GET:       ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL_GET(Packet,Socket);
  TN_PACKET_ADMIN_CLIENT_ALL:           ProcessPacket_TN_PACKET_ADMIN_CLIENT_ALL(Packet,Socket);
  TN_PACKET_ADMIN_LIST_COUNT_GET:       ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT_GET(Packet,Socket);
  TN_PACKET_ADMIN_LIST_COUNT:           ProcessPacket_TN_PACKET_ADMIN_LIST_COUNT(Packet,Socket);
  TN_PACKET_ADMIN_LIST_INDEX_GET:       ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_GET(Packet,Socket);
  TN_PACKET_ADMIN_LIST_INDEX:           ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX(Packet,Socket);
  TN_PACKET_ADMIN_LIST_INDEX_ALL_GET:   ProcessPacket_TN_PACKET_ADMIN_LIST_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_ADMIN_LIST_ALL_GET:         ProcessPacket_TN_PACKET_ADMIN_LIST_ALL_GET(Packet,Socket);
  TN_PACKET_ADMIN_LIST_ALL:             ProcessPacket_TN_PACKET_ADMIN_LIST_ALL(Packet,Socket);
  TN_PACKET_ADMIN_LIST_CLEAR:           ProcessPacket_TN_PACKET_ADMIN_LIST_CLEAR(Packet,Socket);
  TN_PACKET_ADMIN_LIST_ADD:             ProcessPacket_TN_PACKET_ADMIN_LIST_ADD(Packet,Socket);
  TN_PACKET_ADMIN_LIST_REMOVE:          ProcessPacket_TN_PACKET_ADMIN_LIST_REMOVE(Packet,Socket);
  TN_PACKET_ADMIN_LIST_DELETE:          ProcessPacket_TN_PACKET_ADMIN_LIST_DELETE(Packet,Socket);
  TN_PACKET_ADMIN_LIST_RELOAD:          ProcessPacket_TN_PACKET_ADMIN_LIST_RELOAD(Packet,Socket);
  TN_PACKET_ADMIN_LIST_SAVE:            ProcessPacket_TN_PACKET_ADMIN_LIST_SAVE(Packet,Socket);
  TN_PACKET_ADMIN_LIST_CHANGE:          ProcessPacket_TN_PACKET_ADMIN_LIST_CHANGE(Packet,Socket);
  TN_PACKET_ADMIN_SEND_MESSAGE:         ProcessPacket_TN_PACKET_ADMIN_SEND_MESSAGE(Packet,Socket);
  TN_PACKET_ADMIN_PASSWORD:             ProcessPacket_TN_PACKET_ADMIN_PASSWORD(Packet,Socket);
  TN_PACKET_ADMIN_CHANGE_PASSWORD:      ProcessPacket_TN_PACKET_ADMIN_CHANGE_PASSWORD(Packet,Socket);
  TN_PACKET_ADMIN_PARAM_SET:            ProcessPacket_TN_PACKET_ADMIN_PARAM_SET(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_SUPERADMIN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_SUPERADMIN_ADMIN_PASSWORD:        ProcessPacket_TN_PACKET_SUPERADMIN_ADMIN_PASSWORD(Packet,Socket);
  TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD: ProcessPacket_TN_PACKET_SUPERADMIN_CHANGE_ADMIN_PASSWORD(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_EVENT_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_EVENT_KNOWN_COUNT_GET:      ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT_GET(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_COUNT:          ProcessPacket_TN_PACKET_EVENT_KNOWN_COUNT(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_INDEX_GET:      ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_GET(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_INDEX:          ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_EVENT_KNOWN_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_ALL_GET:        ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL_GET(Packet,Socket);
  TN_PACKET_EVENT_KNOWN_ALL:            ProcessPacket_TN_PACKET_EVENT_KNOWN_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_CHANNEL_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CHANNEL_KNOWN_COUNT_GET:      ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT_GET(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_COUNT:          ProcessPacket_TN_PACKET_CHANNEL_KNOWN_COUNT(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_INDEX_GET:      ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_GET(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_INDEX:          ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_CHANNEL_KNOWN_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_ALL_GET:        ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL_GET(Packet,Socket);
  TN_PACKET_CHANNEL_KNOWN_ALL:            ProcessPacket_TN_PACKET_CHANNEL_KNOWN_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_CONFIG_KNOWN(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CONFIG_KNOWN_COUNT_GET:     ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT_GET(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_COUNT:         ProcessPacket_TN_PACKET_CONFIG_KNOWN_COUNT(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_INDEX_GET:     ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_GET(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_INDEX:         ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET: ProcessPacket_TN_PACKET_CONFIG_KNOWN_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_ALL_GET:       ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL_GET(Packet,Socket);
  TN_PACKET_CONFIG_KNOWN_ALL:           ProcessPacket_TN_PACKET_CONFIG_KNOWN_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_EVENT(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_EVENT_REG:                      ProcessPacket_TN_PACKET_EVENT_REG(Packet,Socket);
  TN_PACKET_EVENT_REG_ALL:                  ProcessPacket_TN_PACKET_EVENT_REG_ALL(Packet,Socket);
  TN_PACKET_EVENT_UNREG:                    ProcessPacket_TN_PACKET_EVENT_UNREG(Packet,Socket);
  TN_PACKET_EVENT_UNREG_ALL:                ProcessPacket_TN_PACKET_EVENT_UNREG_ALL(Packet,Socket);
  TN_PACKET_EVENT_EVENT:                    ProcessPacket_TN_PACKET_EVENT_EVENT(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED:               ProcessPacket_TN_PACKET_EVENT_REGISTERED(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_COUNT_GET:     ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT_GET(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_COUNT:         ProcessPacket_TN_PACKET_EVENT_REGISTERED_COUNT(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_INDEX_GET:     ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_GET(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_INDEX:         ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET: ProcessPacket_TN_PACKET_EVENT_REGISTERED_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_ALL_GET:       ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL_GET(Packet,Socket);
  TN_PACKET_EVENT_REGISTERED_ALL:           ProcessPacket_TN_PACKET_EVENT_REGISTERED_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_CHANNEL(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CHANNEL_REG:                      ProcessPacket_TN_PACKET_CHANNEL_REG(Packet,Socket);
  TN_PACKET_CHANNEL_REG_ALL:                  ProcessPacket_TN_PACKET_CHANNEL_REG_ALL(Packet,Socket);
  TN_PACKET_CHANNEL_UNREG:                    ProcessPacket_TN_PACKET_CHANNEL_UNREG(Packet,Socket);
  TN_PACKET_CHANNEL_UNREG_ALL:                ProcessPacket_TN_PACKET_CHANNEL_UNREG_ALL(Packet,Socket);
  TN_PACKET_CHANNEL_CHANNEL:                  ProcessPacket_TN_PACKET_CHANNEL_CHANNEL(Packet,Socket);
  TN_PACKET_CHANNEL_CHANNEL_BUFFERED:         ProcessPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED:               ProcessPacket_TN_PACKET_CHANNEL_REGISTERED(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_COUNT_GET:     ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT_GET(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_COUNT:         ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_COUNT(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_INDEX_GET:     ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_GET(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_INDEX:         ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET: ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_ALL_GET:       ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL_GET(Packet,Socket);
  TN_PACKET_CHANNEL_REGISTERED_ALL:           ProcessPacket_TN_PACKET_CHANNEL_REGISTERED_ALL(Packet,Socket);
  TN_PACKET_CHANNEL_STORED_SEND_ALL:          ProcessPacket_TN_PACKET_CHANNEL_STORED_SEND_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_CONFIG(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_CONFIG_CONFIG:                ProcessPacket_TN_PACKET_CONFIG_CONFIG(Packet,Socket);
  TN_PACKET_CONFIG_STORED:                ProcessPacket_TN_PACKET_CONFIG_STORED(Packet,Socket);
  TN_PACKET_CONFIG_STORED_COUNT_GET:      ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT_GET(Packet,Socket);
  TN_PACKET_CONFIG_STORED_COUNT:          ProcessPacket_TN_PACKET_CONFIG_STORED_COUNT(Packet,Socket);
  TN_PACKET_CONFIG_STORED_INDEX_GET:      ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_GET(Packet,Socket);
  TN_PACKET_CONFIG_STORED_INDEX:          ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX(Packet,Socket);
  TN_PACKET_CONFIG_STORED_INDEX_ALL_GET:  ProcessPacket_TN_PACKET_CONFIG_STORED_INDEX_ALL_GET(Packet,Socket);
  TN_PACKET_CONFIG_STORED_ALL_GET:        ProcessPacket_TN_PACKET_CONFIG_STORED_ALL_GET(Packet,Socket);
  TN_PACKET_CONFIG_STORED_ALL:            ProcessPacket_TN_PACKET_CONFIG_STORED_ALL(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.ProcessPacketGroup_LOG(Packet: TPacketBuffer; Socket: TCustomWinSocket): Boolean;
begin
Result := True;
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_LOG_LOG:  ProcessPacket_TN_PACKET_LOG_LOG(Packet,Socket);
else
  SendTo(Socket,BuildPacket_TN_PACKET_ERROR(Packet,petUnknownPacket),True);
  Result := False;
end;
end;

//------------------------------------------------------------------------------

{$DEFINE Implementation_part}
{$INCLUDE '.\INC\TTelemetryNetServer.ProcessPacket_TN_PACKET.pas'}
{$UNDEF Implementation_part}

//------------------------------------------------------------------------------

Function TTelemetryNetServer.GetClient(ClientID: TGUID): TNSCWinSocket;
var
  i:  Integer;
begin
Result := nil;
For i := 0 to (fServerSocket.Socket.ActiveConnections - 1) do
  If IsEqualGUID(TNSCWinSocket(fServerSocket.Socket.Connections[i]).UniqueIdentificator,ClientID) then
    begin
      Result := TNSCWinSocket(fServerSocket.Socket.Connections[i]);
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SendBufferedChannels;
begin
If fBufferedChannels.ContainsChannel then
  begin
    If (fServerSocket.Socket.ActiveConnections > 0) and SendChannels then
        SendToAll(BuildPacket_TN_PACKET_CHANNEL_CHANNEL_BUFFERED(fBufferedChannels),True);
    fBufferedChannels.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.ExecuteDefferedOperations(CallingEvent: scs_event_t = SCS_TELEMETRY_EVENT_invalid );
var
  DefferedOperation:  TDefferedOperation;

  Function SocketIsValid(Socket: TCustomWinSocket): Boolean;
  var
    ii: Integer;
  begin
    Result := False;
    For ii := 0 to (fServerSocket.Socket.ActiveConnections - 1) do
      If fServerSocket.Socket.Connections[ii] = Socket then
        begin
          Result := True;
          Break;
        end;
  end;

begin
while fDefferedOperations.ContainsOperation do
    begin
      DefferedOperation := fDefferedOperations.PeekOperation;
      // As this method is called in event callback, it is posssible there is
      // collision with buffered operation - more precisely, an event cannot be
      // registered in its own callback - if that is the case, delete operation
      // (if that event was called - therefore is already registered - then
      // there is no need to register it again).
      If DefferedOperation.OperationType = dotEventRegister then
        If DefferedOperation.EventParams.Event = CallingEvent then
          begin
            fDefferedOperations.RemoveOperation;
            Continue;
          end;
      // Execute operation specific actions.
      case DefferedOperation.OperationType of
{--- ---                                                                --- ---}      
        dotEventRegister:
          With DefferedOperation.EventParams do
            If not fTelemetryRecipient.EventRegister(Event) then
              If SocketIsValid(DefferedOperation.FromSocket) then
                SendTo(DefferedOperation.FromSocket,
                       BuildPacket_TN_PACKET_EVENT_REG(Event,fTelemetryRecipient.LastTelemetryResult),
                       True);
{--- ---                                                                --- ---}
        dotEventUnregister:
          With DefferedOperation.EventParams do
            If not fTelemetryRecipient.EventUnregister(Event) then
              If SocketIsValid(DefferedOperation.FromSocket) then
                SendTo(DefferedOperation.FromSocket,
                       BuildPacket_TN_PACKET_EVENT_UNREG(Event,fTelemetryRecipient.LastTelemetryResult),
                       True);
{--- ---                                                                --- ---}
        dotEventRegisterAll:
          fTelemetryRecipient.EventRegisterAll;
{--- ---                                                                --- ---}
        dotEventUnregisterAll:
          fTelemetryRecipient.EventUnregisterAll;
{--- ---                                                                --- ---}
        dotChannelRegister:
          With DefferedOperation.ChannelParams do
            If not fTelemetryRecipient.ChannelRegister(Name,Index,ValueType,Flags) then
              If SocketIsValid(DefferedOperation.FromSocket) then
                SendTo(DefferedOperation.FromSocket,
                       BuildPacket_TN_PACKET_CHANNEL_REG(Name,Index,ValueType,
                         Flags,fTelemetryRecipient.LastTelemetryResult),
                       True);
{--- ---                                                                --- ---}
        dotChannelUnregister:
          With DefferedOperation.ChannelParams do
            If not fTelemetryRecipient.ChannelUnregister(Name,Index,ValueType) then
              If SocketIsValid(DefferedOperation.FromSocket) then
                SendTo(DefferedOperation.FromSocket,
                       BuildPacket_TN_PACKET_CHANNEL_UNREG(Name,Index,ValueType,
                         fTelemetryRecipient.LastTelemetryResult),
                       True);
{--- ---                                                                --- ---}
        dotChannelRegisterAll:
          With DefferedOperation.ChannelsRegAllParams do
            fTelemetryRecipient.ChannelRegisterAll(RegisterPrimaryTypes,
                                                   RegisterSecondaryTypes,
                                                   RegisterTertiaryTypes);
{--- ---                                                                --- ---}
        dotChannelUnregisterAll:
          fTelemetryRecipient.ChannelUnregisterAll;
      else
        {dotNone} // No action.
      end;
      fDefferedOperations.RemoveOperation;
    end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SendTo(Socket: TCustomWinSocket; Packet: TPacketBuffer; FreePacket: Boolean);

  procedure BufferPacket(aSocket: TCustomWinSocket; aPacket: TPacketBuffer; var aFreePacket: Boolean);
  begin
    TNSCWinSocket(aSocket).CircularPacketsBuffer.AddPacket(aPacket,not aFreePacket);
    aFreePacket := False;
  end;
  
begin
OnClientWriteHandler(nil,Socket);
If TNSCWinSocket(Socket).CanSendPacket(Packet) then
  begin
    If Assigned(fOnPacketSending) then fOnPacketSending(Self,Socket,Packet);
    If not TNSCWinSocket(Socket).CircularPacketsBuffer.ContainsPacket then
      begin
        If Socket.SendBuf(Packet.Data^,Packet.Size) < Packet.Size then
          BufferPacket(Socket,Packet,FreePacket)
        else
          If Assigned(fOnPacketSend) then fOnPacketSend(Self,Socket,Packet);
      end
    else BufferPacket(Socket,Packet,FreePacket);
  end;
If FreePacket then FreeMem(Packet.Data,Packet.Size);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SendToAll(Packet: TPacketBuffer; FreePacket: Boolean);
var
  i:  Integer;
begin
If fServerSocket.Socket.ActiveConnections > 1 then
  begin
    For i := 0 to (fServerSocket.Socket.ActiveConnections - 1) do
      SendTo(fServerSocket.Socket.Connections[i],Packet,False);
    If FreePacket then FreeMem(Packet.Data,Packet.Size);
  end
else
  If fServerSocket.Socket.ActiveConnections > 0 then
    SendTo(fServerSocket.Socket.Connections[0],Packet,FreePacket);
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.SendServerSettings(ParameterID: TParameterID = pidAll; Socket: TCustomWinSocket = nil);
var
  i:  TParameterID;
  j:  Integer;

  procedure SendServerSetting(ParamID: TParameterID; aSocket: TCustomWinSocket);
  begin
    case ParamID of
      pidSrvSendEvents:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,SendEvents),True);
      pidSrvSendChannels:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,SendChannels),True);
      pidSrvSendConfigs:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,SendConfigs),True);
      pidSrvActiveMode:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,ActiveMode),True);
      pidSrvBufferChannels:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,BufferChannels),True);
      pidSrvSendFrameEvents:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,SendFrameEvents),True);
      pidSrvSaveSettingsToFile:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,SaveSettingsToFile),True);
      pidRecKeepUtilityEvents:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,fTelemetryRecipient.KeepUtilityEvents),True);
      pidRecStoreConfigurations:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,fTelemetryRecipient.StoreConfigurations),True);
      pidRecManageIndexedChannels:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,fTelemetryRecipient.ManageIndexedChannels),True);
      pidRecStoreChannelValues:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,fTelemetryRecipient.StoreChannelsValues),True);
      pidLstListMode:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,Integer(fAddressList.ListMode)),True);
      pidLstListFileName:
        SendTo(aSocket,BuildPacket_TN_PACKET_PARAM(ParamID,fAddressList.FileName),True);
    end;
  end;

begin
If Assigned(Socket) then
  begin
    If ParameterID = pidAll then
      For i := Low(TParameterID) to High(TParameterID) do
        SendServerSetting(i,Socket)
    else
      SendServerSetting(ParameterID,Socket);
  end
else
  begin
    For j := 0 to (fServerSocket.Socket.ActiveConnections - 1) do
      If ParameterID = pidAll then
        For i := Low(TParameterID) to High(TParameterID) do
          SendServerSetting(i,fServerSocket.Socket.Connections[j])
      else
        SendServerSetting(ParameterID,fServerSocket.Socket.Connections[j]);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.DisconnectClient(Socket: TCustomWinSocket; Reason: TDisconnectReason = drGeneral);
begin
SendTo(Socket,BuildPacket_TN_PACKET_BYE(Reason),True);
Socket.Close;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServer.DisconnectAllClients(Reason: TDisconnectReason = drGeneral; OnlyAdmins: Boolean = False; OnlySuperAdmins: Boolean = False);
var
  i:            Integer;
  ClientSocket: TNSCWinSocket;
begin
For i := (fServerSocket.Socket.ActiveConnections - 1) downto 0 do
  begin
    ClientSocket := TNSCWinSocket(fServerSocket.Socket.Connections[i]);
    If (not OnlyAdmins and not OnlySuperAdmins) or
     (ClientSocket.AdminRights and OnlyAdmins) or
     (ClientSocket.SuperAdminRights and OnlySuperAdmins) then
      DisconnectClient(fServerSocket.Socket.Connections[i],Reason);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.LoadSettings: Boolean;
var
  Ini:  TIniFile;
begin
try
  If FileExists(ExtractFilePath(GetModuleName(hInstance)) + cSettingsFileName) then
    begin
      Ini := TIniFile.Create(ExtractFilePath(GetModuleName(hInstance)) + cSettingsFileName);
      try
        // Load server settings.
        fServerSocket.Port := Ini.ReadInteger('NetServer','Port',def_ServerPort);
        Password := Ini.ReadString('NetServer','Password',def_Password);
        AdminPassword := Ini.ReadString('NetServer','AdminPassword',def_AdminPassword);
        SuperAdminPassword := Ini.ReadString('NetServer','SuperAdminPassword',def_SuperAdminPassword);
        SendEvents := Ini.ReadBool('NetServer','SendEvents',def_SendEvents);
        SendChannels := Ini.ReadBool('NetServer','SendChannels',def_SendChannels);
        SendConfigs := Ini.ReadBool('NetServer','SendConfigs',def_SendConfigs);
        BufferChannels := Ini.ReadBool('NetServer','BufferChannels',def_BufferChannels);
        SendFrameEvents := Ini.ReadBool('NetServer','SendFrameEvents',def_SendFrameEvents);
        SaveSettingsToFile := Ini.ReadBool('NetServer','SaveSettingsToFile',def_SaveSettingsToFile);
        // Load address list settigns.
        fAddressList.ListMode := TAddressListMode(Ini.ReadInteger('AddressList','ListMode',Integer(lmBlackList)));
        fAddressList.FileName := Ini.ReadString('AddressList','FileName',ExtractFilePath(GetModuleName(hInstance)) + fAddressList.FileName);
        Result := True;
      finally
        Ini.Free;
      end;
    end
  else Result := False;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServer.SaveSettings: Boolean;
var
  Ini:  TIniFile;
begin
try
  Ini := TIniFile.Create(ExtractFilePath(GetModuleName(hInstance)) + cSettingsFileName);
  try
    // Save server settings.
    Ini.WriteInteger('NetServer','Port',fServerSocket.Port);
    Ini.WriteString('NetServer','Password',Password);
    Ini.WriteString('NetServer','AdminPassword',AdminPassword);
    Ini.WriteString('NetServer','SuperAdminPassword',SuperAdminPassword);
    Ini.WriteBool('NetServer','SendEvents',SendEvents);
    Ini.WriteBool('NetServer','SendChannels',SendChannels);
    Ini.WriteBool('NetServer','SendConfigs',SendConfigs);
    Ini.WriteBool('NetServer','BufferChannels',BufferChannels);
    Ini.WriteBool('NetServer','SendFrameEvents',SendFrameEvents);
    Ini.WriteBool('NetServer','SaveSettingsToFile',SaveSettingsToFile);
    // Save address list settigns.
    Ini.WriteInteger('AddressList','ListMode',Integer(fAddressList.ListMode));
    Ini.WriteString('AddressList','FileName',fAddressList.FileName);
    Result := True;
  finally
    Ini.Free;
  end;
except
  Result := False;
end;
end;

{------------------------------------------------------------------------------}
{    TTelemetryNetServer // Public methods                                     }
{------------------------------------------------------------------------------}

constructor TTelemetryNetServer.Create(aTelemetryRecipient: TTelemetryRecipient; Port: Word = def_ServerPort);
begin
inherited Create;
If not Assigned(aTelemetryRecipient) then
  raise Exception.Create('TTelemetryNetServer.Create: Telemetry recipient not assigned.');
// Check whether server can support given telemetry/game version.
// Raise exception for unsupported telemetry/game.
If not SupportsTelemetryVersion(aTelemetryRecipient.TelemetryVersion) then
  raise Exception.Create('TTelemetryRecipient.Create: Telemetry version (' +
    SCSGetVersionAsString(aTelemetryRecipient.TelemetryVersion) + ') not supported');
If not SupportsGameVersion(scs_string_t(aTelemetryRecipient.GameID),aTelemetryRecipient.GameVersion) then
  raise Exception.Create('TTelemetryRecipient.Create: Game version (' + aTelemetryRecipient.GameID +
    '; ' + SCSGetVersionAsString(aTelemetryRecipient.GameVersion) + ') not supported');
// Creating/assigning service objects.
fAddressList := TAddressList.Create;
fBufferedChannels := TCircularChannelsBuffer.Create;
fTelemetryRecipient := aTelemetryRecipient;
fDefferedOperations := TDefferedOperationsBuffer.Create;
// Creating and setting server socket (must be created AFTER all service
// objects are created and ready).
fServerSocket := TServerSocket.Create(nil);
// Assigning event handlers for server socket.
fServerSocket.OnClientConnect := OnClientConnectHandler;
fServerSocket.OnClientDisconnect := OnClientDisconnectHandler;
fServerSocket.OnClientError := OnClientErrorHandler;
fServerSocket.OnClientRead := OnClientReadHandler;
fServerSocket.OnClientWrite := OnClientWriteHandler;
fServerSocket.OnGetSocket := OnGetSocketHandler;
// Assigning handlers for service objects.
fAddressList.OnChange := OnAddressListChangeHandler;
// Assigning telemetry recipient events handlers (must be done AFTER assigning
// event handlers for socket).
fTelemetryRecipient.OnLog := OnLogHandler;
fTelemetryRecipient.OnEventRegister := OnEventRegisterHandler;
fTelemetryRecipient.OnEventUnregister := OnEventUnregisterHandler;
fTelemetryRecipient.OnEvent := OnEventHandler;
fTelemetryRecipient.OnChannelRegister := OnChannelRegisterHandler;
fTelemetryRecipient.OnChannelUnregister := OnChannelUnregisterHandler;
fTelemetryRecipient.OnChannel := OnChannelHandler;
fTelemetryRecipient.OnConfig := OnConfigHandler;
fTelemetryRecipient.OnDestroy := OnDestroyHandler;
// Load settings, if it fails, load default values.
If not LoadSettings then
  begin
    fServerSocket.Port := Port;
    Password := def_Password;
    AdminPassword := def_AdminPassword;
    SuperAdminPassword := def_SuperAdminPassword;
    SendEvents := def_SendEvents;
    SendChannels := def_SendChannels;
    SendConfigs := def_SendConfigs;
    BufferChannels := def_BufferChannels;
    SendFrameEvents := def_SendFrameEvents;
    SaveSettingsToFile := def_SaveSettingsToFile;
    fAddressList.ListMode := lmBlackList;
  end;
// Load address list.
fAddressList.LoadFromFile;
// Opening server for listening.
fServerSocket.Open;
end;

//------------------------------------------------------------------------------

destructor TTelemetryNetServer.Destroy;
begin
If Assigned(fOnDestroy) then fOnDestroy(Self);
// Deassigning telemetry recipient events handlers.
fTelemetryRecipient.OnLog := nil;
fTelemetryRecipient.OnEventRegister := nil;
fTelemetryRecipient.OnEventUnregister := nil;
fTelemetryRecipient.OnEvent := nil;
fTelemetryRecipient.OnChannelRegister := nil;
fTelemetryRecipient.OnChannelUnregister := nil;
fTelemetryRecipient.OnChannel := nil;
fTelemetryRecipient.OnDestroy := nil;
// Disconnect all clients.
DisconnectAllClients(drServerTerminated);
// Shutdown and free server socket.
fServerSocket.Close;
fServerSocket.Free;
// Save settings and address list.
If SaveSettingsToFile then
  begin
    SaveSettings;
    fAddressList.SaveToFile;
  end;
// Free service objects.
fDefferedOperations.Free;
fBufferedChannels.Free;
fAddressList.Free;
inherited;
end;

end.
