{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Telemetry recipient class (API control and data receiver).)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-07)
@lastmod(2015-07-14)

  @bold(@NoAutoLink(TelemetryRecipient))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-20 

  This unit contains TTelemetryRecipient class that used to control the
  telemetry API (see class declaration for details) and few classes used to
  manage multicast events for the recipient, namely:
@preformatted(
  TMulticastLogEvent
  TMulticastEventRegisterEvent
  TMulticastEventEvent
  TMulticastChannelRegisterEvent
  TMulticastChannelUnregisterEvent
  TMulticastChannelEvent
  TMulticastConfigEvent
)
  Note that these are not part of the documentation.

  Included files:@preformatted(
    .\Inc\TelemetryMulticastEvents.pas
      Contains declarations and implementations of multicast event classes.)

@html(<hr>)}
unit TelemetryRecipient;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

{$DEFINE TelemetryRecipient}

uses
{$IFNDEF Documentation}
  Classes,
  AuxTypes,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}   
{$ENDIF}
  TelemetryCommon,
  TelemetryIDs,
  TelemetryLists,
  TelemetryVersionObjects,  
  TelemetryInfoProvider,
{$IFDEF Documentation}
  TelemetryValueTypeUtils,
  TelemetryConversions,
  TelemetryStrings;
{$ELSE}
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_telemetry_event,
  scssdk_telemetry_channel;
{$ENDIF}
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                                Declarations                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

const
{:
  @abstract(Maximum allowed channel index.)
  This number is used when registering indexed channel where the channel does
  not have default maximum index value, value of the config containing count is
  not known or the channel is not binded to any config.
}
{$IFDEF SmallMaxChannelIndex}
  MaxChannelIndex = 13;
{$ELSE}
  MaxChannelIndex = 99;
{$ENDIF}

type
  //:Enumerated type used in method TTelemetryRecipient.SetGameCallback to set
  //:one particular game callback.
  TGameCallback = (gcbLog, gcbEventReg, gcbEventUnreg, gcbChannelReg, gcbChannelUnreg);

  //:Event type used when recipient writes to game log.
  TLogEvent = procedure(Sender: TObject; LogType: scs_log_type_t; const LogText: String) of object;
  //:Event type used when telemetry event is registered or unregistered.
  TEventRegisterEvent = procedure(Sender: TObject; Event: scs_event_t; UserData: Pointer) of object;
  //:Event type used when telemetery event occurs.
  TEventEvent = procedure(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer) of object;
  //:Event type used when telemetry channel is registered.
  TChannelRegisterEvent = procedure(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer) of object;
  //:Event type used when telemetry channel is unregistered.
  TChannelUnregisterEvent = procedure(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer) of object;
  //:Event type used when telemetery channel callback occurs.
  TChannelEvent = procedure(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer) of object;
  //:Event type used when config is parsed from configuration telemetry event.
  TConfigEvent = procedure(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t) of object;

{$IFNDEF Documentation}
  {$DEFINE DeclarationPart}
    {$INCLUDE '.\Inc\TelemetryMulticastEvents.pas'}
  {$UNDEF DeclarationPart}
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TTelemetryRecipient                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryRecipient // Class declaration                                   }
{==============================================================================}
{:
  @abstract(@NoAutoLink(TTelemetryRecipient) is used as a main mean of
  controlling the telemetry API.)

  It provides methods for events and channels registration, unregistration and
  many others. It also provides object events that wraps API event and channel
  callbacks.@br
  Before instance of this class is created, a check if it supports required
  telemetry and game version must be performed. For that purpose, it has class
  methods that can be called on class itself (before creating class instance).
  If required versions are not supported, do not @noAutoLink(create) instance of
  this class!

  @bold(Note) - Because recipient internally creates instance of
  TTelemetryInfoProvider, the provider must support required versions of
  telemetry and game as well.

  Recipient can, similarly to TTelemetryInfoProvider, be crated on two ways -
  used-manager and automatically managed. When it is created in user-managed
  mode, internal TelemetryInfoProvider is too created as user-managed and
  vice-versa. Here are main diferences between those two modes:

  Automatically managed (created using any parametrized constructor):@unorderedList(
    @itemSpacing(Compact)
    @item(TelemetryInfoProvider is created as automatically managed (parameters
          are passed from constructor) - list are automatically filled)
    @item(Telemetry and game version-specific preparations are performed)
    @item(Game callbacks are automatically assigned (when supported by used
          constructor))
    @item(KeepUtilityEvents property is set to @true - utility events are
          automatically registered in constructor when it is possible
          (AllowAutoRegistration must be @true and game callbacks must be
          already assigned; @bold(Note) - only one constructor assigns them))
    @item(StoreConfigurations property is set to @true - configuration event
          is automatically registered in constructor when it is possible)
    @item(AllowAutoRegistration property is set to @true (value of this property
          can be changed later))
    @item((Un)Registration methods check whether requested item is already
          registered or not before actual (un)registration))

  User-managed mode (created using no-parameter constructor):@unorderedList(
    @itemSpacing(Compact)
    @item(TelemetryInfoProvider is created as user-managed)
    @item(No telemetry or game version-specific preparations are performed)
    @item(Game callbacks are NOT automatically assigned - therefore things like
          event registration or writing to a game log does not work)
    @item(KeepUtilityEvents property is set to @false - utility events are
          NOT automatically registered in constructor)
    @item(StoreConfigurations property is set to @false - configuration event
          is NOT automatically registered in constructor)
    @item(AllowAutoRegistration property is set to @false (value of this
          property can be changed later))
    @item((Un)Registration methods do NOT check whether requested item is
          already registered or not before actual (un)registration))

    Following methods: @preformatted(
    HighestSupportedTelemetryVersion
    HighestSupportedGameVersion
    SupportsTelemetryVersion
    SupportsTelemetryMajorVersion
    SupportsGameVersion
    SupportsTelemetryAndGameVersion
    SupportsTelemetryAndGameVersionParam)
    ...can and actually must be called directly on class.
    They should be used to check whether both this class and
    TTelemetryInfoProvider class supports required telemetry and game version
    before instantiation (creation of class instance).
}
  TTelemetryRecipient = class(TTelemetryVersionPrepareObject)
  private
  {:
    Holds state indicating whether current instance is user managed (when not,
    it is managed automatically).@br
    This field is set automatically in constructor(s).
  }
    fUserManaged:               Boolean;
  {:
    See AllowAutoRegistration property.
  }
    fAllowAutoRegistration:     Boolean;
  {:
    See TelemetryInfoProvider property.
  }
    fInfoProvider:              TTelemetryInfoProvider;
  {:
    See RegisteredEvents property.
  }
    fRegisteredEvents:          TRegisteredEventsList;
  {:
    See RegisteredChannels property.
  }
    fRegisteredChannels:        TRegisteredChannelsList;
  {:
    See StoredConfigs property.
  }
    fStoredConfigs:             TStoredConfigsList;
  {:
    See StoredChannels property.
  }
    fStoredChannels:            TStoredChannelsList;
  {:
    See LastTelemetryResult property.
  }
    fLastTelemetryResult:       scs_result_t;
  {:
    See TelemetryVersion property.
  }
    fTelemetryVersion:          scs_u32_t;
  {:
    See GameName property.
  }
    fGameName:                  TelemetryString;
  {:
    See GameID property.
  }
    fGameID:                    TelemetryString;
  {:
    See GameVersion property.
  }
    fGameVersion:               scs_u32_t;
  {:
    See KeepUtilityEvents property.
  }
    fKeepUtilityEvents:         Boolean;
  {:
    See StoreConfigurations property.
  }
    fStoreConfigurations:       Boolean;
  {:
    See ManageIndexedChannels property.
  }
    fManageIndexedChannels:     Boolean;
  {:
    See StoreChannels property.)
  }
    fStoreChannels:             Boolean;
  {:
    Holds pointer to game callback routine intended for writing messages into
    game log.@br
    Assigned in constructor from passed parameters, or set to @nil.
  }
    cbLog:                      scs_log_t;
  {:
    Holds pointer to game callback routine used for telemetry event
    registration.@br
    Assigned in constructor from passed parameters, or set to @nil.
  }
    cbRegisterEvent:            scs_telemetry_register_for_event_t;
  {:
    Holds pointer to game callback routine used for telemetry event
    unregistration.@br
    Assigned in constructor from passed parameters, or set to @nil.
  }
    cbUnregisterEvent:          scs_telemetry_unregister_from_event_t;
  {:
    Holds pointer to game callback routine used for telemetry channel
    registration.@br
    Assigned in constructor from passed parameters, or set to @nil.
  }
    cbRegisterChannel:          scs_telemetry_register_for_channel_t;
  {:
    Holds pointer to game callback routine used for telemetry channel
    unregistration.@br
    Assigned in constructor from passed parameters, or set to @nil.
  }
    cbUnregisterChannel:        scs_telemetry_unregister_from_channel_t;
  {:
    Holds referece to OnDestroy event handler.
  }
    fOnDestroy:                 TNotifyEvent;
  {:
    Holds referece to OnLog event handler.
  }
    fOnLog:                     TLogEvent;
  {:
    Holds referece to OnEventRegister event handler.
  }
    fOnEventRegister:           TEventRegisterEvent;
  {:
    Holds referece to OnEventUnregister event handler.
  }
    fOnEventUnregister:         TEventRegisterEvent;
  {:
    Holds referece to OnEvent event handler.
  }
    fOnEvent:                   TEventEvent;
  {:
    Holds referece to OnChannelRegister event handler.
  }
    fOnChannelRegister:         TChannelRegisterEvent;
  {:
    Holds referece to OnChannelUnregister event handler.
  }
    fOnChannelUnregister:       TChannelUnregisterEvent;
  {:
    Holds referece to OnChannel event handler.
  }
    fOnChannel:                 TChannelEvent;
  {:
    Holds referece to OnConfig event handler.
  }
    fOnConfig:                  TConfigEvent;
  {$IFDEF MulticastEvents}
  {:
    Object managing multicast OnDestroyMulti event.
  }
    fOnDestroyMulti:            TMulticastNotifyEvent;
  {:
    Object managing multicast OnLogMulti event.
  }
    fOnLogMulti:                TMulticastLogEvent;
  {:
    Object managing multicast OnEventRegisterMulti event.
  }
    fOnEventRegisterMulti:      TMulticastEventRegisterEvent;
  {:
    Object managing multicast OnEventUnregisterMulti event.
  }
    fOnEventUnregisterMulti:    TMulticastEventRegisterEvent;
  {:
    Object managing multicast OnEventMulti event.
  }
    fOnEventMulti:              TMulticastEventEvent;
  {:
    Object managing multicast OnChannelRegisterMulti event.
  }
    fOnChannelRegisterMulti:    TMulticastChannelRegisterEvent;
  {:
    Object managing multicast OnChannelUnregister event.
  }
    fOnChannelUnregisterMulti:  TMulticastChannelUnregisterEvent;
  {:
    Object managing multicast OnChannelMulti event.
  }
    fOnChannelMulti:            TMulticastChannelEvent;
  {:
    Object managing multicast OnConfigMulti event.
  }
    fOnConfigMulti:             TMulticastConfigEvent;
  {$ENDIF}
  {:
    Setter for KeepUtilityEvents property.

    @param Value New value to be stored in fKeepUtilityEvents.
  }
    procedure SetKeepUtilityEvents(Value: Boolean);
  {:
    Setter for StoreConfigurations property.

    @param Value New value to be stored in fStoreConfigurations.
  }
    procedure SetStoreConfigurations(Value: Boolean);
  {:
    Setter for StoreChannels property.

    @param Value New value to be stored in fStoreChannels.)
  }
    procedure SetStoreChannels(Value: Boolean);
  protected
  {:
    Method called by plugin callback routine set to receive telemetry events.@br
    OnEvent event is called and received telemetry event is then processed.

    @param Event Telemetry event identification number.
    @param(Data  Pointer to data received alongside the telemetry event. Can be
                 @nil.)
  }
    procedure EventHandler(Event: scs_event_t; Data: Pointer; UserData: Pointer); virtual;
  {:
    Method called by plugin callback routine set to receive telemetry
    channels.@br

    @param Name  Name of received telemetry channel.
    @param ID    ID of received channel.
    @param Index Index of received channel.
    @param Value Pointer to actual value of received channel. Can be @nil.
  }
    procedure ChannelHandler(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); virtual;
  {:
    Method used for processing configuration events. It is called from
    EventHandler method when configuration event is received.@br
    Received data are parsed and configuration information are extracted and
    stored in StoredConfigs list (StoreConfigurations must be @true for this).
    OnConfig event is called for every extracted config (after it is stored /
    its stored value changed).@br
    When ManageIndexedChannels is set to @true, then indexed channels are
    automatically (un)registered in this method (refer to source code for
    details).

    @param Data Structure holding actual configuration data.
  }
    procedure ProcessConfigurationEvent(const Data: scs_telemetry_configuration_t); virtual;
  public
  {:
    Calls hanler(s) of OnDestroy event.
  }
    procedure DoOnDestroy(Sender: TObject); virtual;
  {:
    Calls hanler(s) of OnLog event.
  }
    procedure DoOnLog(Sender: TObject; LogType: scs_log_type_t; const LogText: String); virtual;
  {:
    Calls hanler(s) of OnEventRegister event.
  }
    procedure DoOnEventRegister(Sender: TObject; Event: scs_event_t; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnEventUnregister event.
  }
    procedure DoOnEventUnregister(Sender: TObject; Event: scs_event_t; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnEvent event.
  }
    procedure DoOnEvent(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnChannelRegister event.
  }
    procedure DoOnChannelRegister(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnChannelUnregister event.
  }
    procedure DoOnChannelUnregister(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnChannel event.
  }
    procedure DoOnChannel(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer); virtual;
  {:
    Calls hanler(s) of OnConfig event.
  }
    procedure DoOnConfig(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  {:
    @returns Highest supported telemetry version.
  }
    class Function HighestSupportedTelemetryVersion: scs_u32_t; override;
  {:
    @param GameID Game identifier.

    @returns Highest supported version of passed game.
  }
    class Function HighestSupportedGameVersion(GameID: TelemetryString): scs_u32_t; override;
  {:
    @param TelemetryVersion Version of telemetry.

    @returns @True when given telemetry version is supported, otherwise @false.
  }
    class Function SupportsTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean; override;
  {:
    @param TelemetryVersion Version of telemetry.

    @returns(@True when given telemetry major version is supported (minor part
             is ignored), otherwise @false.)
  }
    class Function SupportsTelemetryMajorVersion(TelemetryVersion: scs_u32_t): Boolean; override;
  {:
    @param GameID       Game identifier.
    @param GameVersion  Version of game.

    @returns(@True when given game and its version are supported, otherwise
             @false.)
  }
    class Function SupportsGameVersion(GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; override;
  {:
    @param TelemetryVersion Version of telemetry.
    @param GameID           Game identifier.
    @param GameVersion      Version of game.

    @returns(@True when given telemetry, game and its version are supported,
             otherwise @false.)
  }
    class Function SupportsTelemetryAndGameVersion(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; override;
  {:
    @param TelemetryVersion Version of telemetry.
    @param Parameters       Structure containing other version information.

    @returns(@True when given telemetry, game and its version are supported,
             otherwise @false.)
  }
    class Function SupportsTelemetryAndGameVersionParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean; override;
  {:
    Performs any preparations necessary to support required telemetry
    version.@br
    TelemetryVersion property is set in this method when successful.

    @param(aTelemetryVersion Version of telemetry for which the object should be
                             prepared.)

    @returns(@True when preparation for given version were done successfully,
             otherwise @false.)
  }
    Function PrepareForTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean; override;
  {:
    Performs preparations necessary to support required game and its version.@br
    GameName, GameID and GameVersion properties are set in this method when
    successful.

    @param aGameName     Name of the game.
    @param aGameID       Game identifier.
    @param aGameVersion  Version of game.

    @returns(@True when preparation for given game and its version were done
             successfully, otherwise @false.)
  }
    Function PrepareForGameVersion(const GameName,GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; override;
  {:
    Constructor containig initialization code common to both user-managed and
    automatically managed mode.@br
    This constructor is used only internally, never call it directly!
  }
    constructor CommonCreate;
  {:
    No parameter object constructor.@br
    Creates an user-managed instance.@br
  }
    constructor Create; overload;
  {:
    Parametrized object constructor.@br
    Creates automatically managed instance, but does not assign callbacks since
    they are not passed in paramters - you have to assign them manually after
    object creation.@br
    If passed telemetry/game versions are not supported then an exception is
    raised.

    @param(TelemetryVersion Version of telemetry for which the object should
                            prepare.)
    @param GameID           Game identifier.
    @param GameVersion      Version of game.
    @param GameName         Name of the game (optional parameter).

    @raises(ETLUnsupportedAPI  When telemetry version is not supported by this
                               class or when preparation for it fails.)
    @raises(ETLUnsupportedGame When game and/or its version is not supported by
                               this class or when preparation for it fails.)
  }
    constructor Create(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t; GameName: TelemetryString = ''); overload;
  {:
    Parametrized object constructor.@br
    Creates automatically managed instance and automatically assigns API
    callbacks.@br
    If passed telemetry/game versions are not supported then an exception is
    raised.

    @param(TelemetryVersion       Version of telemetry for which the object
                                  should prepare.)
    @param(Parameters             Structure containing other parameters used in
                                  constructor (callbacks pointers, game version,
                                  etc.).)
    @param(AllowAutoRegistration  Value of this parameter is immediately stored
                                  in AllowAutoRegistration property - telling
                                  appropriate functions (called from this
                                  constructor) whether they can automatically
                                  register game events or not.)

    @raises(ETLUnsupportedAPI  When telemetry version is not supported by this
                               class or when preparation for it fails.)
    @raises(ETLUnsupportedGame When game and/or its version is not supported by
                               this class or when preparation for it fails.)
  }
    constructor Create(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t; AllowAutoRegistration: Boolean = True); overload;
  {:
    Specialized object constructor.@br

    This constructor is designed to create an automatically managed instance
    prepared for latest supported version of passed game.
    It calls parametrized constructor with parameter @code(TelemetryVersion) set
    to value returned by function HighestSupportedTelemetryVersion,
    @code(GameID) set to passed game id and @code(GameVersion) set to value
    returned by function HighestSupportedGameVersion.
    Does not assign callbacks since they are not passed in paramters - you have
    to assign them manually after object creation.@br

    @param GameID   Game identifier.
    @param(GameName Name of the game (optional parameter). This parameter is not
                    procesed, only passed as is.)
  }
    constructor CreateCurrent(GameID: TelemetryString; GameName: TelemetryString = '');
  {:
    Object destructor.@br
    Internal objects are automatically cleared in destructor, so it is
    not necessary to free them explicitly.@br
    Also, all registred telemetry events and channels are unregistered.
    OnDestroy event is called from this method (after unregistrations, before
    destruction of internal objects).
  }
    destructor Destroy; override;
  {:
    Sets API information (TelemetryVersion, GameID, GameVersion and GameName)
    from passed @noAutoLink(parameters).

    @param(TelemetryVersion Version of telemetry API.)
    @param(Parameters       Structure containing other API information.)
  }
    procedure SetAPIInfo(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t); virtual;
  {:
    Use this method to set all game callbacks in one call.

    @param(Parameters Structure provided by telemetry API that contains all
                      necessary callback pointers.)
  }
    procedure SetGameCallbacks(Parameters: scs_telemetry_init_params_t); virtual;
  {:
    Use this method to set one specific game callback.@br
    Can be used to set specific callback to nil and thus disabling it (all
    calls to any callback are preceded by check for assignment).

    @bold(Warning) - @code(CallbackFunction) pointer is not checked for actual
                     type.

    @param(Callback         Indicates to which callback assign given function
                            pointer from second parameter.)
    @param CallbackFunction Pointer to be assigned.
  }
    procedure SetGameCallback(Callback: TGameCallback; CallbackFunction: Pointer); virtual;
  {:
    Use this method to write typed message to game log.@br
    Works only when cbLog callback is assigned.

    @param(LogType Type of message (i.e. if it is error, warning or
                   normal message).)
    @param LogText Actual message text.
  }
    procedure Log(LogType: scs_log_type_t; const LogText: String); overload; virtual;
  {:
    Use this method to write message to game log. Message will be written as
    normal text (LogType set to @code(SCS_LOG_TYPE_message)).@br
    Works only when cbLog callback is assigned.

    @param LogText Actual message text.
  }
    procedure Log(const LogText: String); overload; virtual;
  {:
    Checks whether given event is present in RegisteredEvents list, when
    included, it is assumed that this event is registered in telemetry API.

    @param Event Event to be checked.

    @returns @True when given event is found in list, otherwise @false.
  }
    Function EventRegistered(Event: scs_event_t): Boolean; virtual;
  {:
    Registers specified telemetry event.@br
    Works only when cbRegisterEvent callback is assigned, when it is not,
    function returns false and LastTelemetryResult is set to
    @code(SCS_RESULT_generic_error).
    When instance was created as automatically managed, a check whether
    requested event is already registered is performed. When it is, function
    returns @true but LastTelemetryResult is set to
    @code(SCS_RESULT_already_registered). User-managed instance does not perform
    this check.

    @param Event    Event to be registered.
    @param(UserData Any user data. Passed pointer will be stored with event
                    context.)

    @returns(@True when registration was successful, otherwise @false (property
             LastTelemetryResult contains result code).)
  }
    Function EventRegister(Event: scs_event_t; UserData: Pointer = nil): Boolean; virtual;
  {:
    Registers telemetry event that is listed in known events list at position
    given by @code(Index) parameter. When index falls out of allowed boundaries,
    no event is registered and function returns @false.

    @param Index    Index of requested event in known events list.
    @param(UserData Any user data. Passed pointer will be stored with event
                    context.)

    @returns @True when requested event was registered, @false otherwise.
  }
    Function EventRegisterByIndex(Index: Integer; UserData: Pointer = nil): Boolean; virtual;
  {:
    Unregisters specified telemetry event.@br
    Works only when cbUnregisterEvent callback is assigned, when it is not,
    function returns false and LastTelemetryResult is set to
    @code(SCS_RESULT_generic_error).
    When instance was created as automatically managed, a check whether
    requested event is really registered is performed. When it is not, function
    returns @true but LastTelemetryResult is set to
    @code(SCS_RESULT_not_found). User-managed instance does not perform this
    check.

    @param Event Event to be unregistered.

    @returns(@True when unregistration was successful, otherwise @false
            (property LastTelemetryResult contains result code).)
  }
    Function EventUnregister(Event: scs_event_t): Boolean; virtual;
  {:
    Unregister telemetry event that is listed in registered events list at
    position given by @code(Index) parameter.@br
    When index falls out of allowed boundaries, no channel is unregistered and
    function returns @false.

    @param Index Index of event in registered events list.

    @returns @True when requested event was unregistered, @false otherwise.
  }
    Function EventUnregisterIndex(Index: Integer): Boolean; virtual;
  {:
    Unregisters telemetry event that is listed in known events list at position
    given by @code(Index) parameter. When index falls out of allowed boundaries,
    no event is unregistered and function returns @false.

    @param Index Index of requested event in known events list.

    @returns @True when requested event was unregistered, @false otherwise.
  }
    Function EventUnregisterByIndex(Index: Integer): Boolean; virtual;
  {:
    Registers all events that TTelemetryInfoProvider class is aware of for
    current telemetry and game version.

    @returns Number of successfully registered events.
  }
    Function EventRegisterAll: Integer; virtual;
  {:
    Unregisters all events listed in RegisteredEvents list.

    @returns Number of successfully unregistered events.
  }
    Function EventUnregisterAll: Integer; virtual;
  {:
    Checks whether given channel is present in RegisteredChannels list, when
    included, it is assumed that this channel is registered in telemetry API.

    @param Name      Name of channel to be checked.
    @param Index     Index of channel.
    @param ValueType Value type of checked channel.

    @returns @True when given channel is found in list, otherwise @false.
  }
    Function ChannelRegistered(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean; virtual;
  {:
    Registers specified telemetry channel.@br
    Works only when cbRegisterChannel callback is assigned, when it is not,
    function returns false and LastTelemetryResult is set to
    @code(SCS_RESULT_generic_error).
    When instance was created as automatically managed, a check whether
    requested channel is already registered is performed. When it is, function
    returns @true but LastTelemetryResult is set to
    @code(SCS_RESULT_already_registered). User-managed instance does not perform
    this check.

    @param Name      Name of registered channel.
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.
    @param Flags     Registration flags.
    @param(UserData  Any user data. Passed pointer will be stored with event
                     context.)

    @returns(@True when registration was successful, otherwise @false (property
             LastTelemetryResult contains result code).)
  }
    Function ChannelRegister(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none; UserData: Pointer = nil): Boolean; virtual;
  {:
    Registers telemetry channel that is listed in known channels list at
    position given by @code(Index) parameter.@br
    When channel is marked as indexed then all index-versions of such channel
    are registered.@br
    Channel is registered only for its primary value type.@br
    When index falls out of allowed boundaries, no channel is registered and
    function returns @false.

    @param Index    Index of requested channel in known channels list.
    @param(UserData Any user data. Passed pointer will be stored with event
                    context.)

    @returns @True when requested channel was registered, @false otherwise.
  }
    Function ChannelRegisterByIndex(Index: Integer; UserData: Pointer = nil): Boolean; virtual;
  {:
    Registers telemetry channel of a given name. Channel of this name must be
    listed in known channels list as other information required for
    registration are taken from there.@br
    When channel is marked as indexed then all posible indices of such channel
    are registered.@br
    Channel is registered only for its primary value type.@br
    When channel of given name is not listed between known channels, then no
    channel is registered and function returns @false.

    @param Name     Name of channel to be registered.
    @param(UserData Any user data. Passed pointer will be stored with event
                    context.)

    @returns @True when requested channel was registered, @false otherwise.
  }
    Function ChannelRegisterByName(const Name: TelemetryString; UserData: Pointer = nil): Boolean; virtual;
  {:
    Unregisters specified telemetry channel.
    Works only when cbUnregisterChannel callback is assigned, when it is not,
    function returns false and LastTelemetryResult is set to
    @code(SCS_RESULT_generic_error).
    When instance was created as automatically managed, a check whether
    requested channel is really registered is performed. When it is not,
    function returns @true but LastTelemetryResult is set to
    @code(SCS_RESULT_not_found). User-managed instance does not perform this
    check.

    @param Name      Name of channel to be unregistered.
    @param Index     Index of channel.
    @param ValueType Value type of channel.

    @returns(@True when unregistration was successful, otherwise @false
            (property LastTelemetryResult contains result code).)
  }
    Function ChannelUnregister(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean; virtual;
  {:
    Unregisters telemetry channel that is listed in registered channels list at
    position given by @code(Index) parameter.@br
    When index falls out of allowed boundaries, no channel is unregistered and
    function returns @false.

    @param Index Index of channel in registered channels list.

    @returns @True when requested channel was unregistered, @false otherwise.
  }
    Function ChannelUnregisterIndex(Index: Integer): Boolean; virtual;
  {:
    Unregisters all registered telemetry channels with the same name as channel
    that is listed in known channels list at position given by @code(Index)
    parameter.@br
    When index falls out of allowed boundaries, no channel is unregistered and
    function returns @false.

    @param Index Index of requested channel in known channels list.

    @returns @True when requested channel was unregistered, @false otherwise.
  }
    Function ChannelUnregisterByIndex(Index: Integer): Boolean; virtual;
  {:
    Unregisters all registered telemetry channels with the given name.@br

    @param Name Name of channel(s) to be unregistered.

    @returns @True when requested channel was unregistered, @false otherwise.
  }
    Function ChannelUnregisterByName(const Name: TelemetryString): Boolean; virtual;
  {:
    This method registers all channels that the TTelemetryInfoProvider class is
    aware of for current telemetry and game version.@br
    When channel is marked as indexed, then all channel and index combinations
    are registered (see implementation of this method for details), otherwise
    only channels with index set to @code(SCS_U32_NIL) are registered. When
    ManageIndexedChannels property is set to @true, then top index for indexed
    channel registration is taken from appropriate stored configuration value
    (if such exists).@br

    @param RegPrimaryTypes          Register primary value type.
    @param(SecondarySelectionMask   Bitmask denoting what secondary value types
                                    should be registered. See description of
                                    function SelectSupportedValueTypes (parameter
                                    @code(SecondarySelectionMask)) for details
                                    about how the individual types are selected
                                    based on this mask.)

    @Returns Number of successfully registered channels.
  }
    Function ChannelRegisterAll(RegisterPrimaryType: Boolean = True; SecondarySelectionMask: UInt32 = 0): Integer; virtual;
  {:
    Unregisters all channels listed in RegisteredChannels list.

    @returns Number of successfully unregistered channels.)
  }
    Function ChannelUnregisterAll: Integer; virtual;
  {:
    Checks whether given config is stored in StoredConfigs list.

    @param ConfigReference  Full reference of config that is to be checked.
    @param Index            Index of checked configuration.

    @returns @True when given config is found in list, otherwise @false.
  }
    Function ConfigStored(ConfigReference: TConfigReference; Index: scs_u32_t = SCS_U32_NIL): Boolean; virtual;
  {:
    Checks whether given channel is stored in StoredChannels list.

    @param Name      Name of checked channel.
    @param Index     Index of checked channel.
    @param ValueType Value type of checked channel.

    @returns @True when given channel is found in list, otherwise @false.
  }
    Function ChannelStored(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean; virtual;
  published
  {:
    @True when current instance is user managed, @false when it is managed
    automatically.
  }
    property UserManaged: Boolean read fUserManaged;
  {:
    Indicates whether some events can be automatically registered at certain
    situations (see properties KeepUtilityEvents, StoreConfigurations and
    ManageIndexedChannels).@br
    Initial value depends on instance mode and constructor parameters.
  }
    property AllowAutoRegistration: Boolean read fAllowAutoRegistration write fAllowAutoRegistration;
  {:
    This object provides lists of known events, channels and configs along with
    some methods around these lists/structures.@br
    Its main internal use is in registration of all known events and channels.
  }
    property TelemetryInfoProvider: TTelemetryInfoProvider read fInfoProvider;
  {:
    Internal list that stores contexts for registered events.@br
    Its content is manager automatically during telemetry events
    (un)registrations.
  }
    property RegisteredEvents: TRegisteredEventsList read fRegisteredEvents;
  {:
    Internal list that stores contexts for registered channel.@br
    Its content is manager automatically during telemetry channels
    (un)registrations.
  }
    property RegisteredChannels: TRegisteredChannelsList read fRegisteredChannels;
  {:
    Internal list used to store received configurations when StoreConfigurations
    switch is set to @true.
  }
    property StoredConfigs: TStoredconfigsList read fStoredConfigs;
  {:
    Internal list used to store received channels values when StoreChannels
    switch is set to @true.
  }
    property StoredChannels: TStoredChannelsList read fStoredChannels write fStoredChannels;
  {:
    Result code of last executed API function.@br
    Initialized to SCS_RESULT_ok.
  }
    property LastTelemetryResult: scs_result_t read fLastTelemetryResult;
  {:
    Telemetry version for which this object was created.@br
    Initialized to SCS_U32_NIL.
  }
    property TelemetryVersion: scs_u32_t read fTelemetryVersion write fTelemetryVersion;
  {:
    Name of the game for which this object was created.@br
    Initialized to an empty string.
  }
    property GameName: TelemetryString read fGameName write fGameName;
  {:
    ID of game for which this object was created.@br
    Initialized to an empty string.
  }
    property GameID: TelemetryString read fGameID write fGameID;
  {:
    Version of game for which this object was created.@br
    Initialized to an SCS_U32_NIL.
  }
    property GameVersion: scs_u32_t read fGameVersion write fGameVersion;
  {:
    When set to @true, the recipient automatically registers all known events
    marked as utility (property AllowAutoRegistration must be @true for this)
    and also refuses to unregister such events.@br
    This property is intended to ensure the recipient will stay responsive,
    because events and channels can be (un)registered only in events callbacks.
    If no event would be registered, then such call will not occur, rendering
    recipient unresponsive.@br
    Initialized to @true.
  }
    property KeepUtilityEvents: Boolean read fKeepUtilityEvents write SetKeepUtilityEvents;
  {:
    If @true, any configuration data passed from the game are parsed and
    stored.@br
    When set to @true, the configuration event is automatically registered
    (property AllowAutoRegistration must be also @true).@br
    When set to @false, StoredConfigs list is cleared.@br
    Initialized to @true.
  }
    property StoreConfigurations: Boolean read fStoreConfigurations write SetStoreConfigurations;
  {:
    When @true, registration and unregistration of indexed channels that are
    index-binded to some configuration is automatically managed.@br
    Affected methods:
    @unorderedList(
    @item(ChannelRegisterAll - indexed channels are registered up to index
      (count - 1) stored in config a channel is binded to. If such config is not
      stored or channel is not binded to any config, then indices are taken from
      one of default values (@link(TKnownChannel.MaxIndex per-channel value) or
      constant MaxChannelIndex).)
    @item(ProcessConfigurationEvent - when binded config is parsed, all
      registered channels are scanned and those binded to this config are
      proccessed in the following way:@unorderedList(
        @itemSpacing(Compact)
        @item(Channels with index above or equal to value stored in the config
              are unregistered.)
        @item(All channels with indices below value from the config are
              registered, but only if they are not already.))
      For example, when channels with indices 0 and 2 are registred, and binded
      config is received containing 2, then channel with index 2 is unregistered
      and channel with index 1 is regitered.@br
      Property AllowAutoRegistration must be also @true for this feature to work
      in this method))
    Initialized to @false.
  }
    property ManageIndexedChannels: Boolean read fManageIndexedChannels write fManageIndexedChannels;
  {:
    When @true, all incoming channels along with their values are stored in
    StoredChannels list.@br
    When set to @false, StoredChannels list is cleared.@br
    Initialized to @false.
  }
    property StoreChannels: Boolean read fStoreChannels write SetStoreChannels;
  {:
    Event called before destruction of instance. It is called AFTER all
    registered channels and events are unregistered and KeepUtilityEvents is set
    to @false, but before internal objects are freed.
  }
    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
  {:
    Event called when recipient attempts to write to game log.
  }
    property OnLog: TLogEvent read fOnLog write fOnLog;
  {:
    Event called on every @bold(successful) event registration.@br
    It can be called multiple times when method EventRegisterAll is executed.
  }
    property OnEventRegister: TEventRegisterEvent read fOnEventRegister write fOnEventRegister;
  {:
    Event called on every @bold(successful) event unregistration.@br
    It can be called multiple times when method EventUnregisterAll is executed.
  }
    property OnEventUnregister: TEventRegisterEvent read fOnEventUnregister write fOnEventUnregister;
  {:
    Event called whenever the recipient receives any event from telemetry API.
  }
    property OnEvent: TEventEvent read fOnEvent write fOnEvent;
  {:
    Event called on every @bold(successful) channel registration.@br
    It can be called multiple times when method ChannelRegisterAll is executed.
  }
    property OnChannelRegister: TChannelRegisterEvent read fOnChannelRegister write fOnChannelRegister;
  {:
    Event called on every @bold(successful) channel unregistration.@br
    It can be called multiple times when method ChannelUnregisterAll is
    executed.
  }
    property OnChannelUnregister: TChannelUnregisterEvent read fOnChannelUnregister write fOnChannelUnregister;
  {:
    Event called whenever the recipient receives any channel call from telemetry
    API.

    @bold(Warning) - this event can be called quite often (usually many times
    per frame).
  }
    property OnChannel: TChannelEvent read fOnChannel write fOnChannel;
  {:
    Event called when config is parsed from configuration telemetry event data.
  }
    property OnConfig: TConfigEvent read fOnConfig write fOnConfig;
  {$IFDEF MulticastEvents}
  {:
    Multicast event called before destruction of instance. It is called AFTER
    all registered channels and events are unregistered and KeepUtilityEvents is
    set to @false.
  }
    property OnDestroyMulti: TMulticastNotifyEvent read fOnDestroyMulti;
  {:
    Multicast event called when recipient attempts to write to game log.
  }
    property OnLogMulti: TMulticastLogEvent read fOnLogMulti;
  {:
    Multicast event called on every @bold(successful) event registration.@br
    It can be called multiple times when method EventRegisterAll is executed.
  }
    property OnEventRegisterMulti: TMulticastEventRegisterEvent read fOnEventRegisterMulti;
  {:
    Multicast event called on every @bold(successful) event unregistration.@br
    It can be called multiple times when method EventUnregisterAll is executed.
  }
    property OnEventUnregisterMulti: TMulticastEventRegisterEvent read fOnEventUnregisterMulti;
  {:
    Multicast event called whenever the recipient receives any event from
    telemetry API.
  }
    property OnEventMulti: TMulticastEventEvent read fOnEventMulti;
  {:
    Multicast event called on every @bold(successful) channel registration.@br
    It can be called multiple times when method ChannelRegisterAll is executed.
  }
    property OnChannelRegisterMulti: TMulticastChannelRegisterEvent read fOnChannelRegisterMulti;
  {:
    Multicast event called on every @bold(successful) channel unregistration.@br
    It can be called multiple times when method ChannelUnregisterAll is
    executed.
  }
    property OnChannelUnregisterMulti: TMulticastChannelUnregisterEvent read fOnChannelUnregisterMulti;
  {:
    Multicast event called whenever the recipient receives any channel call from
    telemetry API.

    @bold(Warning) - this event can be called quite often (usually many times
    per frame).
  }
    property OnChannelMulti: TMulticastChannelEvent read fOnChannelMulti;
  {:
    Multicast event called when config is parsed from configuration telemetry
    event data.
  }
    property OnConfigMulti: TMulticastConfigEvent read fOnConfigMulti;
  {$ENDIF}
  end;

{==============================================================================}
{   Unit functions and procedures // Declaration                               }
{==============================================================================}

{:
  @abstract(Function intended as callback for streaming functions, converting
            channel name to ID.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryRecipient object.

  @param Name      Channel name to be converted to ID.
  @param(Recipient TTelemetryRecipient object that will be used for actual
                   conversion.)

  @returns Channel ID obtained from passed name.
}
Function RecipientGetChannelIDFromName(const Name: TelemetryString; Recipient: Pointer): TChannelID;

{:
  @abstract(Function intended as callback for streaming functions, converting
            channel ID to name.)
  @code(UserData) passed to streaming function along with this callback must
  contain valid TTelemetryRecipient object.

  @param ID        Channel ID to be converted to name.
  @param(Recipient TTelemetryRecipient object that will be used for actual
                   conversion.)

  @returns Channel name obtained from passed ID.
}
Function RecipientGetChannelNameFromID(ID: TChannelID; Recipient: Pointer): TelemetryString;

implementation

uses
  SysUtils, Math,
  TelemetryValueTypeUtils, TelemetryConversions, TelemetryStrings;

{$IFNDEF Documentation}
  {$DEFINE ImplementationPart}
    {$INCLUDE '.\Inc\TelemetryMulticastEvents.pas'}
  {$UNDEF ImplementationPart}
{$ENDIF}

{==============================================================================}
{   Unit functions and procedures // Implementation                            }
{==============================================================================}

Function RecipientGetChannelIDFromName(const Name: TelemetryString; Recipient: Pointer): TChannelID;
begin
Result := TTelemetryRecipient(Recipient).TelemetryInfoProvider.KnownChannels.ChannelNameToID(Name);
end;

//------------------------------------------------------------------------------

Function RecipientGetChannelNameFromID(ID: TChannelID; Recipient: Pointer): TelemetryString;
begin
Result := TTelemetryRecipient(Recipient).TelemetryInfoProvider.KnownChannels.ChannelIDToName(ID);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                          Plugin (library) callbacks                          }
{------------------------------------------------------------------------------}
{==============================================================================}
{
Following two procedures are passed to game as plugin/library callbacks, because
methods (of the TTelemetryRecipient) cannot be passed to API (method pointers
and function/procedure pointers are fundamentally different - see
ObjectPascal/Delphi documentation).
They are implemented pretty much only as wrappers to recipient methods.
}

// Procedure used as library callback to receive events.
procedure EventReceiver(event: scs_event_t; event_info: Pointer; context: scs_context_t); stdcall;
begin
try
  If Assigned(context) then
    If Assigned(PEventContext(context)^.Recipient) then
      try
        TTelemetryRecipient(PEventContext(context)^.Recipient).EventHandler(event,event_info,PEventContext(context)^.UserData);
      except
      {$IFDEF Debug}
        on E: ETLException do
          TTelemetryRecipient(PEventContext(context)^.Recipient).Log(SCS_LOG_TYPE_error,
            Format('[Telemetry Library](EventReceiver) Exception intercepted: "%s"',[E.Message]));
      {$ENDIF}
      end;
except
// Do nothing, throw intercepted exception away.
end;
end;

//------------------------------------------------------------------------------

// Procedure used as library callback to receive channels.
procedure ChannelReceiver(name: scs_string_t; index: scs_u32_t; value: p_scs_value_t; context: scs_context_t); stdcall;
begin
try
  If Assigned(context) then
    If Assigned(PChannelContext(context)^.Recipient) then
      try
        with PChannelContext(context)^ do
          TTelemetryRecipient(Recipient).ChannelHandler(APIStringToTelemetryString(name),ChannelInfo.ID,index,value,PChannelContext(context)^.UserData);
      except
      {$IFDEF Debug}
        on E: ETLException do
          TTelemetryRecipient(PEventContext(context)^.Recipient).Log(SCS_LOG_TYPE_error,
            Format('[Telemetry Library](ChannelReceiver) Exception intercepted: "%s"',[E.Message]));
      {$ENDIF}
      end;
except
// Do nothing, throw intercepted exception away.
end;  
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TTelemetryRecipient                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryRecipient // Class implementation                                }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryRecipient // Constants, types, variables, etc...                 }
{------------------------------------------------------------------------------}

const
  // Default (initial) values for TTelemeryRecipient properties (def_um_* values
  // are intended as initial values for user-managed mode).
  def_AllowAutoRegistration = True;
  def_KeepUtilityEvents     = True;
  def_StoreConfigurations   = True;
  def_ManageIndexedChannels = False;
  def_StoreChannels         = False;

  def_um_AllowAutoRegistration = False;
  def_um_KeepUtilityEvents     = False;
  def_um_StoreConfigurations   = False;
  def_um_ManageIndexedChannels = False;
  def_um_StoreChannels         = False;

{------------------------------------------------------------------------------}
{   TTelemetryRecipient // Private methods                                     }
{------------------------------------------------------------------------------}

procedure TTelemetryRecipient.SetKeepUtilityEvents(Value: Boolean);
var
  i:  Integer;
begin
If Value and AllowAutoRegistration then
  begin
    For i := 0 to Pred(fInfoProvider.KnownEvents.Count) do
      If fInfoProvider.KnownEvents[i].Utility  then
        EventRegister(fInfoProvider.KnownEvents[i].Event);
  end;
fKeepUtilityEvents := Value;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.SetStoreConfigurations(Value: Boolean);
begin
If Value and AllowAutoRegistration then
  fStoreConfigurations := EventRegister(SCS_TELEMETRY_EVENT_configuration)
else
  fStoreConfigurations := Value;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.SetStoreChannels(Value: Boolean);
begin
If not Value then fStoredChannels.Clear;
fStoreChannels := Value;
end;

{------------------------------------------------------------------------------}
{    TTelemetryRecipient // Protected methods                                  }
{------------------------------------------------------------------------------}

procedure TTelemetryRecipient.EventHandler(Event: scs_event_t; Data: Pointer; UserData: Pointer);
begin
DoOnEvent(Self,Event,Data,UserData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.ChannelHandler(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
begin
DoOnChannel(Self,Name,Id,Index,Value,UserData);
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.ProcessConfigurationEvent(const Data: scs_telemetry_configuration_t);
var
  TempAttr:       p_scs_named_value_t;
  TempReference:  TConfigReference;
  TempValue:      scs_value_localized_t;

  procedure ManageBindedChannels(ConfigReference: TConfigReference; NewMaxIndex: scs_u32_t);
  var
    i,j:  Integer;
  begin
    // Unregister all channels above current max index.
    For i := (fRegisteredChannels.Count - 1) downto 0 do
      If TelemetrySameText(fRegisteredChannels[i].IndexConfig.ID,ConfigReference.ID) and
         TelemetrySameText(fRegisteredChannels[i].IndexConfig.Attribute,ConfigReference.Attribute) then
        If fRegisteredChannels[i].Index > NewMaxIndex then ChannelUnregisterIndex(i);
    // Register new channels up to current max index.
    For i := 0 to (fInfoProvider.KnownChannels.Count - 1) do
      If TelemetrySameText(fInfoProvider.KnownChannels[i].IndexConfig.ID,ConfigReference.ID) and
         TelemetrySameText(fInfoProvider.KnownChannels[i].IndexConfig.Attribute,ConfigReference.Attribute) then
        For j := 0 to NewMaxIndex do
          If not ChannelRegistered(fInfoProvider.KnownChannels[i].Name,j,fInfoProvider.KnownChannels[i].PrimaryType) then
            ChannelRegister(fInfoProvider.KnownChannels[i].Name,j,fInfoProvider.KnownChannels[i].PrimaryType,SCS_TELEMETRY_CHANNEL_FLAG_none);
  end;

begin
TempAttr := Data.attributes;
while Assigned(TempAttr^.name) do
  begin
    TempReference.ID := APIStringToTelemetryString(Data.id);
    TempReference.Attribute := APIStringToTelemetryString(TempAttr^.name);
    TempValue := scs_value_localized(TempAttr^.value);
    If AllowAutoRegistration and ManageIndexedChannels then
      If (TempAttr^.value._type = SCS_VALUE_TYPE_u32) and fInfoProvider.KnownConfigs.IsBinded(TempReference) then
        ManageBindedChannels(TempReference,TempAttr^.value.value_u32.value - 1);
    If StoreConfigurations then
      If fStoredConfigs.ChangeConfigValue(TempReference,TempAttr^.index,TempValue) < 0 then
        fStoredConfigs.Add(TempReference,TempAttr^.index,TempValue,fInfoProvider.KnownConfigs.IsBinded(TempReference));  
    DoOnConfig(Self,TempReference,TempAttr^.index,TempValue);
    Inc(TempAttr);
  end;
end;

{------------------------------------------------------------------------------}
{    TTelemetryRecipient // Public methods                                     }
{------------------------------------------------------------------------------}

procedure TTelemetryRecipient.DoOnDestroy(Sender: TObject);
begin
If Assigned(fOnDestroy) then fOnDestroy(Sender);
{$IFDEF MulticastEvents}
If Assigned(fOnDestroyMulti) then fOnDestroyMulti.Call(Sender);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnLog(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
begin
If Assigned(fOnLog) then fOnLog(Sender,LogType,LogText);
{$IFDEF MulticastEvents}
fOnLogMulti.Call(Sender,LogType,LogText);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnEventRegister(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
If Assigned(fOnEventRegister) then fOnEventRegister(Sender,Event,UserData);
{$IFDEF MulticastEvents}
fOnEventRegisterMulti.Call(Sender,Event,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnEventUnregister(Sender: TObject; Event: scs_event_t; UserData: Pointer);
begin
If Assigned(fOnEventUnregister) then fOnEventUnregister(Sender,Event,UserData);
{$IFDEF MulticastEvents}
fOnEventUnregisterMulti.Call(Sender,Event,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnEvent(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
begin
If (Event = SCS_TELEMETRY_EVENT_configuration) then
  ProcessConfigurationEvent(p_scs_telemetry_configuration_t(Data)^);
If Assigned(fOnEvent) then fOnEvent(Sender,Event,Data,UserData);
{$IFDEF MulticastEvents}
fOnEventMulti.Call(Sender,Event,Data,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnChannelRegister(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
If Assigned(fOnChannelRegister) then fOnChannelRegister(Sender,Name,ID,Index,ValueType,Flags,UserData);
{$IFDEF MulticastEvents}
fOnChannelRegisterMulti.Call(Sender,Name,ID,Index,ValueType,Flags,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnChannelUnregister(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
If Assigned(fOnChannelUnregister) then fOnChannelUnregister(Sender,Name,ID,Index,ValueType,UserData);
{$IFDEF MulticastEvents}
fOnChannelUnregisterMulti.Call(Sender,Name,ID,Index,ValueType,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnChannel(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
begin
If StoreChannels then
  fStoredChannels.StoreChannelValue(Name,ID,Index,Value);
If Assigned(fOnChannel) then fOnChannel(Sender,Name,ID,Index,Value,UserData);
{$IFDEF MulticastEvents}
fOnChannelMulti.Call(Sender,Name,ID,Index,Value,UserData);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.DoOnConfig(Sender: TObject; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If Assigned(fOnConfig) then fOnConfig(Sender,ConfigReference,Index,Value);
{$IFDEF MulticastEvents}
fOnConfigMulti.Call(Sender,ConfigReference,Index,Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.HighestSupportedTelemetryVersion: scs_u32_t;
begin
Result := Min(inherited HighestSupportedTelemetryVersion,
              TTelemetryInfoProvider.HighestSupportedTelemetryVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.HighestSupportedGameVersion(GameID: TelemetryString): scs_u32_t;
begin
Result := Min(inherited HighestSupportedGameVersion(GameID),
              TTelemetryInfoProvider.HighestSupportedGameVersion(GameID));
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.SupportsTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean;
begin
Result := inherited SupportsTelemetryVersion(TelemetryVersion) and
          TTelemetryInfoProvider.SupportsTelemetryVersion(TelemetryVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.SupportsTelemetryMajorVersion(TelemetryVersion: scs_u32_t): Boolean;
begin
Result := inherited SupportsTelemetryMajorVersion(TelemetryVersion) and
          TTelemetryInfoProvider.SupportsTelemetryMajorVersion(TelemetryVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.SupportsGameVersion(GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
Result := inherited SupportsGameVersion(GameID,GameVersion) and
          TTelemetryInfoProvider.SupportsGameVersion(GameID,GameVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.SupportsTelemetryAndGameVersion(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
Result := inherited SupportsTelemetryAndGameVersion(TelemetryVersion,GameID,GameVersion) and
          TTelemetryInfoProvider.SupportsTelemetryAndGameVersion(TelemetryVersion,GameID,GameVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryRecipient.SupportsTelemetryAndGameVersionParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean;
begin
Result := inherited SupportsTelemetryAndGameVersionParam(TelemetryVersion,Parameters) and
          TTelemetryInfoProvider.SupportsTelemetryAndGameVersionParam(TelemetryVersion,Parameters);
end;


//------------------------------------------------------------------------------

Function TTelemetryRecipient.PrepareForTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean;
begin
Result := inherited PrepareForTelemetryVersion(TelemetryVersion);
If Result then fTelemetryVersion := TelemetryVersion;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.PrepareForGameVersion(const GameName,GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
Result := inherited PrepareForGameVersion(GameName,GameID,GameVersion);
If Result then
  begin
    fGameName := GameName;
    fGameID := GameID;
    fGameVersion := GameVersion;
  end;
end;

//------------------------------------------------------------------------------

constructor TTelemetryRecipient.CommonCreate;
begin
inherited Create;
// These fields are not filled in inherited preparation routines, so they must
// be initialized before these routines are called.
fTelemetryVersion := SCS_U32_NIL;
fGameName := '';
fGameID := '';
fGameVersion := SCS_U32_NIL;
// Fields initialization.
cbLog := nil;
cbRegisterEvent := nil;
cbUnregisterEvent := nil;
cbRegisterChannel := nil;
cbUnregisterChannel := nil;
fRegisteredEvents := TRegisteredEventsList.Create;
fRegisteredChannels := TRegisteredChannelsList.Create;
fStoredConfigs := TStoredConfigsList.Create;
fStoredChannels := TStoredChannelsList.Create;
{$IFDEF MulticastEvents}
fOnDestroyMulti := TMulticastNotifyEvent.Create(Self);
fOnLogMulti := TMulticastLogEvent.Create(Self);
fOnEventRegisterMulti := TMulticastEventRegisterEvent.Create(Self);
fOnEventUnregisterMulti := TMulticastEventRegisterEvent.Create(Self);
fOnEventMulti := TMulticastEventEvent.Create(Self);
fOnChannelRegisterMulti := TMulticastChannelRegisterEvent.Create(Self);
fOnChannelUnregisterMulti := TMulticastChannelUnregisterEvent.Create(Self);
fOnChannelMulti := TMulticastChannelEvent.Create(Self);
fOnConfigMulti := TMulticastConfigEvent.Create(Self);
{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryRecipient.Create;
begin
CommonCreate;
// User managed instance.
fUserManaged := True;
fInfoProvider := TTelemetryInfoProvider.Create;
AllowAutoRegistration := def_um_AllowAutoRegistration;
fLastTelemetryResult := SCS_RESULT_ok;
KeepUtilityEvents := def_um_KeepUtilityEvents;
StoreConfigurations := def_um_StoreConfigurations;
ManageIndexedChannels := def_um_ManageIndexedChannels;
StoreChannels := def_um_StoreChannels;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryRecipient.Create(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t; GameName: TelemetryString = '');
begin
CommonCreate;
// Automatically managed instance.
fUserManaged := False;
// Prepare support for selected game and telemetry versions.
// Raise exception for unsupported telemetry/game.
If not PrepareForTelemetryVersion(TelemetryVersion) then
  raise ETLUnsupportedAPI.CreateFmt('TTelemetryRecipient.Create: Telemetry version (%s) not supported',
                                    [SCSGetVersionAsString(TelemetryVersion)]);
If not PrepareForGameVersion(GameName,GameID,GameVersion) then
  raise ETLUnsupportedGame.CreateFmt('TTelemetryRecipient.Create: Game version (%s %s) not supported',
                                     [TelemetryStringDecode(GameID),SCSGetVersionAsString(GameVersion)]);
// Create telemetry info provider (list of known event, channels, etc.).
fInfoProvider := TTelemetryInfoProvider.Create(TelemetryVersion,GameID,GameVersion);
// Fields initialization.
AllowAutoRegistration := def_AllowAutoRegistration;
KeepUtilityEvents := def_KeepUtilityEvents;
StoreConfigurations := def_StoreConfigurations;
ManageIndexedChannels := def_ManageIndexedChannels;
StoreChannels := def_StoreChannels;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryRecipient.Create(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t; AllowAutoRegistration: Boolean = True);
begin
CommonCreate;
// Automatically managed instance.
fUserManaged := False;
// Prepare support for selected game and telemetry versions.
// Raise exception for unsupported telemetry/game.
If not PrepareForTelemetryVersion(TelemetryVersion) then
  raise ETLUnsupportedAPI.CreateFmt('TTelemetryRecipient.Create: Telemetry version (%s) not supported',
                                    [SCSGetVersionAsString(TelemetryVersion)]);
If not PrepareForGameVersion(APIStringToTelemetryString(Parameters.common.game_name),
  APIStringToTelemetryString(Parameters.common.game_id),Parameters.common.game_version) then
  raise ETLUnsupportedGame.CreateFmt('TTelemetryRecipient.Create: Game version (%s %s) not supported',
                                     [TelemetryStringDecode(APIStringToTelemetryString(Parameters.common.game_id)),
                                     SCSGetVersionAsString(Parameters.common.game_version)]);
// Create telemetry info provider (list of known event, channels, etc.).
fInfoProvider := TTelemetryInfoProvider.Create(TelemetryVersion,
                   APIStringToTelemetryString(Parameters.common.game_id),
                   Parameters.common.game_version);
// Set game callbacks from passed prameters.
SetGameCallBacks(Parameters);
// Fields initialization.
fAllowAutoRegistration := AllowAutoRegistration;
KeepUtilityEvents := def_KeepUtilityEvents;
StoreConfigurations := def_StoreConfigurations;
ManageIndexedChannels := def_ManageIndexedChannels;
StoreChannels := def_StoreChannels;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TTelemetryRecipient.CreateCurrent(GameID: TelemetryString; GameName: TelemetryString = '');
begin
Create(HighestSupportedTelemetryVersion,GameID,HighestSupportedGameVersion(GameID),GameName);
end;

//------------------------------------------------------------------------------

destructor TTelemetryRecipient.Destroy;
begin
// Set KeepUtilityEvents to false to allow unregistration of all events.
KeepUtilityEvents := False;
If Assigned(fRegisteredChannels) then
  ChannelUnregisterAll;
If Assigned(fRegisteredEvents) then
  EventUnregisterAll;
DoOnDestroy(Self);
{$IFDEF MulticastEvents}
// Free all multicast events handling objects.
fOnDestroyMulti.Free;
fOnLogMulti.Free;
fOnEventRegisterMulti.Free;
fOnEventUnregisterMulti.Free;
fOnEventMulti.Free;
fOnChannelRegisterMulti.Free;
fOnChannelUnregisterMulti.Free;
fOnChannelMulti.Free;
fOnConfigMulti.Free;
{$ENDIF}
// Free all contexts and stored configurations.
fStoredChannels.Free;
fStoredConfigs.Free;
fRegisteredChannels.Free;
fRegisteredEvents.Free;
// Free information provider object.
fInfoProvider.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.SetAPIInfo(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t);
begin
fTelemetryVersion := TelemetryVersion;
fGameName := APIStringToTelemetryString(Parameters.common.game_name);
fGameID := APIStringToTelemetryString(Parameters.common.game_id);
fGameVersion := Parameters.common.game_version;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.SetGameCallbacks(Parameters: scs_telemetry_init_params_t);
begin
cbLog := Parameters.common.log;
cbRegisterEvent := Parameters.register_for_event;
cbUnregisterEvent := Parameters.unregister_from_event;
cbRegisterChannel := Parameters.register_for_channel;
cbUnregisterChannel := Parameters.unregister_from_channel;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.SetGameCallback(Callback: TGameCallback; CallbackFunction: Pointer);
begin
case Callback of
  gcbLog:           cbLog := scs_log_t(CallbackFunction);
  gcbEventReg:      cbRegisterEvent := scs_telemetry_register_for_event_t(CallbackFunction);
  gcbEventUnreg:    cbUnregisterEvent := scs_telemetry_unregister_from_event_t(CallbackFunction);
  gcbChannelReg:    cbRegisterChannel := scs_telemetry_register_for_channel_t(CallbackFunction);
  gcbChannelUnreg:  cbUnregisterChannel := scs_telemetry_unregister_from_channel_t(CallbackFunction);
else
  // No action.
end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryRecipient.Log(LogType: scs_log_type_t; const LogText: String);
var
  TempStr:  TelemetryString;
begin
TempStr := TelemetryStringEncode(LogText);
If Assigned(cbLog) then cbLog(LogType,APIString(TempStr));
DoOnLog(Self,LogType,LogText);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TTelemetryRecipient.Log(const LogText: String);
begin
Log(SCS_LOG_TYPE_message,LogText);
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventRegistered(Event: scs_event_t): Boolean;
begin
Result := fRegisteredEvents.IndexOf(Event) >= 0;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventRegister(Event: scs_event_t; UserData: Pointer = nil): Boolean;
var
  NewEventContext:  PEventContext;
begin
Result := False;
If not fUserManaged and EventRegistered(Event) then
  begin
    Result := True;
    fLastTelemetryResult := SCS_RESULT_already_registered;
  end
else
  begin
    If Assigned(cbRegisterEvent) then
      begin
        NewEventContext := fRegisteredEvents.CreateContext(Self,Event,fInfoProvider.KnownEvents.IsUtility(Event),UserData);
        fLastTelemetryResult := cbRegisterEvent(Event,EventReceiver,NewEventContext);
        If LastTelemetryResult = SCS_RESULT_ok then
          begin
            If fRegisteredEvents.Add(NewEventContext) >= 0 then
              begin
                DoOnEventRegister(Self,Event,UserData);
                Result := True;
              end
            else fRegisteredEvents.FreeContext(NewEventContext);
          end
        else fRegisteredEvents.FreeContext(NewEventContext);
      end
    else fLastTelemetryResult := SCS_RESULT_generic_error;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventRegisterByIndex(Index: Integer; UserData: Pointer = nil): Boolean;
begin
Result := False;
If (Index >=0) and (Index < fInfoProvider.KnownEvents.Count) then
  Result := EventRegister(fInfoProvider.KnownEvents[Index].Event,UserData)
else fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventUnregister(Event: scs_event_t): Boolean;
var
  CtxIndex: Integer;
begin
Result := False;
If not fUserManaged and not EventRegistered(Event) then
  begin
    Result := True;
    fLastTelemetryResult := SCS_RESULT_not_found;
  end
else
  begin
    If Assigned(cbUnregisterEvent) and
      not (KeepUtilityEvents and fInfoProvider.KnownEvents.IsUtility(Event)) then
      begin
        fLastTelemetryResult := cbUnregisterEvent(Event);
        If LastTelemetryResult = SCS_RESULT_ok then
          begin
            CtxIndex := fRegisteredEvents.IndexOf(Event);
            If CtxIndex >= 0 then
              begin
                DoOnEventUnregister(Self,Event,fRegisteredEvents.Contexts[CtxIndex]^.UserData);
                fRegisteredEvents.Delete(CtxIndex);
              end
            else DoOnEventUnregister(Self,Event,nil);
            Result := True;
          end;
      end
    else fLastTelemetryResult := SCS_RESULT_generic_error;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventRegisterAll: Integer;
var
  i:  Integer;
begin
Result := 0;
If Assigned(cbRegisterEvent) then
  For i := 0 to Pred(fInfoProvider.KnownEvents.Count) do
    If fInfoProvider.KnownEvents[i].Valid then
      If EventRegister(fInfoProvider.KnownEvents[i].Event) then Inc(Result);
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventUnregisterByIndex(Index: Integer): Boolean;
begin
Result := False;
If (Index >=0) and (Index < fInfoProvider.KnownEvents.Count) then
  Result := EventUnregister(fInfoProvider.KnownEvents[Index].Event)
else fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventUnregisterIndex(Index: Integer): Boolean;
begin
Result := False;
If Assigned(cbUnregisterEvent) and (Index >= 0) and (Index < fRegisteredEvents.Count) then
  Result := EventUnregister(fRegisteredEvents[Index].Event)
else
  fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.EventUnregisterAll: Integer;
var
  i:  Integer;
begin
Result := 0;
If Assigned(cbUnregisterEvent) then
  For i := Pred(fRegisteredEvents.Count) downto 0 do
    If EventUnregister(fRegisteredEvents[i].Event) then Inc(Result);
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelRegistered(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean;
begin
Result := fRegisteredChannels.IndexOf(Name,Index,ValueType) >= 0;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelRegister(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t = SCS_TELEMETRY_CHANNEL_FLAG_none; UserData: Pointer = nil): Boolean;
var
  NewChannelContext:  PChannelContext;
begin
Result := False;
If not fUserManaged and ChannelRegistered(Name,Index,ValueType) then
  begin
    Result := True;
    fLastTelemetryResult := SCS_RESULT_already_registered;
  end
else
  begin
    If Assigned(cbRegisterChannel) then
      begin
        NewChannelContext := fRegisteredChannels.CreateContext(Self,Name,Index,ValueType,Flags,
                               fInfoProvider.KnownChannels.ChannelIndexConfigID(Name),UserData);
        fLastTelemetryResult := cbRegisterChannel(APIString(Name),Index,ValueType,Flags,ChannelReceiver,NewChannelContext);
        If LastTelemetryResult  = SCS_RESULT_ok then
          begin
            If fRegisteredChannels.Add(NewChannelContext) >= 0 then
              begin
                DoOnChannelRegister(Self,Name,GetItemID(Name),Index,ValueType,Flags,UserData);
                Result := True;
              end
            else fRegisteredChannels.FreeContext(NewChannelContext);
          end
        else fRegisteredChannels.FreeContext(NewChannelContext);
      end
    else fLastTelemetryResult := SCS_RESULT_generic_error;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelRegisterByIndex(Index: Integer; UserData: Pointer = nil): Boolean;
var
  i,Counter:        Integer;
  KnownChannelInfo: TKnownChannel;
  MaxIndex:         Integer;
begin
Result := False;
If Assigned(cbRegisterChannel) and (Index >= 0) and (Index < fInfoProvider.KnownChannels.Count) then
  begin
    KnownChannelInfo := fInfoProvider.KnownChannels[Index];
    If KnownChannelInfo.Indexed then
      begin
        If KnownChannelInfo.MaxIndex <> SCS_U32_NIL then
          MaxIndex := KnownChannelInfo.MaxIndex
        else
          MaxIndex := MaxChannelIndex;
        If ManageIndexedChannels and ValidConfigReference(KnownChannelInfo.IndexConfig) then
          begin
            Index := fStoredConfigs.IndexOf(KnownChannelInfo.IndexConfig);
            If Index >= 0 then
              If fStoredConfigs[Index].Value.ValueType = SCS_VALUE_TYPE_u32 then
                MaxIndex := fStoredConfigs[Index].Value.BinaryData.value_u32.value - 1;
          end;
        Counter := 0;
        For i := 0 to MaxIndex do
          If ChannelRegister(KnownChannelInfo.Name,i,KnownChannelInfo.PrimaryType,SCS_TELEMETRY_CHANNEL_FLAG_none,UserData) then Inc(Counter);
        Result := Counter > 0;
      end
    else Result := ChannelRegister(KnownChannelInfo.Name,SCS_U32_NIL,KnownChannelInfo.PrimaryType,SCS_TELEMETRY_CHANNEL_FLAG_none,UserData);
  end
else fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelRegisterByName(const Name: TelemetryString; UserData: Pointer = nil): Boolean;
begin
Result := ChannelRegisterByIndex(fInfoProvider.KnownChannels.IndexOf(Name),UserData);
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelUnregister(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean;
var
  CtxIndex: Integer;
begin
Result := False;
If not fUserManaged and not ChannelRegistered(Name,Index,ValueType) then
  begin
    Result := True;
    fLastTelemetryResult := SCS_RESULT_not_found;
  end
else
  begin
    If Assigned(cbUnregisterChannel) then
      begin
        fLastTelemetryResult := cbUnregisterChannel(scs_string_t(Name),Index,ValueType);
        If LastTelemetryResult = SCS_RESULT_ok then
          begin
            CtxIndex := fRegisteredChannels.IndexOf(Name,Index,ValueType);
            If CtxIndex >= 0 then
              begin
                DoOnChannelUnregister(Self,Name,GetItemID(Name),Index,ValueType,fRegisteredChannels.Contexts[CtxIndex]^.UserData);
                fRegisteredChannels.Delete(CtxIndex);
              end
            else DoOnChannelUnregister(Self,Name,GetItemID(Name),Index,ValueType,nil);
            Result := True;
          end;
      end
    else fLastTelemetryResult := SCS_RESULT_generic_error;
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelUnregisterIndex(Index: Integer): Boolean;
begin
Result := False;
If Assigned(cbUnregisterChannel) and (Index >= 0) and (Index < fRegisteredChannels.Count) then
  begin
    Result := ChannelUnregister(fRegisteredChannels[Index].Name,
                                fRegisteredChannels[Index].Index,
                                fRegisteredChannels[Index].ValueType);
  end
else fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelUnregisterByIndex(Index: Integer): Boolean;
begin
Result := False;
If (Index >= 0) and (Index < fInfoProvider.KnownChannels.Count) then
  Result := ChannelUnregisterByName(fInfoProvider.KnownChannels[Index].Name)
else fLastTelemetryResult := SCS_RESULT_generic_error;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelUnregisterByName(const Name: TelemetryString): Boolean;
var
  i,Counter:  Integer;
begin
Counter := 0;
If Assigned(cbUnregisterEvent) then
  For i := Pred(fRegisteredChannels.Count) downto 0 do
    If TelemetrySameStr(fRegisteredChannels[i].Name,Name) then
      If ChannelUnregister(fRegisteredChannels[i].Name,
                           fRegisteredChannels[i].Index,
                           fRegisteredChannels[i].ValueType) then Inc(Counter);
Result := Counter > 0;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelRegisterAll(RegisterPrimaryType: Boolean = True; SecondarySelectionMask: UInt32 = 0): Integer;
var
  i,j:                Integer;
  KnownChannelInfo:   TKnownChannel;
  MinIdx,MaxIdx:      Integer;
  ChannelValueTypes:  TValueTypesArray;
  TypeIdx:            Integer;

  Function CheckAndRegister(Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean;
  begin
    If not ChannelRegistered(Name,Index,ValueType) then
      Result := ChannelRegister(Name,Index,ValueType,SCS_TELEMETRY_CHANNEL_FLAG_none)
    else Result := True;
  end;

begin
Result := 0;
If Assigned(cbRegisterChannel) then
  For i := 0 to Pred(fInfoProvider.KnownChannels.Count) do
    begin
      KnownChannelInfo := fInfoProvider.KnownChannels[i];
      If KnownChannelInfo.Indexed then
        begin
          // Channel is indexed.
          // If channel is binded to some configuration, and this config is found,
          // then value from config is used as upper index limit, otherwise
          // MaxIndex for given channel is used (MaxChannelIndex constant is used
          // if MaxIndex for given channel is not properly set).
          If KnownChannelInfo.MaxIndex <> SCS_U32_NIL then MaxIdx := KnownChannelInfo.MaxIndex
            else MaxIdx := MaxChannelIndex;
          If ManageIndexedChannels and ValidConfigReference(KnownChannelInfo.IndexConfig) then
            begin
              j := fStoredConfigs.IndexOf(KnownChannelInfo.IndexConfig);
              If j >= 0 then
                If fStoredConfigs[j].Value.ValueType = SCS_VALUE_TYPE_u32 then
                  MaxIdx := fStoredConfigs[j].Value.BinaryData.value_u32.value - 1;
            end;
          MinIdx := 0;  
        end
      else
        begin
          MinIdx := Integer(SCS_U32_NIL);
          MaxIdx := Integer(SCS_U32_NIL);
        end;
      ChannelValueTypes := SelectSupportedValueTypes(KnownChannelInfo.PrimaryType,KnownChannelInfo.SecondaryTypes,
                                                     RegisterPrimaryType,SecondarySelectionMask);
      TypeIdx := Low(TValueTypesArray);
      while (ChannelValueTypes[TypeIdx] <> SCS_VALUE_TYPE_INVALID) and (TypeIdx <= High(TValueTypesArray)) do
        begin
          For j := MinIdx to MaxIdx do
            If CheckAndRegister(KnownChannelInfo.Name,j,ChannelValueTypes[TypeIdx]) then Inc(Result) else Break;
          Inc(TypeIdx);  
        end;
    end;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelUnregisterAll: Integer;
var
  i:  Integer;
begin
Result := 0;
If Assigned(cbUnregisterEvent) then
  For i := (fRegisteredChannels.Count - 1) downto 0 do
    with fRegisteredChannels[i] do
      If ChannelUnregister(Name,Index,ValueType) then Inc(Result);
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ConfigStored(ConfigReference: TConfigReference; Index: scs_u32_t = SCS_U32_NIL): Boolean;
begin
Result := fStoredConfigs.IndexOf(ConfigReference,Index) >= 0;
end;

//------------------------------------------------------------------------------

Function TTelemetryRecipient.ChannelStored(const Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t): Boolean;
begin
Result := fStoredChannels.IndexOf(Name,Index,ValueType) >= 0;
end;

end.
