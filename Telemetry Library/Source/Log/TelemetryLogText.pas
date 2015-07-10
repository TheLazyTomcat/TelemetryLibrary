{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Contains class designed to log telemetry API traffic to text file.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-18)
@lastmod(2015-06-30)

  @bold(@NoAutoLink(TelemetryLogText))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryLogText class (see class declaration for
  details).

  Last change: 2015-06-30

  Change List:@unorderedList(
    @item(2014-05-18 - First stable version.)
    @item(2014-11-05 - Added parameter @code(UserData) to following methods:
                       @unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryLogText.EventRegisterHandler)
                         @item(TTelemetryLogText.EventUnregisterHandler)
                         @item(TTelemetryLogText.EventHandler)
                         @item(TTelemetryLogText.ChannelRegisterHandler)
                         @item(TTelemetryLogText.ChannelUnregisterHandler)
                         @item(TTelemetryLogText.ChannelHandler)))
    @item(2015-06-30 - Small implementation changes.))

@html(<hr>)}
unit TelemetryLogText;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  SimpleLog,
  TelemetryCommon,
  TelemetryIDs,
  TelemetryRecipient,
  TelemetryRecipientBinder,
{$IFDEF Documentation}
  TelemetryStrings,
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
{                              TTelemetryLogText                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogText // Class declaration                                     }
{==============================================================================}
{:
  @abstract(Class designed to log all traffic on telemetry API to human readable
            output file.)
  To use it, just assign individual *Handler methods to appropriate telemetry
  @noAutoLink(recipient) events, or use method AssignHandlers for automatic
  assigning (Recipient property must be assigned).@br
  @bold(Note) - some functions (they are marked) are using
  @noAutoLink(recipient) methods to convert data to text, so you are strongly
  advised to assign telemetry @noAutoLink(recipient) to Recipient property or
  using constructor. If you don't assign it, then you have to make sure you pass
  valid telemetry @noAutoLink(recipient) in @code(Sender) parameters of called
  methods (in this case, do not use Log* methods as they don't have
  @code(Sender) parameter). When no valid @noAutoLink(recipient) is provided,
  then these methods write error note directly into log output.
}
type
  TTelemetryLogText = class(TTelemetryRecipientBinder)
  private
  {:
    See Logger property for details.
  }
    fLogger:          TSimpleLog;
  {:
    See ShowTypesNames property for details.
  }
    fShowTypesNames:  Boolean;
  {:
    See ShowDescriptors for details.
  }
    fShowDescriptors: Boolean;
  protected
  {:
    Returns passed ID converted to text in the form @italic("(id_string)").

    @param ID ID to be converted to text.

    @returns Textual representation of passed ID.
  }
    Function ItemIDString(ID: TItemID): String; virtual;
  {:
    Returns textual representation of passed index in the form
    @italic("[index]") when index differs from @code(SCS_U32_NIL), or an empty
    string otherwise.

    @param Index Index to be converted to text.

    @returns Textual representation of passed index.
  }
    Function IndexString(Index: scs_u32_t): String; virtual;
  public
  {:
    Class constructor.

    Creates Logger object.

    @param(Recipient Telemetry @noAutoLink(recipient) that operates on API. When
                     you assign this parameter, it will be assigned to Recipient
                     property and handlers will be assigned to its events using
                     AssignHandlers method.)
    @param(FileName  Name of the output log file. It is not mandatory to assign
                     full file path (file should be saved to current directory
                     if no path is provided), but it is a good practice.@br
                     When this parameter contains an empty string, then output
                     file is created in the same folder where a module (exe/dll)
                     containing this unit is placed, with the name set to the
                     name of that module (without extension) followed by the
                     time the file was created, and with .log extension.)
  }
    constructor Create(aRecipient: TTelemetryRecipient = nil; FileName: String = '');
  {:
    Class destructor.

    Frees Logger object.
  }
    destructor Destroy; override;
  {:
    Calls appropriate methods of Logger that actually writes log text to output.

    @param LogText      Text to be written to output(s).
  }
    procedure AddLog(LogText: String); virtual;
  {:
    Adds informations about write to a game log.@br

    @param(Sender  Object that called this method (should be of type
                   TTelemetryRecipient).)
    @param LogType Type of log written into game log.
    @param LogText Actual text written into game log.
  }
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
  {:
    Adds informations about game event registration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object that called this method (should be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param UserData User defined data stored in the event context.
  }
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Adds informations about game event unregistration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object that called this method (should be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param UserData User defined data stored in the event context.
  }
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Adds informations about game event to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object that called this method (should be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param Data     Pointer to data accompanying the event. Can be @nil.
    @param UserData User defined data stored in the event context.
  }
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; {%H-}UserData: Pointer); override;
  {:
    Adds informations about channel registration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of registered channel.
    @param ID        ID of registered channel.
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.
    @param Flags     Registration flags.
    @param UserData  User defined data stored in the channel context.
  }
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; {%H-}UserData: Pointer); override;
  {:
    Adds informations about channel unregistration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of unregistered channel.
    @param ID        ID of unregistered channel.
    @param Index     Index of unregistered channel.
    @param ValueType Value type of unregistered channel.
    @param UserData  User defined data stored in the channel context.
  }
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; {%H-}UserData: Pointer); override;
  {:
    Adds informations about channel to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of the channel.
    @param ID        ID of the channel.
    @param Index     Index of the channel.
    @param Value     Actual value of the channel. Can be @nil.
    @param UserData  User defined data stored in the channel context.
  }
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; {%H-}UserData: Pointer); override;
  {:
    Adds informations about received configuration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of the config.
    @param ID        ID of the config.
    @param Index     Index of the config.
    @param Value     Actual value of the config.
  }
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  {:
    Calls method LogHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to LogHandler for details.
  }
    procedure LogLog(LogType: scs_log_type_t; const LogText: String); virtual;
  {:
    Calls method EventRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to EventRegisterHandler for details.
  }
    procedure LogEventRegister(Event: scs_event_t); virtual;
  {:
    Calls method EventUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to EventUnregisterHandler for details.
  }
    procedure LogEventUnregister(Event: scs_event_t); virtual;
  {:
    Calls method EventHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to EventHandler for details.
  }
    procedure LogEvent(Event: scs_event_t; Data: Pointer); virtual;
  {:
    Calls method ChannelRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to ChannelRegisterHandler for details.
  }
    procedure LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
  {:
    Calls method ChannelUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to ChannelUnregisterHandler for details.
  }
    procedure LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
  {:
    Calls method ChannelHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to ChannelHandler for details.
  }
    procedure LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
  {:
    Calls method ConfigHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to ConfigHandler for details.
  }
    procedure LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  published
  {:
    Reference to internally used object that actually performs all writes to
    outputs.
  }
    property Logger: TSimpleLog read fLogger;
  {:
    Determines whether type identifiers should be added into output when
    converting values to text.@br
    Initialized to @false.
  }
    property ShowTypesNames: Boolean read fShowTypesNames write fShowTypesNames;
  {:
    Determines whether field descriptors should be added into output when
    converting composite values to text.@br
    Initialized to @false.
  }
    property ShowDescriptors: Boolean read fShowDescriptors write fShowDescriptors;
  end;

implementation

uses
  SysUtils,
  TelemetryStrings;

{==============================================================================}
{   TTelemetryLogText // Class implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogText // Constants, types, variables, etc...                   }
{------------------------------------------------------------------------------}

const
  // Default (initial) values for selected logger properties.
  def_InMemoryLog     = False;
  def_StreamToFile    = True;
  def_StreamAppend    = False;
  def_IndentNewLines  = True;

  // Default (initial) values for TTelemetryLogText properties.
  def_ShowTypesNames  = False;
  def_ShowDescriptors = False;

  // Predefined strings used in log.
  ls_Event_Reg     = 'Event registered: ';
  ls_Event_Unreg   = 'Event unregistered: ';
  ls_Event         = 'Event: ';
  ls_Channel_Reg   = 'Channel registered: ';
  ls_Channel_Unreg = 'Channel unregistered: ';
  ls_Channel       = 'Channel: ';
  ls_Config        = 'Config: ';
  ls_NoRecipient   = 'Error, no telemetry recipient provided.';

{------------------------------------------------------------------------------}
{   TTelemetryLogText // Protected methods                                     }
{------------------------------------------------------------------------------}

Function TTelemetryLogText.ItemIDString(ID: TItemID): String;
begin
Result := '(' + ItemIDtoStr(ID) + ')';
end;

//------------------------------------------------------------------------------

Function TTelemetryLogText.IndexString(Index: scs_u32_t): String;
begin
If Index <> SCS_U32_NIL then Result := '[' + IntToStr(Index) + ']'
 else Result := '';
end;


{------------------------------------------------------------------------------}
{   TTelemetryLogText // Public methods                                        }
{------------------------------------------------------------------------------}

constructor TTelemetryLogText.Create(aRecipient: TTelemetryRecipient = nil; FileName: String = '');
begin
inherited Create(aRecipient);
fLogger := TSimpleLog.Create;
If FileName <> '' then
  fLogger.StreamFileName := FileName
else
  fLogger.StreamFileName := ExtractFilePath(GetModuleName(hInstance)) +
                            ChangeFileExt(ExtractFileName(GetModuleName(hInstance)),'') +
                            '_' + FormatDateTime('yyyy-mm-dd-hh-nn-ss',Now) + '.log';
fLogger.InternalLog := def_InMemoryLog;
fLogger.StreamToFile := def_StreamToFile;
fLogger.StreamAppend := def_StreamAppend;
fLogger.IndentNewLines := def_IndentNewLines;
fShowTypesNames := def_ShowTypesNames;
fShowDescriptors := def_ShowDescriptors;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogText.Destroy;
begin
fLogger.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.AddLog(LogText: String);
begin
If Assigned(fLogger) then fLogger.AddLog(LogText);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
var
  TempString: String;
begin
case LogType of
  SCS_LOG_TYPE_message: TempString := '';
  SCS_LOG_TYPE_warning: TempString := '<warning>';
  SCS_LOG_TYPE_error:   TempString := '<error>';
else
  TempString := '';
end;
AddLog(TempString + LogText);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Event_Reg + '(' + IntToStr(Event) + ')' + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)))
else
  AddLog('TTelemetryLogText.EventRegisterHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Event_Unreg + '(' + IntToStr(Event) + ')' + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)))
else
  AddLog('TTelemetryLogText.EventUnregisterHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
var
  WorkRecipient:  TTelemetryRecipient;
  TempStr:        String;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  begin
    TempStr := EventDataToStr(Event,Data,ShowTypesNames,ShowDescriptors);
    If TempStr <> '' then
      AddLog(ls_Event + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)) + sLineBreak + TempStr)
    else
      AddLog(ls_Event + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)));
  end
else
  AddLog('TTelemetryLogText.EventHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
begin
AddLog(ls_Channel_Reg + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ', ' + SCSValueTypeToStr(ValueType) + ', 0x' + IntToHex(Flags,8))
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
begin
AddLog(ls_Channel_Unreg + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ', ' + SCSValueTypeToStr(ValueType))
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Channel + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ': ' +  ChannelValueToStr(Value,ShowTypesNames,ShowDescriptors))
else
  AddLog('TTelemetryLogText.ChannelHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
AddLog(ls_Config + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ': ' + SCSValueLocalizedToStr(Value,ShowTypesNames,ShowDescriptors))
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogLog(LogType: scs_log_type_t; const LogText: String);
begin
LogHandler(nil,LogType,LogText);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogEventRegister(Event: scs_event_t);
begin
EventRegisterHandler(nil,Event,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogEventUnregister(Event: scs_event_t);
begin
EventUnregisterHandler(nil,Event,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogEvent(Event: scs_event_t; Data: Pointer);
begin
EventHandler(nil,Event,Data,nil);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
ChannelRegisterHandler(nil,Name,ID,Index,ValueType,Flags,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
ChannelUnregisterHandler(nil,Name,ID,Index,ValueType,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
ChannelHandler(nil,Name,ID,Index,Value,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
ConfigHandler(nil,Name,ID,Index,Value);
end;

end.
