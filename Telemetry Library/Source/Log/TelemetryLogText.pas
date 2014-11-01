{@html(<hr>)
@abstract(Contains class designed to log telemetry API traffic to text file.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-18)
@lastmod(2014-05-18)

  @bold(@NoAutoLink(TelemetryLogText))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryLogText class (see class declaration for
  details).

  Last change:  2014-05-18

  Change List:@unorderedList(
    @item(2014-05-18 - First stable version.))

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
{    TTelemetryLogText // Class declaration                                    }
{==============================================================================}
{
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

  @member(fLogger See Logger for details.)

  @member(fShowTypesNames See ShowTypesNames for details.)

  @member(fShowDescriptors See ShowDescriptors for details.)

  

  @member(AddLog
    Calls appropriate methods of Logger that actually writes log text to output.

    @param LogText      Text to be written to output(s).)

  @member(ItemIDString
    Returns passed ID converted to text in the form @italic("(id_string)").

    @param ID ID to be converted to text.

    @returns Textual representation of passed ID.)

  @member(IndexString
    Returns textual representation of passed index in the form
    @italic("[index]") when index differs from @code(SCS_U32_NIL), or an empty
    string otherwise.

    @param Index Index to be converted to text.

    @returns Textual representation of passed index.)



  @member(Create
    Class constructor.

    Creates Logger object.

    @param(Recipient Telemetry @noAutoLink(recipient) that operates on API. When
                     you assign this parameter, it will be assigned to Recipient
                     property and handlers will be assigned to its events using
                     AssignHandlers method.)
    @param(FileName  Name of the output log file. It is not mandatory to assign
                     full file path (file should be saved to current directory),
                     but it is a good practice.@br
                     When this parameter contains an empty string, then output
                     file is created in the same folder where a module (exe/dll)
                     containing this unit is placed, with the name set to the
                     name of that module (without extension) followed by the
                     time the file was created, and with .log extension.))

  @member(Destroy
    Class destructor.

    Frees Logger object.)

  @member(LogHandler
    Method adding informations from a write to game log.@br

    @param(Sender  Object that called this method (should be of type
                   TTelemetryRecipient).)
    @param LogType Type of log written into game log.
    @param LogText Actual text written into game log.)

  @member(EventRegisterHandler
    Method adding informations about game event registration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender Object that called this method (should be of type
                  TTelemetryRecipient).)
    @param Event  Game event identification number.)

  @member(EventUnregisterHandler
    Method adding informations about game event unregistration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender Object that called this method (should be of type
                  TTelemetryRecipient).)
    @param Event  Game event identification number.)

  @member(EventHandler
    Method adding information about game event to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender Object that called this method (should be of type
                  TTelemetryRecipient).)
    @param Event  Game event identification number.
    @param Data   Pointer to data accompanying the event. Can be @nil.)

  @member(ChannelRegisterHandler
    Method adding informations about channel registration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of registered channel.
    @param ID        ID of registered channel.
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.
    @param Flags     Registration flags.)

  @member(ChannelUnregisterHandler
    Method adding informations about channel unregistration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of unregistered channel.
    @param ID        ID of unregistered channel.
    @param Index     Index of unregistered channel.
    @param ValueType Value type of unregistered channel.)

  @member(ChannelHandler
    Method adding informations about channel to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of the channel.
    @param ID        ID of the channel.
    @param Index     Index of the channel.
    @param Value     Actual value of the channel. Can be @nil.)

  @member(ConfigHandler
    Method adding informations about received configuration to the log.

    @param(Sender    Object that called this method (should be of type
                     TTelemetryRecipient).)
    @param Name      Name of the config.
    @param ID        ID of the config.
    @param Index     Index of the config.
    @param Value     Actual value of the config.)

  @member(LogLog
    Calls method LogHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to it for details.)

  @member(LogEventRegister
    Calls method EventRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to it for details.)

  @member(LogEventUnregister
    Calls method EventUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to it for details.)

  @member(LogEvent
    Calls method EventHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to it for details.)

  @member(LogChannelRegister
    Calls method ChannelRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to it for details.)

  @member(LogChannelUnregister
    Calls method ChannelUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to it for details.)

  @member(LogChannel
    Calls method ChannelHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to it for details.)

  @member(LogConfig
    Calls method ConfigHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to it for details.)



  @member(Logger
    Reference to internally used object that actually performs all writes to
    outputs.)

  @member(ShowTypesNames
    Determines whether type identifiers should be added into output when
    converting values to text.@br
    Initialized to @false.)

  @member(ShowDescriptors
    Determines whether field descriptors should be added into output when
    converting composite values to text.@br
    Initialized to @false.)
}
type
  TTelemetryLogText = class(TTelemetryRecipientBinder)
  private
    fLogger:          TSimpleLog;
    fShowTypesNames:  Boolean;
    fShowDescriptors: Boolean;
  protected
    Function ItemIDString(ID: TItemID): String; virtual;
    Function IndexString(Index: scs_u32_t): String; virtual;
  public
    constructor Create(Recipient: TTelemetryRecipient = nil; FileName: String = '');
    destructor Destroy; override;
    procedure AddLog(LogText: String); virtual;
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t); override;
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t); override;
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer); override;
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); override;
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); override;
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); override;
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
    procedure LogLog(LogType: scs_log_type_t; const LogText: String); virtual;
    procedure LogEventRegister(Event: scs_event_t); virtual;
    procedure LogEventUnregister(Event: scs_event_t); virtual;
    procedure LogEvent(Event: scs_event_t; Data: Pointer); virtual;
    procedure LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
    procedure LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
    procedure LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
    procedure LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  published
    property Logger: TSimpleLog read fLogger;
    property ShowTypesNames: Boolean read fShowTypesNames write fShowTypesNames;
    property ShowDescriptors: Boolean read fShowDescriptors write fShowDescriptors;
  end;

implementation

uses
  SysUtils,
  TelemetryStrings;

{==============================================================================}
{    TTelemetryLogText // Class implementation                                 }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TTelemetryLogText // Constants, types, variables, etc...                  }
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
{    TTelemetryLogText // Protected methods                                    }
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
{    TTelemetryLogText // Public methods                                       }
{------------------------------------------------------------------------------}

constructor TTelemetryLogText.Create(Recipient: TTelemetryRecipient = nil; FileName: String = '');
begin
inherited Create(Recipient);
fLogger := TSimpleLog.Create;
If FileName <> '' then
  fLogger.StreamFileName := FileName
else
  fLogger.StreamFileName := ExtractFilePath(GetModuleName(hInstance)) +
                            ChangeFileExt(ExtractFileName(GetModuleName(hInstance)),'') +
                            '_' + FormatDateTime('yyyy-mm-dd-hh-nn-ss',Now) + '.log';
fLogger.InMemoryLog := def_InMemoryLog;
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
  TempString: TelemetryString;
begin
case LogType of
  SCS_LOG_TYPE_message: TempString := '';
  SCS_LOG_TYPE_warning: TempString := '<warning>';
  SCS_LOG_TYPE_error:   TempString := '<error>';
else
  TempString := '';
end;
AddLog(TempString + TelemetryStringEncode(LogText));
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventRegisterHandler(Sender: TObject; Event: scs_event_t);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Event_Reg + '(' + IntToStr(Event) + ')' + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)))
else
  AddLog('TTelemetryLogText.EventRegisterHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventUnregisterHandler(Sender: TObject; Event: scs_event_t);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Event_Unreg + '(' + IntToStr(Event) + ')' + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)))
else
  AddLog('TTelemetryLogText.EventUnregisterHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer);
var
  WorkRecipient:  TTelemetryRecipient;
  TempStr:        String;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  begin
    TempStr := WorkRecipient.EventGetDataAsString(Event,Data,ShowTypesNames,ShowDescriptors);
    If TempStr <> '' then
      AddLog(ls_Event + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)) + sLineBreak + TempStr)
    else
      AddLog(ls_Event + TelemetryStringDecode(WorkRecipient.TelemetryInfoProvider.EventGetName(Event)));
  end
else
  AddLog('TTelemetryLogText.EventHandler: ' + ls_NoRecipient);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
AddLog(ls_Channel_Reg + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ', ' + SCSValueTypeToStr(ValueType) + ', 0x' + IntToHex(Flags,8))
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
AddLog(ls_Channel_Unreg + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ', ' + SCSValueTypeToStr(ValueType))
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
var
  WorkRecipient:  TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  AddLog(ls_Channel + ItemIDString(ID) + TelemetryStringDecode(Name) + IndexString(Index) + ': ' +  WorkRecipient.ChannelGetValueAsString(Value,ShowTypesNames,ShowDescriptors))
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
EventRegisterHandler(nil,Event);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogEventUnregister(Event: scs_event_t);
begin
EventUnregisterHandler(nil,Event);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogEvent(Event: scs_event_t; Data: Pointer);
begin
EventHandler(nil,Event,Data);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
ChannelRegisterHandler(nil,Name,ID,Index,ValueType,Flags);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
ChannelUnregisterHandler(nil,Name,ID,Index,ValueType);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
ChannelHandler(nil,Name,ID,Index,Value);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogText.LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
ConfigHandler(nil,Name,ID,Index,Value);
end;

end.
