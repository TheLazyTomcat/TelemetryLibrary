{@html(<hr>)
@abstract(Contains classes designed to parse binary logs.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-10)
@lastmod(2014-11-05)

  @bold(@NoAutoLink(TelemetryLogBinaryParser))

  ©František Milt, all rights reserved.

  This unit contains classes that are designed to parse binary logs and/or
  convert them to text logs, namely:
@preformatted(
  TTelemetryLogBinaryReader
   |- TTelemetryLogBinaryReader_0_1

  TTelemetryLogBinaryStreamParser
   |- TTelemetryLogBinaryFileParser
       |- TTelemetryLogBinaryToTextConverter
)
  Last change:  2014-11-05

  Change List:@unorderedList(
    @item(2014-05-10 - First stable version.)
    @item(2014-10-24 - Small implementation changes.)
    @item(2014-11-05 - Type of parameter/field @code(Size) changed from signed
                       to unsigned integer in following cases:@unorderedList(
                        @itemSpacing(Compact)
                        @item(event type TDataLogEvent)
                        @item(event type TDataTimeLogEvent)
                        @item(fields TLogEntry.Size and TLogEntry.Info)
                        @item(TTelemetryLogBinaryStreamParser.DoOnDataLog)
                        @item(TTelemetryLogBinaryToTextConverter.DoOnDataLog)))
    @item(2014-11-05 - Small implementation changes.))

@html(<hr>)}
unit TelemetryLogBinaryParser;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  Classes,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}
  TelemetryCommon,
  TelemetryIDs,
  TelemetryInfoProvider,
  TelemetryRecipient,
  TelemetryLogText,
  TelemetryLogBinary,
{$IFDEF Documentation}
  TelemetryConversions,
  TelemetryStreaming,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

type
  // Event type used when reader/parser passes unprocessed log data.
  TDataLogEvent = procedure(Sender: TObject; Data: Pointer; Size: LongWord) of object;
  // Event type used when reader/parser passes text log data or any interpreted data.
  TTextLogEvent = procedure(Sender: TObject; const Text: String) of object;

  // Event type used when reader/parser passes unprocessed log data with time.
  TDataTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Data: Pointer; Size: LongWord) of object;
  // Event type used when reader/parser passes text log data or any interpreted data with time.
  TTextTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Text: String) of object;
  // Event type used when reader/parser passes informations about a write to game log with time.
  TLogTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String) of object;
  // Event type used when reader/parser passes log data containing informations about game event (un)registration with time.
  TEventRegisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Event: scs_event_t) of object;
  // Event type used when reader/parser passes log data containing informations about game event with time.
  TEventTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer) of object;
  // Event type used when reader/parser passes log data containing informations about channel registration with time.
  TChannelRegisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t) of Object;
  // Event type used when reader/parser passes log data containing informations about channel unregistration with time.
  TChannelUnregisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t) of Object;
  // Event type used when reader/parser passes log data containing informations about channel value with time.
  TChannelTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t) of Object;
  // Event type used when reader/parser passes log data containing informations about config value with time.
  TConfigTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t) of object;


{$IFDEF MulticastEvents}
  {$DEFINE DeclarationPart}
    {$INCLUDE '.\Inc\TelemetryLogBinaryParser_MulticastEvents.pas'}
  {$UNDEF DeclarationPart}
{$ENDIF}

{
  Structure returned by parser when you access specific log in the stream by its
  index.
  
  @member Time Full @noAutoLink(time) when the log was written.
  @member Data Unprocessed @noAutoLink(data) the accessed log contains.
  @member Size Size of @noAutoLink(data).
  @member(Info Other informations about the log. Content of this field differs
               for each file structure. Currently, these are implemented:
               @unorderedList(
                 @item(File structure 0 and 1:
@preformatted(
    bits  0..7   - Type of block from which the data was read.
    bits  8..15  - Flags of block from which the data was read.
    bits 16..31  - Unused.))))
}
  TLogEntry = record
    Time: TDateTime;
    Data: Pointer;
    Size: LongWord;
    Info: LongWord;
  end;

{
  Use this procedure to free resources taken by variable of type TLogEntry.

  @param Value Variable whose resources should be freed.
}
  procedure LogEntryFree(var Value: TLogEntry);

const
  // Contains empty/invalid value of type TLogEntry.
  cInvalidLogEntry: TLogEntry = (Time: 0; Data: nil; Size: 0; Info: 0);

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryReader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader // Class declaration                             }
{==============================================================================}
{
  @abstract(Base class for all internal readers used to write binary logs.)
  This class is intended as common ancestor for all internal reader classes.
  These classes are used internaly to read different binary log structures, and
  should not be used outside of parsers. All descendants must implement all
  abstract methods. They also may add some new methods, but they should be used
  in parsers wery carefully and only if really needed.

  @member(fStream See Stream property for details.)

  @member(fFileInfo See FileInfo property for details.)

  @member(fLogCounter See LogCounter property for details.)

  @member(fTelemetryInfoProvider See TelemetryInfoProvider property for
    details.)

  @member(fFirstStreamPosition See FirstStreamPosition property for details.)

  @member(fReadingTerminated See ReadingTerminated property for details.)

  @member(fOnDataLog Stores reference to OnDataLog event handler.)

  @member(fOnTextLog Stores reference to OnTextLog event handler.)

  @member(fOnLogLog Stores reference to OnLogLog event handler.)

  @member(fOnEventRegister Stores reference to OnEventRegister event handler.)

  @member(fOnEventUnregister Stores reference to OnEventUnregister event
    handler.)

  @member(fOnEvent Stores reference to OnEvent event handler.)

  @member(fOnChannelRegister Stores reference to OnChannelRegister event
    handler.)

  @member(fOnChannelUnregister Stores reference to OnChannelUnregister event
    handler.)

  @member(fOnChannel Stores reference to OnChannel event handler.)

  @member(fOnConfig Stores reference to OnConfig event handler.)



  @member(GetLogCount
    Getter for LogCount property.)

  @member(GetLogEntry
    Getter for LogEntries array property.)

  @member(IncLogCounter
    Increases LogCounter property by a number passed in @code(N) parameter.

    @param N Number by which the LogCounter will be increased.)



  @member(Create
    Class constructor.

    Creates internal telemetry info provider.

    @param(Stream   @noAutoLink(Stream) from which the reader will read. Must
                    not be @nil. Position must be set directly behind the file
                    header and API info section, not at the beginning of the
                    @noAutoLink(Stream) itself.)
    @param(FileInfo Informations about input file. It must be completely filled
                    by parser that is creating current instance in order for
                    this class to work properly. Provided information are used
                    as parameters when creating telemetry info provider. When
                    not filled, the behavior of this class is not defined.))

  @member(Destroy
    Class destructor.

    Frees internal telemetry info provider.)

  @member(StartReading
    Method that must be called before any log is read. Its job is to prepare
    anything that is necesary for actual reading (variables initialization,
    ...).@br
    Sets ReadingTerminated to @false and Stream position to
    FirstStreamPosition.@br
    Can be used to effectively reset the reading.)

  @member(EndReading
    Method that must be called before destruction of instance. Its job is to
    free any allocated resources.)

  @member(ReadNextLogEntry
    Reads next log entry from current position in Stream. When parameter
    @code(IncreaseLogCounter) is set to @true and result is also @true, then
    LogCounter is increased by one.@br
    Read data are processed and passed to appropriate event.

    @param IncreaseLogCounter Determines whether to increase log counter.

    @returns @True when log was read successfuly, @false otherwise.)

  @member(ReadAllLogEntries
    Reads all log entries from current position in Stream.@br
    Internally calls ReadNextLogEntry so long until it returns @false.)

  @member(ReadLogEntry
    Reads log entry selected by passed index. Works only for structures that
    supports non-sequential reading (eg. structure 0).@br
    Internally calls ReadNextLogEntry.@br
    Position in Stream is not changed in this method.

    @param Index Index of requested log entry.

    @returns @True when the entry was read successfuly, @false otherwise.)

  @member(LogEntries
    Array property mapped to log entries - that is, each item in this array
    corresponds to one log entry.@br
    When you try to access item that is out of allowed boundary or the reading
    fails, then it returns cInvalidLogEntry.@br
    Works only for structures that supports non-sequential reading.@br
    @bold(Warning) - you have to manually free returned pointer (you can use
                     procedure LogEntryFree for that purpose).)



  @member(OnDataLog
    Event called when reader reads log with undefined internal data structure.)

  @member(OnTextLog
    Event called when reader reads text log or log that can be interpreted as a
    text.)

  @member(OnLogLog
    Event called when reader reads log containing informations about a write to
    game log.)

  @member(OnEventRegister
    Event called when reader reads log containing informations about game event
    registration.)

  @member(OnEventUnregister
    Event called when reader reads log containing informations about game event
    unregistration.)

  @member(OnEvent
    Event called when reader reads log containing informations about game
    event.)

  @member(OnChannelRegister
    Event called when reader reads log containing informations about channel
    registration.)

  @member(OnChannelUnregister
    Event called when reader reads log containing informations about channel
    unregistration.)

  @member(OnChannel
    Event called when reader reads log containing informations about channel.)

  @member(OnConfig
    Event called when reader reads log containing informations about config.)

  @member(LogCount
    Contains number of log entries in input. Works only for structures that
    supports non-sequential reading.)

  @member(LogCounter
    Contains number of logs read so far.)

  @member(Stream
    Contains reference to a @noAutoLink(stream) that is used to read the log.
    Not initialized, filled in constructor.@br
    @bold(Warning) - do not change properties of this object!)

  @member(FileInfo
    Information about file that is being read. Filled in constructor.)

  @member(TelemetryInfoProvider
    Telemetry info provider that is used internally when reading some values
    (eg. for ID <> Name conversions). Managed automatically.)

  @member(FirstStreamPosition
    Contains position property of Stream when it was passed to constructor.)

  @member(ReadingTerminated
    Determines whether sequential reading can or cannot continue (eg. end of
    @noAutoLink(stream)/file, termination block was read, an error occured,
    etc.).)
}
type
  TTelemetryLogBinaryReader = class(TObject)
  private
    fStream:                    TStream;
    fFileInfo:                  TTelemetryLogBinaryFileInfo;
    fLogCounter:                Integer;
    fTelemetryInfoProvider:     TTelemetryInfoProvider;
    fFirstStreamPosition:       Int64;
    fReadingTerminated:         Boolean;
    fOnDataLog:                 TDataTimeLogEvent;
    fOnTextLog:                 TTextTimeLogEvent;
    fOnLogLog:                  TLogTimeLogEvent;
    fOnEventRegister:           TEventRegisterTimeLogEvent;
    fOnEventUnregister:         TEventRegisterTimeLogEvent;
    fOnEvent:                   TEventTimeLogEvent;
    fOnChannelRegister:         TChannelRegisterTimeLogEvent;
    fOnChannelUnregister:       TChannelUnregisterTimeLogEvent;
    fOnChannel:                 TChannelTimeLogEvent;
    fOnConfig:                  TConfigTimeLogEvent;
  protected
    procedure IncLogCounter(N: Integer = 1); virtual;
    Function GetLogCount: Integer; virtual; abstract;
    Function GetLogEntry(Index: Integer): TLogEntry; virtual; abstract;
  public
    constructor Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
    destructor Destroy; override;
    procedure StartReading; virtual;
    procedure EndReading; virtual;
    Function ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean; virtual; abstract;
    procedure ReadAllLogEntries; virtual;
    Function ReadLogEntry(Index: Integer): Boolean; virtual; abstract;
    property LogEntries[Index: Integer]: TLogEntry read GetLogEntry; default;
  published
    property OnDataLog: TDataTimeLogEvent read fOnDataLog write fOnDataLog;
    property OnTextLog: TTextTimeLogEvent read fOnTextLog write fOnTextLog;
    property OnLogLog: TLogTimeLogEvent read fOnLogLog write fOnLogLog;
    property OnEventRegister: TEventRegisterTimeLogEvent read fOnEventRegister write fOnEventRegister;
    property OnEventUnregister: TEventRegisterTimeLogEvent read fOnEventUnregister write fOnEventUnregister;
    property OnEvent: TEventTimeLogEvent read fOnEvent write fOnEvent;
    property OnChannelRegister: TChannelRegisterTimeLogEvent read fOnChannelRegister write fOnChannelRegister;
    property OnChannelUnregister: TChannelUnregisterTimeLogEvent read fOnChannelUnregister write fOnChannelUnregister;
    property OnChannel: TChannelTimeLogEvent read fOnChannel write fOnChannel;
    property OnConfig: TConfigTimeLogEvent read fOnConfig write fOnConfig;
    property LogCount: Integer read GetLogCount;
    property LogCounter: Integer read fLogCounter;
    property Stream: TStream read fStream;
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
    property TelemetryInfoProvider: TTelemetryInfoProvider read fTelemetryInfoProvider;
    property FirstStreamPosition: Int64 read fFirstStreamPosition;
    property ReadingTerminated: Boolean read fReadingTerminated write fReadingTerminated;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryLogBinaryReader_0_1                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_0_1 // Class declaration                         }
{==============================================================================}
{
  @abstract(Internal reader class used to read file of structure 0 and 1.)
  @bold(Note) - when reading payload af a block, the position in Stream advances
  exactly by a value stored in a block header as payload size, no more, no less.

  @member(fBlocksOffsets
    Array that is loaded with blocks offsets. Used only for structure 0.)

  @member(GetLogCount
    Getter for LogCount property. Returns length of fBlocksOffsets array.)

  @member(GetLogEntry
    Getter for LogEntries array property. It find and accesses individual
    entries by offsets stored in fBlocksOffsets array. See LogEntries property
    for other informations.)

  @member(GetFlagValue
    Returns value of flag selected by flag mask and stored in @code(Flags).

    @param Flags    Set of flags storing required flag value.
    @param FlagMask Number determining at which bit should the value be read.

    @returns Value of the selected flag.)

  @member(ReadBlocksOffsets
    Reads table of block offsets when reading structure 0. Stream position is
    not changed in this method.)

  @member(ReadBlockHeader
    Reads block header at current Stream position.)

  @member(ReadBlockPayload_Invalid
    Reads payload of invalid block.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Generic
    Reads payload of generic block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Text
    Reads payload of text block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Log
    Reads payload of log block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_EventReg
    Reads payload of game event registration block and passes it to appropriate
    event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_EventUnreg
    Reads payload of game event unregistration block and passes it to
    appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Event
    Reads payload of game event block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_ChannelReg
    Reads payload of channel registration block and passes it to appropriate
    event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_ChannelUnreg
    Reads payload of channel unregistration block and passes it to appropriate
    event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Channel
    Reads payload of channel block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Config
    Reads payload of config block and passes it to appropriate event.

    @returns(@True when the payload was read successfuly and when there can be
             another block after the actually read, @false othewise.))

  @member(ReadBlockPayload_Termination
    Reads payload of termination block. Allways returns @false as reading should
    terminate at this block.)

  @member(ReadBlock
    Reads block at current Stream position.

    @returns(@True when the block was read successfuly and when there can be
             another block after the actually read, @false othewise.))


  @member(StartReading
    See @inherited. Calls ReadBlocksOffsets when readed file is of structure 0.)

  @member(EndReading
    See @inherited. Empties the table of block offsets.)

  @member(ReadNextLogEntry
    See @inherited. Internally calls ReadBlock method.)

  @member(ReadLogEntry
    See @inherited.)
}
  TTelemetryLogBinaryReader_0_1 = class(TTelemetryLogBinaryReader)
  private
    fBlocksOffsets: Array of LongWord;
  protected
    Function GetLogCount: Integer; override;
    Function GetLogEntry(Index: Integer): TLogEntry; override;
    Function GetFlagValue(Flags, FlagMask: Byte): Boolean; virtual;
    procedure ReadBlocksOffsets; virtual;
    Function ReadBlockHeader(var BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Invalid(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Generic(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Text(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Log(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_EventReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_EventUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Event(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_ChannelReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_ChannelUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Channel(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Config(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlockPayload_Termination(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
    Function ReadBlock: Boolean; virtual;
  public
    procedure StartReading; override;
    procedure EndReading; override;
    Function ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean; override;
    Function ReadLogEntry(Index: Integer): Boolean; override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                       TTelemetryLogBinaryStreamParser                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryStreamParser // Class declaration                       }
{==============================================================================}
{
  @abstract(Class designed to parse a @noAutoLink(stream) containing binary
            log.)
  It is designed as a @noAutoLink(stream) parsers, meaning it reads over the
  input @noAutoLink(stream) and when it runs into a valid log, it reads it,
  processes it when possible and calls appropriate event to which it passes all
  processed data. For example, when it finds entry containing information about
  channel registration, it reads it and calls event OnChannelRegisterLog (or
  similar alternative). These events have the same signature as events used by
  telemetry recipient, so you can bind the same handlers to them.@br
  Parsers internally uses appropriate readers to read files with differnt
  structures.

  @member(fStream See Stream property for details.)

  @member(fLogReader
    Internal reader used to read files with different structures. Managed
    automatically. Crated in constructor as an instance of one of the
    TTelemetryLogBinaryReader descendants. Actual class used depends on
    structure of readed file.)

  @member(fFileInfo See FileInfo property for details.)

  @member(fOnDataTimeLog Stores reference to OnDataTimeLog event handler.)

  @member(fOnTextTimeLog Stores reference to OnTextTimeLog event handler.)

  @member(fOnLogTimeLog Stores reference to OnLogTimeLog event handler.)

  @member(fOnEventRegisterTimeLog Stores reference to OnEventRegisterTimeLog
    event handler.)

  @member(fOnEventUnregisterTimeLog Stores reference to OnEventUnregisterTimeLog
    event handler.)

  @member(fOnEventTimeLog Stores reference to OnEventTimeLog event handler.)

  @member(fOnChannelRegisterTimeLog Stores reference to OnChannelRegisterTimeLog
    event handler.)

  @member(fOnChannelUnregisterTimeLog Stores reference to
    OnChannelUnregisterTimeLog event handler.)

  @member(fOnChannelTimeLog Stores reference to OnChannelTimeLog event handler.)

  @member(fOnConfigTimeLog Stores reference to OnConfigTimeLog event handler.)

  @member(fOnDataLog Stores reference to OnDataLog event handler.)

  @member(fOnTextLog Stores reference to OnTextLog event handler.)

  @member(fOnLogLog Stores reference to OnLogLog event handler.)

  @member(fOnEventRegisterLog Stores reference to OnEventRegisterLog event
    handler.)

  @member(fOnEventUnregisterLog Stores reference to OnEventUnregisterLog event
    handler.)

  @member(fOnEventLog Stores reference to OnEventLog event handler.)

  @member(fOnChannelRegisterLog Stores reference to OnChannelRegisterLog event
    handler.)

  @member(fOnChannelUnregisterLog Stores reference to OnChannelUnregisterLog
    event handler.)

  @member(fOnChannelLog Stores reference to OnChannelLog event handler.)

  @member(fOnConfigLog Stores reference to OnConfigLog event handler.)

  @member(fOnDataTimeLogMulti Object managing multicast OnDataTimeLogMulti
    event.)

  @member(fOnTextTimeLogMulti Object managing multicast OnTextTimeLogMulti
    event.)

  @member(fOnLogTimeLogMulti Object managing multicast OnLogTimeLogMulti
    event.)

  @member(fOnEventRegisterTimeLogMulti Object managing multicast
    OnEventRegisterTimeLogMulti event.)

  @member(fOnEventUnregisterTimeLogMulti Object managing multicast
    OnEventUnregisterTimeLogMulti event.)

  @member(fOnEventTimeLogMulti Object managing multicast OnEventTimeLogMulti
    event.)

  @member(fOnChannelRegisterTimeLogMulti Object managing multicast
    OnChannelRegisterTimeLogMulti event.)

  @member(fOnChannelUnregisterTimeLogMulti Object managing multicast
    OnChannelUnregisterTimeLogMulti event.)

  @member(fOnChannelTimeLogMulti Object managing multicast OnChannelTimeLogMulti
    event.)

  @member(fOnConfigTimeLogMulti Object managing multicast OnConfigTimeLogMulti
    event.)

  @member(fOnDataLogMulti Object managing multicast OnDataLogMulti event.)

  @member(fOnTextLogMulti Object managing multicast OnTextLogMulti event.)

  @member(fOnLogLogMulti Object managing multicast OnLogLogMulti event.)  

  @member(fOnEventRegisterLogMulti Object managing multicast
    OnEventRegisterLogMulti event.)

  @member(fOnEventUnregisterLogMulti Object managing multicast
    OnEventUnregisterLogMulti event.)

  @member(fOnEventLogMulti Object managing multicast OnEventLogMulti event.)

  @member(fOnChannelRegisterLogMulti Object managing multicast
    OnChannelRegisterLogMulti event.)

  @member(fOnChannelUnregisterLogMulti Object managing multicast
    OnChannelUnregisterLogMulti event.)

  @member(fOnChannelLogMulti Object managing multicast OnChannelLogMulti event.)

  @member(fOnConfigLogMulti Object managing multicast OnConfigLogMulti event.)

  @member(GetLogCount
    Getter for LogCount property. Gets the value from internal reader object.)

  @member(GetLogCounter
    Getter for LogCounter property. Gets the value from internal reader object.)

  @member(GetLogEntry
    Getter for LogEntries array property. Gets actual value from internal reader
    object.)


  @member(ReadFileHeader
    Read file hader and API informations from current Stream position and
    returns them in @code(@noAutoLink(FileInfo)) parameter.

    @param FileInfo Output value containing read data.)

  @member(DoOnDataLog
    Method managing OnData* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnDataLog OnDataLog) event.)

  @member(DoOnTextLog
    Method managing OnText* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnTextLog OnTextLog) event.)

  @member(DoOnLog
    Method managing OnLog* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnLogLog OnLogLog) event.)

  @member(DoOnEventRegister
    Method managing OnEventRegister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEventRegister OnEventRegister) event.)

  @member(DoOnEventUnregister
    Method managing OnEventUnregister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEventUnregister OnEventUnregisterg)
    event.)

  @member(DoOnEvent
    Method managing OnEvent* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEvent OnEvent) event.)

  @member(DoOnChannelRegister
    Method managing OnChannelRegister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannelRegister OnChannelRegister) event.)

  @member(DoOnChannelUnregister
    Method managing OnChannelUnregister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannelUnregister OnChannelUnregister)
    event.)

  @member(DoOnChannel
    Method managing OnChannel* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannel OnChannel) event.)

  @member(DoOnConfig
    Method managing OnConfig* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnConfig OnConfig) event.)


  @member(Create
    Class constructor.

    Creates all automatically managed objects, reads file header (by calling
    method ReadFileHeader) and assigns handlers to internal reader events.

    @param(Stream @noAutoLink(Stream) from which the parser will read. Must not
                  be @nil.))

  @member(Destroy
    Class destructor.

    Frees all automatically managed objects.)

  @member(ReadNextLogEntry
    Reads next log entry from current position in Stream. Read data are
    processed and passed to appropriate event.@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadNextLogEntry
    ReadNextLogEntry) method.

    @returns @True when log was read successfuly, @false otherwise.)


  @member(ReadAllLogEntries
    Reads all log entries from current position in Stream.@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadAllLogEntries
    ReadAllLogEntries) method.)

  @member(ReadLogEntry
    Reads log entry selected by passed index. Works only for structures that
    supports non-sequential reading (eg. structure 0).@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadLogEntry
    ReadLogEntry) method.@br
    Position in Stream is not changed in this method.

    @param Index Index of requested log entry.

    @returns @True when the entry was read successfuly, @false otherwise.)

  @member(LogEntries
    Array property mapped to log entries - that is, each item in this array
    corresponds to one log entry.@br
    When you try to access item that is out of allowed boundary or the reading
    fails, then it returns cInvalidLogEntry.@br
    Works only for structures that supports non-sequential reading.@br
    @bold(Warning) - you have to manually free returned pointer (you can use
                     procedure LogEntryFree for that purpose).)



  @member(Stream
    Contains reference to a @noAutoLink(stream) that is used to read the log.
    Not initialized, filled in constructor.@br
    @bold(Warning) - do not change properties of this object!)

  @member(FileInfo
    Information about file that is being read. Filled in constructor.)

  @member(LogCount
    Contains number of log entries in input. Works only for structures that
    supports non-sequential reading.)

  @member(LogCounter
    Contains number of logs read so far.)

  @member(OnDataTimeLog
    Event called to pass unprocessed data with time when parser finds log with
    data of unknown structure.)

  @member(OnTextTimeLog
    Event called to pass text with time when parser finds log containing only
    text or log containing data which can be expressed as text.)

   @member(OnLogTimeLog
    Event called to pass read data with time when parser fings log containing
    informations about a write to game log.)

  @member(OnEventRegisterTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about game event registration.)

  @member(OnEventUnregisterTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about game event unregistration.)

  @member(OnEventTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about game event.)

  @member(OnChannelRegisterTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about channel registration.)

  @member(OnChannelUnregisterTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about channel unregistration.)

  @member(OnChannelTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about channel value.)

  @member(OnConfigTimeLog
    Event called to pass read data with time when parser finds log containing
    informations about config value.)

  @member(OnDataLog
    Event called to pass unprocessed data when parser finds log with data of
    unknown structure.)

  @member(OnTextLog
    Event called to pass text when parser finds log containing only text or log
    containing data which can be expressed as text.)

   @member(OnLogLog
    Event called to pass read data when parser fings log containing informations
    about a write to game log.)

  @member(OnEventRegisterLog
    Event called to pass read data when parser finds log containing informations
    about game event registration.)

  @member(OnEventUnregisterLog
    Event called to pass read time when parser finds log containing informations
    about game event unregistration.)

  @member(OnEventLog
    Event called to pass read data when parser finds log containing informations
    about game event.)

  @member(OnChannelRegisterLog
    Event called to pass read data when parser finds log containing informations
    about channel registration.)

  @member(OnChannelUnregisterLog
    Event called to pass read data when parser finds log containing informations
    about channel unregistration.)

  @member(OnChannelLog
    Event called to pass read data when parser finds log containing informations
    about channel value.)

  @member(OnConfigLog
    Event called to pass read data when parser finds log containing informations
    about config value.)

  @member(OnDataTimeLogMulti
    Muticast event called to pass unprocessed data with time when parser finds
    log with data of unknown structure.)

  @member(OnTextTimeLogMulti
    Muticast event called to pass text with time when parser finds log
    containing only text or log containing data which can be expressed as text.)

   @member(OnLogTimeLogMulti
    Muticast event called to pass read data with time when parser fings log
    containing informations about a write to game log.)

  @member(OnEventRegisterTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about game event registration.)

  @member(OnEventUnregisterTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about game event unregistration.)

  @member(OnEventTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about game event.)

  @member(OnChannelRegisterTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about channel registration.)

  @member(OnChannelUnregisterTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about channel unregistration.)

  @member(OnChannelTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about channel registration.)

  @member(OnConfigTimeLogMulti
    Muticast event called to pass read data with time when parser finds log
    containing informations about config value.)

  @member(OnDataLogMulti
    Muticast event called to pass unprocessed data when parser finds log with
    data of unknown structure.)

  @member(OnTextLogMulti
    Muticast event called to pass text when parser finds log containing only
    text or log containing data which can be expressed as text.)

   @member(OnLogLogMulti
    Muticast event called to pass read data with time when parser fings log
    containing informations about a write to game log.)

  @member(OnEventRegisterLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about game event registration.)

  @member(OnEventUnregisterLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about game event unregistration.)

  @member(OnEventLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about game event.)

  @member(OnChannelRegisterLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about channel registration.)

  @member(OnChannelUnregisterLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about channel unregistration.)

  @member(OnChannelLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about channel registration.)

  @member(OnConfigLogMulti
    Muticast event called to pass read data when parser finds log containing
    informations about config value.)
}
  TTelemetryLogBinaryStreamParser = class(TObject)
  private
    fStream:                          TStream;
    fLogReader:                       TTelemetryLogBinaryReader;
    fFileInfo:                        TTelemetryLogBinaryFileInfo;
    fOnDataTimeLog:                   TDataTimeLogEvent;
    fOnTextTimeLog:                   TTextTimeLogEvent;
    fOnLogTimeLog:                    TLogTimeLogEvent;
    fOnEventRegisterTimeLog:          TEventRegisterTimeLogEvent;
    fOnEventUnregisterTimeLog:        TEventRegisterTimeLogEvent;
    fOnEventTimeLog:                  TEventTimeLogEvent;
    fOnChannelRegisterTimeLog:        TChannelRegisterTimeLogEvent;
    fOnChannelUnregisterTimeLog:      TChannelUnregisterTimeLogEvent;
    fOnChannelTimeLog:                TChannelTimeLogEvent;
    fOnConfigTimeLog:                 TConfigTimeLogEvent;
  {$IFDEF NoTimeLogEvents}
    fOnDataLog:                       TDataLogEvent;
    fOnTextLog:                       TTextLogEvent;
    fOnLogLog:                        TLogEvent;
    fOnEventRegisterLog:              TEventRegisterEvent;
    fOnEventUnregisterLog:            TEventRegisterEvent;
    fOnEventLog:                      TEventEvent;
    fOnChannelRegisterLog:            TChannelRegisterEvent;
    fOnChannelUnregisterLog:          TChannelUnregisterEvent;
    fOnChannelLog:                    TChannelEvent;
    fOnConfigLog:                     TConfigEvent;
  {$ENDIF}
  {$IFDEF MulticastEvents}
    fOnDataTimeLogMulti:              TMulticastDataTimeLogEvent;
    fOnTextTimeLogMulti:              TMulticastTextTimeLogEvent;
    fOnLogTimeLogMulti:               TMulticastLogTimeLogEvent;
    fOnEventRegisterTimeLogMulti:     TMulticastEventRegisterTimeLogEvent;
    fOnEventUnregisterTimeLogMulti:   TMulticastEventRegisterTimeLogEvent;
    fOnEventTimeLogMulti:             TMulticastEventTimeLogEvent;
    fOnChannelRegisterTimeLogMulti:   TMulticastChannelRegisterTimeLogEvent;
    fOnChannelUnregisterTimeLogMulti: TMulticastChannelUnregisterTimeLogEvent;
    fOnChannelTimeLogMulti:           TMulticastChannelTimeLogEvent;
    fOnConfigTimeLogMulti:            TMulticastConfigTimeLogEvent;
  {$IFDEF NoTimeLogEvents}
    fOnDataLogMulti:                  TMulticastDataLogEvent;
    fOnTextLogMulti:                  TMulticastTextLogEvent;
    fOnLogLogMulti:                   TMulticastLogEvent;
    fOnEventRegisterLogMulti:         TMulticastEventRegisterEvent;
    fOnEventUnregisterLogMulti:       TMulticastEventRegisterEvent;
    fOnEventLogMulti:                 TMulticastEventEvent;
    fOnChannelRegisterLogMulti:       TMulticastChannelRegisterEvent;
    fOnChannelUnregisterLogMulti:     TMulticastChannelUnregisterEvent;
    fOnChannelLogMulti:               TMulticastChannelEvent;
    fOnConfigLogMulti:                TMulticastConfigEvent;
  {$ENDIF}
  {$ENDIF}
    Function GetLogCount: Integer;
    Function GetLogCounter: Integer;
    Function GetLogEntry(Index: Integer): TLogEntry;
  protected
    procedure ReadFileHeader(var FileInfo: TTelemetryLogBinaryFileInfo); virtual;
    procedure DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: LongWord); virtual;
    procedure DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String); virtual;
    procedure DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); virtual;
    procedure DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t); virtual;
    procedure DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t); virtual;
    procedure DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); virtual;
    procedure DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
    procedure DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
    procedure DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
    procedure DoOnConfig(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    Function ReadNextLogEntry: Boolean; virtual;
    procedure ReadAllLogEntries; virtual;
    Function ReadLogEntry(Index: Integer): Boolean; virtual;
    property LogEntries[Index: Integer]: TLogEntry read GetLogEntry; default;    
  published
    property Stream: TStream read fStream;
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
    property LogCount: Integer read GetLogCount;
    property LogCounter: Integer read GetLogCounter;
    property OnDataTimeLog: TDataTimeLogEvent read fOnDataTimeLog write fOnDataTimeLog;
    property OnTextTimeLog: TTextTimeLogEvent read fOnTextTimeLog write fOnTextTimeLog;
    property OnLogTimeLog: TLogTimeLogEvent read fOnLogTimeLog write fOnLogTimeLog;
    property OnEventRegisterTimeLog: TEventRegisterTimeLogEvent read fOnEventRegisterTimeLog write fOnEventRegisterTimeLog;
    property OnEventUnregisterTimeLog: TEventRegisterTimeLogEvent read fOnEventUnregisterTimeLog write fOnEventUnregisterTimeLog;
    property OnEventTimeLog: TEventTimeLogEvent read fOnEventTimeLog write fOnEventTimeLog;
    property OnChannelRegisterTimeLog: TChannelRegisterTimeLogEvent read fOnChannelRegisterTimeLog write fOnChannelRegisterTimeLog;
    property OnChannelUnregisterTimeLog: TChannelUnregisterTimeLogEvent read fOnChannelUnregisterTimeLog write fOnChannelUnregisterTimeLog;
    property OnChannelTimeLog: TChannelTimeLogEvent read fOnChannelTimeLog write fOnChannelTimeLog;
    property OnConfigTimeLog: TConfigTimeLogEvent read fOnConfigTimeLog write fOnConfigTimeLog;
  {$IFDEF NoTimeLogEvents}
    property OnDataLog: TDataLogEvent read fOnDataLog write fOnDataLog;
    property OnTextLog: TTextLogEvent read fOnTextLog write fOnTextLog;
    property OnLogLog: TLogEvent read fOnLogLog write fOnLogLog;
    property OnEventRegisterLog: TEventRegisterEvent read fOnEventRegisterLog write fOnEventRegisterLog;
    property OnEventUnregisterLog: TEventRegisterEvent read fOnEventUnregisterLog write fOnEventUnregisterLog;
    property OnEventLog: TEventEvent read fOnEventLog write fOnEventLog;
    property OnChannelRegisterLog: TChannelRegisterEvent read fOnChannelRegisterLog write fOnChannelRegisterLog;
    property OnChannelUnregisterLog: TChannelUnregisterEvent read fOnChannelUnregisterLog write fOnChannelUnregisterLog;
    property OnChannelLog: TChannelEvent read fOnChannelLog write fOnChannelLog;
    property OnConfigLog: TConfigEvent read fOnConfigLog write fOnConfigLog;
  {$ENDIF}
  {$IFDEF MulticastEvents}
    property OnDataTimeLogMulti: TMulticastDataTimeLogEvent read fOnDataTimeLogMulti;
    property OnTextTimeLogMulti: TMulticastTextTimeLogEvent read fOnTextTimeLogMulti;
    property OnLogTimeLogMulti: TMulticastLogTimeLogEvent read fOnLogTimeLogMulti;
    property OnEventRegisterTimeLogMulti: TMulticastEventRegisterTimeLogEvent read fOnEventRegisterTimeLogMulti;
    property OnEventUnregisterTimeLogMulti: TMulticastEventRegisterTimeLogEvent read fOnEventUnregisterTimeLogMulti;
    property OnEventTimeLogMulti: TMulticastEventTimeLogEvent read fOnEventTimeLogMulti;
    property OnChannelRegisterTimeLogMulti: TMulticastChannelRegisterTimeLogEvent read fOnChannelRegisterTimeLogMulti;
    property OnChannelUnregisterTimeLogMulti: TMulticastChannelUnregisterTimeLogEvent read fOnChannelUnregisterTimeLogMulti;
    property OnChannelTimeLogMulti: TMulticastChannelTimeLogEvent read fOnChannelTimeLogMulti;
    property OnConfigTimeLogMulti: TMulticastConfigTimeLogEvent read fOnConfigTimeLogMulti;
  {$IFDEF NoTimeLogEvents}
    property OnDataLogMulti: TMulticastDataLogEvent read fOnDataLogMulti;
    property OnTextLogMulti: TMulticastTextLogEvent read fOnTextLogMulti;
    property OnLogLogMulti: TMulticastLogEvent read fOnLogLogMulti;
    property OnEventRegisterLogMulti: TMulticastEventRegisterEvent read fOnEventRegisterLogMulti;
    property OnEventUnregisterLogMulti: TMulticastEventRegisterEvent read fOnEventUnregisterLogMulti;
    property OnEventLogMulti: TMulticastEventEvent read fOnEventLogMulti;
    property OnChannelRegisterLogMulti: TMulticastChannelRegisterEvent read fOnChannelRegisterLogMulti;
    property OnChannelUnregisterLogMulti: TMulticastChannelUnregisterEvent read fOnChannelUnregisterLogMulti;
    property OnChannelLogMulti: TMulticastChannelEvent read fOnChannelLogMulti;
    property OnConfigLogMulti: TMulticastConfigEvent read fOnConfigLogMulti;
  {$ENDIF}
  {$ENDIF}
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryLogBinaryFileParser                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryFileParser // Class declaration                         }
{==============================================================================}
{
  @abstract(Class designed to parse a file containing binary log.)
  It is implemented as direct descendant of TTelemetryLogBinaryStreamParser
  class, you can refer to it for more details.@br
  It creates TFileStream for requested file and passes it to inherited
  constructor.

  @member(fFileName See FileName property for details.)


  @member(Create
    Class constructor.

    Creates internal file stream and passes it to inherited constructor.

    @param(FileName Name of the input log file. Must contain valid file name,
                    but it is not mandatory to assign full file path.))

  @member(Destroy
    Class destructor.

    Frees internal file stream.)


  @member(FileName
    Contains actual name of the readed file.)
}
  TTelemetryLogBinaryFileParser = class(TTelemetryLogBinaryStreamParser)
  private
    fFileName: String;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;
  published
    property FileName: String read fFileName;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                      TTelemetryLogBinaryToTextConverter                      }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryToTextConverter // Class declaration                    }
{==============================================================================}
{
  @abstract(Converts given binary log file to a text log file.)
  Implemented as descendant of TTelemetryLogBinaryFileParser class. It creates
  internal text logger and uses own methods to pass parsed data to it. It also
  creates dummy telemetry recipient using data from API information section
  because it is required by text logger.

  @member(fOutFileName See OutFileName property for details.)

  @member(fDummyRecipient
    Telemetry recipient created for internal text logger. Parametres (telemetry
    version, etc.) required to create this object are obtained from input log
    file API information section.)

  @member(fTextLogger
    Object used to write text output.)

  @member(DoOnDataLog
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnTextLog
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnLog
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnEventRegister
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnEventUnregister
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnEvent
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnChannelRegister
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnChannelUnregister
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnChannel
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)

  @member(DoOnConfig
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.)


  @member(Create
    Class constructor.

    Creates dummy recipient and text logger.

    @param(FileName    Name of the input log file. Must contain valid file name,
                       but it is not mandatory to assign full file path.)
    @param(OutFileName Name of output text file. When left empty, then name
                       of output file is created by appending @code(.LOG)
                       extension to the name of the input file.))

  @member(Destroy
    Class destructor.

    Frees all internal objects.)

  @member(Convert
    Runs the conversions. Calls ReadAllLogEntries method.)


  @member(OutFileName
    Name of the output file, that is a file to which the text log is written.)
}
  TTelemetryLogBinaryToTextConverter = class(TTelemetryLogBinaryFileParser)
  private
    fOutFileName:     String;
    fDummyRecipient:  TTelemetryRecipient;
    fTextLogger:      TTelemetryLogText;
  protected
    procedure DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: LongWord); override;
    procedure DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String); override;
    procedure DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); override;
    procedure DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t); override;
    procedure DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t); override;
    procedure DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); override;
    procedure DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); override;
    procedure DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); override;
    procedure DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); override;
    procedure DoOnConfig(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  public
    constructor Create(FileName: String; OutFileName: String = '');
    destructor Destroy; override;
    procedure Convert; virtual;
  published
    property OutFileName: String read fOutFileName;
  end;

implementation

uses
  SysUtils,
  TelemetryConversions, TelemetryStreaming;

{$IFDEF MulticastEvents}
  {$DEFINE ImplementationPart}
    {$INCLUDE '.\Inc\TelemetryLogBinaryParser_MulticastEvents.pas'}
  {$UNDEF ImplementationPart}
{$ENDIF}

procedure LogEntryFree(var Value: TLogEntry);
begin
If Assigned(Value.Data) then FreeMem(Value.Data,Value.Size);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryReader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader // Protected methods                             }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryReader.IncLogCounter(N: Integer = 1);
begin
Inc(fLogCounter,N);
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryReader.Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
begin
inherited Create;
If Assigned(Stream) then fStream := Stream
  else raise Exception.Create('TTelemetryLogBinaryReader.Create: Stream not assigned.');
fFileInfo := FileInfo;
If not TTelemetryInfoProvider.SupportsTelemetryAndGameVersion(FileInfo.APIInfo.TelemetryVersion,
                                                              FileInfo.APIInfo.GameID,
                                                              FileInfo.APIInfo.GameVersion) then
  raise Exception.Create('TTelemetryLogBinaryReader_0_1.Create: Game version (' +
    SCSGetVersionAsString(FileInfo.APIInfo.TelemetryVersion) + '; ' +
    TelemetryStringDecode(FileInfo.APIInfo.GameID) + ' ' +
    SCSGetVersionAsString(FileInfo.APIInfo.GameVersion) + ') not supported.')
else
  fTelemetryInfoProvider := TTelemetryInfoProvider.Create(FileInfo.APIInfo.TelemetryVersion,
                                                          FileInfo.APIInfo.GameID,
                                                          FileInfo.APIInfo.GameVersion);
fFirstStreamPosition := Stream.Position;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryReader.Destroy;
begin
fTelemetryInfoProvider.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader.StartReading;
begin
fLogCounter := 0;
fReadingTerminated := False;
Stream.Position := fFirstStreamPosition;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader.EndReading;
begin
fReadingTerminated := True;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader.ReadAllLogEntries;
begin
while ReadNextLogEntry do;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryLogBinaryReader_0_1                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_0_1 // Class implementation                      }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_0_1 // Protected methods                         }
{------------------------------------------------------------------------------}

Function TTelemetryLogBinaryReader_0_1.GetLogCount: Integer;
begin
Result := Length(fBlocksOffsets)
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.GetLogEntry(Index: Integer): TLogEntry;
var
  StreamPosition: Int64;
  BlockHeader:    TTelemetryLogBinaryBlockHeader;
begin
If (Index >= Low(fBlocksOffsets)) and (Index <= High(fBlocksOffsets)) then
  begin
    StreamPosition := Stream.Position;
    try
      Stream.Seek(fBlocksOffsets[Index],soBeginning);
      If ReadBlockHeader(BlockHeader) then
        begin
          Result.Time := TimeStampToDateTime(BlockHeader.BlockTime);
          Result.Size := BlockHeader.BlockPayloadSize;
          Result.Data := AllocMem(Result.Size);
          Result.Info := (BlockHeader.BlockFlags shl 8) or BlockHeader.BlockType;
          If Stream.Read(Result.Data^,Result.Size) <> BlockHeader.BlockPayloadSize then
            begin
              FreeMem(Result.Data,Result.Size);
              Result := cInvalidLogEntry;
            end;
        end;
    finally
      Stream.Position := StreamPosition;
    end;
  end
else Result := cInvalidLogEntry;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.GetFlagValue(Flags, FlagMask: Byte): Boolean;
begin
Result := (Flags and FlagMask) <> 0;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader_0_1.ReadBlocksOffsets;
var
  i:  Integer;
begin
Stream.Seek(-(SizeOf(LongWord) + SizeOf(Integer)),soEnd);
SetLength(fBlocksOffsets,Stream_ReadoutInteger(Stream));
Stream.Seek(-(SizeOf(LongWord) + SizeOf(Integer) + Length(fBlocksOffsets) * SizeOf(LongWord)),soEnd);
For i := Low(fBlocksOffsets) to High(fBlocksOffsets) do
  fBlocksOffsets[i] := Stream_ReadoutInteger(Stream);
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockHeader(var BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := Stream.Read(BlockHeader,SizeOf(TTelemetryLogBinaryBlockHeader)) >= SizeOf(TTelemetryLogBinaryBlockHeader);
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Invalid(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Generic(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  TempPtr:  Pointer;
  CurrPos:  Int64;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    If Assigned(OnDataLog) then
      begin
        TempPtr := AllocMem(BlockHeader.BlockPayloadSize);
        try
          Stream.ReadBuffer(TempPtr^,BlockHeader.BlockPayloadSize);
          OnDataLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),TempPtr,BlockHeader.BlockPayloadSize);
        finally
          FreeMem(TempPtr,BlockHeader.BlockPayloadSize);
        end;
      end;
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Text(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  TempStr:  TelemetryString;
  CurrPos:  Int64;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    If Assigned(OnTextLog) then
      begin
        TempStr := Stream_ReadoutString(Stream);
        OnTextLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),TelemetryStringDecode(TempStr));
      end;
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Log(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  LogType:  scs_log_type_t;
  TempStr:  TelemetryString;
  CurrPos:  Int64;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    If Assigned(OnLogLog) then
      begin
        LogType := Stream_ReadoutInteger(Stream);
        TempStr := Stream_ReadoutString(Stream);
        OnLogLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),LogType,TelemetryStringDecode(TempStr));
      end;
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_EventReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:  Int64;
  Event:    scs_event_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    Event := Stream_ReadoutInteger(Stream);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnEventRegister) then OnEventRegister(Self,TimeStampToDateTime(BlockHeader.BlockTime),Event);
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_EventReg: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;
 
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_EventUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:  Int64;
  Event:    scs_event_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    Event := Stream_ReadoutInteger(Stream);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnEventUnregister) then OnEventUnregister(Self,TimeStampToDateTime(BlockHeader.BlockTime),Event);
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_EventUnreg: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Event(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:  Int64;
  Event:    scs_event_t;
  Data_f:   scs_telemetry_frame_start_t;
  Data_c:   scs_telemetry_configuration_t;
  Data:     Pointer;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    Event := Stream_ReadoutInteger(Stream);
    case Event of
{*}   SCS_TELEMETRY_EVENT_frame_start:
        begin
          Stream.Read(Data_f,SizeOf(scs_telemetry_frame_start_t));
          Data := @Data_f;
        end;
{*}   SCS_TELEMETRY_EVENT_configuration:
        begin
          Stream_Read_scs_telemetry_configuration(
            Stream,
            Data_c,
            GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED),
            GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY),
            InfoProviderGetConfigNameFromID,
            TelemetryInfoProvider);
          Data := @Data_c;
        end;
    else
      Data := nil;
    end;
    try
      If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
        begin
          If Assigned(OnEvent) then OnEvent(Self,TimeStampToDateTime(BlockHeader.BlockTime),Event,Data);
        end
      else
        raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Event: Erroneous block payload size (expected: ' +
              IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    finally
      If Event = SCS_TELEMETRY_EVENT_configuration then scs_telemetry_configuration_free(Data_c,True);
    end;
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_ChannelReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:          Int64;
  ChannelName:      TelemetryString;
  ChannelID:        TChannelID;
  ChannelIndex:     scs_u32_t;
  ChannelValueType: scs_value_type_t;
  ChannelRegFlags:  scs_u32_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    If GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY) then
      begin
        ChannelID := Stream_ReadoutInteger(Stream);
        ChannelName := TelemetryInfoProvider.KnownChannels.ChannelIDToName(ChannelID);
      end
    else
      begin
        ChannelName := Stream_ReadoutString(Stream);
        ChannelID := TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelName);
      end;
    ChannelIndex := Stream_ReadoutInteger(Stream);
    ChannelValueType := Stream_ReadoutInteger(Stream);
    ChannelRegFlags := Stream_ReadoutInteger(Stream);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnChannelRegister) then
          OnChannelRegister(Self,TimeStampToDateTime(BlockHeader.BlockTime),ChannelName,ChannelID,ChannelIndex,ChannelValueType,ChannelRegFlags);
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_ChannelReg: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_ChannelUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:          Int64;
  ChannelName:      TelemetryString;
  ChannelID:        TChannelID;
  ChannelIndex:     scs_u32_t;
  ChannelValueType: scs_value_type_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    If GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY) then
      begin
        ChannelID := Stream_ReadoutInteger(Stream);
        ChannelName := TelemetryInfoProvider.KnownChannels.ChannelIDToName(ChannelID);
      end
    else
      begin
        ChannelName := Stream_ReadoutString(Stream);
        ChannelID := TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelName);
      end;
    ChannelIndex := Stream_ReadoutInteger(Stream);
    ChannelValueType := Stream_ReadoutInteger(Stream);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnChannelUnregister) then
          OnChannelUnregister(Self,TimeStampToDateTime(BlockHeader.BlockTime),ChannelName,ChannelID,ChannelIndex,ChannelValueType);
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_ChannelUnreg: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Channel(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:      Int64;
  ChannelData:  scs_named_value_localized_t;
  ChannelValue: scs_value_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    Stream_Read_scs_named_value_Localized(
      Stream,
      ChannelData,
      GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED),
      GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY),
      InfoProviderGetChannelNameFromID,
      TelemetryInfoProvider);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnChannel) then
          begin
            ChannelValue := scs_value(ChannelData.Value);
            try
              OnChannel(Self,
                        TimeStampToDateTime(BlockHeader.BlockTime),
                        ChannelData.Name,
                        TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelData.Name),
                        ChannelData.Index,
                        @ChannelValue);
            finally
              scs_value_free(ChannelValue);
            end;
          end;
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Channel: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;    
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Config(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  CurrPos:    Int64;
  ConfigData: scs_named_value_localized_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    CurrPos := Stream.Position;
    Stream_Read_scs_named_value_Localized(
      Stream,
      ConfigData,
      GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED),
      GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY),
      InfoProviderGetConfigNameFromID,
      TelemetryInfoProvider);
    If Stream.Position = (CurrPos + BlockHeader.BlockPayloadSize) then
      begin
        If Assigned(OnConfig) then
          begin
            OnConfig(Self,TimeStampToDateTime(BlockHeader.BlockTime),ConfigData.Name,
                     TelemetryInfoProvider.KnownConfigs.ConfigNameToID(ConfigData.Name),
                     ConfigData.Index,ConfigData.Value);
          end;
      end
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Config: Erroneous block payload size (expected: ' +
            IntToStr(BlockHeader.BlockPayloadSize) + ', reality: ' + IntToStr(Stream.Position - CurrPos) + ')');
    Stream.Position := CurrPos + BlockHeader.BlockPayloadSize;
    Result := (Stream.Position + SizeOf(TTelemetryLogBinaryBlockHeader)) <= Stream.Size;    
  end
else Result := False;
end;
        
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlockPayload_Termination(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := False;
end;
       
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadBlock: Boolean;
var
  BlockHeader:  TTelemetryLogBinaryBlockHeader;
begin
Result := False;
If not ReadingTerminated and ReadBlockHeader(BlockHeader) then
  begin
    case BlockHeader.BlockType of
      LB_BLOCK_TYPE_INVALID:      Result := ReadBlockPayload_Invalid(BlockHeader);
      LB_BLOCK_TYPE_GENERIC:      Result := ReadBlockPayload_Generic(BlockHeader);
      LB_BLOCK_TYPE_TEXT:         Result := ReadBlockPayload_Text(BlockHeader);
      LB_BLOCK_TYPE_LOG:          Result := ReadBlockPayload_Log(BlockHeader);
      LB_BLOCK_TYPE_EVENTREG:     Result := ReadBlockPayload_EventReg(BlockHeader);
      LB_BLOCK_TYPE_EVENTUNREG:   Result := ReadBlockPayload_EventUnreg(BlockHeader);
      LB_BLOCK_TYPE_EVENT:        Result := ReadBlockPayload_Event(BlockHeader);
      LB_BLOCK_TYPE_CHANNELREG:   Result := ReadBlockPayload_ChannelReg(BlockHeader);
      LB_BLOCK_TYPE_CHANNELUNREG: Result := ReadBlockPayload_ChannelUnreg(BlockHeader);
      LB_BLOCK_TYPE_CHANNEL:      Result := ReadBlockPayload_Channel(BlockHeader);
      LB_BLOCK_TYPE_CONFIG:       Result := ReadBlockPayload_Config(BlockHeader);
      LB_BLOCK_TYPE_TERMINATE:    Result := ReadBlockPayload_Termination(BlockHeader);
    else
      raise Exception.Create('TTelemetryLogBinaryReader_0_1.ReadBlock: Unknown block type (' + 
                              IntToStr(BlockHeader.BlockType) + ').');
    end;
  end;
ReadingTerminated := not Result;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_0_1 // Public methods                            }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryReader_0_1.StartReading;
var
  StreamPosition: Int64;
begin
inherited;
SetLength(fBlocksOffsets,0);
If FileInfo.Header.DataStructure = 0 then
  begin
    StreamPosition := Stream.Position;
    // Check whether the file is large enough to accommodate table of blocks offsets.
    If (Stream.Size - Stream.Position) >= SizeOf(TTelemetryLogBinaryBlockHeader) + SizeOf(Integer) + SizeOf(LongWord) then
      begin
        // Check control number and read the table if it matches.
        Stream.Seek(-SizeOf(LongWord),soEnd);
        If Stream_ReadoutInteger(Stream) = LB_STRUCT0_CONTROLNUMBER then ReadBlocksOffsets;
      end;
    Stream.Position := StreamPosition;
  end;
end;
             
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader_0_1.EndReading;
begin
SetLength(fBlocksOffsets,0);
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean;
begin
try
  Result := ReadBlock;
except
  Result := False;
end;
If IncreaseLogCounter and Result then IncLogCounter;
end;
              
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0_1.ReadLogEntry(Index: Integer): Boolean; 
var
  StreamPosition:   Int64;
  TerminationState: Boolean;
begin
If (Index >= Low(fBlocksOffsets)) and (Index <= High(fBlocksOffsets)) then
  begin
    StreamPosition := Stream.Position;
    try
      Stream.Seek(fBlocksOffsets[Index],soBeginning);
      TerminationState := ReadingTerminated;
      try
        ReadingTerminated := False;
        Result := ReadNextLogEntry(False);
      finally
        ReadingTerminated := TerminationState;
      end;
    finally
      Stream.Position := StreamPosition;
    end;
  end
else Result := False;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                       TTelemetryLogBinaryStreamParser                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryStreamParser // Class implementation                    }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStreamParser // Private methods                         }
{------------------------------------------------------------------------------}

Function TTelemetryLogBinaryStreamParser.GetLogCount: Integer;
begin
Result := fLogReader.LogCount;
end;
                         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryStreamParser.GetLogCounter: Integer;
begin
Result := fLogReader.LogCounter;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryStreamParser.GetLogEntry(Index: Integer): TLogEntry;
begin
Result := fLogReader[Index];
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStreamParser // Protected methods                       }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryStreamParser.ReadFileHeader(var FileInfo: TTelemetryLogBinaryFileInfo);
begin
If (Stream.Size - Stream.Position) < (SizeOf(TTelemetryLogBinaryFileHeader) + cMinimumAPIInfoSectionSize) then
  raise Exception.Create('TTelemetryLogBinaryStreamParser.ReadFileHeader: File too small.');
fStream.ReadBuffer(FileInfo.Header,SizeOf(TTelemetryLogBinaryFileHeader));
If FileInfo.Header.MagicNumber <> LB_MAGIC_NUMBER then
  raise Exception.Create('TTelemetryLogBinaryStreamParser.ReadFileHeader: Bad file signature.');
FileInfo.APIInfo.TelemetryVersion := Stream_ReadoutInteger(fStream);
FileInfo.APIInfo.GameID := Stream_ReadoutString(fStream);
FileInfo.APIInfo.GameVersion := Stream_ReadoutInteger(fStream);
FileInfo.APIInfo.GameName := Stream_ReadoutString(fStream);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: LongWord);
begin
If Assigned(fOnDataTimeLog) then fOnDataTimeLog(Self,Time,Data,Size);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnDataLog) then fOnDataLog(Self,Data,Size);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnDataTimeLogMulti.Call(Self,Time,Data,Size);
{$IFDEF NoTimeLogEvents}
fOnDataLogMulti.Call(Self,Data,Size);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String);
begin
If Assigned(fOnTextTimeLog) then fOnTextTimeLog(Self,Time,Text);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnTextLog) then fOnTextLog(Self,Text);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnTextTimeLogMulti.Call(Self,Time,Text);
{$IFDEF NoTimeLogEvents}
fOnTextLogMulti.Call(Self,Text);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String);
begin
If Assigned(fOnLogTimeLog) then fOnLogTimeLog(Self,Time,LogType,LogText);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnLogLog) then fOnLogLog(Self,LogType,LogText);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnLogTimeLogMulti.Call(Self,Time,LogType,LogText);
{$IFDEF NoTimeLogEvents}
fOnLogLogMulti.Call(Self,LogType,LogText);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t);
begin
If Assigned(fOnEventRegisterTimeLog) then fOnEventRegisterTimeLog(Self,Time,Event);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnEventRegisterLog) then fOnEventRegisterLog(Self,Event,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnEventRegisterTimeLogMulti.Call(Self,Time,Event);
{$IFDEF NoTimeLogEvents}
fOnEventRegisterLogMulti.Call(Self,Event,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t);
begin
If Assigned(fOnEventUnregisterTimeLog) then fOnEventUnregisterTimeLog(Self,Time,Event);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnEventUnregisterLog) then fOnEventUnregisterLog(Self,Event,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnEventUnregisterTimeLogMulti.Call(Self,Time,Event);
{$IFDEF NoTimeLogEvents}
fOnEventUnregisterLogMulti.Call(Self,Event,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer);
begin
If Assigned(fOnEventTimeLog) then fOnEventTimeLog(Self,Time,Event,Data);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnEventLog) then fOnEventLog(Self,Event,Data,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnEventTimeLogMulti.Call(Self,Time,Event,Data);
{$IFDEF NoTimeLogEvents}
fOnEventLogMulti.Call(Self,Event,Data,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
If Assigned(fOnChannelRegisterTimeLog) then fOnChannelRegisterTimeLog(Self,Time,Name,ID,Index,ValueType,Flags);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnChannelRegisterLog) then fOnChannelRegisterLog(Self,Name,ID,Index,ValueType,Flags,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnChannelRegisterTimeLogMulti.Call(Self,Time,Name,ID,Index,ValueType,Flags);
{$IFDEF NoTimeLogEvents}
fOnChannelRegisterLogMulti.Call(Self,Name,ID,Index,ValueType,Flags,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
If Assigned(fOnChannelUnregisterTimeLog) then fOnChannelUnregisterTimeLog(Self,Time,Name,ID,Index,ValueType);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnChannelUnregisterLog) then fOnChannelUnregisterLog(Self,Name,ID,Index,ValueType,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnChannelUnregisterTimeLogMulti.Call(Self,Time,Name,ID,Index,ValueType);
{$IFDEF NoTimeLogEvents}
fOnChannelUnregisterLogMulti.Call(Self,Name,ID,Index,ValueType,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
If Assigned(fOnChannelTimeLog) then fOnChannelTimeLog(Self,Time,Name,ID,Index,Value);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnChannelLog) then fOnChannelLog(Self,Name,ID,Index,Value,nil);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnChannelTimeLogMulti.Call(Self,Time,Name,ID,Index,Value);
{$IFDEF NoTimeLogEvents}
fOnChannelLogMulti.Call(Self,Name,ID,Index,Value,nil);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnConfig(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If Assigned(fOnConfigTimeLog) then fOnConfigTimeLog(Self,Time,Name,ID,Index,Value);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnConfigLog) then fOnConfigLog(Self,Name,ID,Index,Value);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnConfigTimeLogMulti.Call(Self,Time,Name,ID,Index,Value);
{$IFDEF NoTimeLogEvents}
fOnConfigLogMulti.Call(Self,Name,ID,Index,Value);
{$ENDIF}
{$ENDIF}
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStreamParser // Public methods                          }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryStreamParser.Create(Stream: TStream);
begin
inherited Create;
If Assigned(Stream) then fStream := Stream
  else raise Exception.Create('TTelemetryLogBinaryStreamParser.Create: Telemetry Recipient not assigned.');
ReadFileHeader(fFileInfo);
case fFileInfo.Header.DataStructure of
  0..1: fLogReader := TTelemetryLogBinaryReader_0_1.Create(Stream,fFileInfo);
else
  raise Exception.Create('TTelemetryLogBinaryStreamParser.Create: Unknown data structure (' +
                          IntToStr(fFileInfo.Header.DataStructure) + ').');
end;
{$IFDEF MulticastEvents}
fOnDataTimeLogMulti := TMulticastDataTimeLogEvent.Create(Self);
fOnTextTimeLogMulti := TMulticastTextTimeLogEvent.Create(Self);
fOnLogTimeLogMulti := TMulticastLogTimeLogEvent.Create(Self);
fOnEventRegisterTimeLogMulti := TMulticastEventRegisterTimeLogEvent.Create(Self);
fOnEventUnregisterTimeLogMulti := TMulticastEventRegisterTimeLogEvent.Create(Self);
fOnEventTimeLogMulti := TMulticastEventTimeLogEvent.Create(Self);
fOnChannelRegisterTimeLogMulti := TMulticastChannelRegisterTimeLogEvent.Create(Self);
fOnChannelUnregisterTimeLogMulti := TMulticastChannelUnregisterTimeLogEvent.Create(Self);
fOnChannelTimeLogMulti := TMulticastChannelTimeLogEvent.Create(Self);
fOnConfigTimeLogMulti := TMulticastConfigTimeLogEvent.Create(Self);
{$IFDEF NoTimeLogEvents}
fOnDataLogMulti := TMulticastDataLogEvent.Create(Self);
fOnTextLogMulti := TMulticastTextLogEvent.Create(Self);
fOnLogLogMulti := TMulticastLogEvent.Create(Self);
fOnEventRegisterLogMulti := TMulticastEventRegisterEvent.Create(Self);
fOnEventUnregisterLogMulti := TMulticastEventRegisterEvent.Create(Self);
fOnEventLogMulti := TMulticastEventEvent.Create(Self);
fOnChannelRegisterLogMulti := TMulticastChannelRegisterEvent.Create(Self);
fOnChannelUnregisterLogMulti := TMulticastChannelUnregisterEvent.Create(Self);
fOnChannelLogMulti := TMulticastChannelEvent.Create(Self);
fOnConfigLogMulti := TMulticastConfigEvent.Create(Self);
{$ENDIF}
{$ENDIF}
fLogReader.OnDataLog := DoOnDataLog;
fLogReader.OnTextLog := DoOnTextLog;
fLogReader.OnLogLog := DoOnLog;
fLogReader.OnEventRegister := DoOnEventRegister;
fLogReader.OnEventUnregister := DoOnEventUnregister;
fLogReader.OnEvent := DoOnEvent;
fLogReader.OnChannelRegister := DoOnChannelRegister;
fLogReader.OnChannelUnregister := DoOnChannelUnregister;
fLogReader.OnChannel := DoOnChannel;
fLogReader.OnConfig := DoOnConfig;
fLogReader.StartReading;
end;
                      
//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryStreamParser.Destroy;
begin
If Assigned(fLogReader) then fLogReader.EndReading;
fLogReader.Free;
{$IFDEF MulticastEvents}
fOnDataTimeLogMulti.Free;
fOnTextTimeLogMulti.Free;
fOnLogTimeLogMulti.Free;
fOnEventRegisterTimeLogMulti.Free;
fOnEventUnregisterTimeLogMulti.Free;
fOnEventTimeLogMulti.Free;
fOnChannelRegisterTimeLogMulti.Free;
fOnChannelUnregisterTimeLogMulti.Free;
fOnChannelTimeLogMulti.Free;
fOnConfigTimeLogMulti.Free;
{$IFDEF NoTimeLogEvents}
fOnDataLogMulti.Free;
fOnTextLogMulti.Free;
fOnLogLogMulti.Free;
fOnEventRegisterLogMulti.Free;
fOnEventUnregisterLogMulti.Free;
fOnEventLogMulti.Free;
fOnChannelRegisterLogMulti.Free;
fOnChannelUnregisterLogMulti.Free;
fOnChannelLogMulti.Free;
fOnConfigLogMulti.Free;
{$ENDIF}
{$ENDIF}
inherited;
end;
                  
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryStreamParser.ReadNextLogEntry: Boolean;
begin
Result := fLogReader.ReadNextLogEntry;
end;
                
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.ReadAllLogEntries;
begin
fLogReader.ReadAllLogEntries;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryStreamParser.ReadLogEntry(Index: Integer): Boolean;
begin
Result := fLogReader.ReadLogEntry(Index);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryLogBinaryFileParser                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryFileParser // Class implementation                      }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryFileParser // Public methods                            }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryFileParser.Create(FileName: String);
begin
inherited Create(TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite));
fFileName := FileName;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryFileParser.Destroy;
var
  TempStream: TStream;
begin
TempStream := Stream;
inherited;
TempStream.Free;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                      TTelemetryLogBinaryToTextConverter                      }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryToTextConverter // Class implementation                 }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryToTextConverter // Protected methods                    }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryToTextConverter.DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: LongWord);
begin
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Implement at least some rudimentary conversion.'}
{$ENDIF}
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.AddLog('$');
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.AddLog(Text);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogLog(LogType,LogText);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogEventRegister(Event);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogEventUnregister(Event);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogEvent(Event,Data);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogChannelRegister(Name,ID,Index,ValueType,Flags);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogChannelUnregister(Name,ID,Index,ValueType);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogChannel(Name,ID,Index,Value);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnConfig(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogConfig(Name,ID,Index,Value);
inherited;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryToTextConverter // Public methods                       }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryToTextConverter.Create(FileName: String; OutFileName: String = '');
begin
inherited Create(FileName);
If OutFileName <> '' then
  fOutFileName := OutFileName
else
  fOutFileName := FileName + '.log';
If not TTelemetryRecipient.SupportsTelemetryAndGameVersion(FileInfo.APIInfo.TelemetryVersion,
                                                           FileInfo.APIInfo.GameID,
                                                           FileInfo.APIInfo.GameVersion) then
  raise Exception.Create('TTelemetryLogBinaryToTextConverter.Create: Game version (' +
    SCSGetVersionAsString(FileInfo.APIInfo.TelemetryVersion) + '; ' +
    TelemetryStringDecode(FileInfo.APIInfo.GameID) + ' ' +
    SCSGetVersionAsString(FileInfo.APIInfo.GameVersion) + ') not supported.')
else
  fDummyRecipient := TTelemetryRecipient.Create(FileInfo.APIInfo.TelemetryVersion,
                                                FileInfo.APIInfo.GameID,
                                                FileInfo.APIInfo.GameVersion,
                                                FileInfo.APIInfo.GameName);
fTextLogger := TTelemetryLogText.Create(nil,fOutFileName);
fTextLogger.Recipient := fDummyRecipient;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryToTextConverter.Destroy;
begin
fTextLogger.Free;
fDummyRecipient.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.Convert;
begin
ReadAllLogEntries;
end;

end.