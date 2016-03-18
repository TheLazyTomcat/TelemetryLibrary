{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Contains classes designed to parse binary logs.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-10)
@lastmod(2015-07-10)

  @bold(@NoAutoLink(TelemetryLogBinaryParser))

  ©2013-2015 František Milt, all rights reserved.

  Last change: 2015-07-10

  This unit contains classes that are designed to parse binary logs and/or
  convert them to text logs, namely:
@preformatted(
  TTelemetryLogBinaryReader
   |- TTelemetryLogBinaryReader_0
       |- TTelemetryLogBinaryReader_1

  TTelemetryLogBinaryStreamParser
   |- TTelemetryLogBinaryFileParser
       |- TTelemetryLogBinaryToTextConverter
)
  Included files:@preformatted(
    .\Inc\TelemetryMulticastEvents.pas
      Contains declarations and implementations of multicast event classes.)
      
@html(<hr>)}
unit TelemetryLogBinaryParser;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

{$DEFINE TelemetryLogBinaryParser}

uses
{$IFNDEF Documentation}
  Classes,
  AuxTypes,
  BinaryStreaming,
{$IFDEF MulticastEvents}
  MulticastEvent,
{$ENDIF}
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
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

type
  //:Event type used when reader/parser passes unprocessed log data.
  TDataLogEvent = procedure(Sender: TObject; Data: Pointer; Size: UInt32) of object;
  //:Event type used when reader/parser passes text log data or any interpreted data.
  TTextLogEvent = procedure(Sender: TObject; const Text: String) of object;

  //:Event type used when reader/parser passes unprocessed log data with time.
  TDataTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Data: Pointer; Size: UInt32) of object;
  //:Event type used when reader/parser passes text log data or any interpreted data with time.
  TTextTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Text: String) of object;
  //:Event type used when reader/parser passes information about a write to game log with time.
  TLogTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String) of object;
  //:Event type used when reader/parser passes log data containing information about game event (un)registration with time.
  TEventRegisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Event: scs_event_t) of object;
  //:Event type used when reader/parser passes log data containing information about game event with time.
  TEventTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer) of object;
  //:Event type used when reader/parser passes log data containing information about channel registration with time.
  TChannelRegisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t) of Object;
  //:Event type used when reader/parser passes log data containing information about channel unregistration with time.
  TChannelUnregisterTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t) of Object;
  //:Event type used when reader/parser passes log data containing information about channel value with time.
  TChannelTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t) of Object;
  //:Event type used when reader/parser passes log data containing information about config value with time.
  TConfigTimeLogEvent = procedure(Sender: TObject; Time: TDateTime; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t) of object;


{$IFNDEF Documentation}
  {$DEFINE DeclarationPart}
    {$INCLUDE '..\Inc\TelemetryMulticastEvents.pas'}
  {$UNDEF DeclarationPart}
{$ENDIF}

{:
  Structure returned by parser when you access specific log in the stream by its
  index.
  
  @member Time @noAutoLink(Time) when the log was written.
  @member Data Unprocessed @noAutoLink(data) the accessed log contains.
  @member(Info Other information about the log. Content of this field differs
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
    Data: array of Byte;
    Info: UInt32;
  end;

const
  //:Contains empty/invalid value of type TLogEntry.
  InvalidLogEntry: TLogEntry = (Time: 0; Data: nil; Info: 0);

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryReader                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader // Class declaration                             }
{==============================================================================}
{:
  @abstract(Base class for all internal readers used to write binary logs.)
  This class is intended as common ancestor for all internal reader classes.
  These classes are used internaly to read different binary log structures, and
  should not be used outside of parsers. All descendants must implement all
  abstract methods. They also may add some new methods, but they should be used
  in parsers wery carefully and only if really needed.
}
type
  TTelemetryLogBinaryReader = class(TObject)
  private
  {:
    See Stream property for details.
  }
    fStream:                    TStream;
  {:
    See FileInfo property for details.
  }
    fFileInfo:                  TTelemetryLogBinaryFileInfo;
  {:
    See LogCounter property for details.
  }
    fLogCounter:                Integer;
  {:
    See TelemetryInfoProvider property for details.
  }
    fTelemetryInfoProvider:     TTelemetryInfoProvider;
  {:
    See FirstStreamPosition property for details.
  }
    fFirstStreamPosition:       Int64;
  {:
    See ReadingTerminated property for details.
  }
    fReadingTerminated:         Boolean;
  {:
    Stores reference to OnDataLog event handler.
  }
    fOnDataLog:                 TDataTimeLogEvent;
  {:
    Stores reference to OnTextLog event handler.
  }
    fOnTextLog:                 TTextTimeLogEvent;
  {:
    Stores reference to OnLogLog event handler.
  }
    fOnLogLog:                  TLogTimeLogEvent;
  {:
    Stores reference to OnEventRegister event handler.
  }
    fOnEventRegister:           TEventRegisterTimeLogEvent;
  {:
    Stores reference to OnEventUnregister event handler.
  }
    fOnEventUnregister:         TEventRegisterTimeLogEvent;
  {:
    Stores reference to OnEvent event handler.
  }
    fOnEvent:                   TEventTimeLogEvent;
  {:
    Stores reference to OnChannelRegister event handler.
  }
    fOnChannelRegister:         TChannelRegisterTimeLogEvent;
  {:
    Stores reference to OnChannelUnregister event handler.
  }
    fOnChannelUnregister:       TChannelUnregisterTimeLogEvent;
  {:
    Stores reference to OnChannel event handler.
  }
    fOnChannel:                 TChannelTimeLogEvent;
  {:
    Stores reference to OnConfig event handler.
  }
    fOnConfig:                  TConfigTimeLogEvent;
  protected
  {:
    Increases LogCounter property by a number passed in @code(N) parameter.

    @param Delta Number by which the LogCounter will be increased.
  }
    procedure IncLogCounter(Delta: Integer = 1); virtual;
  {:
    Getter for LogCount property.
  }
    Function GetLogCount: Integer; virtual; abstract;
  {:
    Getter for LogEntries array property.
  }
    Function GetLogEntry(Index: Integer): TLogEntry; virtual; abstract;
  public
  {:
    Class constructor.

    Creates internal telemetry info provider.

    @param(Stream   @noAutoLink(Stream) from which the reader will read. Must
                    not be @nil. Position must be set directly behind the file
                    header and API info section, not at the beginning of the
                    @noAutoLink(Stream) itself.)
    @param(FileInfo Information about input file. It must be completely filled
                    by parser that is creating current instance in order for
                    this class to work properly. Provided information are used
                    as parameters when creating telemetry info provider. When
                    not filled, the behavior of this class is not defined.)

    @raises ETLNilReference    When @noAutoLink(@code(Stream)) is not assigned.
    @raises(ETLUnsupportedGame When telemetry version, game and/or its version
                               are not supported by this class.)
  }
    constructor Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
  {:
    Class destructor.

    Frees internal telemetry info provider.
  }
    destructor Destroy; override;
  {:
    Method that must be called before any log is read. Its job is to prepare
    anything that is necesary for actual reading (variables initialization,
    ...).@br
    Sets ReadingTerminated to @false and Stream position to
    FirstStreamPosition.@br
    Can be used to effectively reset the reading.
  }
    procedure StartReading; virtual;
  {:
    Method that must be called before destruction of instance. Its job is to
    free any allocated resources.
  }
    procedure EndReading; virtual;
  {:
    Reads next log entry from current position in Stream. When parameter
    @code(IncreaseLogCounter) is set to @true and result is also @true, then
    LogCounter is increased by one.@br
    Read data are processed and passed to appropriate event.

    @param IncreaseLogCounter Determines whether to increase log counter.

    @returns @True when log was read successfuly, @false otherwise.
  }
    Function ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean; virtual; abstract;
  {:
    Reads all log entries from current position in Stream.@br
    Repeatedly calls ReadNextLogEntry until it returns @false.)
  }
    procedure ReadAllLogEntries; virtual;
  {:
    Reads log entry selected by passed index. Works only for structures that
    supports non-sequential reading (eg. structure 1).@br
    Position in Stream is not changed in this method.

    @param Index Index of requested log entry.

    @returns @True when the entry was read successfuly, @false otherwise.
  }
    Function ReadLogEntry(Index: Integer): Boolean; virtual; abstract;
  {:
    Array property mapped to log entries - that is, each item in this array
    corresponds to one log entry.@br
    When you try to access item that is out of allowed boundary or the reading
    fails, then it returns InvalidLogEntry.@br
    Works only for structures that supports non-sequential reading.@br
  }
    property LogEntries[Index: Integer]: TLogEntry read GetLogEntry; default;
  {:
    Information about file that is being read. Filled in constructor.)
  }
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
  published
  {:
    Event called when reader reads log with undefined internal data structure.
  }
    property OnDataLog: TDataTimeLogEvent read fOnDataLog write fOnDataLog;
  {:
    Event called when reader reads text log or log that can be interpreted as a
    text.
  }
    property OnTextLog: TTextTimeLogEvent read fOnTextLog write fOnTextLog;
  {:
    Event called when reader reads log containing information about a write to
    game log.
  }
    property OnLogLog: TLogTimeLogEvent read fOnLogLog write fOnLogLog;
  {:
    Event called when reader reads log containing information about game event
    registration.
  }
    property OnEventRegister: TEventRegisterTimeLogEvent read fOnEventRegister write fOnEventRegister;
  {:
    Event called when reader reads log containing information about game event
    unregistration.
  }
    property OnEventUnregister: TEventRegisterTimeLogEvent read fOnEventUnregister write fOnEventUnregister;
  {:
    Event called when reader reads log containing information about game
    event.
  }
    property OnEvent: TEventTimeLogEvent read fOnEvent write fOnEvent;
  {:
    Event called when reader reads log containing information about channel
    registration.
  }
    property OnChannelRegister: TChannelRegisterTimeLogEvent read fOnChannelRegister write fOnChannelRegister;
  {:
    Event called when reader reads log containing information about channel
    unregistration.
  }
    property OnChannelUnregister: TChannelUnregisterTimeLogEvent read fOnChannelUnregister write fOnChannelUnregister;
  {:
    Event called when reader reads log containing information about channel.
  }
    property OnChannel: TChannelTimeLogEvent read fOnChannel write fOnChannel;
  {:
    Event called when reader reads log containing information about config.
  }
    property OnConfig: TConfigTimeLogEvent read fOnConfig write fOnConfig;
  {:
    Contains number of log entries in input. Works only for structures that
    supports non-sequential reading.@br
    If current structure does not support non-sequential reading, it will return
    -1.
  }
    property LogCount: Integer read GetLogCount;
  {:
    Contains number of logs read so far.
  }
    property LogCounter: Integer read fLogCounter;
  {:
    Contains reference to a @noAutoLink(stream) that is used to read the log.
    Not initialized, filled in constructor.@br
    @bold(Warning) - do not change properties of this object!
  }
    property Stream: TStream read fStream;
  {:
    Telemetry info provider that is used internally when reading some values
    (eg. for ID <> Name conversions). Managed automatically.
  }
    property TelemetryInfoProvider: TTelemetryInfoProvider read fTelemetryInfoProvider;
  {:
    Contains position property of Stream when it was passed to constructor.
  }
    property FirstStreamPosition: Int64 read fFirstStreamPosition;
  {:
    Determines whether sequential reading can or cannot continue (eg. end of
    @noAutoLink(stream)/file, termination block was read, an error occured,
    etc.).
  }
    property ReadingTerminated: Boolean read fReadingTerminated write fReadingTerminated;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryReader_0                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_0 // Class declaration                           }
{==============================================================================}
{:
  Internal reader class used to read file of structure 0.
}
  TTelemetryLogBinaryReader_0 = class(TTelemetryLogBinaryReader)
  protected
  {:
    Getter for LogCount property. Always returns -1.
  }
    Function GetLogCount: Integer; override;
  {:
    Getter for LogEntries array property. Always returns InvalidLogEntry.
  }
    Function GetLogEntry({%H-}Index: Integer): TLogEntry; override;
  {:
    Returns value of flag selected by flag mask and stored in @code(Flags)
    parameter.

    @param Flags    Set of flags storing required flag value.
    @param FlagMask Mask determining at which bit should the value be read.

    @returns Value of the selected flag.
  }
    Function GetFlagValue(Flags, FlagMask: UInt8): Boolean; virtual;
  {:
    Reads block header at current Stream position.

    @param BlockHeader Variable in which the read header is returned.
  }
    Function ReadBlockHeader(var BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Does not read any data as payload of invalid block must be empty.

    @param BlockHeader Information read from header of the block.

    @returns Always returns @true.
  }
    Function ReadBlockPayload_Invalid({%H-}BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of generic block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.
  }
    Function ReadBlockPayload_Generic(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of text block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
  }
    Function ReadBlockPayload_Text(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of log block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
  }
    Function ReadBlockPayload_Log(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of game event registration block and passes it to appropriate
    event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBadData When stored payload size does not match size of stored
                       data.)
  }
    Function ReadBlockPayload_EventReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of game event unregistration block and passes it to
    appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBadData When stored payload size does not match size of stored
                       data.)
  }
    Function ReadBlockPayload_EventUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of game event block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
    @raises(ETLBadData        When stored payload size does not match size of
                              stored data.)
  }
    Function ReadBlockPayload_Event(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of channel registration block and passes it to appropriate
    event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
    @raises(ETLBadData        When stored payload size does not match size of
                              stored data.)
  }
    Function ReadBlockPayload_ChannelReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of channel unregistration block and passes it to appropriate
    event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
    @raises(ETLBadData        When stored payload size does not match size of
                              stored data.)                              
  }
    Function ReadBlockPayload_ChannelUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of channel block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
    @raises(ETLBadData        When stored payload size does not match size of
                              stored data.)
  }
    Function ReadBlockPayload_Channel(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of config block and passes it to appropriate event.

    @param BlockHeader Information read from header of the block.

    @returns @True when the payload was read successfuly, @false othewise.

    @raises(ETLBufferTooSmall When block payload is too small to contain any
                              valid data.)
    @raises(ETLBadData        When stored payload size does not match size of
                              stored data.)
  }
    Function ReadBlockPayload_Config(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads payload of termination block.

    @param BlockHeader Information read from header of the block.

    @returns Always returns @false as reading should terminate at this block.
  }
    Function ReadBlockPayload_Termination({%H-}BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean; virtual;
  {:
    Reads block at current Stream position.

    @returns(@True when the block was read successfuly, @false othewise.)

    @raises ETLUnknownData When block of unknown type is encountered.
  }
    Function ReadBlock: Boolean; virtual;
  public
  {:
    See @inherited.
  }
    procedure StartReading; override;
  {:
    See @inherited.
  }
    procedure EndReading; override;
  {:
    See @inherited.
    Internally calls ReadBlock method.
  }
    Function ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean; override;
  {:
    See @inherited.
    Always returns @false.
  }
    Function ReadLogEntry({%H-}Index: Integer): Boolean; override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryReader_1                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_1 // Class declaration                           }
{==============================================================================}
{:
  Internal reader class used to read file of structure 1.
}
  TTelemetryLogBinaryReader_1 = class(TTelemetryLogBinaryReader_0)
  private
  {:
    Array that stores table of blocks offsets.
  }
    fBlocksOffsets: array of UInt32;
  protected
  {:
    Getter for LogCount property. Returns length of fBlocksOffsets array.
  }
    Function GetLogCount: Integer; override;
  {:
    Getter for LogEntries array property. It finds and accesses individual
    entries by offsets stored in fBlocksOffsets array.@br
    If index falls out of allowed boundaries, an InvalidLogEntry is returned.
  }
    Function GetLogEntry(Index: Integer): TLogEntry; override;
  {:
    Reads table of block offsets from the stream. Stream position is
    not changed in this method.
  }
    procedure ReadBlocksOffsets;
  public
  {:
    See @inherited.
  }
    procedure StartReading; override;
  {:
    See @inherited. Empties the table of block offsets.
  }
    procedure EndReading; override;
  {:
    Reads log entry selected by passed index.@br
    Position in Stream is not changed in this method.

    @param Index Index of requested log entry.

    @returns @True when the entry was read successfuly, @false otherwise.
  }
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
{:
  @abstract(Class designed to parse a @noAutoLink(stream) containing binary
            log.)
  It is designed as a @noAutoLink(stream) parser, meaning it reads over the
  input @noAutoLink(stream) and when it runs into a valid log, it reads it,
  processes it when possible and calls appropriate event to which it passes all
  processed data. For example, when it finds entry containing information about
  channel registration, it reads it and calls event OnChannelRegisterLog (or
  similar alternative). These events have the same signature as events used by
  telemetry recipient, so you can bind the same handlers to them.@br
  Parser internally uses appropriate readers to read files with different
  structures.
}
  TTelemetryLogBinaryStreamParser = class(TObject)
  private
  {:
    See Stream property for details.
  }
    fStream:                          TStream;
  {:
    Internal reader used to read files with different structures. Managed
    automatically. Crated in constructor as an instance of one of the
    TTelemetryLogBinaryReader descendants. Actual class used depends on
    structure of read file.
  }
    fLogReader:                       TTelemetryLogBinaryReader;
  {:
    See FileInfo property for details.
  }
    fFileInfo:                        TTelemetryLogBinaryFileInfo;
  {:
    Stores reference to OnDataTimeLog event handler.
  }
    fOnDataTimeLog:                   TDataTimeLogEvent;
  {:
    Stores reference to OnTextTimeLog event handler.
  }
    fOnTextTimeLog:                   TTextTimeLogEvent;
  {:
    Stores reference to OnLogTimeLog event handler.
  }
    fOnLogTimeLog:                    TLogTimeLogEvent;
  {:
    Stores reference to OnEventRegisterTimeLog event handler.
  }
    fOnEventRegisterTimeLog:          TEventRegisterTimeLogEvent;
  {:
    Stores reference to OnEventUnregisterTimeLog event handler.
  }
    fOnEventUnregisterTimeLog:        TEventRegisterTimeLogEvent;
  {:
    Stores reference to OnEventTimeLog event handler.
  }
    fOnEventTimeLog:                  TEventTimeLogEvent;
  {:
    Stores reference to OnChannelRegisterTimeLog event handler.
  }
    fOnChannelRegisterTimeLog:        TChannelRegisterTimeLogEvent;
  {:
    Stores reference to OnChannelUnregisterTimeLog event handler.
  }
    fOnChannelUnregisterTimeLog:      TChannelUnregisterTimeLogEvent;
  {:
    Stores reference to OnChannelTimeLog event handler.
  }
    fOnChannelTimeLog:                TChannelTimeLogEvent;
  {:
    Stores reference to OnConfigTimeLog event handler.
  }
    fOnConfigTimeLog:                 TConfigTimeLogEvent;
  {$IFDEF NoTimeLogEvents}
  {:
    Stores reference to OnDataLog event handler.
  }
    fOnDataLog:                       TDataLogEvent;
  {:
    Stores reference to OnTextLog event handler.
  }
    fOnTextLog:                       TTextLogEvent;
  {:
    Stores reference to OnLogLog event handler.
  }
    fOnLogLog:                        TLogEvent;
  {:
    Stores reference to OnEventRegisterLog event handler.
  }
    fOnEventRegisterLog:              TEventRegisterEvent;
  {:
    Stores reference to OnEventUnregisterLog event handler.
  }
    fOnEventUnregisterLog:            TEventRegisterEvent;
  {:
    Stores reference to OnEventLog event handler.
  }
    fOnEventLog:                      TEventEvent;
  {:
    Stores reference to OnChannelRegisterLog event handler.
  }
    fOnChannelRegisterLog:            TChannelRegisterEvent;
  {:
    Stores reference to OnChannelUnregisterLog event handler.
  }
    fOnChannelUnregisterLog:          TChannelUnregisterEvent;
  {:
    Stores reference to OnChannelLog event handler.
  }
    fOnChannelLog:                    TChannelEvent;
  {:
    Stores reference to OnConfigLog event handler.
  }
    fOnConfigLog:                     TConfigEvent;
  {$ENDIF}
  {$IFDEF MulticastEvents}
  {:
    Object managing multicast OnDataTimeLogMulti event.
  }
    fOnDataTimeLogMulti:              TMulticastDataTimeLogEvent;
  {:
    Object managing multicast OnTextTimeLogMulti event.
  }
    fOnTextTimeLogMulti:              TMulticastTextTimeLogEvent;
  {:
    Object managing multicast OnLogTimeLogMulti event.
  }
    fOnLogTimeLogMulti:               TMulticastLogTimeLogEvent;
  {:
    Object managing multicast OnEventRegisterTimeLogMulti event.
  }
    fOnEventRegisterTimeLogMulti:     TMulticastEventRegisterTimeLogEvent;
  {:
    Object managing multicast OnEventUnregisterTimeLogMulti event.
  }
    fOnEventUnregisterTimeLogMulti:   TMulticastEventRegisterTimeLogEvent;
  {:
    Object managing multicast OnEventTimeLogMulti event.
  }
    fOnEventTimeLogMulti:             TMulticastEventTimeLogEvent;
  {:
    Object managing multicast OnChannelRegisterTimeLogMulti event.
  }
    fOnChannelRegisterTimeLogMulti:   TMulticastChannelRegisterTimeLogEvent;
  {:
    Object managing multicast OnChannelUnregisterTimeLogMulti event.
  }
    fOnChannelUnregisterTimeLogMulti: TMulticastChannelUnregisterTimeLogEvent;
  {:
    Object managing multicast OnChannelTimeLogMulti event.
  }
    fOnChannelTimeLogMulti:           TMulticastChannelTimeLogEvent;
  {:
    Object managing multicast OnConfigTimeLogMulti event.
  }
    fOnConfigTimeLogMulti:            TMulticastConfigTimeLogEvent;
  {$IFDEF NoTimeLogEvents}
  {:
    Object managing multicast OnDataLogMulti event.
  }
    fOnDataLogMulti:                  TMulticastDataLogEvent;
  {:
    Object managing multicast OnTextLogMulti event.
  }
    fOnTextLogMulti:                  TMulticastTextLogEvent;
  {:
    Object managing multicast OnLogLogMulti event.
  }
    fOnLogLogMulti:                   TMulticastLogEvent;
  {:
    Object managing multicast OnEventRegisterLogMulti event.
  }
    fOnEventRegisterLogMulti:         TMulticastEventRegisterEvent;
  {:
    Object managing multicast OnEventUnregisterLogMulti event.
  }
    fOnEventUnregisterLogMulti:       TMulticastEventRegisterEvent;
  {:
    Object managing multicast OnEventLogMulti event.
  }
    fOnEventLogMulti:                 TMulticastEventEvent;
  {:
    Object managing multicast OnChannelRegisterLogMulti event.
  }
    fOnChannelRegisterLogMulti:       TMulticastChannelRegisterEvent;
  {:
    Object managing multicast OnChannelUnregisterLogMulti event.
  }
    fOnChannelUnregisterLogMulti:     TMulticastChannelUnregisterEvent;
  {:
    Object managing multicast OnChannelLogMulti event.
  }
    fOnChannelLogMulti:               TMulticastChannelEvent;
  {:
    Object managing multicast OnConfigLogMulti event.
  }
    fOnConfigLogMulti:                TMulticastConfigEvent;
  {$ENDIF}
  {$ENDIF}
  {:
    Getter for LogCount property. Gets the value from internal reader object.
  }
    Function GetLogCount: Integer;
  {:
    Getter for LogCounter property. Gets the value from internal reader object.
  }
    Function GetLogCounter: Integer;
  {:
    Getter for LogEntries array property. Gets actual value from internal reader
    object.
  }
    Function GetLogEntry(Index: Integer): TLogEntry;
  protected
  {:
    Reads file header and API information from current Stream position and
    returns them in @code(@noAutoLink(FileInfo)) parameter.

    @param FileInfo Variable to which a read information are stored.

    @param FileInfo Output value containing read data.

    @raises ETLBufferTooSmall When file is too small to contain a valid header.
    @raises ETLBadData        When file signature does not match.
  }
    procedure ReadFileHeader(var FileInfo: TTelemetryLogBinaryFileInfo); virtual;
  {:
    Method managing OnData* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnDataLog OnDataLog) event.
  }
    procedure DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: UInt32); virtual;
  {:
    Method managing OnText* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnTextLog OnTextLog) event.
  }
    procedure DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String); virtual;
  {:
    Method managing OnLog* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnLogLog OnLogLog) event.
  }
    procedure DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); virtual;
  {:
    Method managing OnEventRegister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEventRegister OnEventRegister) event.
  }
    procedure DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t); virtual;
  {:
    Method managing OnEventUnregister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEventUnregister OnEventUnregisterg)
    event.
  }
    procedure DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t); virtual;
  {:
    Method managing OnEvent* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnEvent OnEvent) event.
  }
    procedure DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); virtual;
  {:
    Method managing OnChannelRegister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannelRegister OnChannelRegister) event.
  }
    procedure DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
  {:
    Method managing OnChannelUnregister* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannelUnregister OnChannelUnregister)
    event.
  }
    procedure DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
  {:
    Method managing OnChannel* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnChannel OnChannel) event.
  }
    procedure DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
  {:
    Method managing OnConfig* events calling.@br
    Parameters are passed to event(s) calls with no change.@br
    This method is assigned in constructor to internal reader
    @link(TTelemetryLogBinaryReader.OnConfig OnConfig) event.
  }
    procedure DoOnConfig(Sender: TObject; Time: TDateTime; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  public
  {:
    Class constructor.

    Creates all automatically managed objects, reads file header (by calling
    method ReadFileHeader) and assigns handlers to internal reader events.

    @param(Stream @noAutoLink(Stream) from which the parser will read. Must not
                  be @nil.)

    @raises ETLNilReference When @noAutoLink(@code(Stream)) is not assigned.
    @raises ETLUnknownData  When binary log of unknown data structure is read.
  }
    constructor Create(Stream: TStream);
  {:
    Class destructor.

    Frees all automatically managed objects.
  }
    destructor Destroy; override;
  {:
    Reads next log entry from current position in Stream. Read data are
    processed and passed to appropriate event.@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadNextLogEntry
    ReadNextLogEntry) method.

    @returns @True when log was read successfuly, @false otherwise.
  }
    Function ReadNextLogEntry: Boolean; virtual;
  {:
    Reads all log entries from current position in Stream.@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadAllLogEntries
    ReadAllLogEntries) method.
  }
    procedure ReadAllLogEntries; virtual;
  {:
    Reads log entry selected by passed index. Works only for structures that
    supports non-sequential reading (eg. structure 1).@br
    Calls internal readers @link(TTelemetryLogBinaryReader.ReadLogEntry
    ReadLogEntry) method.@br
    Position in Stream is not changed in this method.

    @param Index Index of requested log entry.

    @returns @True when the entry was read successfuly, @false otherwise.
  }
    Function ReadLogEntry(Index: Integer): Boolean; virtual;
  {:
    Array property mapped to log entries - that is, each item in this array
    corresponds to one log entry.@br
    When you try to access item that is out of allowed boundary or the reading
    fails, then it returns InvalidLogEntry.@br
    Works only for structures that supports non-sequential reading.@br
  }
    property LogEntries[Index: Integer]: TLogEntry read GetLogEntry; default;
  {:
    Information about file that is being read. Filled in constructor.
  }
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
  published
  {:
    Contains reference to a @noAutoLink(stream) that is used to read the log.
    Not initialized, filled in constructor.@br
    @bold(Warning) - do not change properties of this object!
  }
    property Stream: TStream read fStream;
  {:
    Contains number of log entries in input. Works only for structures that
    supports non-sequential reading.
  }
    property LogCount: Integer read GetLogCount;
  {:
    Contains number of logs read so far.
  }
    property LogCounter: Integer read GetLogCounter;
  {:
    Event called to pass unprocessed data with time when parser finds log with
    data of unknown structure.
  }
    property OnDataTimeLog: TDataTimeLogEvent read fOnDataTimeLog write fOnDataTimeLog;
  {:
    Event called to pass text with time when parser finds log containing only
    text or log containing data which can be expressed as text.
  }
    property OnTextTimeLog: TTextTimeLogEvent read fOnTextTimeLog write fOnTextTimeLog;
  {:
    Event called to pass read data with time when parser fings log containing
    information about a write to game log.
  }
    property OnLogTimeLog: TLogTimeLogEvent read fOnLogTimeLog write fOnLogTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about game event registration.
  }
    property OnEventRegisterTimeLog: TEventRegisterTimeLogEvent read fOnEventRegisterTimeLog write fOnEventRegisterTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about game event unregistration.
  }
    property OnEventUnregisterTimeLog: TEventRegisterTimeLogEvent read fOnEventUnregisterTimeLog write fOnEventUnregisterTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about game event.
  }
    property OnEventTimeLog: TEventTimeLogEvent read fOnEventTimeLog write fOnEventTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about channel registration.
  }
    property OnChannelRegisterTimeLog: TChannelRegisterTimeLogEvent read fOnChannelRegisterTimeLog write fOnChannelRegisterTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about channel unregistration.
  }
    property OnChannelUnregisterTimeLog: TChannelUnregisterTimeLogEvent read fOnChannelUnregisterTimeLog write fOnChannelUnregisterTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about channel value.
  }
    property OnChannelTimeLog: TChannelTimeLogEvent read fOnChannelTimeLog write fOnChannelTimeLog;
  {:
    Event called to pass read data with time when parser finds log containing
    information about config value.
  }
    property OnConfigTimeLog: TConfigTimeLogEvent read fOnConfigTimeLog write fOnConfigTimeLog;
  {$IFDEF NoTimeLogEvents}
  {:
    Event called to pass unprocessed data when parser finds log with data of
    unknown structure.
  }
    property OnDataLog: TDataLogEvent read fOnDataLog write fOnDataLog;
  {:
    Event called to pass text when parser finds log containing only text or log
    containing data which can be expressed as text.
  }
    property OnTextLog: TTextLogEvent read fOnTextLog write fOnTextLog;
  {:
    Event called to pass read data when parser fings log containing information
    about a write to game log.
  }
    property OnLogLog: TLogEvent read fOnLogLog write fOnLogLog;
  {:
    Event called to pass read data when parser finds log containing information
    about game event registration.
  }
    property OnEventRegisterLog: TEventRegisterEvent read fOnEventRegisterLog write fOnEventRegisterLog;
  {:
    Event called to pass read time when parser finds log containing information
    about game event unregistration.
  }
    property OnEventUnregisterLog: TEventRegisterEvent read fOnEventUnregisterLog write fOnEventUnregisterLog;
  {:
    Event called to pass read data when parser finds log containing information
    about game event.
  }
    property OnEventLog: TEventEvent read fOnEventLog write fOnEventLog;
  {:
    Event called to pass read data when parser finds log containing information
    about channel registration.
  }
    property OnChannelRegisterLog: TChannelRegisterEvent read fOnChannelRegisterLog write fOnChannelRegisterLog;
  {:
    Event called to pass read data when parser finds log containing information
    about channel unregistration.
  }
    property OnChannelUnregisterLog: TChannelUnregisterEvent read fOnChannelUnregisterLog write fOnChannelUnregisterLog;
  {:
    Event called to pass read data when parser finds log containing information
    about channel value.
  }
    property OnChannelLog: TChannelEvent read fOnChannelLog write fOnChannelLog;
  {:
    Event called to pass read data when parser finds log containing information
    about config value.
  }
    property OnConfigLog: TConfigEvent read fOnConfigLog write fOnConfigLog;
  {$ENDIF}
  {$IFDEF MulticastEvents}
  {:
    Muticast event called to pass unprocessed data with time when parser finds
    log with data of unknown structure.
  }
    property OnDataTimeLogMulti: TMulticastDataTimeLogEvent read fOnDataTimeLogMulti;
  {:
    Muticast event called to pass text with time when parser finds log
    containing only text or log containing data which can be expressed as text.
  }
    property OnTextTimeLogMulti: TMulticastTextTimeLogEvent read fOnTextTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser fings log
    containing information about a write to game log.
  }
    property OnLogTimeLogMulti: TMulticastLogTimeLogEvent read fOnLogTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about game event registration.
  }
    property OnEventRegisterTimeLogMulti: TMulticastEventRegisterTimeLogEvent read fOnEventRegisterTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about game event unregistration.
  }
    property OnEventUnregisterTimeLogMulti: TMulticastEventRegisterTimeLogEvent read fOnEventUnregisterTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about game event.
  }
    property OnEventTimeLogMulti: TMulticastEventTimeLogEvent read fOnEventTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about channel registration.
  }
    property OnChannelRegisterTimeLogMulti: TMulticastChannelRegisterTimeLogEvent read fOnChannelRegisterTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about channel unregistration.
  }
    property OnChannelUnregisterTimeLogMulti: TMulticastChannelUnregisterTimeLogEvent read fOnChannelUnregisterTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about channel registration.
  }
    property OnChannelTimeLogMulti: TMulticastChannelTimeLogEvent read fOnChannelTimeLogMulti;
  {:
    Muticast event called to pass read data with time when parser finds log
    containing information about config value.
  }
    property OnConfigTimeLogMulti: TMulticastConfigTimeLogEvent read fOnConfigTimeLogMulti;
  {$IFDEF NoTimeLogEvents}
  {:
    Muticast event called to pass unprocessed data when parser finds log with
    data of unknown structure.
  }
    property OnDataLogMulti: TMulticastDataLogEvent read fOnDataLogMulti;
  {:
    Muticast event called to pass text when parser finds log containing only
    text or log containing data which can be expressed as text.
  }
    property OnTextLogMulti: TMulticastTextLogEvent read fOnTextLogMulti;
  {:
    Muticast event called to pass read data with time when parser fings log
    containing information about a write to game log.
  }
    property OnLogLogMulti: TMulticastLogEvent read fOnLogLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about game event registration.
  }
    property OnEventRegisterLogMulti: TMulticastEventRegisterEvent read fOnEventRegisterLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about game event unregistration.
  }
    property OnEventUnregisterLogMulti: TMulticastEventRegisterEvent read fOnEventUnregisterLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about game event.
  }
    property OnEventLogMulti: TMulticastEventEvent read fOnEventLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about channel registration.
  }
    property OnChannelRegisterLogMulti: TMulticastChannelRegisterEvent read fOnChannelRegisterLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about channel unregistration.
  }
    property OnChannelUnregisterLogMulti: TMulticastChannelUnregisterEvent read fOnChannelUnregisterLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about channel registration.
  }
    property OnChannelLogMulti: TMulticastChannelEvent read fOnChannelLogMulti;
  {:
    Muticast event called to pass read data when parser finds log containing
    information about config value.
  }
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
{:
  @abstract(Class designed to parse a file containing binary log.)
  It is implemented as direct descendant of TTelemetryLogBinaryStreamParser
  class, you can refer to it for more details.@br
  It creates TFileStream for requested file and passes it to inherited
  constructor.
}
  TTelemetryLogBinaryFileParser = class(TTelemetryLogBinaryStreamParser)
  private
  {:
    See FileName property for details.
  }
    fFileName: String;
  public
  {:
    Class constructor.

    Creates internal file stream and passes it to inherited constructor.

    @param(FileName Name of the input log file. Must contain valid file name,
                    but it is not mandatory to assign full file path.)
  }
    constructor Create(FileName: String);
  {:
    Class destructor.

    Frees internal file stream.
  }
    destructor Destroy; override;
  published
  {:
    Contains actual name of the read file.
  }
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
{:
  @abstract(Converts given binary log file to a text log file.)
  Implemented as descendant of TTelemetryLogBinaryFileParser class. It creates
  internal text logger and uses own methods to pass parsed data to it. It also
  creates dummy telemetry recipient using data from API information section
  because it is required by text logger.
}
  TTelemetryLogBinaryToTextConverter = class(TTelemetryLogBinaryFileParser)
  private
  {:
    See OutFileName property for details.
  }
    fOutFileName:     String;
  {:
    Telemetry recipient created for internal text logger. Parameters (telemetry
    version, etc.) required to create this object are obtained from input log
    file API information section.
  }
    fDummyRecipient:  TTelemetryRecipient;
  {:
    Object used to write text output.
  }
    fTextLogger:      TTelemetryLogText;
  protected
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: UInt32); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnLog(Sender: TObject; Time: TDateTime; LogType: scs_log_type_t; const LogText: String); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnEventRegister(Sender: TObject; Time: TDateTime; Event: scs_event_t); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnEventUnregister(Sender: TObject; Time: TDateTime; Event: scs_event_t); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnEvent(Sender: TObject; Time: TDateTime; Event: scs_event_t; Data: Pointer); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnChannelRegister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnChannelUnregister(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnChannel(Sender: TObject; Time: TDateTime; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); override;
  {:
    Passes received data to approprite text logger method.@br
    See @inherited for other functions of this method.
  }
    procedure DoOnConfig(Sender: TObject; Time: TDateTime; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t); override;
  public
  {:
    Class constructor.

    Creates dummy recipient and text logger.

    @param(FileName    Name of the input log file. Must contain valid file name,
                       but it is not mandatory to assign full file path.)
    @param(OutFileName Name of output text file. When left empty, then name
                       of output file is created by appending @code(.LOG)
                       extension to the name of the input file.)

    @raises(ETLUnsupportedGame When telemetry version, game and/or its version
                               are not supported by this class.)                       
  }
    constructor Create(FileName: String; OutFileName: String = '');
  {:
    Class destructor.

    Frees all internal objects.
  }
    destructor Destroy; override;
  {:
    Runs the conversions. Calls ReadAllLogEntries method.
  }
    procedure Convert; virtual;
  published
  {:
    Name of the output file, that is a file to which the text log is written.
  }
    property OutFileName: String read fOutFileName;
  end;

implementation

uses
  SysUtils,
  TelemetryConversions, TelemetryStreaming
{$IF Defined(FPC) and not Defined(Unicode)}
  (*
    If compiler throws error that LazUTF8 unit cannot be found, you have to
    add LazUtils to required packages (Project > Project Inspector).
  *)
  , LazUTF8
{$IFEND};

{$IFNDEF Documentation}
  {$DEFINE ImplementationPart}
    {$INCLUDE '..\Inc\TelemetryMulticastEvents.pas'}
  {$UNDEF ImplementationPart}
{$ENDIF}

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

procedure TTelemetryLogBinaryReader.IncLogCounter(Delta: Integer = 1);
begin
Inc(fLogCounter,Delta);
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryReader.Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
begin
inherited Create;
If Assigned(Stream) then fStream := Stream
  else raise ETLNilReference.Create('TTelemetryLogBinaryReader.Create: Stream not assigned.');
fFileInfo := FileInfo;
If TTelemetryInfoProvider.SupportsTelemetryAndGameVersion(FileInfo.APIInfo.TelemetryVersion,
                                                          FileInfo.APIInfo.GameID,
                                                          FileInfo.APIInfo.GameVersion) then
  fTelemetryInfoProvider := TTelemetryInfoProvider.Create(FileInfo.APIInfo.TelemetryVersion,
                                                          FileInfo.APIInfo.GameID,
                                                          FileInfo.APIInfo.GameVersion)
else
  raise ETLUnsupportedGame.CreateFmt('TTelemetryLogBinaryReader.Create: Game version (%s; %s %s) not supported.',
                                     [SCSGetVersionAsString(FileInfo.APIInfo.TelemetryVersion),
                                     TelemetryStringDecode(FileInfo.APIInfo.GameID),
                                     SCSGetVersionAsString(FileInfo.APIInfo.GameVersion)]);
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
while ReadNextLogEntry do {nothing};
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryReader_0                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_0 // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_0 // Protected methods                           }
{------------------------------------------------------------------------------}

Function TTelemetryLogBinaryReader_0.GetLogCount: Integer;
begin
Result := -1;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.GetLogEntry(Index: Integer): TLogEntry;
begin
Result := InvalidLogEntry;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.GetFlagValue(Flags, FlagMask: UInt8): Boolean;
begin
Result := (Flags and FlagMask) <> 0;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockHeader(var BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := Stream.Read(BlockHeader,SizeOf(TTelemetryLogBinaryBlockHeader)) >= SizeOf(TTelemetryLogBinaryBlockHeader);
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Invalid(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Generic(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  TempPtr:  Pointer;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If Assigned(fOnDataLog) then
      begin
        TempPtr := AllocMem(BlockHeader.BlockPayloadSize);
        try
          Stream.ReadBuffer(TempPtr^,BlockHeader.BlockPayloadSize);
          fOnDataLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),TempPtr,BlockHeader.BlockPayloadSize);
        finally
          FreeMem(TempPtr,BlockHeader.BlockPayloadSize);
        end;
      end
    else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
    Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Text(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= SizeOf(Integer) then
      begin
        If Assigned(fOnTextLog) then
          fOnTextLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),TelemetryStringDecode(Stream_ReadoutTelemetryString(Stream)))
        else
          Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Text: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,SizeOf(Integer)]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Log(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  LogType:  scs_log_type_t;
  TempStr:  TelemetryString;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= (SizeOf(scs_log_type_t) + TelemetryStringBytes) then
      begin
        If Assigned(fOnLogLog) then
          begin
            LogType := scs_log_type_t(Stream_ReadUInt32(Stream));
            TempStr := Stream_ReadoutTelemetryString(Stream);
            fOnLogLog(Self,TimeStampToDateTime(BlockHeader.BlockTime),LogType,TelemetryStringDecode(TempStr));
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Log: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,SizeOf(scs_log_type_t) + SizeOf(Integer)]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_EventReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize = SizeOf(scs_event_t) then
      begin
        If Assigned(fOnEventRegister) then
          fOnEventRegister(Self,TimeStampToDateTime(BlockHeader.BlockTime),scs_event_t(Stream_ReadUInt32(Stream)))
        else
          Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_EventReg: Erroneous block payload size (expected %d, got %d).',
                                    [SizeOf(scs_event_t),BlockHeader.BlockPayloadSize]);
  end
else Result := False;
end;
 
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_EventUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize = SizeOf(scs_event_t) then
      begin
        If Assigned(fOnEventUnregister) then
          fOnEventUnregister(Self,TimeStampToDateTime(BlockHeader.BlockTime),scs_event_t(Stream_ReadUInt32(Stream)))
        else
          Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_EventUnreg: Erroneous block payload size (expected %d, got %d).',
                                    [SizeOf(scs_event_t),BlockHeader.BlockPayloadSize]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Event(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  StartPos:   Int64;
  Event:      scs_event_t;
  Data_frm:   scs_telemetry_frame_start_t;
  Data_cfg:   scs_telemetry_configuration_t;
  Data:       Pointer;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= SizeOf(scs_event_t) then
      begin
        If Assigned(fOnEvent) then
          begin
            StartPos := Stream.Position;
            Event := scs_event_t(Stream_ReadUInt32(Stream));
            case Event of
          {*} SCS_TELEMETRY_EVENT_frame_start:
                begin
                  Stream.Read({%H-}Data_frm,SizeOf(scs_telemetry_frame_start_t));
                  Data := @Data_frm;
                end;
          {*} SCS_TELEMETRY_EVENT_configuration:
                begin
                  Stream_Read_scs_telemetry_configuration(Stream,Data_cfg,GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED));
                  Data := @Data_cfg;
                end;
            else
              Data := nil;
            end;
            try
              If (Stream.Position - StartPos) = BlockHeader.BlockPayloadSize then
                fOnEvent(Self,TimeStampToDateTime(BlockHeader.BlockTime),Event,Data)
              else
                raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Event: Stored payload size does not match (stored %d, reality %d)',
                                           [BlockHeader.BlockPayloadSize,Stream.Position - StartPos]);
            finally
              If Event = SCS_TELEMETRY_EVENT_configuration then
                scs_telemetry_configuration_free(Data_cfg,True);
            end;
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Event: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,SizeOf(scs_event_t)]);
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelReg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
const
  MinPayloadSize = SizeOf(TChannelID) + SizeOf(scs_u32_t) +
                   SizeOf(scs_value_type_t) + SizeOf(scs_u32_t);
var
  StartPos:         Int64;
  ChannelName:      TelemetryString;
  ChannelID:        TChannelID;
  ChannelIndex:     scs_u32_t;
  ChannelValueType: scs_value_type_t;
  ChannelRegFlags:  scs_u32_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= MinPayloadSize then
      begin
        If Assigned(fOnChannelRegister) then
          begin
            StartPos := Stream.Position;
            If GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY) then
              begin
                ChannelID := Stream_ReadUInt32(Stream);
                ChannelName := TelemetryInfoProvider.KnownChannels.ChannelIDToName(ChannelID);
              end
            else
              begin
                ChannelName := Stream_ReadoutTelemetryString(Stream);
                ChannelID := TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelName);
              end;
            ChannelIndex := Stream_ReadUint32(Stream);
            ChannelValueType := Stream_ReadUInt32(Stream);
            ChannelRegFlags := Stream_ReadUInt32(Stream);
            If (Stream.Position - StartPos) = BlockHeader.BlockPayloadSize then
              fOnChannelRegister(Self,TimeStampToDateTime(BlockHeader.BlockTime),ChannelName,ChannelID,ChannelIndex,ChannelValueType,ChannelRegFlags)
            else
              raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelReg: Stored payload size does not match (stored %d, reality %d)',
                                         [BlockHeader.BlockPayloadSize,Stream.Position - StartPos]);
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelReg: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,MinPayloadSize]);
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelUnreg(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
const
  MinPayloadSize = SizeOf(TChannelID) + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t);
var
  StartPos:         Int64;
  ChannelName:      TelemetryString;
  ChannelID:        TChannelID;
  ChannelIndex:     scs_u32_t;
  ChannelValueType: scs_value_type_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= MinPayloadSize then
      begin
        If Assigned(fOnChannelUnregister) then
          begin
            StartPos := Stream.Position;
            If GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY) then
              begin
                ChannelID := Stream_ReadUInt32(Stream);
                ChannelName := TelemetryInfoProvider.KnownChannels.ChannelIDToName(ChannelID);
              end
            else
              begin
                ChannelName := Stream_ReadoutTelemetryString(Stream);
                ChannelID := TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelName);
              end;
            ChannelIndex := Stream_ReadUInt32(Stream);
            ChannelValueType := Stream_ReadUInt32(Stream);
            If (Stream.Position - StartPos) = BlockHeader.BlockPayloadSize then
              fOnChannelUnregister(Self,TimeStampToDateTime(BlockHeader.BlockTime),ChannelName,ChannelID,ChannelIndex,ChannelValueType)
            else
              raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelUnreg: Stored payload size does not match (stored %d, reality %d)',
                                         [BlockHeader.BlockPayloadSize,Stream.Position - StartPos]);
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_ChannelUnreg: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,MinPayloadSize]);
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Channel(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
const
  MinPayloadSize = SizeOf(TChannelID) + SizeOf(scs_u32_t);
var
  StartPos:     Int64;
  ChannelData:  scs_named_value_localized_t;
  ChannelValue: scs_value_t;
begin
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= MinPayloadSize then
      begin
        If Assigned(fOnChannel) then
          begin
            StartPos := Stream.Position;
            Stream_Read_scs_named_value_Localized(
              Stream,
              ChannelData,
              GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED),
              GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_IDONLY),
              InfoProviderGetChannelNameFromID,
              TelemetryInfoProvider);
            If (Stream.Position - StartPos) = BlockHeader.BlockPayloadSize then
              begin
                ChannelValue := scs_value(ChannelData.Value);
                try
                  fOnChannel(Self,
                             TimeStampToDateTime(BlockHeader.BlockTime),
                             ChannelData.Name,
                             TelemetryInfoProvider.KnownChannels.ChannelNameToID(ChannelData.Name),
                             ChannelData.Index,
                             @ChannelValue);
                finally
                  scs_value_free(ChannelValue);
                end;
              end
            else
              raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Channel: Stored payload size does not match (stored %d, reality %d)',
                                         [BlockHeader.BlockPayloadSize,Stream.Position - StartPos]);
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Channel: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,MinPayloadSize]);
  end
else Result := False;
end;
         
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Config(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
var
  MinPayloadSize:   TMemSize;
  StartPos:         Int64;
  ConfigReference:  TConfigReference;
  ConfigIndex:      scs_u32_t;
  ConfigData:       scs_value_localized_t;
begin
MinPayloadSize := (2 * TelemetryStringBytes) + SizeOf(scs_u32_t);
If (Stream.Position + BlockHeader.BlockPayloadSize) <= Stream.Size then
  begin
    If BlockHeader.BlockPayloadSize >= MinPayloadSize then
      begin
        If Assigned(fOnConfig) then
          begin
            StartPos := Stream.Position;
            ConfigReference.ID := Stream_ReadoutTelemetryString(Stream);
            ConfigReference.Attribute := Stream_ReadoutTelemetryString(Stream);
            ConfigIndex := Stream_ReadUInt32(Stream);
            Stream_Read_scs_value_localized(Stream,ConfigData,GetFlagValue(BlockHeader.BlockFlags,LB_BLOCK_FLAG_MINIMIZED));
            If (Stream.Position - StartPos) = BlockHeader.BlockPayloadSize then
              fOnConfig(Self,TimeStampToDateTime(BlockHeader.BlockTime),ConfigReference,ConfigIndex,ConfigData)
            else
              raise ETLBadData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Config: Stored payload size does not match (stored %d, reality %d)',
                                         [BlockHeader.BlockPayloadSize,Stream.Position - StartPos]);
          end
        else Stream.Position := Stream.Position + BlockHeader.BlockPayloadSize;
        Result := True;
      end
    else raise ETLBufferTooSmall.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlockPayload_Config: Payload is too small (got %d, minimum %d).',
                                           [BlockHeader.BlockPayloadSize,MinPayloadSize]);
  end
else Result := False;
end;
        
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlockPayload_Termination(BlockHeader: TTelemetryLogBinaryBlockHeader): Boolean;
begin
Result := False;
end;
       
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadBlock: Boolean;
var
  BlockHeader:  TTelemetryLogBinaryBlockHeader;
begin
Result := False;
If not ReadingTerminated and ReadBlockHeader({%H-}BlockHeader) then
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
      raise ETLUnknownData.CreateFmt('TTelemetryLogBinaryReader_0.ReadBlock: Unknown block type (%d).',[BlockHeader.BlockType]);
    end;
  end;
ReadingTerminated := not Result;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_0 // Public methods                              }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryReader_0.StartReading;
begin
inherited;
end;
             
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader_0.EndReading;
begin
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadNextLogEntry(IncreaseLogCounter: Boolean = True): Boolean;
begin
try
  Result := ReadBlock;
except
  Result := False;
end;
If IncreaseLogCounter and Result then IncLogCounter;
end;
              
//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_0.ReadLogEntry(Index: Integer): Boolean; 
begin
Result := False;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryReader_1                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryReader_1 // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_1 // Protected methods                           }
{------------------------------------------------------------------------------}

Function TTelemetryLogBinaryReader_1.GetLogCount: Integer;
begin
Result := Length(fBlocksOffsets);
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_1.GetLogEntry(Index: Integer): TLogEntry;
var
  StreamPosition: Int64;
  BlockHeader:    TTelemetryLogBinaryBlockHeader;
begin
If (Index >= Low(fBlocksOffsets)) and (Index <= High(fBlocksOffsets)) then
  begin
    StreamPosition := Stream.Position;
    try
      Stream.Seek(fBlocksOffsets[Index],soBeginning);
      If ReadBlockHeader({%H-}BlockHeader) then
        begin
          Result.Time := TimeStampToDateTime(BlockHeader.BlockTime);
          SetLength(Result.Data,BlockHeader.BlockPayloadSize);
          Result.Info := (UInt32(BlockHeader.BlockFlags) shl 8) or BlockHeader.BlockType;
          If Length(Result.Data) > 0 then
            If Stream.Read(Result.Data[0],BlockHeader.BlockPayloadSize) <> BlockHeader.BlockPayloadSize then
              Result := InvalidLogEntry;
        end;
    finally
      Stream.Position := StreamPosition;
    end;
  end
else Result := InvalidLogEntry;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader_1.ReadBlocksOffsets;
begin
Stream.Seek(-(SizeOf(UInt32) + SizeOf(Int32)),soEnd);
SetLength(fBlocksOffsets,Stream_ReadInt32(Stream));
Stream.Seek(-(SizeOf(UInt32) + SizeOf(Int32) + Length(fBlocksOffsets) * SizeOf(UInt32)),soEnd);
If Length(fBlocksOffsets) > 0 then
  Stream.ReadBuffer(fBlocksOffsets[0],Length(fBlocksOffsets) * SizeOf(UInt32));
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryReader_1 // Public methods                              }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryReader_1.StartReading;
var
  StreamPosition: Int64;
begin
inherited;
SetLength(fBlocksOffsets,0);
StreamPosition := Stream.Position;
try
  // Check whether the file is large enough to accommodate table of blocks offsets.
  If (Stream.Size - Stream.Position) >= SizeOf(TTelemetryLogBinaryBlockHeader) + SizeOf(Int32) + SizeOf(UInt32) then
    begin
      // Check control number and read the table if it matches.
      Stream.Seek(-SizeOf(UInt32),soEnd);
      If Stream_ReadUInt32(Stream) = LB_STRUCT0_CONTROLNUMBER then ReadBlocksOffsets;
    end;
finally
  Stream.Position := StreamPosition;
end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryReader_1.EndReading;
begin
inherited;
SetLength(fBlocksOffsets,0);
end;

//------------------------------------------------------------------------------

Function TTelemetryLogBinaryReader_1.ReadLogEntry(Index: Integer): Boolean;
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
If (Stream.Size - Stream.Position) < (SizeOf(TTelemetryLogBinaryFileHeader) + MinimumAPIInfoSectionSize) then
  raise ETLBufferTooSmall.Create('TTelemetryLogBinaryStreamParser.ReadFileHeader: File is too small.');
fStream.ReadBuffer(FileInfo.Header,SizeOf(TTelemetryLogBinaryFileHeader));
If FileInfo.Header.MagicNumber <> LB_SIGNATURE then
  raise ETLBadData.Create('TTelemetryLogBinaryStreamParser.ReadFileHeader: Bad file signature.');
FileInfo.APIInfo.TelemetryVersion := Stream_ReadUInt32(fStream);
FileInfo.APIInfo.GameID := Stream_ReadoutTelemetryString(fStream);
FileInfo.APIInfo.GameVersion := Stream_ReadUInt32(fStream);
FileInfo.APIInfo.GameName := Stream_ReadoutTelemetryString(fStream);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStreamParser.DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: UInt32);
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

procedure TTelemetryLogBinaryStreamParser.DoOnConfig(Sender: TObject; Time: TDateTime; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t);
begin
If Assigned(fOnConfigTimeLog) then fOnConfigTimeLog(Self,Time,ConfigReference,Index,Value);
{$IFDEF NoTimeLogEvents}
If Assigned(fOnConfigLog) then fOnConfigLog(Self,ConfigReference,Index,Value);
{$ENDIF}
{$IFDEF MulticastEvents}
fOnConfigTimeLogMulti.Call(Self,Time,ConfigReference,Index,Value);
{$IFDEF NoTimeLogEvents}
fOnConfigLogMulti.Call(Self,ConfigReference,Index,Value);
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
  else raise ETLNilReference.Create('TTelemetryLogBinaryStreamParser.Create: Telemetry Recipient not assigned.');
fStream.Position := 0;
ReadFileHeader(fFileInfo);
case fFileInfo.Header.DataStructure of
  0:  fLogReader := TTelemetryLogBinaryReader_0.Create(Stream,fFileInfo);
  1:  fLogReader := TTelemetryLogBinaryReader_1.Create(Stream,fFileInfo);
else
  raise ETLUnknownData.CreateFmt('TTelemetryLogBinaryStreamParser.Create: Unknown data structure (%d).',[fFileInfo.Header.DataStructure]);
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
{$IF Defined(FPC) and not Defined(Unicode)}
inherited Create(TFileStream.Create(UTF8ToSys(FileName),fmOpenRead or fmShareDenyWrite));
{$ELSE}
inherited Create(TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite));
{$IFEND}
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

procedure TTelemetryLogBinaryToTextConverter.DoOnDataLog(Sender: TObject; Time: TDateTime; Data: Pointer; Size: UInt32);
begin
fTextLogger.Logger.AddLogTime(IntToStr(Size) + 'bytes of unknown data.',Time);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryToTextConverter.DoOnTextLog(Sender: TObject; Time: TDateTime; const Text: String);
begin
fTextLogger.Logger.AddLogTime(Text,Time);
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

procedure TTelemetryLogBinaryToTextConverter.DoOnConfig(Sender: TObject; Time: TDateTime; ConfigReference: TConfigReference; Index: scs_u32_t; Value: scs_value_localized_t);
begin
fTextLogger.Logger.ForceTimeSet(Time);
fTextLogger.LogConfig(ConfigReference,Index,Value);
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
If TTelemetryRecipient.SupportsTelemetryAndGameVersion(FileInfo.APIInfo.TelemetryVersion,
                                                       FileInfo.APIInfo.GameID,
                                                       FileInfo.APIInfo.GameVersion) then

  fDummyRecipient := TTelemetryRecipient.Create(FileInfo.APIInfo.TelemetryVersion,
                                                FileInfo.APIInfo.GameID,
                                                FileInfo.APIInfo.GameVersion,
                                                FileInfo.APIInfo.GameName)
else
  raise ETLUnsupportedGame.CreateFmt('TTelemetryLogBinaryToTextConverter.Create: Game version (%s; %s %s) not supported.',
                                     [SCSGetVersionAsString(FileInfo.APIInfo.TelemetryVersion),
                                     TelemetryStringDecode(FileInfo.APIInfo.GameID),
                                     SCSGetVersionAsString(FileInfo.APIInfo.GameVersion)]);
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
