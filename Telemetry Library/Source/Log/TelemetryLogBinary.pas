{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Contains classes designed to log telemetry API traffic to a binary
          output.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-18)
@lastmod(2015-07-10)

  @bold(@NoAutoLink(TelemetryLogBinary))

  ©František Milt, all rights reserved.

  This unit contains classes that are designed to write binary logs, namely:
@preformatted(
  TTelemetryLogBinaryWriter
   |- TTelemetryLogBinaryWriter_1
       |- TTelemetryLogBinaryWriter_0

  TTelemetryLogBinaryStream
   |- TTelemetryLogBinaryFile
)

  Last change: 2015-07-10

  Change List:@unorderedList(
    @item(2014-05-18 - First stable version.)
    @item(2014-10-24 - Small implementation changes.)
    @item(2014-11-05 - Type of parameter @code(Size) changed from signed to
                       unsigned integer in following functions:@unorderedList(
                        @itemSpacing(Compact)
                        @item(TTelemetryLogBinaryWriter.LogData)
                        @item(TTelemetryLogBinaryWriter_1.LogData)
                        @item(TTelemetryLogBinaryStream.LogData)))
    @item(2014-11-05 - Added parameter @code(UserData) to following methods:
                       @unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryLogBinaryStream.EventRegisterHandler)
                         @item(TTelemetryLogBinaryStream.EventUnregisterHandler)
                         @item(TTelemetryLogBinaryStream.EventHandler)
                         @item(TTelemetryLogBinaryStream.ChannelRegisterHandler)
                         @item(TTelemetryLogBinaryStream.ChannelUnregisterHandler)
                         @item(TTelemetryLogBinaryStream.ChannelHandler)))
    @item(2015-06-30 - Constant @code(cMinimumAPIInfoSectionSize) renamed to
                       MinimumAPIInfoSectionSize.)
    @item(2015-06-30 - Constant @code(LB_MAGIC_NUMBER) renamed to LB_SIGNATURE.)
    @item(2015-06-30 - Class @code(TTelemetryLogBinaryWriter_0_1) renamed to
                       TTelemetryLogBinaryWriter_1 as it now writes only
                       structure 1.)
    @item(2015-06-30 - Added class TTelemetryLogBinaryWriter_0 for writing
                       structure 0.)
    @item(2015-07-01 - Small implementation changes.)
    @item(2015-07-10 - Class of all exceptions changed to a proper internal
                       class.)
    @item(2015-07-10 - Added exceptions description into documentation.))

  ToDo:@unorderedList(
    @item(Add 64bit variant of structure 0 (64bit blocks offsets).)
    @item(Add structure that saves data in some compressed format.)
    @item(Optimize reallocation and saving of block offset table.))

@html(<hr>)

  @bold(Structure of binary log output created by classes from this unit)

  In the following text, a "file" is meant to be a binary log output created by
  classes from this unit (it might be a stream, not just file).

  Each file starts with a header of this structure:
@preformatted(
         value       |   size    |   value type

    Magic number        4 bytes     32b uint (must be equal to 0x6C624C54)
    Time of creation    8 bytes     TTimeStamp
    Data structure      2 bytes     16b uint
    Reserved field 1    4 bytes
    Reserved field 2    4 bytes
)
  Each file must start with this header and API information section, if it is
  smaller (minimum size of API info section is 16 bytes), then it is broken.
  Also, each valid file must start with valid magic number
  (@code(0x6C624C54)).@br
  Time of creation is the local system time when the header was saved.
  It is stored as TTimeStamp structure which is declared as follows:
@preformatted(
    value |   size   | value type

    Time    4 bytes     32b int
    Date    4 bytes     32b int
)
  ...where Time is number of milliseconds that have elapsed since midnight, and
  Date indicates the number of calendar days since the start of the calendar
  (the number of days since 1/1/0001 plus one).
  Data structure determines how content of the file looks like after the
  API information section.@br
  Do not use reserved fields and do not assume nothing about their content!

  After the header, API information are stored. They are not part of the header
  because they have variable size - you have to write or read each field at a
  time. They are be used when reading the file, for example when there is need
  to convert item IDs to valid item names. These information are stored as
  follows:
@preformatted(
       value          |   size   |  value type

    Telemetry version   4 bytes    scs_u32_t
    Game ID             variable   String
    Game version        4 bytes    scs_u32_t
    Game name           variable   Sring
)

  After that, actual log data are stored (note - there might be no data at
  all), their actual structure depends on selected data structure.@br
  Right now, two structures are implemented - structure 0 and structure 1.
  Structure 1 is exactly the same as structure 0, only it does not contain table
  of block offsets at the end, so it won't be described in detail - you can
  refer to structure 0 for information and just ommit blocks offsets info.

  Strings are stored in the file as following structure:
@preformatted(
    value  |   size   |  value type

    Length   4 bytes     32b int
    Chars    variable    Array of UTF8 chars (bytes)
)
  Length is the length of a stored string in bytes.@br
  Chars is the string itself. It is allways UTF8-encoded.

@html(<hr>)

  @bold(Structure 0)

  Data in structure 0 are stored as a stream of blocks of variable sizes.

  Each block starts with a header of this structure:
@preformatted(
           value       |  size   |   value type

    Block type           1 byte     8b uint
    Block flags          1 byte     8b uint
    Block time           8 bytes    TTimeStamp
    Block payload size   2 bytes    16b uint
)
  Block type denotes structure and kind of data stored inside payload of the
  block (see further).@br
  Currently, two block flags are defined, but some new can be added in the
  future. See further for details about idividual flags.@br
  Block time stores time when the block was added to the output (for
  technicalities, refer to file header @code(Time of creation) field, as it is
  stored the same way).@br
  Block payload size is size of data this block is holding in bytes. It can be
  zero. When reading the block, position in stream must advance exactly by the
  number of bytes stored in this field, no more, no less. When stored payload
  size indicates that reading should go beyond end of file, you have to stop
  reading and assume the file is corrupted from current block onwards. Also,
  when stored payload size does not correspond to actual data that should be
  stored (eg. size is smaller than space required for expected structure), you
  must stop reading.

  Each block type can be present any number of times in the output stream, or
  not present at all. All blocks must be saved in consecutive order, it is not
  allowed that any block is time-stamped earlier than block preceding it in the
  stream.

  After the block header, an optional payload is stored. Structure of payload
  data depends on block type. For structure 0, following block types are
  defined:
@unorderedList(
@item(Block type 0x00 (dec 000) - Invalid block
  @code((LB_BLOCK_TYPE_INVALID))

  This block does not contain any data.)

@item(Block type 0x01 (dec 001) - Generic block
  @code((LB_BLOCK_TYPE_GENERIC))

  Can contain any data of any size. Structure of data is not defined.@br
  @bold(Warning) - maximum size of data is 65535 bytes.)


@item(Block type 0x02 (dec 002) - Text block
  @code((LB_BLOCK_TYPE_TEXT))

  Contains one string. Nothing more.)

@item(Block type 0x03 (dec 003) - Log block
  @code((LB_BLOCK_TYPE_LOG))

  Contains information about a write to game log (console).

  Structure of payload:
@preformatted(
     value  |   size   |  value type

    LogType   4 bytes     scs_log_type_t
    LogText   variable    String
))


@item(Block type 0x04 (dec 004) - Event registration block
  @code((LB_BLOCK_TYPE_EVENTREG))

  Information about registered game event.

  Structure of payload:
@preformatted(
    value |   size   |  value type

    Event   4 bytes    scs_event_t
))

@item(Block type 0x05 (dec 005) - Event unregistration block
  @code((LB_BLOCK_TYPE_EVENTUNREG))

  Information about unregistered game event.

  Structure of payload:
@preformatted(
    value |   size   |  value type

    Event   4 bytes    scs_event_t
))

@item(Block type 0x06 (dec 006) - Event block @code((LB_BLOCK_TYPE_EVENT))

  Information about occuring game event.

  Structure of payload:
@preformatted(
    value      |   size   |  value type

    Event        4 bytes     scs_event_t
    Event data   variable
)
  Content of event data is not defined here. It depends on what event has
  occured, but as of now, there are only two possible structures that can be
  stored in event data field:
  @unorderedList(
  @item(For event @code(SCS_TELEMETRY_EVENT_frame_start), one
  @code(scs_telemetry_frame_start_t) structure is stored.)

  @item(For event @code(SCS_TELEMETRY_EVENT_configuration), data are stored
  using function Stream_Write_scs_telemetry_configuration or its alternatives,
  refer to this function for information about resulting data layout.)))

@item(Block type 0x07 (dec 007) - Channel registration block
  @code((LB_BLOCK_TYPE_CHANNELREG))

  Information about registered channel.

  Structure of payload:
@preformatted(
          value       |   size   |  value type

  When block flag ID only is not set:

    Channel name        variable    String
    Channel index       4 bytes     scs_u32_t
    Channel value type  4 bytes     scs_value_type_t
    Registration flags  4 byes      scs_u32_t

  When block flag ID only is set:

    Channel ID          4 bytes     TItemID
    Channel index       4 bytes     scs_u32_t
    Channel value type  4 bytes     scs_value_type_t
    Registration flags  4 byes      scs_u32_t
))

@item(Block type 0x08 (dec 008) - Channel unregistration block
  @code((LB_BLOCK_TYPE_CHANNELUNREG))

  Information about unregistered channel.

  Structure of payload:
@preformatted(
          value       |   size   |  value type

  When block flag ID only is not set:

    Channel name        variable    String
    Channel index       4 bytes     scs_u32_t
    Channel value type  4 bytes     scs_value_type_t

  When block flag ID only is set:

    Channel ID          4 bytes     TItemID
    Channel index       4 bytes     scs_u32_t
    Channel value type  4 bytes     scs_value_type_t
))

@item(Block type 0x09 (dec 009) - Channel block
  @code((LB_BLOCK_TYPE_CHANNEL))

  Information about occuring channel and its value.@br
  Data are stored using function Stream_Write_scs_named_value_localized or its
  alternatives, refer to this function for information regarding resulting data
  layout.
)

@item(Block type 0x0A (dec 010) - Config block
  @code((LB_BLOCK_TYPE_CONFIG))

  Contains information about stored config including its value.@br
  Data are stored using function Stream_Write_scs_named_value_localized or its
  alternatives, refer to this function for information regarding resulting data
  layout.
)

@item(Block type 0xFF (dec 255) - Termination block
  @code((LB_BLOCK_TYPE_TERMINATE))

  Should not contain any data. When the file is read sequentially, reading must
  stop on this block, even when not at the end of the file. This block is put
  directly in front of blocks offsets table.
))

  Defined block flags:
@unorderedList(
  @item(bit 0 (0x01) - Reserved@br
        Ignore this bit.)
  @item(bit 1 (0x02) - ID only @code((LB_BLOCK_FLAG_IDONLY))@br
        Indicating whether channel or config block is stored with ID instead of
        full name. Valid only for block types 4 through 8, ignored for other
        blocks.)
  @item(bit 2 (0x04) - Minimized @code((LB_BLOCK_FLAG_MINIMIZED))@br
        Indicating whether @code(scs_value_t) structure or its alternatives are
        stored minimized. Valid only for block types 4, 7 and 8, ignored for
        other blocks.)
  @item(bits 3 through 7 - Reserved@br
        Ignore these bits.)
)

  At the end of the file is stored table of blocks offsets. It contains offsets
  of all blocks stored in file from the start of the file, in bytes. This table
  is always directly preceded by a termination block, so the resulting structure
  is:
@preformatted(
          value       |   size   |  value type

    Termination block   12 bytes    TBlockHeader
    Offsets             variable    Array of 32b uint
    Count               4 Bytes     32b int
    Control number      4 bytes     32b uint (must be equal to 0x616F4C54)
)
  Termination block is there for cases when the file is read sequentially, so
  the reading does not continue into the table and data in it are not processed
  as blocks.@br
  Count contains number of items in offsets array. Items in this array are 32bit
  unsigned integers, so actual size of array in bytes is 4 * Count. This count
  can be 0.@br
  At the end of file, a control number is stored. It must be equal to
  @code(0x616F4C54), if it isn't, you must assume that the file does not
  actually contain table of block offsets and read it as it were a file of
  structure 1.
  Since the table is stored when the file is closed, it might happed that the
  table is not stored, for example when logger was prematurely terminated by an
  error or by user intervention.

  Structure 0 diagram:
  @html(<div align="center">)
  @image(..\..\AutoDocumentation\Docs\tlb_struct_0.png)
  @html(</div>)
  
@html(<hr>)

  @bold(Structure 1)

  Data in structure 1 are stored exactly the same as in structure 0. Only
  difference is, that structure 1 does not store table of block offsets at the
  end of file.@br
  You can refer to structure 0 definition for information about structure 1
  resulting layout, just ommit the offsets table. 

@html(<hr>)}
unit TelemetryLogBinary;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
  Classes,
{$ENDIF}
  TelemetryCommon,
  TelemetryIDs,
  TelemetryRecipient,
  TelemetryRecipientBinder,
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

const
  //:Magic number (file signature) for binary log output.
  LB_SIGNATURE             = $6C624C54;
  //:Control number for structure 0 table of blocks offsets.
  LB_STRUCT0_CONTROLNUMBER = $616F4C54;

  //:Identifier number for invalid block.
  LB_BLOCK_TYPE_INVALID       = $00;
  //:Identifier number for generic block.
  LB_BLOCK_TYPE_GENERIC       = $01;
  //:Identifier number for text block.
  LB_BLOCK_TYPE_TEXT          = $02;
  //:Identifier number for log block.
  LB_BLOCK_TYPE_LOG           = $03;
  //:Identifier number for event registration block.
  LB_BLOCK_TYPE_EVENTREG      = $04;
  //:Identifier number for event unregistration block.
  LB_BLOCK_TYPE_EVENTUNREG    = $05;
  //:Identifier number for event block.
  LB_BLOCK_TYPE_EVENT         = $06;
  //:Identifier number for channel registration block.
  LB_BLOCK_TYPE_CHANNELREG    = $07;
  //:Identifier number for channel unregistration block.
  LB_BLOCK_TYPE_CHANNELUNREG  = $08;
  //:Identifier number for channel block.
  LB_BLOCK_TYPE_CHANNEL       = $09;
  //:Identifier number for config block.
  LB_BLOCK_TYPE_CONFIG        = $0A;
  //:Identifier number for termination block.
  LB_BLOCK_TYPE_TERMINATE     = $FF;


  //:Flag mask for IDOnly block flag.
  LB_BLOCK_FLAG_IDONLY    = $02;
  //:Flag mask for Minimized block flag.
  LB_BLOCK_FLAG_MINIMIZED = $04;

  //:Minimum number of bytes required to store API information section.
  MinimumAPIInfoSectionSize = 16{bytes};

type
{:
  @abstract(Structure corresponding to a binary log output header.)
  Used to read/write binary output header and store information about a file.

  @member MagicNumber    File signature, must be equal to @code(0x6C624C54).
  @member TimeOfCreation Time of creation of the file.
  @member DataStructure  Determines how content of the file looks like.
  @member Reserved1      Not used.
  @member Reserved2      Not used.
}
  TTelemetryLogBinaryFileHeader = packed record
    MagicNumber:    LongWord;
    TimeOfCreation: TTimeStamp;
    DataStructure:  Word;
    Reserved1:      LongWord;
    Reserved2:      LongWord;
  end;
  //:Pointer to TTelemetryLogBinaryFileHeader structure.
  PTelemetryLogBinaryFileHeader = ^TTelemetryLogBinaryFileHeader;

{:
  Structure used to store information about telemetry API (versions, ID, ...).

  @member TelemetryVersion Version of telemetry API.
  @member GameID           ID of game that runs the API.
  @member GameVersion      API-specific version of game that runs the API.
  @member GameName         Name of the game. 
}
  TTelemetryLogBinaryAPIInfo = record
    TelemetryVersion: scs_u32_t;
    GameID:           TelemetryString;
    GameVersion:      scs_u32_t;
    GameName:         TelemetryString;
  end;
  //:Pointer to TTelemetryLogBinaryAPIInfo structure.
  PTelemetryLogBinaryAPIInfo = ^TTelemetryLogBinaryAPIInfo;

{:
  Structure used to store information about binary log file.

  @member Header  Information about a file.
  @member APIInfo Information about API used in file generation.
}
  TTelemetryLogBinaryFileInfo = record
    Header:   TTelemetryLogBinaryFileHeader;
    APIInfo:  TTelemetryLogBinaryAPIInfo;
  end;
  //:Pointer to TTelemetryLogBinaryFileInfo structure.
  PTelemetryLogBinaryFileInfo = ^TTelemetryLogBinaryFileInfo;

{:
  @abstract(Structure corresponding to a block header in output structures 0 and
            1.)
  Used to read/write block headers and to pass information between routines.

  @member BlockType        Type of block.
  @member BlockFlags       Flags of the block.
  @member BlockTime        Time when the block was saved.
  @member BlockPayloadSize Size of block payload (actual data the block stores).
}
  TTelemetryLogBinaryBlockHeader = packed record
    BlockType:        Byte;
    BlockFlags:       Byte;
    BlockTime:        TTimeStamp;
    BlockPayloadSize: Word;
  end;
  //:Pointer to TTelemetryLogBinaryBlockHeader structure.
  PTelemetryLogBinaryBlockHeader = ^TTelemetryLogBinaryBlockHeader;  

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryWriter                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryWriter // Class declaration                             }
{==============================================================================}
{:
  @abstract(Base class for all writers used to write binary logs.)
  This class is intended as sort of template (common ancestor) for all writer
  classes. These classes are used internaly to write different binary log
  structures, and should not be used outside of loggers. All descendants must
  implement all abstract methods. They also may add some new methods, but they
  should be used in loggers wery carefully and only if really needed.@br
  Log* methods all have @code(Recipient) parameter. This parameter must contain
  reference to a valid telemetry recipient, as its methods can be used in
  descendant classes for conversions or to obtain information required to write
  a log.@br
  All writing is done at current Stream position.
}
  TTelemetryLogBinaryWriter = class(TObject)
  private
  {:
    See Stream property for details.
  }
    fStream:          TStream;
  {:
  See FileInfo property for details.
  }
    fFileInfo:        TTelemetryLogBinaryFileInfo;
  {:
  See SaveMinimized property for details.
  }
    fSaveMinimized:   Boolean;
  {:
  See SaveItemIDOnly property for details.
  }
    fSaveItemIDOnly:  Boolean;
  public
  {:
    Class constructor.

    @param(Stream   @noAutoLink(Stream) to which the writer will write the log.
                    Must not be @nil. It must have file header and API
                    information already written in and position must be set
                    directly after them (not at the start of the
                    @noAutoLink(Stream)!).)
    @param(FileInfo Information about output file. It must be completely filled
                    by logger that is creating current instance in order for
                    this class to work properly. When not filled, the behavior
                    of this class is not defined.)

    @raises(ETLNilReference When parameter @noAutoLink(@code(Stream)) is not
                            assigned.)
  }
    constructor Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
  {:
    Method that must be called before any log is written. Its job is to prepare
    anything that is necesary for actual logging (variables initialization,
    ...).
  }
    procedure StartWriting; virtual; abstract;
  {:
    Method intended to write log containing generic data, that is, data without
    predefined structure.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Data      Pointer to data to be written. Must not be @nil.
    @param Size      Size of the data in bytes.
  }
    procedure LogData(Recipient: TTelemetryRecipient; Data: Pointer; Size: Word); virtual; abstract;
  {:
    Method intended to write log containing any text.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Str       Text to be written. Can be empty.
  }
    procedure LogStr(Recipient: TTelemetryRecipient; Str: TelemetryString); virtual; abstract;
  {:
    Method intended to write information about a write to game log (console).

    @param Recipient Telemetry recipient. Must contain valid object.
    @param LogType   Type of log written into game log (error, warning, ...).
    @param LogText   Actual text written into game log.
  }
    procedure LogLog(Recipient: TTelemetryRecipient; LogType: scs_log_type_t; const LogText: String); virtual; abstract;
  {:
    Method intended to write log containing information about game event
    registration.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Event     Event identifier number.
  }
    procedure LogEventRegister(Recipient: TTelemetryRecipient; Event: scs_event_t); virtual; abstract;
  {:
    Method intended to write log containing information about game event
    unregistration.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Event     Event identifier number.
  }
    procedure LogEventUnregister(Recipient: TTelemetryRecipient; Event: scs_event_t); virtual; abstract;
  {:
    Method intended to write log containing information about game event.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Event     Event identifier number.
    @param(Data      Pointer to data appended to currently logged event.
                     Can be @nil.)
  }
    procedure LogEvent(Recipient: TTelemetryRecipient; Event: scs_event_t; Data: Pointer); virtual; abstract;
  {:
    Method intended to write log containing information about channel
    registration.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Name      Name of registered telemetry channel.
    @param ID        ID of registered telemetry chanenl.
    @param Index     Index of registered telemetry channel.
    @param ValueType Type of value of registered telemetry channel.
    @param Flags     Registration flags.
  }
    procedure LogChannelRegister(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual; abstract;
  {:
    Method intended to write log containing information about channel
    unregistration.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Name      Name of unregistered telemetry channel.
    @param ID        ID of unregistered telemetry chanenl.
    @param Index     Index of unregistered telemetry channel.
    @param ValueType Type of value of unregistered telemetry channel.
  }
    procedure LogChannelUnregister(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual; abstract;
  {:
    Method intended to write log containing information about received channel
    value.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Name      Name of logged telemetry channel.
    @param ID        ID of logged  telemetry chanenl.
    @param Index     Index of logged telemetry channel.
    @param Value     Actual value of logged telemetry channel. Can be @nil.
  }
    procedure LogChannel(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual; abstract;
  {:
    Method intended to write log containing information about received config
    value.

    @param Recipient Telemetry recipient. Must contain valid object.
    @param Name      Name of logged config value.
    @param ID        ID of logged config value.
    @param Index     Index of logged config value.
    @param Value     Actual value of logged config value.
  }
    procedure LogConfig(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual; abstract;
  {:
    Method that must be called before destruction of instance. Its job is to
    free any allocated resources, write any buffered values or simply anything
    that has to be saved after the last log is written.
  }
    procedure EndWriting; virtual; abstract;
  {:
    Information about file that is being written. Filled in constructor.
  }
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
  published
  {:
    Contains reference to a @noAutoLink(stream) that is used for writing.
    Not initialized, filled in constructor.@br
    @bold(Warning) - do not change properties of this object!
  }
    property Stream: TStream read fStream;
  {:
    Determines whether some values are stored minimized. Refer to functions with
    parameter @code(Minimize(d)) in unit TelemetryStreaming (eg.
    Ptr_Write_scs_value) and to binary output documentation for details.
  }
    property SaveMinimized: Boolean read fSaveMinimized write fSaveMinimized;
  {:
    Determines whether item names are stored as full strings or only as IDs.
    Refer to functions with parameter @code(ItemIDOnly) in unit
    TelemetryStreaming (eg. Ptr_Write_scs_named_value) and to binary output
    documentation for details.
  }
    property SaveItemIDOnly: Boolean read fSaveItemIDOnly write fSaveItemIDOnly;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryWriter_1                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryWriter_1 // Class declaration                           }
{==============================================================================}
{:
  Writer class used to write binary log of structure 1.
}
  TTelemetryLogBinaryWriter_1 = class(TTelemetryLogBinaryWriter)
  protected
  {:
    Writes block header filled with information provided in parameters to
    Stream at current position.

    @param BlockType        Type of block.
    @param BlockFlags       Flags of block.
    @param BlockPayloadSize Size of block payload, in bytes. Can be zero.
  }
    procedure WriteBlockHeader(BlockType, BlockFlags: Byte; BlockPayloadSize: Word); virtual;
  {:
    Sets flag determined by @code(FlagMask) in @code(Flags) parameted to
    @code(Value).

    @param Flags    Set of flags that has to be changed.
    @param FlagMask Number determining which bit has to be changed.
    @param Value    New value for the flag.
  }
    procedure SetFlagValue(var Flags: Byte; FlagMask: Byte; Value: Boolean); virtual;
  public
  {:
    See @inherited.
  }
    procedure StartWriting; override;
  {:
    Writes an invalod block into output.
  }
    procedure LogInvalidBlock; virtual;
  {:
    Writes termination block into output.@br
    You can continue to write new blocks, but they will be ignored when the file
    is read sequentially.
  }
    procedure LogTerminationBlock; virtual;
  {:
    See @inherited.
  }
    procedure LogData({%H-}Recipient: TTelemetryRecipient; Data: Pointer; Size: Word); override;
  {:
    See @inherited.
  }
    procedure LogStr({%H-}Recipient: TTelemetryRecipient; Str: TelemetryString); override;
  {:
    See @inherited.
  }
    procedure LogLog({%H-}Recipient: TTelemetryRecipient; LogType: scs_log_type_t; const LogText: String); override;
  {:
    See @inherited.
  }
    procedure LogEventRegister({%H-}Recipient: TTelemetryRecipient; Event: scs_event_t); override;
  {:
    See @inherited.
  }
    procedure LogEventUnregister({%H-}Recipient: TTelemetryRecipient; Event: scs_event_t); override;
  {:
    See @inherited.
  }
    procedure LogEvent(Recipient: TTelemetryRecipient; Event: scs_event_t; Data: Pointer); override;
  {:
    See @inherited.
  }
    procedure LogChannelRegister({%H-}Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); override;
  {:
    See @inherited.
  }
    procedure LogChannelUnregister({%H-}Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); override;
  {:
    See @inherited.
  }
    procedure LogChannel(Recipient: TTelemetryRecipient; const Name: TelemetryString; {%H-}ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); override;
  {:
    See @inherited.
  }
    procedure LogConfig(Recipient: TTelemetryRecipient; const Name: TelemetryString; {%H-}ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  {:
    See @inherited.
  }
    procedure EndWriting; override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryWriter_0                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TTelemetryLogBinaryWriter_0 // Class declaration                          }
{==============================================================================}
{:
    Writer class used to write binary log of structure 0.
}
  TTelemetryLogBinaryWriter_0 = class(TTelemetryLogBinaryWriter_1)
  private
  {:
    Array used to store table of blocks offsets.
  }
    fBlocksOffsets: array of LongWord;
  protected
  {:
    Adds current Stream position as a new item into blocks offsets table
    (fBlocksOffsets array).
  }
    procedure AddBlockOffset; virtual;
  {:
    See @inherited.
    Adds new offset to the table of blocks offsets by calling AddBlockOffset
    method.
  }
    procedure WriteBlockHeader(BlockType, BlockFlags: Byte; BlockPayloadSize: Word); override;
  public
  {:
    See @inherited.@br
    Empties table of blocks offsets.
  }
    procedure StartWriting; override;
  {:
    See @inherited.@br
    Writes table of block offsets into output.
  }
    procedure EndWriting; override;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryStream                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryStream // Class declaration                             }
{==============================================================================}
{:
  @abstract(Class designed to log all traffic on telemetry API as a binary log
            into provided @noAutoLink(stream).)
  Only file header and API information are actually written in this class,
  everything else (log data) is written using instance of appropriate (for
  selected file structure) writer.@br
  *Handler methods all require valid telemetry @noAutoLink(recipient) object. It
  can be provided in Recipient property (prefered) or in @code(Sender) parameter
  of a method. If you fail to provide it, these methods will raise an exception.
}
  TTelemetryLogBinaryStream = class(TTelemetryRecipientBinder)
  private
  {:
    See Stream property for details.
  }
    fStream:          TStream;
  {:
    Holds reference to an object that is used to write selected file
    structure.@br
    This object is internal-only and is automatically managed.
  }
    fLogWriter:       TTelemetryLogBinaryWriter;
  {:
    See FileInfo property for details.
  }
    fFileInfo:        TTelemetryLogBinaryFileInfo;
  {:
    See SaveMinimized property for details.
  }
    fSaveMinimized:   Boolean;
  {:
    See SaveItemIDOnly property for details.
  }
    fSaveItemIDOnly:  Boolean;
  {:
    Setter for SaveMinimized property.@br
    It also sets appropriate property of internal log writer.

    @param Value New value of the property.
  }
    procedure SetSaveMinimized(Value: Boolean);
  {:
    Setter for SaveItemIDOnly property.@br
    It also sets appropriate property of internal log writer.

    @param Value New value of the property.
  }
    procedure SetSaveItemIDOnly(Value: Boolean);
  protected
  {:
    Fills passed structure with all information available. Called from the
    constructor. It sets all field of file header and API information. API info
    is taken from an object referenced by Recipient property - meaning you must
    pass valid telemetry @noAutoLink(recipient) to the constructor.

    @param FileInfo Structure that has to be filled.
  }
    procedure PrepareFileInfo(var FileInfo: TTelemetryLogBinaryFileInfo); virtual;
  {:
    Writes passed file header and API information section into Stream at
    current position.

    @param FileInfo Structure containing information that will be written.
  }
    procedure WriteFileHeader(FileInfo: TTelemetryLogBinaryFileInfo); virtual;
  public
  {:
    Class constructor.

    Creates internal log writer and starts writing.

    @param(Recipient     Telemetry @noAutoLink(recipient) that operates on API.
                         You must provide valid instance as API information
                         section will be filled usig its properties. Constructor
                         also assigns all handler methods to its event - you can
                         change that (deassign handlers, set Recipient property
                         to other instance, etc.) after the constructor returns,
                         but rememeber that handlers require valid
                         @noAutoLink(recipient) instance either in Recipient
                         property or passed in @code(Sender) paramater.)
    @param(Stream        @noAutoLink(Stream) to which the output will be
                         written.@br
                         Stream is cleared before any writing occurs.)
    @param(DataStructure Structure of output binary log. See binary log
                         documentation for details. If you pass unsupported
                         structure, the constructor raises an exception.)

    @raises(ETLNilReference When either of parameters @code(aRecipient) and
                            @noAutoLink(@code(Stream)) is not assigned.)
    @raises(ETLUnknownData When number passed in parameter @code(DataStructure)
                           is not known.)
  }
    constructor Create(aRecipient: TTelemetryRecipient; Stream: TStream; DataStructure: Word = 0);
  {:
    Class destructor.

    Ends writing and frees internal log writer.
  }
    destructor Destroy; override;
  {:
    Method adding generic data to output.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender  Object calling this method (must be of type
                   TTelemetryRecipient).)
    @param Data    Pointer to data to be written. Must not be @nil.
    @param Size    Size of the data in bytes.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure LogData(Sender: TObject; Data: Pointer; Size: Word); virtual;
  {:
    Method adding a text entry to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender  Object calling this method (must be of type
                   TTelemetryRecipient).)
    @param Str     Text to be written. Can be empty.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure LogStr(Sender: TObject; Str: TelemetryString); virtual;
  {:
    Method adding information about a write to game log (console).@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender  Object calling this method (must be of type
                   TTelemetryRecipient).)
    @param LogType Type of log written into game log (error, warning, ...).
    @param LogText Text written into game log.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String); override;
  {:
    Method adding information about game event registration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object calling this method (must be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param UserData User defined data stored in the event context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure EventRegisterHandler(Sender: TObject; Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Method adding information about game event unregistration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object calling this method (must be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param UserData User defined data stored in the event context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure EventUnregisterHandler(Sender: TObject; Event: scs_event_t; {%H-}UserData: Pointer); override;
  {:
    Method adding information about game event to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender   Object calling this method (must be of type
                    TTelemetryRecipient).)
    @param Event    Game event identification number.
    @param Data     Pointer to data accompanying the event. Can be @nil.
    @param UserData User defined data stored in the event context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; {%H-}UserData: Pointer); override;
  {:
    Method adding information about channel registration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object calling this method (must be of type
                     TTelemetryRecipient).)
    @param Name      Name of registered channel.
    @param ID        ID of registered channel.
    @param Index     Index of registered channel.
    @param ValueType Value type of registered channel.
    @param Flags     Registration flags.
    @param UserData  User defined data stored in the channel context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; {%H-}UserData: Pointer); override;
  {:
    Method adding information about channel unregistration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object calling this method (must be of type
                     TTelemetryRecipient).)
    @param Name      Name of unregistered channel.
    @param ID        ID of unregistered channel.
    @param Index     Index of unregistered channel.
    @param ValueType Value type of unregistered channel.
    @param UserData  User defined data stored in the channel context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; {%H-}UserData: Pointer); override;
  {:
    Method adding information about channel to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object calling this method (must be of type
                     TTelemetryRecipient).)
    @param Name      Name of the channel.
    @param ID        ID of the channel.
    @param Index     Index of the channel.
    @param Value     Actual value of the channel. Can be @nil.
    @param UserData  User defined data stored in the channel context.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; {%H-}UserData: Pointer); override;
  {:
    Method adding information about received configuration to the log.@br
    @bold(Note) - requires valid telemetry @noAutoLink(recipient).

    @param(Sender    Object calling this method (must be of type
                     TTelemetryRecipient).)
    @param Name      Name of the config.
    @param ID        ID of the config.
    @param Index     Index of the config.
    @param Value     Actual value of the config.

    @raises(ETLInvalidReference When no valid @noAutoLink(recipient) can be
                                obtained.)
  }
    procedure ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); override;
  {:
    Calls method LogHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to called method for details.
  }
    procedure LogLog(LogType: scs_log_type_t; const LogText: String); virtual;
  {:
    Calls method EventRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to called method for details.
  }
    procedure LogEventRegister(Event: scs_event_t); virtual;
  {:
    Calls method EventUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to called method for details.
  }
    procedure LogEventUnregister(Event: scs_event_t); virtual;
  {:
    Calls method EventHandler with unchanged parameters and @code(Sender) set to
    @nil. Refer to called method for details.
  }
    procedure LogEvent(Event: scs_event_t; Data: Pointer); virtual;
  {:
    Calls method ChannelRegisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to called method for details.
  }
    procedure LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t); virtual;
  {:
    Calls method ChannelUnregisterHandler with unchanged parameters and
    @code(Sender) set to @nil. Refer to called method for details.
  }
    procedure LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t); virtual;
  {:
    Calls method ChannelHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to called method for details.
  }
    procedure LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t); virtual;
  {:
    Calls method ConfigHandler with unchanged parameters and @code(Sender) set
    to @nil. Refer to called method for details.
  }
    procedure LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t); virtual;
  {:
    Information about file that is being written. Filled in constructor.
  }
    property FileInfo: TTelemetryLogBinaryFileInfo read fFileInfo;
  published
  {:
    Contains reference to a @noAutoLink(stream) that is used as output for
    writing the log.
  }
    property Stream: TStream read fStream;
  {:
    Determines whether some values are stored minimized. Refer to writer
    @link(TTelemetryLogBinaryWriter.SaveMinimized property) of the same name for
    details.
  }
    property SaveMinimized: Boolean read fSaveMinimized write SetSaveMinimized;
  {:
    Determines whether item names are stored as full strings or only as IDs.
    Refer to writer @link(TTelemetryLogBinaryWriter.SaveItemIDOnly property) of
    the same name for details.
  }
    property SaveItemIDOnly: Boolean read fSaveItemIDOnly write SetSaveItemIDOnly;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryLogBinaryFile                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryFile // Class declaration                               }
{==============================================================================}
{:
  @abstract(Class designed to log all traffic on telemetry API to a binary log
            file.)
  It is a direct descendant of TTelemetryLogBinaryStream class (it creates
  @code(TFileStream) and passes it as a @noAutoLink(@code(Stream)) parameter to
  TTelemetryLogBinaryStream), so it has all saving options the same and
  resulting data layout is also exactly the same.
}
  TTelemetryLogBinaryFile = class(TTelemetryLogBinaryStream)
  private
  {:
    See FileName property for details.
  }
    fFileName: String;
  public
  {:
    Class constructor.

    For details about @noAutoLink(@code(Recipient)) parameter, see
    @link(TTelemetryLogBinaryStream.Create inherited constructor).

    @param(Recipient      Telemetry recipient that will be logged. Must not be
                          @nil.)
    @param(FileName       Name of the output log file. It is not mandatory to
                          assign full file path (file should be saved to current
                          directory if no path is provided), but it is a good
                          practice.@br
                          When this parameter contains an empty string, then
                          output file is created in the same folder where a
                          module (exe/dll) containing this unit is placed, with
                          the name set to the name of that module (without
                          extension) followed by the time the file was created,
                          and with .tbl extension.)
    @param DataStructure  Structure of output.
  }
    constructor Create(aRecipient: TTelemetryRecipient; FileName: String = ''; DataStructure: Word = 0);
  {:
    Class destructor.
  }
    destructor Destroy; override;
  published
  {:
    Contains actual name of output file (file to which the log is written).
  }
    property FileName: String read fFileName;
  end;


implementation

uses
  TelemetryConversions, TelemetryStreaming;

  
{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryWriter                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryWriter // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryWriter // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryWriter.Create(Stream: TStream; FileInfo: TTelemetryLogBinaryFileInfo);
begin
inherited Create;
If Assigned(Stream) then fStream := Stream
  else raise ETLNilReference.Create('TTelemetryLogBinaryWriter.Create: Stream not assigned.');
fFileInfo := FileInfo;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryLogBinaryWriter_1                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryWriter_1 // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryWriter_1 // Protected methods                           }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryWriter_1.WriteBlockHeader(BlockType, BlockFlags: Byte; BlockPayloadSize: Word);
var
  TempBlockHeader:  TTelemetryLogBinaryBlockHeader;
begin
TempBlockHeader.BlockType := BlockType;
TempBlockHeader.BlockFlags := BlockFlags;
TempBlockHeader.BlockTime := DateTimeToTimeStamp(Now);
TempBlockHeader.BlockPayloadSize := BlockPayloadSize;
Stream.WriteBuffer(TempBlockHeader,SizeOf(TTelemetryLogBinaryBlockHeader));
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.SetFlagValue(var Flags: Byte; FlagMask: Byte; Value: Boolean);
begin
If Value then
  Flags := Flags or FlagMask
else
  Flags := Flags and not FlagMask;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryWriter_1 // Public methods                              }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryWriter_1.StartWriting;
begin
// nothing to do here
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogInvalidBlock;
begin
WriteBlockHeader(LB_BLOCK_TYPE_INVALID,0,0);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogTerminationBlock;
begin
WriteBlockHeader(LB_BLOCK_TYPE_TERMINATE,0,0);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogData(Recipient: TTelemetryRecipient; Data: Pointer; Size: Word);
begin
WriteBlockHeader(LB_BLOCK_TYPE_GENERIC,0,Size);
Stream.WriteBuffer(Data^,Size);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogStr(Recipient: TTelemetryRecipient; Str: TelemetryString);
begin
WriteBlockHeader(LB_BLOCK_TYPE_TEXT,0,SizeOfString(Str));
Stream_WriteString(Stream,Str);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogLog(Recipient: TTelemetryRecipient; LogType: scs_log_type_t; const LogText: String);
var
  TempStr:  TelemetryString;
begin
TempStr := TelemetryStringEncode(LogText);
WriteBlockHeader(LB_BLOCK_TYPE_LOG,0,SizeOf(scs_log_type_t) + SizeOfString(TempStr));
Stream_WriteInteger(Stream,LongInt(LogType));
Stream_WriteString(Stream,TempStr);
end;
     
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogEventRegister(Recipient: TTelemetryRecipient; Event: scs_event_t);
begin
WriteBlockHeader(LB_BLOCK_TYPE_EVENTREG,0,SizeOf(scs_event_t));
Stream_WriteInteger(Stream,LongInt(Event));
end;
      
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogEventUnregister(Recipient: TTelemetryRecipient; Event: scs_event_t);
begin
WriteBlockHeader(LB_BLOCK_TYPE_EVENTUNREG,0,SizeOf(scs_event_t));
Stream_WriteInteger(Stream,LongInt(Event));
end;
      
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogEvent(Recipient: TTelemetryRecipient; Event: scs_event_t; Data: Pointer);
var
  DataSize:   Word;
  BlockFlags: Byte;
begin
BlockFlags := 0;
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_IDONLY,SaveItemIDOnly);
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_MINIMIZED,SaveMinimized);
If Assigned(Data) then
  begin
    case Event of
      SCS_TELEMETRY_EVENT_frame_start:
        begin
          DataSize := SizeOf(scs_telemetry_frame_start_t);
          WriteBlockHeader(LB_BLOCK_TYPE_EVENT,BlockFlags,SizeOf(scs_event_t) + DataSize);
          Stream_WriteInteger(Stream,LongInt(Event));
          Stream.WriteBuffer(Data^,DataSize);
        end;
      SCS_TELEMETRY_EVENT_configuration:
        begin
          DataSize := Size_scs_telemetry_configuration(p_scs_telemetry_configuration_t(Data)^,SaveMinimized,SaveItemIDOnly);
          WriteBlockHeader(LB_BLOCK_TYPE_EVENT,BlockFlags,SizeOf(scs_event_t) + DataSize);
          Stream_WriteInteger(Stream,LongInt(Event));
          Stream_Write_scs_telemetry_configuration(Stream,p_scs_telemetry_configuration_t(Data)^,SaveMinimized,SaveItemIDOnly,RecipientGetConfigIDFromName,Recipient);
        end;
    end;
  end
else
  begin
    WriteBlockHeader(LB_BLOCK_TYPE_EVENT,BlockFlags,SizeOf(scs_event_t));
    Stream_WriteInteger(Stream,LongInt(Event));
  end;
end;
     
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogChannelRegister(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
var
  BlockFlags:  Byte;
begin
BlockFlags := 0;
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_IDONLY,SaveItemIDOnly);
If SaveItemIDOnly then
  begin
    WriteBlockHeader(LB_BLOCK_TYPE_CHANNELREG,BlockFlags,SizeOf(TItemID) + 2 * SizeOf(scs_u32_t) + SizeOf(scs_value_type_t));
    Stream_WriteInteger(Stream,LongInt(ID));
  end
else
  begin
    WriteBlockHeader(LB_BLOCK_TYPE_CHANNELREG,BlockFlags,SizeOfString(Name) + 2 * SizeOf(scs_u32_t) + SizeOf(scs_value_type_t));
    Stream_WriteString(Stream,Name);
  end;
Stream_WriteInteger(Stream,LongInt(Index));
Stream_WriteInteger(Stream,LongInt(ValueType));
Stream_WriteInteger(Stream,LongInt(Flags));
end;
     
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogChannelUnregister(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
var
  BlockFlags:  Byte;
begin
BlockFlags := 0;
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_IDONLY,SaveItemIDOnly);
If SaveItemIDOnly then
  begin
    WriteBlockHeader(LB_BLOCK_TYPE_CHANNELUNREG,BlockFlags,SizeOf(TItemID) + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t));
    Stream_WriteInteger(Stream,LongInt(ID));
  end
else
  begin
    WriteBlockHeader(LB_BLOCK_TYPE_CHANNELUNREG,BlockFlags,SizeOfString(Name) + SizeOf(scs_u32_t) + SizeOf(scs_value_type_t));
    Stream_WriteString(Stream,Name);
  end;
Stream_WriteInteger(Stream,LongInt(Index));
Stream_WriteInteger(Stream,LongInt(ValueType));
end;
           
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogChannel(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
var
  TempValue:  scs_named_value_localized_t;
  BlockFlags: Byte;
begin
BlockFlags := 0;
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_IDONLY,SaveItemIDOnly);
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_MINIMIZED,SaveMinimized);
If Assigned(Value) then
  begin
    TempValue.Name := Name;
    TempValue.Index := Index;
    TempValue.Value := scs_value_localized(Value^);
    WriteBlockHeader(LB_BLOCK_TYPE_CHANNEL,BlockFlags,Size_scs_named_value_localized(TempValue,SaveMinimized,SaveItemIDOnly));
    Stream_Write_scs_named_value_localized(Stream,TempValue,SaveMinimized,SaveItemIDOnly,RecipientGetChannelIDFromName,Recipient);
  end;
end;
         
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.LogConfig(Recipient: TTelemetryRecipient; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  TempValue:  scs_named_value_localized_t;
  BlockFlags: Byte;
begin
BlockFlags := 0;
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_IDONLY,SaveItemIDOnly);
SetFlagValue(BlockFlags,LB_BLOCK_FLAG_MINIMIZED,SaveMinimized);
TempValue.Name := Name;
TempValue.Index := Index;
TempValue.Value := Value;
WriteBlockHeader(LB_BLOCK_TYPE_CONFIG,BlockFlags,Size_scs_named_value_localized(TempValue,SaveMinimized,SaveItemIDOnly));
Stream_Write_scs_named_value_localized(Stream,TempValue,SaveMinimized,SaveItemIDOnly,RecipientGetConfigIDFromName,Recipient);
end;
          
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_1.EndWriting;
begin
// nothing to do here
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryWriter_0                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryWriter_0 // Class implementation                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryWriter_0 // Protected methods                           }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryWriter_0.AddBlockOffset;
begin
SetLength(fBlocksOffsets,Length(fBlocksOffsets) + 1);
fBlocksOffsets[High(fBlocksOffsets)] := Stream.Position;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_0.WriteBlockHeader(BlockType, BlockFlags: Byte; BlockPayloadSize: Word);
begin
AddBlockOffset;
inherited;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryWriter_0 // Public methods                              }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryWriter_0.StartWriting;
begin
SetLength(fBlocksOffsets,0);
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryWriter_0.EndWriting;
var
  i:  Integer;
begin
inherited;
LogTerminationBlock;
For i := Low(fBlocksOffsets) to High(fBlocksOffsets) do
  Stream_WriteInteger(Stream,LongInt(fBlocksOffsets[i]));
Stream_WriteInteger(Stream,Length(fBlocksOffsets));
Stream_WriteInteger(Stream,LongInt(LB_STRUCT0_CONTROLNUMBER));
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TTelemetryLogBinaryStream                           }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryStream // Class implementation                          }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStream // Constants, types, variables, etc...           }
{------------------------------------------------------------------------------}

const
  // Default (initial) values for TTelemetryLogBinaryStream properties.
  def_SaveMinimized  = True;
  def_SaveItemIDOnly = False;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStream // Private methods                               }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryStream.SetSaveMinimized(Value: Boolean);
begin
fSaveMinimized := Value;
fLogWriter.SaveMinimized := Value;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.SetSaveItemIDOnly(Value: Boolean);
begin
fSaveItemIDOnly := Value;
fLogWriter.SaveItemIDOnly := Value;
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStream // Protected methods                             }
{------------------------------------------------------------------------------}

procedure TTelemetryLogBinaryStream.PrepareFileInfo(var FileInfo: TTelemetryLogBinaryFileInfo);
begin
FileInfo.Header.MagicNumber := LB_SIGNATURE;
FileInfo.Header.TimeOfCreation := DateTimeToTimeStamp(Now);
FileInfo.Header.DataStructure := 0;
FileInfo.Header.Reserved1 := 0;
FileInfo.Header.Reserved2 := 0;
FileInfo.APIInfo.TelemetryVersion := Recipient.TelemetryVersion;
FileInfo.APIInfo.GameID := Recipient.GameID;
FileInfo.APIInfo.GameVersion := Recipient.GameVersion;
FileInfo.APIInfo.GameName := Recipient.GameName;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.WriteFileHeader(FileInfo: TTelemetryLogBinaryFileInfo);
begin
fStream.WriteBuffer(FileInfo.Header,SizeOf(TTelemetryLogBinaryFileHeader));
Stream_WriteInteger(fStream,LongInt(FileInfo.APIInfo.TelemetryVersion));
Stream_WriteString(fStream,FileInfo.APIInfo.GameID);
Stream_WriteInteger(fStream,LongInt(FileInfo.APIInfo.GameVersion));
Stream_WriteString(fStream,FileInfo.APIInfo.GameName);
end;

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryStream // Public methods                                }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryStream.Create(aRecipient: TTelemetryRecipient; Stream: TStream; DataStructure: Word = 0);
begin
If not Assigned(aRecipient) then
  raise ETLNilReference.Create('TTelemetryLogBinaryStream.Create: Telemetry recipient not assigned.');
inherited Create(aRecipient);
If Assigned(Stream) then fStream := Stream
  else raise ETLNilReference.Create('TTelemetryLogBinaryStream.Create: Stream not assigned.');
fStream.Size := 0;
PrepareFileInfo(fFileInfo);
fFileInfo.Header.DataStructure := DataStructure;
case DataStructure of
  0:  fLogWriter := TTelemetryLogBinaryWriter_0.Create(Stream,fFileInfo);
  1:  fLogWriter := TTelemetryLogBinaryWriter_1.Create(Stream,fFileInfo);
else
  raise ETLUnknownData.Create('TTelemetryLogBinaryStream.Create: Unknown data structure (' + IntToStr(DataStructure) + ').');
end;
SaveMinimized := def_SaveMinimized;
SaveItemIDOnly := def_SaveItemIDOnly;
WriteFileHeader(fFileInfo);
fLogWriter.StartWriting;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryStream.Destroy;
begin
If Assigned(fLogWriter) then
  fLogWriter.EndWriting;
fLogWriter.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogData(Sender: TObject; Data: Pointer; Size: Word);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogData(WorkRecipient,Data,Size)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ConfigHandler: No valid recipient.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogStr(Sender: TObject; Str: TelemetryString);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogStr(WorkRecipient,Str)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ConfigHandler: No valid recipient.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogHandler(Sender: TObject; LogType: scs_log_type_t; const LogText: String);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogLog(WorkRecipient,LogType,LogText)
else
  raise Exception.Create('TTelemetryLogBinaryStream.LogHandler: No valid recipient.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.EventRegisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogEventRegister(WorkRecipient,Event)
else
  raise Exception.Create('TTelemetryLogBinaryStream.EventRegisterHandler: No valid recipient.');
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.EventUnregisterHandler(Sender: TObject; Event: scs_event_t; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogEventUnregister(WorkRecipient,Event)
else
  raise Exception.Create('TTelemetryLogBinaryStream.EventUnregisterHandler: No valid recipient.');
end;
  
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.EventHandler(Sender: TObject; Event: scs_event_t; Data: Pointer; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogEvent(WorkRecipient,Event,Data)
else
  raise Exception.Create('TTelemetryLogBinaryStream.EventHandler: No valid recipient.');
end;
       
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.ChannelRegisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogChannelRegister(WorkRecipient,Name,ID,Index,ValueType,Flags)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ChannelRegisterHandler: No valid recipient.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.ChannelUnregisterHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogChannelUnregister(WorkRecipient,Name,ID,Index,ValueType)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ChannelUnregisterHandler: No valid recipient.');
end;
     
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.ChannelHandler(Sender: TObject; const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t; UserData: Pointer);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogChannel(WorkRecipient,Name,ID,Index,Value)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ChannelHandler: No valid recipient.');
end;
    
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.ConfigHandler(Sender: TObject; const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
var
  WorkRecipient: TTelemetryRecipient;
begin
If GetWorkingRecipient(Sender,WorkRecipient) then
  fLogWriter.LogConfig(WorkRecipient,Name,ID,Index,Value)
else
  raise Exception.Create('TTelemetryLogBinaryStream.ConfigHandler: No valid recipient.');
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogLog(LogType: scs_log_type_t; const LogText: String);
begin
LogHandler(nil,LogType,LogText);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogEventRegister(Event: scs_event_t);
begin
EventRegisterHandler(nil,Event,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogEventUnregister(Event: scs_event_t);
begin
EventUnregisterHandler(nil,Event,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogEvent(Event: scs_event_t; Data: Pointer);
begin
EventHandler(nil,Event,Data,nil);
end;
 
//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogChannelRegister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t);
begin
ChannelRegisterHandler(nil,Name,ID,Index,ValueType,Flags,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogChannelUnregister(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; ValueType: scs_value_type_t);
begin
ChannelUnregisterHandler(nil,Name,ID,Index,ValueType,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogChannel(const Name: TelemetryString; ID: TChannelID; Index: scs_u32_t; Value: p_scs_value_t);
begin
ChannelHandler(nil,Name,ID,Index,Value,nil);
end;

//------------------------------------------------------------------------------

procedure TTelemetryLogBinaryStream.LogConfig(const Name: TelemetryString; ID: TConfigID; Index: scs_u32_t; Value: scs_value_localized_t);
begin
ConfigHandler(nil,Name,ID,Index,Value);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryLogBinaryFile                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryLogBinaryFile // Class implementation                            }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryLogBinaryFile // Public methods                                  }
{------------------------------------------------------------------------------}

constructor TTelemetryLogBinaryFile.Create(aRecipient: TTelemetryRecipient; FileName: String = ''; DataStructure: Word = 0);
var
  TempStream: TFileStream;
begin
If FileName = '' then
  FileName := ExtractFilePath(GetModuleName(hInstance)) +
              ChangeFileExt(ExtractFileName(GetModuleName(hInstance)),'') +
              '_' + FormatDateTime('yyyy-mm-dd-hh-nn-ss',Now) + '.tbl';
TempStream := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
inherited Create(aRecipient,TempStream,DataStructure);
fFileName := FileName;
end;

//------------------------------------------------------------------------------

destructor TTelemetryLogBinaryFile.Destroy;
var
  TempStream: TStream;
begin
TempStream := Stream;
inherited;
TempStream.Free;
end;

end.
