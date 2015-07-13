{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Types, constants, routines, etc. used troughout the Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-04)
@lastmod(2015-07-12)

  @bold(@NoAutoLink(TelemetryCommon))

  ©2013-2015 František Milt, all rights reserved.

  This file is intended to provide types, constants, routines, etc. used
  throughout the Telemetry library (that is, in more than one unit). It also
  declares internal exception classes, specifically:
@preformatted(
  ETLException
   |- ETLUnsupportedAPI
   |- ETLUnsupportedGame
   |- ETLIndexOfBounds
   |- ETLNilReference
   |- ETLInvalidReference
   |- ETLUnknownData
   |- ETLBufferTooSmall
   |- ETLRegFailed
   |- ETLInitFailed
   |- ETLBadData
)
  Last change: 2015-07-12

  Change List:@unorderedList(
    @item(2013-10-04 - First stable version.)
    @item(2014-04-06 - TGameSupportInfo.GameID and
                       scs_value_localized_t.StringData fields type changed to
                       @code(TelemetryString).)
    @item(2014-04-18 - cConfigFieldsSeparator constant moved to TelemetryIDs
                       unit.)
    @item(2014-04-20 - Added following types:@unorderedList(
                         @itemSpacing(Compact)
                         @item(scs_named_value_localized_t)
                         @item(p_scs_named_value_localized_t)
                         @item(scs_telemetry_configuration_localized_t)
                         @item(p_scs_telemetry_configuration_localized_t)))                          
    @item(2014-04-20 - Functions scs_value_localized and scs_value moved to
                       TelemetryStreaming unit.)
    @item(2014-05-05 - TMulticastEvent placeholder added.)
    @item(2014-11-02 - Added types @code(PtrInt) and @code(PtrUInt).)
    @item(2015-04-20 - Added TMemSize type.)
    @item(2015-04-20 - Constants @code(cEmptySCSValue) and
                       @code(cEmptySCSValueLocalized) renamed to
                       @code(EmptySCSValue) and @code(EmptySCSValueLocalized)
                       respectively.)
    @item(2015-06-25 - Removed TMulticastEvent placeholder.)
    @item(2015-06-25 - Added TStrSize type.)
    @item(2015-06-25 - TGameSupportInfo changed to TSupportedGame,
                       PGameSupportInfo changed to PSupportedGame.)
    @item(2015-06-25 - Added list of supported API versions and list of
                       supported games (SupportedTelemetryVersions,
                       SupportedGames).)
    @item(2015-06-25 - Renamed and completed following constants:@unorderedList(
                         @itemSpacing(Compact)
                         @item(@code(EmptySCSValue) renamed to scs_value_empty)
                         @item(@code(EmptySCSValueLocalized) renamed to
                               scs_value_localized_empty)))
    @item(2015-06-29 - Returned TMulticastEvent placeholder.)
    @item(2015-07-10 - Added declaration of internal exception classes.)
    @item(2015-07-12 - Added documentation for exception classes.))

@html(<hr>)}
unit TelemetryCommon;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
{$ENDIF} 
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_eut2,
  scssdk_telemetry_eut2;
{$ENDIF}

type
{:
  @abstract(Common ancestor for all exception classes in the library.)

  Can be raised on error that does not fit any of the specific descendant
  exception classes. All exceptions raised in the Telemetry library are of this
  class or one of its descendant.

  Note - exception of other class can be raised within any call, simply because
         it is using RTL functions and other libraries that are not actual part
         of Telemetry library.
}
  ETLException        = class(Exception);
{:
  Raised when there is a need to operate on telemetry API but its version is not
  supported.
}
  ETLUnsupportedAPI   = class(ETLException);
{:
  Raised when there is need to operate on telemetry provided by a game that is
  not supported.
}
  ETLUnsupportedGame  = class(ETLException);
{:
  Raised when passed index is out of allowed boundary (arrays, lists, ...).
}
  ETLIndexOfBounds    = class(ETLException);
{:
  Raised when a nil reference (pointer or object) is passed to where a valid
  reference is required.  
}
  ETLNilReference     = class(ETLException);
{:
  Raised when passed reference is of wrong type or class, or is generally
  invalid.
}
  ETLInvalidReference = class(ETLException);
{:
  Raised when code needs to work with some data but they are of unknown type or
  structure.
}
  ETLUnknownData      = class(ETLException);
{:
  Raised when a memory block or file is smaler than necessary.
}
  ETLBufferTooSmall   = class(ETLException);
{:
  Raised when registration of telemetry event or channel fails.
}
  ETLRegFailed        = class(ETLException);
{:
  Raised when an initialization of some part of the code fails.
}
  ETLInitFailed       = class(ETLException);
{:
  Raised when data the library works with are in some way corrupted or invalid.
}
  ETLBadData          = class(ETLException);


{$IFDEF Documentation}
  {$IFDEF IncludeMulticastEventHandlers}
  {:
    @abstract(Placeholder intended to complete the classes hierarchy tree in
    documentation.)
    Actual class is defined in unit MulticastEvent and is not included in
    documentation of telemetry library. Refer to mentioned unit located in
    folder @italic(Source\Libs) for details.
  }  
    TMulticastEvent = class(TObject);
  {$ENDIF}
{$ENDIF}

  //:Type used to cast pointer to a signed integer for calculation of arbitrary
  //:address.
{$IFDEF x64}
  PtrInt  = Int64;
{$ELSE}
  PtrInt  = LongInt;
{$ENDIF}

  //:Type used to cast pointer to an unsigned integer for calculation of
  //:arbitrary address.
{$IFDEF x64}
  PtrUInt = UInt64;
{$ELSE}
  PtrUInt = LongWord;
{$ENDIF}

  //:Type used to pass or get size of memory, e.g. when allocating memory.
  TMemSize = PtrUInt;

  //:Type used to pass or get length of a string.
  TStrSize = PtrInt;

{:
  Structure used in lists of supported games and their versions.

  @member GameID       Internal game identificator (not a game name).
  @member(GameVersion  Internal game version (API-specific value - not equal
                       to actual game version).)
}
  TSupportedGame = record
    GameID:       TelemetryString;
    GameVersion:  scs_u32_t;
  end;
  //:Pointer to TGameSupportInfo structure.
  PSupportedGame = ^TSupportedGame;

const
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Remember to update.'}
{:
  These constants can change with telemetry development, remember to update them
  if you add support for new telemetry version.
}
{$ENDIF}

  //:List of Telemetry API versions supported by this library.
  SupportedTelemetryVersions: Array[0..0] of scs_u32_t =
   (SCS_TELEMETRY_VERSION_1_00 {1.0});

  //:List of games and their versions supported by this library.
  SupportedGames: Array[0..10] of TSupportedGame =
   ((GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_00 {EUT2 1.0}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_01 {EUT2 1.1}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_02 {EUT2 1.2}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_03 {EUT2 1.3}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_04 {EUT2 1.4}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_05 {EUT2 1.5}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_06 {EUT2 1.6}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_07 {EUT2 1.7}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_08 {EUT2 1.8}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_09 {EUT2 1.9}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_10 {EUT2 1.10}));

type
{:
  @abstract(Structure used to store content of @code(scs_value_t) variable.)
  @code(scs_value_t) is using pointers for some values, so it cannot be stored
  directly, as only reference and no actual data would be stored.@br
  Use this structure along with call to scs_value_localized function to store
  content of a variable of type @code(scs_value_t).

  @member ValueType  Type of stored value.
  @member BinaryData Stored binary data (eg. integers, floats).
  @member(StringData Stored string data (used only when string data are stored,
                     empty otherwise).)
}
  scs_value_localized_t = record
    ValueType:  scs_value_type_t;
    BinaryData: scs_value_t;
    StringData: TelemetryString;
  end;
  //:Pointer to scs_value_localized_t structure.
  p_scs_value_localized_t = ^scs_value_localized_t;


{:
  @abstract(Structure used to store content of @code(scs_named_value_t)
            variable.)
  @code(scs_named_value_t) is using pointers for some values, so it cannot be
  stored directly, as only reference and no actual data would be stored.@br
  Use this structure along with call to scs_named_value_localized function to
  store content of a variable of type @code(scs_named_value_t).

  @member Name  @NoAutoLink(Name) of the @NoAutoLink(value).
  @member Index @NoAutoLink(Index) of the @NoAutoLink(value).
  @member Value Named @NoAutoLink(value) itself.
}  
  scs_named_value_localized_t = record
    Name:   TelemetryString;
    Index:  scs_u32_t;
    Value:  scs_value_localized_t;
  end;
  //:Pointer to scs_named_value_localized_t structure.
  p_scs_named_value_localized_t = ^scs_named_value_localized_t;


{:
  @abstract(Structure used to store content of
  @code(scs_telemetry_configuration_t) variable.)
  @code(scs_telemetry_configuration_t) is using pointers for some values, so it
  cannot be stored directly, as only reference and no actual data would be
  stored.@br
  Use this structure along with call to scs_telemetry_configuration_localized
  function to store content of a variable of type
  @code(scs_telemetry_configuration_t).

  @member ID         Configuration identifier.
  @member(Attributes Array of named values (@NoAutoLink(attributes)) this
                     configuration contains.)
}
  scs_telemetry_configuration_localized_t = record
    ID:         TelemetryString;
    Attributes: Array of scs_named_value_localized_t
  end;
  //:Pointer to scs_telemetry_configuration_localized_t structure.
  p_scs_telemetry_configuration_localized_t = ^scs_telemetry_configuration_localized_t;

const
  //:Constant containing an empty @code(scs_value_t) structure, or, more
  //:precisely, structure with invalid value type.
  scs_value_empty: scs_value_t = (
    _type:            SCS_VALUE_TYPE_INVALID;
    _padding:         $00000000;
    value_dplacement: (
      position:         (x: 0.0; y: 0.0; z: 0.0);
      orientation:      (heading: 0.0; pitch: 0.0; roll:0.0);
      _padding:         $00000000));

  //:Constant containing an empty scs_value_localized_t structure (invalid value
  //:type).
  scs_value_localized_empty: scs_value_localized_t = (
    ValueType:  SCS_VALUE_TYPE_INVALID;
    BinaryData: (
      _type:            SCS_VALUE_TYPE_INVALID;
      _padding:         $00000000;
      value_dplacement: (
        position:         (x: 0.0; y: 0.0; z: 0.0);
        orientation:      (heading: 0.0; pitch: 0.0; roll:0.0);
        _padding:         $00000000));
    StringData: '');

implementation

end.
