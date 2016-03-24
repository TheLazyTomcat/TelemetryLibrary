{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Types, constants, routines, etc. used troughout the Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-04)
@lastmod(2016-03-22)

  @bold(@NoAutoLink(TelemetryCommon))

  ©2013-2016 František Milt, all rights reserved.

  Last change: 2016-03-22

  This file is intended to provide types, constants, routines, etc. used
  throughout the Telemetry library (that is, in more than one unit).
  It also contains declaration of exception classes used in the library, those
  are:
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

@html(<hr>)}
unit TelemetryCommon;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

{$IFNDEF Documentation}
uses
  SysUtils,
{$IFDEF CondensedHeaders}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry,
  scssdk_eut2,
  scssdk_telemetry_eut2,
  scssdk_ats,
  scssdk_telemetry_ats;
{$ENDIF}
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
{:
  Raised when item that is added to some list or array already exists there.
}
  ETLAlreadyExists    = class(ETLException);
{:
  Raised when requested item is not found in a list or array.
}
  ETLNotfound         = class(ETLException);

//------------------------------------------------------------------------------

{:
  Structure used in lists of supported games and their versions.

  @member GameID       Internal game identifier (not a game name).
  @member(GameVersion  Internal game version (API-specific value - not equal
                       to actual game version).)
}
  TSupportedGame = record
    GameID:       TelemetryString;
    GameVersion:  scs_u32_t;
  end;
  //:Pointer to TGameSupportInfo structure.
  PSupportedGame = ^TSupportedGame;

//------------------------------------------------------------------------------

{:
  @abstract(Used where there is need to pass or return two indices to fully
  define some reference (eg. indices in two-dimensional array).)

  @member Index1 First-level index.
  @member Index2 Second-level index.
}
  TDoubleIndex = record
    Index1: Integer;
    Index2: Integer;
  end;
  //:Pointer to TDoubleIndex structure.
  PDoubleIndex = ^TDoubleIndex;

//------------------------------------------------------------------------------

{:
  Structure used to reference a specific config (an @noAutoLink(attribute) in a
  specified configuration).

  @member ID        Idetifier of the configuration.
  @member(Attribute Identifier of an @noAutoLink(attribute) in configuration
                    specified by field @code(ID).)
}
  TConfigReference = record
    ID:         TelemetryString;
    Attribute:  TelemetryString;
  end;
  //:Pointer to TConfigReference structure.
  PConfigReference = ^TConfigReference;

//------------------------------------------------------------------------------

const
  //:Invalid TDoubleIndex value.
  InvalidDoubleIndex: TDoubleIndex = (Index1: -1; Index2: -1);

  //:Empty config reference.
  EmptyConfigReference: TConfigReference = (Id: ''; Attribute: '');

//------------------------------------------------------------------------------

{$IFDEF DevHints}
  {$MESSAGE HINT 'Remember to update.'}
{
  These constants can and will change with telemetry development, remember to
  update them when you add support for new telemetry version.
}
{$ENDIF}

  //:List of Telemetry API versions supported by this library.
  SupportedTelemetryVersions: Array[0..0] of scs_u32_t =
   (SCS_TELEMETRY_VERSION_1_00 {1.0});

  //:List of games and their versions supported by this library.
  SupportedGames: Array[0..13] of TSupportedGame =
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
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_10 {EUT2 1.10}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_11 {EUT2 1.11}),
    (GameID: SCS_GAME_ID_EUT2; GameVersion: SCS_TELEMETRY_EUT2_GAME_VERSION_1_12 {EUT2 1.12}),
    (GameID: SCS_GAME_ID_ATS;  GameVersion: SCS_TELEMETRY_ATS_GAME_VERSION_1_00  {ATS 1.0}));

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

    
{==============================================================================}
{   Unit functions and procedures // Declaration                               }
{==============================================================================}
{:
  Checks whether passed double index is valid (none of the field can be smaller
  than zero).

  @param Idx Double index to be checked for validity.

  @returns @True when both internal indices are zero or above, @false otherwise.
}
Function ValidDoubleIndex(Indices: TDoubleIndex): Boolean;

//------------------------------------------------------------------------------

{:
  Creates TDoubleIndex structure filled with passed independent indices.

  @param Index1 First-level index.
  @param Index2 Second-level index.

  @returns Filled double index structure.
}
Function DoubleIndex(Index1,Index2: Integer): TDoubleIndex;

//------------------------------------------------------------------------------

{:
  Returns full config reference build from passed config ID and attribute name.

  @param ID        ID of configuration.
  @param Attribute Name of attribute.

  @returns Full config reference.
}
  Function ConfigReference(const ID, Attribute: TelemetryString): TConfigReference;

//------------------------------------------------------------------------------

{:
  @abstract(Checks whether passed config reference is valid.)
  Current implementation only checks whether both fields are not empty.

  @param ConfigReference Reference to be checked for validity.

  @returns @True when passed reference is valid, @false otherwise.
}
  Function ValidConfigReference(ConfigReference: TConfigReference): Boolean;


implementation

{==============================================================================}
{   Unit functions and procedures // Implementation                            }
{==============================================================================}

Function ValidDoubleIndex(Indices: TDoubleIndex): Boolean;
begin
Result := (Indices.Index1 >= 0) and (Indices.Index2 >= 0);
end;

//------------------------------------------------------------------------------

Function DoubleIndex(Index1,Index2: Integer): TDoubleIndex;
begin
Result.Index1 := Index1;
Result.Index2 := Index2;
end;

//------------------------------------------------------------------------------

Function ConfigReference(const ID, Attribute: TelemetryString): TConfigReference;
begin
Result.ID := ID;
Result.Attribute := Attribute;
end;

//------------------------------------------------------------------------------

Function ValidConfigReference(ConfigReference: TConfigReference): Boolean;
begin
Result := (ConfigReference.ID <> '') and (ConfigReference.Attribute <> '');
end;

end.
