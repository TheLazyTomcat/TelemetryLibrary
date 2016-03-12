{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Base classes for objects that are checked for telemetry and game
          version support.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-17)
@lastmod(2015-06-28)

  @bold(@NoAutoLink(TelemetryVersionObjects))

  ©František Milt, all rights reserved.
  
  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TTelemetryAbstractVersionObject
   |- TTelemetryVersionObject
       |- TTelemetryVersionPrepareObject
)

  Last change:  2015-06-28

  Change List:@unorderedList(
    @item(2013-10-17 - First stable version.)
    @item(2014-04-06 - Type of parameters @code(GameName) and @code(GameID) in
                       method TTelemetryVersionPrepareObject.PrepareForGameVersion
                       changed to @code(TelemetryString).)
    @item(2014-04-06 - Added support for eut2 1.8.)
    @item(2014-05-01 - Added method
                       TTelemetryAbstractVersionObject.HighestSupportedGameVersion
                       and its variants in descendant classes.)
    @item(2014-10-23 - Added support for eut2 1.9.)
    @item(2014-10-23 - Repaired bug in implementation of method
                       TTelemetryVersionObject.HighestSupportedGameVersion.)
    @item(2014-10-23 - Following methods were moved to public section:@unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryVersionPrepareObject.PrepareForTelemetryVersion)
                         @item(TTelemetryVersionPrepareObject.PrepareForGameVersion)))
    @item(2014-10-23 - Added following methods:@unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryVersionPrepareObject.PrepareFor)
                         @item(TTelemetryVersionPrepareObject.PrepareForParam)))
    @item(2014-10-23 - Type of parameter @code(GameID) in following methods
                       changed to @code(TelemetryString):@unorderedList(
                         @itemSpacing(Compact)
                         @item(TTelemetryAbstractVersionObject.HighestSupportedGameVersion)
                         @item(TTelemetryAbstractVersionObject.SupportsGameVersion)
                         @item(TTelemetryAbstractVersionObject.SupportsTelemetryAndGameVersion)))
    @item(2014-11-07 - Added support for eut2 1.10.)
    @item(2015-06-28 - Small implementation changes.))

@html(<hr>)}
unit TelemetryVersionObjects;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFDEF Documentation}
  TelemetryCommon,
  TelemetryStrings,
{$ENDIF}
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_telemetry,
  scssdk_eut2,
  scssdk_telemetry_eut2;
{$ENDIF}

type
{==============================================================================}
{------------------------------------------------------------------------------}
{                       TTelemetryAbstractVersionObject                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryAbstractVersionObject // Class declaration                       }
{==============================================================================}
{:
  @abstract(Common, fully abstract ancestor for all classes that needs to be
  checked for version support before creation of an instance.)

  This class defines set of methods used to check version support. Use it as an
  ancestor for classes where you want to fully control methods implementation or
  for classes that supports different set of versions than is defined for
  TTelemetryVersionObject.@br
  All methods must be called directly on class. They are intended to be used to
  check whether the class supports required telemetry and game version before
  instantiation (creation of class instance).
}
  TTelemetryAbstractVersionObject = class(TObject)
  public
  {:
    @returns Highest supported telemetry version.
  }
    class Function HighestSupportedTelemetryVersion: scs_u32_t; virtual; abstract;
  {:
    @param GameID Game identifier.

    @returns Highest supported version of passed game.
  }
    class Function HighestSupportedGameVersion(GameID: TelemetryString): scs_u32_t; virtual; abstract;
  {:
    @param TelemetryVersion Version of telemetry.

    @returns @True when given telemetry version is supported, otherwise @false.
  }
    class Function SupportsTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean; virtual; abstract;
  {:
    @param TelemetryVersion Version of telemetry.

    @returns(@True when given telemetry major version is supported (minor part
             is ignored), otherwise @false.)
  }
    class Function SupportsTelemetryMajorVersion(TelemetryVersion: scs_u32_t): Boolean; virtual; abstract;
  {:
    @param GameID       Game identifier.
    @param GameVersion  Version of game.

    @returns(@True when given game and its version are supported, otherwise
             @false.)
  }
    class Function SupportsGameVersion(GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; virtual; abstract;
  {:
    @param TelemetryVersion Version of telemetry.
    @param GameID           Game identifier.
    @param GameVersion      Version of game.

    @returns(@True when given telemetry, game and its version are supported,
             otherwise @false.)
  }
    class Function SupportsTelemetryAndGameVersion(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; virtual; abstract;
  {:
    @param TelemetryVersion Version of telemetry.
    @param Parameters       Structure containing other version information.

    @returns(@True when given telemetry, game and its version are supported,
             otherwise @false.)
  }
    class Function SupportsTelemetryAndGameVersionParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean; virtual; abstract;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryVersionObject                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryVersionObject // Class declaration                               }
{==============================================================================}
{:
  @abstract(Common ancestor for all classes that needs to be checked for version
  support before creation of an instance.)

  This class implements all methods from TTelemetryAbstractVersionObject. Used
  as an ancestor for classes that supports exactly the same versions set as this
  class.@br
  All methods must be called directly on class. They are intended to be used to
  check whether the class supports required telemetry  and game version before
  instantiation (creation of class instance).

  Supported versions as of 2014-11-07:
@unorderedList(
  @itemSpacing(Compact)
  @item(Telemetry 1.0)
  @item(eut2 1.0)
  @item(eut2 1.1)
  @item(eut2 1.2)
  @item(eut2 1.3)
  @item(eut2 1.4)
  @item(eut2 1.5)
  @item(eut2 1.6)
  @item(eut2 1.7)
  @item(eut2 1.8)
  @item(eut2 1.9)
  @item(eut2 1.10)
)
}
  TTelemetryVersionObject = class(TTelemetryAbstractVersionObject)
  public
    //:HighestSupportedTelemetryVersion See @inherited.
    class Function HighestSupportedTelemetryVersion: scs_u32_t; override;
    //:HighestSupportedGameVersion See @inherited.
    class Function HighestSupportedGameVersion(GameID: TelemetryString): scs_u32_t; override;
    //:SupportsTelemetryVersion See @inherited.
    class Function SupportsTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean; override;
    //:SupportsTelemetryMajorVersion See @inherited.
    class Function SupportsTelemetryMajorVersion(TelemetryVersion: scs_u32_t): Boolean; override;
    //:SupportsGameVersion See @inherited.
    class Function SupportsGameVersion(GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; override;
    //:SupportsTelemetryAndGameVersion See @inherited.
    class Function SupportsTelemetryAndGameVersion(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; override;
    //:SupportsTelemetryAndGameVersionParam See @inherited.
    class Function SupportsTelemetryAndGameVersionParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean; override;
  end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryVersionPrepareObject                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryVersionPrepareObject // Class declaration                        }
{==============================================================================}
{:
  @abstract(Common ancestor for all classes that needs to be prepared for
  selected telemetry and/or game version.)

  Methods beginning with @code(Prepare_) are called to prepare created object
  for specific telemetry/game version.@br
  Each telemetry/game version has its own method, even if no action is needed.
  For each version, all lower or equal version methods are called in ascending
  order (every method calls its predecessor at the beginning of its own code).
  For example, for version 1.2, methods 1_0, 1_1 and 1_2 would be called.
}
  TTelemetryVersionPrepareObject = class(TTelemetryVersionObject)
  protected
  {:
  Preparation for telemetry 1.0.
  }
    procedure Prepare_Telemetry_1_0; virtual;
  {:
  Preparation for eut2 1.0.
  }
    procedure Prepare_Game_eut2_1_0; virtual;
  {:
  Preparation for eut2 1.1.@br
  Calls Prepare_Game_eut2_1_0.
  }
    procedure Prepare_Game_eut2_1_1; virtual;
  {:
  Preparation for eut2 1.2.@br
  Calls Prepare_Game_eut2_1_1.
  }
    procedure Prepare_Game_eut2_1_2; virtual;
  {:
  Preparation for eut2 1.3.@br
  Calls Prepare_Game_eut2_1_2.
  }
    procedure Prepare_Game_eut2_1_3; virtual;
  {:
  Preparation for eut2 1.4.@br
  Calls Prepare_Game_eut2_1_3.
  }
    procedure Prepare_Game_eut2_1_4; virtual;
  {:
  Preparation for eut2 1.5.@br
  Calls Prepare_Game_eut2_1_4.
  }
    procedure Prepare_Game_eut2_1_5; virtual;
  {:
  Preparation for eut2 1.6.@br
  Calls Prepare_Game_eut2_1_5.
  }
    procedure Prepare_Game_eut2_1_6; virtual;
  {:
  Preparation for eut2 1.7.@br
  Calls Prepare_Game_eut2_1_6.
  }
    procedure Prepare_Game_eut2_1_7; virtual;
  {:
  Preparation for eut2 1.8.@br
  Calls Prepare_Game_eut2_1_7.
  }
    procedure Prepare_Game_eut2_1_8; virtual;
  {:
  Preparation for eut2 1.9.@br
  Calls Prepare_Game_eut2_1_8.
  }
    procedure Prepare_Game_eut2_1_9; virtual;
  {:
  Preparation for eut2 1.10.@br
  Calls Prepare_Game_eut2_1_9.
  }
    procedure Prepare_Game_eut2_1_10; virtual;
  public
  {:
    Performs any preparations necessary to support required telemetry version.

    @param(TelemetryVersion Version of telemetry for which the object should be
                            prepared.)

    @returns(@True when preparation for given version were done successfully,
             otherwise @false.)
  }
    Function PrepareForTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean; virtual;
  {:
    Performs preparations necessary to support required game and its version.

    @param GameName     Name of the game.
    @param GameID       Game identifier.
    @param GameVersion  Version of game.

    @returns(@True when preparation for given game and its version were done
             successfully, otherwise @false.)
  }
    Function PrepareForGameVersion(const {%H-}GameName, GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; virtual;
  {:
    Performs preparations necessary to support required telemetry version and
    game and its version.

    @param(TelemetryVersion Version of telemetry for which the object should be
                            prepared.)
    @param GameName         Name of the game.
    @param GameID           Game identifier.
    @param GameVersion      Version of game.

    @returns(@True when preparation for given telemetry version and game and its
             version were done successfully, otherwise @false.)
  }
    Function PrepareFor(TelemetryVersion: scs_u32_t; const GameName, GameID: TelemetryString; GameVersion: scs_u32_t): Boolean; virtual;
  {:
    Performs preparations necessary to support required telemetry version and
    game and its version.

    @param(TelemetryVersion Version of telemetry for which the object should be
                            prepared.)
    @param(Parameters       Structure containing other necessary game and
                            version information.)

    @returns(@True when preparation for given telemetry version and game and its
             version were done successfully, otherwise @false.)
  }
    Function PrepareForParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean; virtual;
  end;

implementation

uses
  TelemetryCommon, TelemetryStrings;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryVersionObject                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryVersionObject // Class implementation                            }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryVersionObject // Public methods                                  }
{------------------------------------------------------------------------------}

class Function TTelemetryVersionObject.HighestSupportedTelemetryVersion: scs_u32_t;
begin
Result := SupportedTelemetryVersions[High(SupportedTelemetryVersions)];
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.HighestSupportedGameVersion(GameID: TelemetryString): scs_u32_t;
var
  i:  Integer;
begin
Result := SCS_U32_NIL;
For i := Low(SupportedGames) to High(SupportedGames) do
  If TelemetrySameStr(GameID,SupportedGames[i].GameID) then
    Result := SupportedGames[i].GameVersion;
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.SupportsTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(SupportedTelemetryVersions) to High(SupportedTelemetryVersions) do
  If TelemetryVersion = SupportedTelemetryVersions[i] then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.SupportsTelemetryMajorVersion(TelemetryVersion: scs_u32_t): Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(SupportedTelemetryVersions) to High(SupportedTelemetryVersions) do
  If (TelemetryVersion and $FFFF0000) = (SupportedTelemetryVersions[i] and $FFFF0000) then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.SupportsGameVersion(GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(SupportedGames) to High(SupportedGames) do
  If TelemetrySameStr(GameID,SupportedGames[i].GameID) and
    (GameVersion = SupportedGames[i].GameVersion) then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.SupportsTelemetryAndGameVersion(TelemetryVersion: scs_u32_t; GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
Result := SupportsTelemetryVersion(TelemetryVersion) and SupportsGameVersion(GameID,GameVersion);
end;

//------------------------------------------------------------------------------

class Function TTelemetryVersionObject.SupportsTelemetryAndGameVersionParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean;
begin
Result := SupportsTelemetryAndGameVersion(TelemetryVersion,
                                          APIStringToTelemetryString(Parameters.common.game_id),
                                          Parameters.common.game_version);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryVersionPrepareObject                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryVersionPrepareObject // Class implementation                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryVersionPrepareObject // Protected methods                        }
{------------------------------------------------------------------------------}

procedure TTelemetryVersionPrepareObject.Prepare_Telemetry_1_0;
begin
// No action.
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_0;
begin
// No action.
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_1;
begin
Prepare_Game_eut2_1_0;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_2;
begin
Prepare_Game_eut2_1_1;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_3;
begin
Prepare_Game_eut2_1_2;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_4;
begin
Prepare_Game_eut2_1_3;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_5;
begin
Prepare_Game_eut2_1_4;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_6;
begin
Prepare_Game_eut2_1_5;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_7;
begin
Prepare_Game_eut2_1_6;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_8;
begin
Prepare_Game_eut2_1_7;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_9;
begin
Prepare_Game_eut2_1_8;
end;

//------------------------------------------------------------------------------

procedure TTelemetryVersionPrepareObject.Prepare_Game_eut2_1_10;
begin
Prepare_Game_eut2_1_9;
end;

{------------------------------------------------------------------------------}
{   TTelemetryVersionPrepareObject // Public methods                           }
{------------------------------------------------------------------------------}

Function TTelemetryVersionPrepareObject.PrepareForTelemetryVersion(TelemetryVersion: scs_u32_t): Boolean;
begin
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Remember to update.'}
{$ENDIF}
Result := True;
case TelemetryVersion of
  SCS_TELEMETRY_VERSION_1_00: Prepare_Telemetry_1_0;  {1.0}
else
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryVersionPrepareObject.PrepareForGameVersion(const GameName, GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Remember to update.'}
{$ENDIF}
Result := True;
If TelemetrySameStr(GameId,SCS_GAME_ID_EUT2) then  {eut2, Euro Truck Simulator 2}
  begin
    case GameVersion of
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_00: Prepare_Game_eut2_1_0;  {1.0}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_01: Prepare_Game_eut2_1_1;  {1.1}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_02: Prepare_Game_eut2_1_2;  {1.2}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_03: Prepare_Game_eut2_1_3;  {1.3}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_04: Prepare_Game_eut2_1_4;  {1.4}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_05: Prepare_Game_eut2_1_5;  {1.5}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_06: Prepare_Game_eut2_1_6;  {1.6}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_07: Prepare_Game_eut2_1_7;  {1.7}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_08: Prepare_Game_eut2_1_8;  {1.8}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_09: Prepare_Game_eut2_1_9;  {1.9}
      SCS_TELEMETRY_EUT2_GAME_VERSION_1_10: Prepare_Game_eut2_1_10; {1.10}
    else
      Result := False;
    end;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TTelemetryVersionPrepareObject.PrepareFor(TelemetryVersion: scs_u32_t; const GameName, GameID: TelemetryString; GameVersion: scs_u32_t): Boolean;
begin
Result := PrepareForTelemetryVersion(TelemetryVersion) and
          PrepareForGameVersion(GameName,GameID,GameVersion);
end;

//------------------------------------------------------------------------------

Function TTelemetryVersionPrepareObject.PrepareForParam(TelemetryVersion: scs_u32_t; Parameters: scs_telemetry_init_params_t): Boolean;
begin
Result := PrepareForTelemetryVersion(TelemetryVersion) and
          PrepareForGameVersion(APIStringToTelemetryString(Parameters.common.game_name),
                                APISTringToTelemetryString(Parameters.common.game_id),
                                Parameters.common.game_version);
end;

end.
