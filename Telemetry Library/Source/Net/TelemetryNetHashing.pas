{*******************************************************************************
@abstract(Hashing types and functions.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-08)
@lastmod(2013-10-08)

  TelemetryNetHashing

  ©František Milt, all rights reserved.

  This file contains types definitions, constants, routines, etc. used for
  hashing (e.g. password hashing).

  Last change:  2013-10-08

  Change List:@unorderedList(
    @item(2013-10-08  - First stable version.))

*******************************************************************************}
unit TelemetryNetHashing;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  CRC32, MD5;

type
  // Used to distinguish function used for hashing.
  THashType = (htUnknown, htCRC32, htMD5);

  TCRC32Hash = CRC32.TCRC32;
  PCRC32Hash = ^TCRC32Hash;

  TMD5Hash = MD5.TMD5Hash;
  PMD5Hash = ^TMD5Hash;

  // Computes CRC32 hash (checksum) of ansi version of given string.
  // @param Str String to be hashed.
  // @returns CRC32 hash of passed string.
  Function HashCRC32(Str: String): TCRC32Hash;

  // Computes MD5 hash of ansi version of given string.
  // @param Str String to be hashed.
  // @returns MD5 hash of passed string.
  Function HashMD5(Str: String): TMD5Hash;

  // Checks whether given hash is the same as hash of given string.@br
  // When used hash function is not known or any error occurs, function returns
  // @false.
  // @param Text String controled for hash.
  // @param HashType Type of hashing function used to create hash.
  // @param Hash Pointer to actual hash.
  // @returns @True when hash fits, otherwise @false.
  Function IsTextHash(Text: String; HashType: THashType; Hash: Pointer): Boolean;

implementation

Function HashCRC32(Str: String): TCRC32Hash;
begin
Result := StringCRC32(AnsiString(Str));
end;

//------------------------------------------------------------------------------

Function HashMD5(Str: String): TMD5Hash;
begin
Result := StringMD5(AnsiString(Str));
end;

//------------------------------------------------------------------------------

Function IsTextHash(Text: String; HashType: THashType; Hash: Pointer): Boolean;
begin
try
  case HashType of
    htCRC32:  Result := PCRC32Hash(Hash)^ = HashCRC32(Text);
    htMD5:    Result := CompareMD5(PMD5Hash(Hash)^,HashMD5(Text));
  else
    Result := False;
  end;
except
  Result := False;
end;
end;

end.
