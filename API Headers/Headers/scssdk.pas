(**
 * @file scssdk.h
 *
 * @brief Common SDK types and structures.
 *)
(*<unit>*) 
unit scssdk;

interface

{$INCLUDE scssdk_defs.inc}

(*<interface>*)
// String types used in the API.
type
{$IF not Declared(TUTF8Char)}
  TUTF8Char = type AnsiChar;
{$IFEND}
{$IF not Declared(PUTF8Char)}
  PUTF8Char = ^TUTF8Char;
{$IFEND}
  
  TelemetryString = type UTF8String;

// Types used trough the SDK.
  scs_u8_t      = Byte;               p_scs_u8_t      = ^scs_u8_t;
  scs_u16_t     = Word;               p_scs_u16_t     = ^scs_u16_t;

{$IF (SizeOf(LongInt) = 4) and (SizeOf(LongWord) = 4)}
  scs_s32_t     = LongInt;
  scs_u32_t     = LongWord;
{$ELSEIF (SizeOf(Integer) = 4) and (SizeOf(Cardinal) = 4)}
  scs_s32_t     = Integer;
  scs_u32_t     = Cardinal;
{$ELSE}
  {$MESSAGE FATAL 'Cannot declare 32bit integers'}
{$IFEND}
  p_scs_s32_t     = ^scs_s32_t;       p_scs_u32_t     = ^scs_u32_t;

  scs_u64_t     = UInt64;             p_scs_u64_t     = ^scs_u64_t;
  scs_float_t   = Single;             p_scs_float_t   = ^scs_float_t;
  scs_double_t  = Double;             p_scs_double_t  = ^scs_double_t;
  scs_string_t  = PUTF8Char;          p_scs_string_t  = ^scs_string_t;

const
  SCS_U32_NIL = scs_u32_t(-1);

(**
 * @brief Type of value provided during callback registration and passed back
 * to the callback.
 *)
type
  scs_context_t = Pointer;

(**
 * @brief Timestamp value.
 *
 * Value is expressed in microseconds.
 *)
type
  scs_timestamp_t = scs_u64_t;
  p_scs_timestamp_t = ^scs_timestamp_t;

// Common return codes.
type
  scs_result_t = scs_s32_t;
  p_scs_result_t = ^scs_result_t;

const
  SCS_RESULT_ok                 = scs_result_t(0);  // Operation succeeded.
  SCS_RESULT_unsupported        = scs_result_t(-1); // Operation or specified parameters are not supported. (e.g. the plugin does not support the requested version of the API)
  SCS_RESULT_invalid_parameter  = scs_result_t(-2); // Specified parameter is not valid (e.g. null value of callback, invalid combination of flags).
  SCS_RESULT_already_registered = scs_result_t(-3); // There is already a registered callback for the specified function (e.g. event/channel).
  SCS_RESULT_not_found          = scs_result_t(-4); // Specified item (e.g. channel) was not found.
  SCS_RESULT_unsupported_type   = scs_result_t(-5); // Specified value type is not supported (e.g. channel does not provide that value type).
  SCS_RESULT_not_now            = scs_result_t(-6); // Action (event/callback registration) is not allowed in the current state. Indicates incorrect use of the api.
  SCS_RESULT_generic_error      = scs_result_t(-7); // Error not convered by other existing code.

// Types of messages printed to log.
type
  scs_log_type_t = scs_s32_t;
  p_scs_log_type_t = ^scs_log_type_t;

const
  SCS_LOG_TYPE_message  = scs_log_type_t(0);
  SCS_LOG_TYPE_warning  = scs_log_type_t(1);
  SCS_LOG_TYPE_error    = scs_log_type_t(2);

(**
 * @brief Logs specified message to the game log.
 *
 * @param type Type of message. Controls generated prefixes and colors in console.
 * @param message Message to log.
 *)
type
  scs_log_t = procedure(const aType: scs_log_type_t; const aMessage: scs_string_t); stdcall;

// Common initialization structures.

(**
 * @brief Initialization parameters common to most APIs provided
 * by the SDK.
 *)
type
  scs_sdk_init_params_v100_t = Record
    (**
     * @brief Name of the game for display purposes.
     *
     * This is UTF8 encoded string containing name of the game
     * for display to the user. The exact format is not defined,
     * might be changed between versions and should be not parsed.
     *
     * This pointer will be never NULL.
     *)
    game_name:    scs_string_t;
    (**
     * @brief Identification of the game.
     *
     * If the library wants to identify the game to do any
     * per-game configuration, this is the field which should
     * be used.
     *
     * This string contains only following characters:
     * @li lower-cased letters
     * @li digits
     * @li underscore
     *
     * This pointer will be never NULL.
     *)
    game_id:      scs_string_t;
    (**
     * @brief Version of the game for purpose of the specific api
     * which is being initialized.
     *
     * Does NOT match the patch level of the game.
     *)
    game_version: scs_u32_t;
{$IFDEF SCS_ARCHITECTURE_x64}
    (**
     * @brief Explicit alignment for the 64 bit pointer.
     *)
    _padding:     scs_u32_t;
{$ENDIF}
    (**
     * @brief Function used to write messages to the game log.
     *
     * Each message is printed on a separate line.
     *
     * This pointer will be never NULL.
     *)
    log:          scs_log_t;
  end;
  p_scs_sdk_init_params_v100_t = ^scs_sdk_init_params_v100_t;

// Routines for API strings conversions. 
Function APIStringToTelemetryString(const Str: scs_string_t): TelemetryString;
Function TelemetryStringToAPIString(const Str: TelemetryString): scs_string_t;
procedure APIStringFree(var Str: scs_string_t);
Function TelemetryStringDecode(const Str: TelemetryString): String;
Function TelemetryStringEncode(const Str: String): TelemetryString;
Function APIString(const Str: TelemetryString): scs_string_t; overload;

// Routines replacing some of the C macros functionality.
Function SCSCheckSize(ActualSize, {%H-}Expected32,{%H-}Expected64: Cardinal): Boolean;

Function SCSMakeVersion(Major, Minor: scs_u16_t): scs_u32_t;
Function SCSGetMajorVersion(Version: scs_u32_t): scs_u16_t;
Function SCSGetMinorVersion(Version: scs_u32_t): scs_u16_t;
Function SCSGetVersionAsString(Version: scs_u32_t): String;
(*</interface>*)

implementation

uses
  SysUtils;
  
(*<implementation>*)    
Function APIStringToTelemetryString(const Str: scs_string_t): TelemetryString;
begin
If Assigned(Str) then
  begin
    SetLength(Result,StrLen(PAnsiChar(Str)));
    Move(Str^,PUTF8Char(Result)^,Length(Result) * SizeOf(TUTF8Char));
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function TelemetryStringToAPIString(const Str: TelemetryString): scs_string_t;
begin
If Length(Str) > 0 then
  Result := scs_string_t(StrNew(PAnsiChar(Str)))
else
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure APIStringFree(var Str: scs_string_t);
begin
If Assigned(Str) then
  begin
    StrDispose(PAnsiChar(Str));
    Str := nil;
  end;
end;

//------------------------------------------------------------------------------

Function TelemetryStringDecode(const Str: TelemetryString): String;
begin
{$IFDEF Unicode}
Result := UTF8Decode(Str);
{$ELSE}
{$IFDEF FPC}
Result := Str;
{$ELSE}
Result := UTF8ToAnsi(Str);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TelemetryStringEncode(const Str: String): TelemetryString;
begin
{$IFDEF Unicode}
Result := UTF8Encode(Str);
{$ELSE}
{$IFDEF FPC}
Result := Str;
{$ELSE}
Result := AnsiToUTF8(Str);
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function APIString(const Str: TelemetryString): scs_string_t;
begin
Result := scs_string_t(PAnsiChar(Str));
end;

//------------------------------------------------------------------------------

Function SCSCheckSize(ActualSize, Expected32, Expected64: Cardinal): Boolean;
begin
{$IFDEF SCS_ARCHITECTURE_x64}
  Result := ActualSize = Expected64;
{$ELSE}
  {$IFDEF SCS_ARCHITECTURE_x86}
  Result := ActualSize = Expected32;
  {$ELSE}
  {$MESSAGE FATAL 'Undefined architecture!'}  //better prevent compilation
  Halt(666); //architecture is not known, initiate immediate abnormal termination
  {$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function SCSMakeVersion(Major, Minor: scs_u16_t): scs_u32_t;
begin
Result := (Major shl 16) or Minor;
end;

//------------------------------------------------------------------------------

Function SCSGetMajorVersion(Version: scs_u32_t): scs_u16_t;
begin
Result := (Version shr 16) and $FFFF;
end;

//------------------------------------------------------------------------------

Function SCSGetMinorVersion(Version: scs_u32_t): scs_u16_t;
begin
Result := Version and $FFFF;
end;

//------------------------------------------------------------------------------

Function SCSGetVersionAsString(Version: scs_u32_t): String;
begin
Result := IntToStr(SCSGetMajorVersion(Version)) + '.' + 
          IntToStr(SCSGetMinorVersion(Version));
end;
(*</implementation>*)

{$IFDEF AssertTypeSize}
initialization
(*<initialization>*)
  Assert(SCSCheckSize(SizeOf(scs_sdk_init_params_v100_t),16,32));
(*</initialization>*)
{$ENDIF}

(*</unit>*) 
end.
