{@html(<hr>)
@abstract(Unit providing routines operating on @code(TelemetryString) type and
          routines converting selected binary types to text.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-04-30)
@lastmod(2014-04-30)

  @bold(@NoAutoLink(TelemetryStrings))

  ©František Milt, all rights reserved.

  This unit is intended to provide some basic routines for manipulation and
  processing of @code(TelemetryString) type (UTF8 encoded string) and also
  routines designed to return human readable (i.e. textual) representation of
  binary data stored in variables of selected types.

  Last change:  2014-04-30

  Change List:@unorderedList(
    @item(2014-04-30 - First stable version.)
    @item(2014-04-30 - Unit @code(TelemetryRecipientAux) was completely merged
                       into this unit.))

  Todo:@unorderedList(
    @item(Documentation for new functions.))

@html(<hr>)}
unit TelemetryStrings;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
  SysUtils,
  TelemetryCommon,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

{==============================================================================}
{    Unit constants, types, variables, etc...                                  }
{==============================================================================}
var
  // Used for thread safety in conversions dependent on LocaleID.
  // Initialized in Initialization section of this unit (with id set to
  // LOCALE_USER_DEFAULT).
  TelemetryStringsFormatSettings: TFormatSettings;

{==============================================================================}
{    Unit Functions and procedures declarations                                }
{==============================================================================}

{
  @abstract(Compares strings based on the current locale with case sensitivity.)
  Since the @code(TelemetryString) is UTF8-encoded and there is no function
  for comparison of such strings, both strings are converted to WideString
  before actual comparison takes place.@br
  This function can be slow, so if performance is important, consider using
  TelemetrySameStrNoConv instead.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameStr(const S1, S2: TelemetryString): Boolean;

{
  @abstract(Compares strings based on the current locale without case
  sensitivity.)
  Since the @code(TelemetryString) is UTF8-encoded and there is no function
  for comparison of such strings, both strings are converted to WideString
  before actual comparison takes place.@br
  This function can be slow, so if performance is important, consider using
  TelemetrySameTextNoConv instead.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameText(const S1, S2: TelemetryString): Boolean;

{
  @abstract(Compares strings based on the current locale with case sensitivity
  and without internal conversions.)
  Unlike TelemetrySameStr, this function does not convert input strings to
  WideString before comparison. Instead, both strings are treated as normal
  AnsiString. This requires that both strings contains only ASCII characters
  (that is, up to #126), otherwise the function can, and probably will, return
  wrong result.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameStrNoConv(const S1, S2: TelemetryString): Boolean;

{
  @abstract(Compares strings based on the current locale without case
  sensitivity and without internal conversions.)
  Unlike TelemetrySameText, this function does not convert input strings to
  WideString before comparison. Instead, both strings are treated as normal
  AnsiString. This requires that both strings contains only ASCII characters
  (that is, up to #126), otherwise the function can, and probably will, return
  wrong result.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameTextNoConv(const S1, S2: TelemetryString): Boolean;

//==============================================================================

{
  @abstract(Returns identifier of given SCS value type.)
  Identifiers are not defined by the API, for details about naming individual
  types refer to function implementation.

  @param SCSValueType Value type.

  @returns Textual identifier of given value type.
}
Function SCSValueTypeToStr(SCSValueType: scs_value_type_t): String;

//------------------------------------------------------------------------------

{
  @abstract(Returns value type given by identifier.)
  If identifier is not recognized, then @code(SCS_VALUE_TYPE_INVALID) is
  returned.@br
  Function is case-insensitive.@br
  Identifiers are not defined by the API, for details about naming individual
  types refer to function implementation.

  @param Str Textual identifier of value type.

  @returns Value type with name corresponding to passed textual identifier.
}
Function SCSValueTypeFromStr(const Str: String): scs_value_type_t;

//------------------------------------------------------------------------------

Function ValueToStr(const Value; ValueType: scs_value_type_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
Function ValueToStr(const Value; ValueType: scs_value_type_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
Function ValueToStr(const Value; ValueType: scs_value_type_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of @code(scs_value_t) structure.)
  When type of the value is not known, an empty string is returned.

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueToStr(const Value: scs_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
Function SCSValueToStr(const Value: scs_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
Function SCSValueToStr(const Value: scs_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of scs_value_localized_t structure.)

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueLocalizedToStr(Value: scs_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
//Function SCSValueLocalizedToStr(Value: scs_value_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
//Function SCSValueLocalizedToStr(Value: scs_value_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of @code(scs_named_value_t)
            structure.)

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueToStr(const Value: scs_named_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of scs_named_value_localized_t
            structure.)

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of @code(scs_telemetry_frame_start_t)
            structure.)

  @param Data     Structure to be converted to text.
  @param(TypeName When set, value type identifiers for individual fields are
                  added to output string.)

  @returns Textual representation of given structure.
}
Function TelemetryEventFrameStartToStr(const Data: scs_telemetry_frame_start_t; TypeName: Boolean = False): String;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of
            @code(scs_telemetry_configuration_t) structure.)

  @param Data            Structure to be converted to text.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.                          
}
Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;

//------------------------------------------------------------------------------

{
  @abstract(Returns textual representation of
            scs_telemetry_configuration_localized_t structure.)

  @param Data            Structure to be converted to text.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.                          
}
Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;

implementation

uses
  Windows;

{==============================================================================}
{    Constants, types, variables, etc...                                       }
{==============================================================================}

const
  // Table of identifiers for individual value types (scs_value_type_t).
  // Index of each string corresponds to value type number this string is
  // identifying.
  cSCSValueTypeIdentifiers: Array[0..12] of String =
    ('none','bool','s32','u32','u64','float','double','fvector','dvector',
     'euler','fplacement','dplacement','string');

{==============================================================================}
{    Unit Functions and procedures implementation                              }
{==============================================================================}

Function TelemetrySameStr(const S1, S2: TelemetryString): Boolean;
begin
{$IFDEF Unicode}
Result := AnsiSameStr(UTF8Decode(S1),UTF8Decode(S2));
{$ELSE}
Result := WideSameStr(UTF8Decode(S1),UTF8Decode(S2));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TelemetrySameText(const S1, S2: TelemetryString): Boolean;
begin
{$IFDEF Unicode}
Result := AnsiSameText(UTF8Decode(S1),UTF8Decode(S2));
{$ELSE}
Result := WideSameText(UTF8Decode(S1),UTF8Decode(S2));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TelemetrySameStrNoConv(const S1, S2: TelemetryString): Boolean;
begin
Result := AnsiSameStr(S1,S2);
end;

//------------------------------------------------------------------------------

Function TelemetrySameTextNoConv(const S1, S2: TelemetryString): Boolean;
begin
Result := AnsiSameText(S1,S2);
end;

//==============================================================================

Function SCSValueTypeToStr(SCSValueType: scs_value_type_t): String;
begin
If (Integer(SCSValueType) >= Low(cSCSValueTypeIdentifiers)) and
   (Integer(SCSValueType) <= High(cSCSValueTypeIdentifiers)) then
  Result := cSCSValueTypeIdentifiers[Integer(SCSValueType)]
else
  Result := 'unknown';
end;

//------------------------------------------------------------------------------

Function SCSValueTypeFromStr(const Str: String): scs_value_type_t;
var
  i:  Integer;
begin
Result := SCS_VALUE_TYPE_INVALID;
For i := Low(cSCSValueTypeIdentifiers) to High(cSCSValueTypeIdentifiers) do
  If AnsiSameText(cSCSValueTypeIdentifiers[i],Str) then
    begin
      Result := scs_value_type_t(i);
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function ValueToStr(const Value; ValueType: scs_value_type_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := ValueToStr(Value,ValueType,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function ValueToStr(const Value; ValueType: scs_value_type_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := ValueToStr(Value,ValueType,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function ValueToStr(const Value; ValueType: scs_value_type_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
type
  TDescriptorsArray = Array[0..5] of String;
const
  cFullDescriptors:  TDescriptorsArray = ('X: ','Y: ','Z: ','Heading: ','Pitch: ','Roll: ');
  cEmptyDescriptors: TDescriptorsArray = ('','','','','','');
var
  Descriptors:  TDescriptorsArray;
begin
If ShowDescriptors then Descriptors := cFullDescriptors
  else Descriptors := cEmptyDescriptors;
case ValueType of
  SCS_VALUE_TYPE_INVALID:
    Result := '';
  SCS_VALUE_TYPE_bool:
    Result := BoolToStr(scs_value_bool_t(Value).value <> 0,True);
  SCS_VALUE_TYPE_s32:
    Result := IntToStr(scs_value_s32_t(Value).value);
  SCS_VALUE_TYPE_u32:
    Result := IntToStr(scs_value_u32_t(Value).value);
  SCS_VALUE_TYPE_u64:
    Result := IntToStr(scs_value_u64_t(Value).value);
  SCS_VALUE_TYPE_float:
    Result := FloatToStrF(scs_value_float_t(Value).value,Format,Precision,Digits,FormatSettings);
  SCS_VALUE_TYPE_double:
    Result := FloatToStrF(scs_value_double_t(Value).value,Format,Precision,Digits,FormatSettings);
  SCS_VALUE_TYPE_fvector:
    Result := '[' + Descriptors[0] + FloatToStrF(scs_value_fvector_t(Value).x,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[1] + FloatToStrF(scs_value_fvector_t(Value).y,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[2] + FloatToStrF(scs_value_fvector_t(Value).z,Format,Precision,Digits,FormatSettings) + ']';
  SCS_VALUE_TYPE_dvector:
    Result := '[' + Descriptors[0] + FloatToStrF(scs_value_dvector_t(Value).x,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[1] + FloatToStrF(scs_value_dvector_t(Value).y,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[2] + FloatToStrF(scs_value_dvector_t(Value).z,Format,Precision,Digits,FormatSettings) + ']';
  SCS_VALUE_TYPE_euler:
    Result := '[' + Descriptors[3] + FloatToStrF(scs_value_euler_t(Value).heading,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[4] + FloatToStrF(scs_value_euler_t(Value).pitch,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[5] + FloatToStrF(scs_value_euler_t(Value).roll,Format,Precision,Digits,FormatSettings) + ']';
  SCS_VALUE_TYPE_fplacement:
    Result := '[' + Descriptors[0] + FloatToStrF(scs_value_fplacement_t(Value).position.x,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[1] + FloatToStrF(scs_value_fplacement_t(Value).position.y,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[2] + FloatToStrF(scs_value_fplacement_t(Value).position.z,Format,Precision,Digits,FormatSettings) +
            '] [' + Descriptors[3] + FloatToStrF(scs_value_fplacement_t(Value).orientation.heading,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[4] + FloatToStrF(scs_value_fplacement_t(Value).orientation.pitch,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[5] + FloatToStrF(scs_value_fplacement_t(Value).orientation.roll,Format,Precision,Digits,FormatSettings) + ']';
  SCS_VALUE_TYPE_dplacement:
    Result := '[' + Descriptors[0] + FloatToStrF(scs_value_dplacement_t(Value).position.x,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[1] + FloatToStrF(scs_value_dplacement_t(Value).position.y,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[2] + FloatToStrF(scs_value_dplacement_t(Value).position.z,Format,Precision,Digits,FormatSettings) +
            '] [' + Descriptors[3] + FloatToStrF(scs_value_dplacement_t(Value).orientation.heading,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[4] + FloatToStrF(scs_value_dplacement_t(Value).orientation.pitch,Format,Precision,Digits,FormatSettings) +
             ', ' + Descriptors[5] + FloatToStrF(scs_value_dplacement_t(Value).orientation.roll,Format,Precision,Digits,FormatSettings) + ']';
  SCS_VALUE_TYPE_string:
    Result := TelemetryStringDecode(APIStringToTelemetryString(scs_value_t(Value).value_string.value));
else
  Result := '';
end;
If TypeName then Result := '(' + SCSValueTypeToStr(ValueType) + ') ' + Result;
end;

//------------------------------------------------------------------------------

Function SCSValueToStr(const Value: scs_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSValueToStr(Value,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSValueToStr(const Value: scs_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSValueToStr(Value,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSValueToStr(const Value: scs_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
case Value._type of
  SCS_VALUE_TYPE_bool:        Result := ValueToStr(Value.value_bool,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_s32:         Result := ValueToStr(Value.value_s32,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_u32:         Result := ValueToStr(Value.value_u32,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_u64:         Result := ValueToStr(Value.value_u64,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_float:       Result := ValueToStr(Value.value_float,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_double:      Result := ValueToStr(Value.value_double,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_fvector:     Result := ValueToStr(Value.value_fvector,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_dvector:     Result := ValueToStr(Value.value_dvector,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_euler:       Result := ValueToStr(Value.value_euler,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_fplacement:  Result := ValueToStr(Value.value_fplacement,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_dplacement:  Result := ValueToStr(Value.value_dplacement,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  SCS_VALUE_TYPE_string:      Result := ValueToStr(Value.value_string,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
else
 {SCS_VALUE_TYPE_INVALID}
  Result := ValueToStr(Value,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
end;
end;

//------------------------------------------------------------------------------

Function SCSValueLocalizedToStr(Value: scs_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    If TypeName then Result := '(' + SCSValueTypeToStr(Value.ValueType) + ') ' + TelemetryStringDecode(Value.StringData)
      else Result := TelemetryStringDecode(Value.StringData);
else
  Value.BinaryData._type := Value.ValueType;
  Result := SCSValueToStr(Value.BinaryData,TypeName,ShowDescriptors);
end;
end;

//------------------------------------------------------------------------------

Function SCSNamedValueToStr(const Value: scs_named_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := TelemetryStringDecode(APIStringToTelemetryString(Value.name));
If Value.index <> SCS_U32_NIL then Result := Result + '[' + IntToStr(Value.index) + ']';
Result := Result + ': ' + SCSValueToStr(Value.value,TypeName,ShowDescriptors);
end;

//------------------------------------------------------------------------------

Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := Value.Name;
If Value.Index <> SCS_U32_NIL then Result := Result + '[' + IntToStr(Value.Index) + ']';
Result := Result + ': ' + SCSValueLocalizedToStr(Value.Value,TypeName,ShowDescriptors);
end;

//------------------------------------------------------------------------------

Function TelemetryEventFrameStartToStr(const Data: scs_telemetry_frame_start_t; TypeName: Boolean = False): String;
begin
If TypeName then
  Result := 'Flags: (u32) ' + IntToHex(Data.flags,SizeOf(Data.flags) * 2) + sLineBreak +
            'Render time: (u64) ' + IntToStr(Data.render_time) + sLineBreak +
            'Simulation time: (u64) ' + IntToStr(Data.simulation_time) + sLineBreak +
            'Pause simulation time: (u64) ' + IntToStr(Data.paused_simulation_time)
else
  Result := 'Flags: ' + IntToHex(Data.flags,SizeOf(Data.flags) * 2) + sLineBreak +
            'Render time: ' + IntToStr(Data.render_time) + sLineBreak +
            'Simulation time: ' + IntToStr(Data.simulation_time) + sLineBreak +
            'Pause simulation time: ' + IntToStr(Data.paused_simulation_time);
end;

//------------------------------------------------------------------------------

Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
var
  TempAttr: p_scs_named_value_t;
begin
Result := TelemetryStringDecode(APIStringToTelemetryString(Data.id));
TempAttr := Data.attributes;
while Assigned(TempAttr^.name) do
  begin
    Result := Result + sLineBreak + SCSNamedValueToStr(TempAttr^,TypeName,ShowDescriptors);
    Inc(TempAttr);
  end;
end;

//------------------------------------------------------------------------------

Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
var
  i:  Integer;
begin
Result := TelemetryStringDecode(Data.ID);
For i := Low(Data.Attributes) to High(Data.Attributes) do
  Result := Result + sLineBreak + SCSNamedValueLocalizedToStr(Data.Attributes[i],TypeName,ShowDescriptors);
end;

//------------------------------------------------------------------------------

initialization
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT,TelemetryStringsFormatSettings);
  TelemetryStringsFormatSettings.DecimalSeparator := '.';

end.
