{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{:@html(<hr>)
@abstract(Provides routines operating on @code(TelemetryString) type and
          routines converting selected binary types to text.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-04-30)
@lastmod(2015-06-28)

  @bold(@NoAutoLink(TelemetryStrings))

  ©2013-2015 František Milt, all rights reserved.

  This unit is intended to provide some basic routines for manipulation and
  processing of @code(TelemetryString) type (UTF8 encoded string) and also
  routines designed to return human readable (i.e. textual) representation of
  binary data stored in variables of selected types.

  Last change: 2015-06-28

  Change List:@unorderedList(
    @item(2014-04-30 - First stable version.)
    @item(2014-04-30 - Unit @code(TelemetryRecipientAux) was completely merged
                       into this unit.)
    @item(2014-11-04 - Added following functions:@unorderedList(
                        @itemSpacing(Compact)
                        @item(TelemetrySameStrSwitch)
                        @item(TelemetrySameTextSwitch)
                        @item(ValueToStr (multiple overloaded variants))
                        @item(new variants of SCSValueToStr)
                        @item(new variants of SCSValueLocalizedToStr)
                        @item(new variants of SCSNamedValueToStr)
                        @item(new variants of SCSNamedValueLocalizedToStr)
                        @item(new variants of TelemetryEventConfigurationToStr)
                        @item(new variants of
                              TelemetryEventConfigurationLocalizedToStr)))
    @item(2014-11-04 - Small implementation changes.)
    @item(2015-06-25 - Following functions were renamed:@unorderedList(
                        @itemSpacing(Compact)
                        @item(@noAutoLink(TelemetrySameStr) renamed to
                              TelemetrySameStrConv)
                        @item(@noAutoLink(TelemetrySameText) renamed to
                              TelemetrySameTextConv)
                        @item(TelemetrySameStrSwitch renamed to
                              TelemetrySameStr)
                        @item(TelemetrySameTextSwitch renamed to
                              TelemetrySameText)))
    @item(2015-06-25 - Implementation changes.)
    @item(2015-06-28 - Added functions EventDataToStr and ChannelValueToStr.))

@html(<hr>)}
unit TelemetryStrings;

interface

{$INCLUDE '.\Telemetry_defs.inc'}

uses
{$IFNDEF Documentation}
  SysUtils,
{$ENDIF}  
  TelemetryCommon,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

{==============================================================================}
{   Unit constants, types, variables, etc...                                   }
{==============================================================================}
var
{:
  @abstract(Used for thread safety in conversions dependent on LocaleID.)
  Initialized in Initialization section of this unit (with id set to
  LOCALE_USER_DEFAULT).@br
  But note that this variable is NOT thread safe by itself. If you want to
  access it from multiple threads, then thread safety is your responsibility.
}
  TelemetryStringsFormatSettings: TFormatSettings;

{==============================================================================}
{   Unit functions and procedures declarations                                 }
{==============================================================================}

{:
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
Function TelemetrySameStrConv(const S1, S2: TelemetryString): Boolean;

{:
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
Function TelemetrySameTextConv(const S1, S2: TelemetryString): Boolean;

{:
  @abstract(Compares strings based on the current locale with case sensitivity
  and without internal conversions.)
  Unlike TelemetrySameStrConv, this function does not convert input strings to
  WideString before comparison. Instead, both strings are treated as normal
  AnsiString. This requires that both strings contains only ASCII characters
  (that is, up to #126), otherwise the function can, and probably will, return
  wrong result.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameStrNoConv(const S1, S2: TelemetryString): Boolean;

{:
  @abstract(Compares strings based on the current locale without case
  sensitivity and without internal conversions.)
  Unlike TelemetrySameTextConv, this function does not convert input strings to
  WideString before comparison. Instead, both strings are treated as normal
  AnsiString. This requires that both strings contains only ASCII characters
  (that is, up to #126), otherwise the function can, and probably will, return
  wrong result.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameTextNoConv(const S1, S2: TelemetryString): Boolean;

{:
  @abstract(Compares strings based on the current locale with case sensitivity.)
  This function internally calls TelemetrySameStrNoConv when switch
  @code(AssumeASCIIString) is defined. When it is not defined, it calls
  TelemetrySameStrConv.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameStr(const S1, S2: TelemetryString): Boolean;

{:
  @abstract(Compares strings based on the current locale without case
  sensitivity.)
  This function internally calls TelemetrySameTextNoConv when switch
  @code(AssumeASCIIString) is defined. When it is not defined, it calls
  TelemetrySameTextConv.

  @param S1 First string to compare.
  @param S2 Second string to compare.

  @returns @True when the strings have the same value, @false otherwise.
}
Function TelemetrySameText(const S1, S2: TelemetryString): Boolean;

//==============================================================================

{:
  @abstract(Returns identifier of given SCS value type.)
  Identifiers are not defined by the API, for details about naming individual
  types refer to function implementation.

  @param SCSValueType Value type.

  @returns Textual identifier of given value type.
}
Function SCSValueTypeToStr(SCSValueType: scs_value_type_t): String;

//------------------------------------------------------------------------------

{:
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

{:
  @abstract(Returns textual representation of value passed in general buffer.)
  Actual type of the value must be passed in parameter ValueType. When passed
  type of the value is not known or is not valid, an empty string is returned.

  @param Value           Buffer containing value that has be converted to text.
  @param ValueType       Type of the value passed in general buffer.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function ValueToStr(const Value; ValueType: scs_value_type_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of value passed in general buffer.)
  Actual type of the value must be passed in parameter ValueType. When passed
  type of the value is not known or is not valid, an empty string is returned.

  @param Value           Buffer containing value that has be converted to text.
  @param ValueType       Type of the value passed in general buffer.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function ValueToStr(const Value; ValueType: scs_value_type_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of value passed in general buffer.)
  Actual type of the value must be passed in parameter ValueType. When passed
  type of the value is not known or is not valid, an empty string is returned.

  @param Value           Buffer containing value that has be converted to text.
  @param ValueType       Type of the value passed in general buffer.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function ValueToStr(const Value; ValueType: scs_value_type_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
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

{:
  @abstract(Returns textual representation of @code(scs_value_t) structure.)
  When type of the value is not known, an empty string is returned.

  @param Value           Actual value to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueToStr(const Value: scs_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of @code(scs_value_t) structure.)
  When type of the value is not known, an empty string is returned.

  @param Value           Actual value to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueToStr(const Value: scs_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  Returns textual representation of scs_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueLocalizedToStr(Value: scs_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueLocalizedToStr(Value: scs_value_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSValueLocalizedToStr(Value: scs_value_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  Returns textual representation of @code(scs_named_value_t) structure.

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueToStr(const Value: scs_named_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of @code(scs_named_value_t) structure.

  @param Value           Actual value to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueToStr(const Value: scs_named_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of @code(scs_named_value_t) structure.

  @param Value           Actual value to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueToStr(const Value: scs_named_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  Returns textual representation of scs_named_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_named_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_named_value_localized_t structure.

  @param Value           Actual value to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifier is added to output
                         string.)
  @param(ShowDescriptors When set, fileds descriptors are shown for composite
                         values.)

  @returns Textual representation of given value.
}
Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  Returns textual representation of @code(scs_telemetry_frame_start_t) structure.

  @param Data     Structure to be converted to text.
  @param(TypeName When set, value type identifiers for individual fields are
                  added to output string.)

  @returns Textual representation of given structure.
}
Function TelemetryEventFrameStartToStr(const Data: scs_telemetry_frame_start_t; TypeName: Boolean = False): String;

//------------------------------------------------------------------------------

{:
  Returns textual representation of @code(scs_telemetry_configuration_t) structure.

  @param Data            Structure to be converted to text.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of @code(scs_telemetry_configuration_t) structure.

  @param Data            Structure to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of @code(scs_telemetry_configuration_t) structure.

  @param Data            Structure to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  Returns textual representation of scs_telemetry_configuration_localized_t
  structure.

  @param Data            Structure to be converted to text.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_telemetry_configuration_localized_t
  structure.

  @param Data            Structure to be converted to text.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  Returns textual representation of scs_telemetry_configuration_localized_t
  structure.

  @param Data            Structure to be converted to text.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)  
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of given structure.
}
Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Returns textual representation of event data.)
  If there are no data to bo converted, then an empty string is returned.

  @param Event           Type of event that is converted to text.
  @param Data            Data for given event. Can be @nil.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of event data.
}
Function EventDataToStr(Event: scs_event_t; Data: Pointer; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of event data.)
  If there are no data to bo converted, then an empty string is returned.

  @param Event           Type of event that is converted to text.
  @param Data            Data for given event. Can be @nil.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of event data.
}
Function EventDataToStr(Event: scs_event_t; Data: Pointer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of event data.)
  If there are no data to bo converted, then an empty string is returned.

  @param Event           Type of event that is converted to text.
  @param Data            Data for given event. Can be @nil.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of event data.
}
Function EventDataToStr(Event: scs_event_t; Data: Pointer; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

//------------------------------------------------------------------------------

{:
  @abstract(Returns textual representation of channel value passed as a pointer.)
  When the pointer is not assigned, this function will return an empty string.

  @param Value           Pointer to actual channel value. Can be @nil.
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of channel value.
}
Function ChannelValueToStr(Value: p_scs_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of channel value passed as a pointer.)
  When the pointer is not assigned, this function will return an empty string.

  @param Value           Pointer to actual channel value. Can be @nil.
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of channel value.
}
Function ChannelValueToStr(Value: p_scs_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

{:
  @abstract(Returns textual representation of channel value passed as a pointer.)
  When the pointer is not assigned, this function will return an empty string.

  @param Value           Pointer to actual channel value. Can be @nil.
  @param(Format          Format of floating point number to text conversion
                         (eg. scientific).)
  @param(Precision       Precision of floating point number (affects number to
                         text conversion).)
  @param(Digits          Number of digits in output string for floating point
                         number to text conversion.)
  @param(FormatSettings  Settings used for formatting an output string when
                         floating point number is converted to text.)
  @param(TypeName        When set, value type identifiers for individual
                         attribute values are added to output.)
  @param(ShowDescriptors When set, fields descriptors are shown for composite
                         values.)

  @returns Textual representation of channel value.
}
Function ChannelValueToStr(Value: p_scs_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;

implementation

uses
  Windows;

{==============================================================================}
{   Constants, types, variables, etc...                                        }
{==============================================================================}

const
  // Table of identifiers for individual value types (scs_value_type_t).
  // Index of each string corresponds to value type number this string is
  // identifying.
  SCSValueTypeIdentifiers: Array[0..12] of String =
    ('none','bool','s32','u32','u64','float','double','fvector','dvector',
     'euler','fplacement','dplacement','string');

{==============================================================================}
{   Unit functions and procedures implementation                               }
{==============================================================================}

Function TelemetrySameStrConv(const S1, S2: TelemetryString): Boolean;
begin
{$IFDEF Unicode}
Result := AnsiSameStr(UTF8Decode(S1),UTF8Decode(S2));
{$ELSE}
Result := WideSameStr(UTF8Decode(S1),UTF8Decode(S2));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TelemetrySameTextConv(const S1, S2: TelemetryString): Boolean;
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

//------------------------------------------------------------------------------

Function TelemetrySameStr(const S1, S2: TelemetryString): Boolean;
begin
{$IFDEF AssumeASCIIString}
Result := TelemetrySameStrNoConv(S1,S2);
{$ELSE}
Result := TelemetrySameStrConv(S1,S2);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TelemetrySameText(const S1, S2: TelemetryString): Boolean;
begin
{$IFDEF AssumeASCIIString}
Result := TelemetrySameTextNoConv(S1,S2);
{$ELSE}
Result := TelemetrySameTextConv(S1,S2);
{$ENDIF}
end;

//==============================================================================

Function SCSValueTypeToStr(SCSValueType: scs_value_type_t): String;
begin
If (Integer(SCSValueType) >= Low(SCSValueTypeIdentifiers)) and
   (Integer(SCSValueType) <= High(SCSValueTypeIdentifiers)) then
  Result := SCSValueTypeIdentifiers[Integer(SCSValueType)]
else
  Result := 'unknown';
end;

//------------------------------------------------------------------------------

Function SCSValueTypeFromStr(const Str: String): scs_value_type_t;
var
  i:  Integer;
begin
Result := SCS_VALUE_TYPE_INVALID;
For i := Low(SCSValueTypeIdentifiers) to High(SCSValueTypeIdentifiers) do
  If AnsiSameText(SCSValueTypeIdentifiers[i],Str) then
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
  FullDescriptors:  TDescriptorsArray = ('X: ','Y: ','Z: ','Heading: ','Pitch: ','Roll: ');
  EmptyDescriptors: TDescriptorsArray = ('','','','','','');
var
  Descriptors:  TDescriptorsArray;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  Function FloatValueToStr(Value: Single): String; overload;
  begin
    Result := FloatToStrF(Value,Format,Precision,Digits,FormatSettings);
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  Function FloatValueToStr(Value: Double): String; overload;
  begin
    Result := FloatToStrF(Value,Format,Precision,Digits,FormatSettings);
  end;

begin
If ShowDescriptors then Descriptors := FullDescriptors
  else Descriptors := EmptyDescriptors;
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
    Result := FloatValueToStr(scs_value_float_t(Value).value);
  SCS_VALUE_TYPE_double:
    Result := FloatValueToStr(scs_value_double_t(Value).value);
  SCS_VALUE_TYPE_fvector:
    Result := '[' + Descriptors[0] + FloatValueToStr(scs_value_fvector_t(Value).x) +
             ', ' + Descriptors[1] + FloatValueToStr(scs_value_fvector_t(Value).y) +
             ', ' + Descriptors[2] + FloatValueToStr(scs_value_fvector_t(Value).z) + ']';
  SCS_VALUE_TYPE_dvector:
    Result := '[' + Descriptors[0] + FloatValueToStr(scs_value_dvector_t(Value).x) +
             ', ' + Descriptors[1] + FloatValueToStr(scs_value_dvector_t(Value).y) +
             ', ' + Descriptors[2] + FloatValueToStr(scs_value_dvector_t(Value).z) + ']';
  SCS_VALUE_TYPE_euler:
    Result := '[' + Descriptors[3] + FloatValueToStr(scs_value_euler_t(Value).heading) +
             ', ' + Descriptors[4] + FloatValueToStr(scs_value_euler_t(Value).pitch) +
             ', ' + Descriptors[5] + FloatValueToStr(scs_value_euler_t(Value).roll) + ']';
  SCS_VALUE_TYPE_fplacement:
    Result := '[' + Descriptors[0] + FloatValueToStr(scs_value_fplacement_t(Value).position.x) +
             ', ' + Descriptors[1] + FloatValueToStr(scs_value_fplacement_t(Value).position.y) +
             ', ' + Descriptors[2] + FloatValueToStr(scs_value_fplacement_t(Value).position.z) +
            '] [' + Descriptors[3] + FloatValueToStr(scs_value_fplacement_t(Value).orientation.heading) +
             ', ' + Descriptors[4] + FloatValueToStr(scs_value_fplacement_t(Value).orientation.pitch) +
             ', ' + Descriptors[5] + FloatValueToStr(scs_value_fplacement_t(Value).orientation.roll) + ']';
  SCS_VALUE_TYPE_dplacement:
    Result := '[' + Descriptors[0] + FloatValueToStr(scs_value_dplacement_t(Value).position.x) +
             ', ' + Descriptors[1] + FloatValueToStr(scs_value_dplacement_t(Value).position.y) +
             ', ' + Descriptors[2] + FloatValueToStr(scs_value_dplacement_t(Value).position.z) +
            '] [' + Descriptors[3] + FloatValueToStr(scs_value_dplacement_t(Value).orientation.heading) +
             ', ' + Descriptors[4] + FloatValueToStr(scs_value_dplacement_t(Value).orientation.pitch) +
             ', ' + Descriptors[5] + FloatValueToStr(scs_value_dplacement_t(Value).orientation.roll) + ']';
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

  Function ValueToStrWrapper(const ValueBuff): String;
  begin
    Result := ValueToStr(ValueBuff,Value._type,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  end;

begin
case Value._type of
  SCS_VALUE_TYPE_bool:        Result := ValueToStrWrapper(Value.value_bool);
  SCS_VALUE_TYPE_s32:         Result := ValueToStrWrapper(Value.value_s32);
  SCS_VALUE_TYPE_u32:         Result := ValueToStrWrapper(Value.value_u32);
  SCS_VALUE_TYPE_u64:         Result := ValueToStrWrapper(Value.value_u64);
  SCS_VALUE_TYPE_float:       Result := ValueToStrWrapper(Value.value_float);
  SCS_VALUE_TYPE_double:      Result := ValueToStrWrapper(Value.value_double);
  SCS_VALUE_TYPE_fvector:     Result := ValueToStrWrapper(Value.value_fvector);
  SCS_VALUE_TYPE_dvector:     Result := ValueToStrWrapper(Value.value_dvector);
  SCS_VALUE_TYPE_euler:       Result := ValueToStrWrapper(Value.value_euler);
  SCS_VALUE_TYPE_fplacement:  Result := ValueToStrWrapper(Value.value_fplacement);
  SCS_VALUE_TYPE_dplacement:  Result := ValueToStrWrapper(Value.value_dplacement);
  SCS_VALUE_TYPE_string:      Result := ValueToStrWrapper(Value.value_string);
else
 {SCS_VALUE_TYPE_INVALID}
  Result := ValueToStrWrapper(Value);
end;
end;

//------------------------------------------------------------------------------

Function SCSValueLocalizedToStr(Value: scs_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSValueLocalizedToStr(Value,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSValueLocalizedToStr(Value: scs_value_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSValueLocalizedToStr(Value,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSValueLocalizedToStr(Value: scs_value_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
case Value.ValueType of
  SCS_VALUE_TYPE_string:
    If TypeName then Result := '(' + SCSValueTypeToStr(Value.ValueType) + ') ' + TelemetryStringDecode(Value.StringData)
      else Result := TelemetryStringDecode(Value.StringData);
else
  Value.BinaryData._type := Value.ValueType;
  Result := SCSValueToStr(Value.BinaryData,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
end;
end;

//------------------------------------------------------------------------------

Function SCSNamedValueToStr(const Value: scs_named_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSNamedValueToStr(Value,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSNamedValueToStr(const Value: scs_named_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSNamedValueToStr(Value,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSNamedValueToStr(const Value: scs_named_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
begin
Result := TelemetryStringDecode(APIStringToTelemetryString(Value.name));
If Value.index <> SCS_U32_NIL then Result := Result + '[' + IntToStr(Value.index) + ']';
Result := Result + ': ' + SCSValueToStr(Value.value,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
end;
end;

//------------------------------------------------------------------------------

Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := SCSNamedValueLocalizedToStr(Value,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
begin
Result := SCSNamedValueLocalizedToStr(Value,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SCSNamedValueLocalizedToStr(const Value: scs_named_value_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
begin
Result := TelemetryStringDecode(Value.Name);
If Value.Index <> SCS_U32_NIL then Result := Result + '[' + IntToStr(Value.Index) + ']';
Result := Result + ': ' + SCSValueLocalizedToStr(Value.Value,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
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
begin
Result := TelemetryEventConfigurationToStr(Data,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
begin
Result := TelemetryEventConfigurationToStr(Data,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TelemetryEventConfigurationToStr(const Data: scs_telemetry_configuration_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
var
  TempAttr: p_scs_named_value_t;
begin
Result := TelemetryStringDecode(APIStringToTelemetryString(Data.id));
TempAttr := Data.attributes;
while Assigned(TempAttr^.name) do
  begin
    Result := Result + sLineBreak + SCSNamedValueToStr(TempAttr^,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
    Inc(TempAttr);
  end;
end;

//------------------------------------------------------------------------------

Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := TelemetryEventConfigurationLocalizedToStr(Data,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
begin
Result := TelemetryEventConfigurationLocalizedToStr(Data,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TelemetryEventConfigurationLocalizedToStr(const Data: scs_telemetry_configuration_localized_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String; overload;
var
  i:  Integer;
begin
Result := TelemetryStringDecode(Data.ID);
For i := Low(Data.Attributes) to High(Data.Attributes) do
  Result := Result + sLineBreak + SCSNamedValueLocalizedToStr(Data.Attributes[i],Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
end;

//------------------------------------------------------------------------------

Function EventDataToStr(Event: scs_event_t; Data: Pointer; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := EventDataToStr(Event,Data,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function EventDataToStr(Event: scs_event_t; Data: Pointer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := EventDataToStr(Event,Data,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function EventDataToStr(Event: scs_event_t; Data: Pointer; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
If Assigned(Data) then
  case Event of
    SCS_TELEMETRY_EVENT_frame_start:
      Result := TelemetryEventFrameStartToStr(p_scs_telemetry_frame_start_t(Data)^,TypeName);
    SCS_TELEMETRY_EVENT_configuration:
      Result := TelemetryEventConfigurationToStr(p_scs_telemetry_configuration_t(Data)^,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors);
  else
    Result := '';
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function ChannelValueToStr(Value: p_scs_value_t; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := ChannelValueToStr(Value,TelemetryStringsFormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function ChannelValueToStr(Value: p_scs_value_t; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
Result := ChannelValueToStr(Value,ffGeneral,15,0,FormatSettings,TypeName,ShowDescriptors);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function ChannelValueToStr(Value: p_scs_value_t; Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings; TypeName: Boolean = False; ShowDescriptors: Boolean = False): String;
begin
If Assigned(Value) then
  Result := SCSValueToStr(Value^,Format,Precision,Digits,FormatSettings,TypeName,ShowDescriptors)
else
  Result := '';
end;

//------------------------------------------------------------------------------

initialization
  // Initialize default format settings.
  {$WARN SYMBOL_PLATFORM OFF}
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT,TelemetryStringsFormatSettings);
  {$WARN SYMBOL_PLATFORM ON}
  TelemetryStringsFormatSettings.DecimalSeparator := '.';

end.
