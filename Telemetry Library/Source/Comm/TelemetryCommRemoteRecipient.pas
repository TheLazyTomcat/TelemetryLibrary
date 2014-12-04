{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit TelemetryCommRemoteRecipient;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryRecipient,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk,
  scssdk_value,
  scssdk_telemetry_event;
{$ENDIF}

type
  TEventRegisteredEvent = procedure(Sender: TObject; Event: scs_event_t; Registered: Boolean) of object;
  TEventRegisterResultEvent = procedure(Sender: TObject; Event: scs_event_t; Result: scs_result_t) of object;
  TChannelRegisteredEvent = procedure(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean) of Object;
  TChannelRegisterResultEvent = procedure(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; Result: scs_result_t) of Object;
  TChannelUnregisterResultEvent = procedure(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Result: scs_result_t) of Object;
  TBufferedChannelsEvent = procedure(Sender: TObject; Count: Integer; Channels: Pointer) of object;
  TConfigStoredEvent = procedure(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; Stored: Boolean) of object;

  TTelemetryCommRemoteRecipient = class(TTelemetryRecipient)
  private
    fParseConfigurationEvents:  Boolean;
  protected
    procedure ProcessConfigurationEvent(const Data: scs_telemetry_configuration_t); override;
  public
    procedure DoOnEventRegistered(Sender: TObject; Event: scs_event_t; Registered: Boolean); virtual; abstract;
    procedure DoOnEventRegisterResult(Sender: TObject; Event: scs_event_t; Result: scs_result_t); virtual; abstract;
    procedure DoOnEventUnregisterResult(Sender: TObject; Event: scs_event_t; Result: scs_result_t); virtual; abstract;
    procedure DoOnChannelRegistered(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Registered: Boolean); virtual; abstract;
    procedure DoOnChannelRegisterResult(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Flags: scs_u32_t; Result: scs_result_t); virtual; abstract;
    procedure DoOnChannelUnregisterResult(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; ValueType: scs_value_type_t; Result: scs_result_t); virtual; abstract;
    procedure DoOnBufferedChannels(Sender: TObject; Count: Integer; Channels: Pointer); virtual; abstract;
    procedure DoOnConfigStored(Sender: TObject; Name: TelemetryString; Index: scs_u32_t; Stored: Boolean); virtual; abstract;
  published
    property ParseConfigurationEvents: Boolean read fParseConfigurationEvents write fParseConfigurationEvents;
  end;

implementation

procedure TTelemetryCommRemoteRecipient.ProcessConfigurationEvent(const Data: scs_telemetry_configuration_t);
begin
If ParseConfigurationEvents then
  inherited ProcessConfigurationEvent(Data);
end;

end.
