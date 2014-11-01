{*******************************************************************************
@abstract(Types, constants, routines, etc. used troughout the Net part of
          Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-08)
@lastmod(2013-10-08)

  TelemetryNetCommon

  ©František Milt, all rights reserved

  This file contains types definitions, constants, classes, etc. used in more
  than one unit of Net part of Telemetry library (e.g. in both client and
  server).

  Last change:  2013-10-08

  Change List:@unorderedList(
    @item(2013-10-08  - First stable version.))

*******************************************************************************}
unit TelemetryNetCommon;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

const
  // Default (can be changed to user defined value) address at which the server
  // is expected to run.
  def_ServerAddress = '127.0.0.1';
  
  // Default (can be changed to user defined value) port at which the telemetry
  // server will listen for incoming connections.
  def_ServerPort = 8753;

  // @abstract(Maximum amount of data small buffer can accommodate.)
  // This value is used when reading data from a socket. If amount of data
  // received is larger than this value, then large buffer must be allocated,
  // if it is lower or equal, then small on-stack buffer can be used. This
  // behavior is there to avoid unnecessary memory allocation for small amount
  // of data.
  cLargeBufferThreshold = 4096;

  // Empty (zeroed-out) TGUID structure. It is used as placeholder when actual
  // GUID is not known or needed.
  cEmptyGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

type
  // Enumerated type used in some packets to get or set particular server
  // settings.
  TParameterID = (pidInvalid, pidAll, pidSrvSendEvents, pidSrvSendChannels,
                  pidSrvSendConfigs, pidSrvActiveMode, pidSrvBufferChannels,
                  pidSrvSendFrameEvents, pidSrvSaveSettingsToFile,
                  pidRecKeepUtilityEvents, pidRecStoreConfigurations,
                  pidRecManageIndexedChannels, pidRecStoreChannelValues,
                  pidLstListMode, pidLstListFileName);

  // Used in BYE packet to determine reason why the connection will be closed.
  TDisconnectReason = (drUnknown, drGeneral, drError, drServerTerminated,
                       drClientTerminated, drWrongPassword, drPasswordChange,
                       drServerUnsupported, drTelemetryUnsupported,
                       drClientDisconnected, drDisconnectedByAdmin,
                       drNotAllowed);
                       
  // Used to describe operation administrator want to execute on selected
  // client.
  TClientOperation = (copDisconnect,copMakeAdmin,copStripAdmin);

implementation

end.
