{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{@html(<hr>)
@abstract(Types, constants, routines, etc. used troughout communication part of
          Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-15)
@lastmod(2014-05-15)

  @bold(@NoAutoLink(TelemetryCommon))

  ©František Milt, all rights reserved.

  This file is intended to provide types, constants, routines, etc. used
  throughout (in more than one unit) the communication part of Telemetry
  library.

  Last change:  2014-05-15

  Change List:@unorderedList(
    @item(2014-05-15 - First stable version.))

@html(<hr>)}
unit TelemetryCommCommon;

interface

type
  {
    @abstract(Type used to describe version of communication protocol.)
    Connection endpoint must support required version in order to communicate.
  }
  TProtocolVersion = LongWord;


  TCommunicationMode = (cmServer,cmClient,cmPeer,cmUnknown);

const
  // Constant used as connection data when packet should be sent to all peers.
  cSendToAll = nil;

implementation

end.
