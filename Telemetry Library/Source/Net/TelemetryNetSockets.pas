{*******************************************************************************
@abstract(Sockets-based classes used in communication.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-10)
@lastmod(2013-10-10)

  TelemetryNetSockets

  ©František Milt, all rights reserved.

  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TTelemetryNetServerClientWinSocket(TNSCWinSocket)
  TTelemetryNetClientWinSocket(TNCWinSocket)
  TTelemetryNetClientSocket(TNClientSocket)

)
  Last change:  2013-10-10

  Change List:@unorderedList(
    @item(2013-10-10  - First stable version.))

*******************************************************************************}
unit TelemetryNetSockets;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  Classes, Winsock, ScktComp,
  TelemetryNetIncomingStream,
  TelemetryNetCircularBuffers,
  TelemetryNetPackets;

{==============================================================================}
{------------------------------------------------------------------------------}
{                      TTelemetryNetServerClientWinSocket                      }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TTelemetryNetServerClientWinSocket // Class declaration                   }
{==============================================================================}
{
  @abstract(Descendant of TServerClientWinSocket used in server as client
            socket.)
  It implements incoming and outgoing buffers and set of state flags used in
  communication. Each client connecting to the server spawns instance of this
  class and this object is then used for server-client communication and
  client management.

@member(fCircularPacketsBuffer See CircularPacketsBuffer property.)

@member(fIncomingStream See IncomingStream property.)

@member(fUniqueIdentificator See UniqueIdentificator property.)

@member(fWaitingForPassword See WaitingForPassword property.)

@member(fReadyToWork See ReadyToWork property.)

@member(fAdminRights See AdminRights property.)

@member(fSuperAdminRights See SuperAdminRights property.)

@member(fOnStatusChange Holds reference to OnStatusChange event handler.)

@member(SetWaitingForPassword
    Setter for WaitingForPassword property.@br
    When passed value differs from value of fWaitingForPassword, then
    OnStatusChange event is called.

    @param Value New value to be stored in fWaitingForPassword.)

@member(SetReadyToWork
    Setter for ReadyToWork property.@br
    When passed value differs from value of fReadyToWork, then OnStatusChange
    event is called.

    @param Value New value to be stored in fReadyToWork.)

@member(SetAdminRights
    Setter for AdminRights property.@br
    When passed value differs from value of fAdminRights, then OnStatusChange
    event is called.

    @param Value New value to be stored in fAdminRights.)

@member(SetSuperAdminRights
    Setter for SuperAdminRights property.@br
    When passed value differs from value of fSuperAdminRights, then
    OnStatusChange event is called.

    @param Value New value to be stored in fSuperAdminRights.)



@member(Create
    Object constructor.@br
    @bold(Note:) This object has altered order in which it calls events
    (it prevents calling of OnClientConnect event). Refer to constructor
    implementation for details.@br
    For details about ancestor class and parameters of this method, refer to
    delphi help/documentation, TServerClientWinSocket class.)

@member(Destroy
    Object destructor.)

@member(CanSendPacket
    Checks whether this socket client can send passed packet or not. It depends
    on packet type and state of the client (e.g. it is is ready to work, has
    admin rights, etc.).@br
    For detailed definitions, refer to method implementation.

    @param Packet Packet for which we need to know if this socket can send it.

    @returns @True when socket can send given packet, @false otherwise.)

@member(CanReceivePacket
    Checks whether this socket client can receive passed packet or not. It
    depends on packet type and state of the client (e.g. it is is ready to work,
    has admin rights, etc.).@br
    For detailed definitions, refer to method implementation.

    @param(Packet Packet for which we need to know if this socket can receive
                  it.)

    @returns @True when socket can receive given packet, @false otherwise.)



@member(CircularPacketsBuffer
    Circular items buffer used to store outgoing packets that cannot be send
    immediately.)

@member(IncomingStream
    Object used to buffer incoming data and process them into usable packets.
    Use this object event OnPacket to actually receive packets.)

@member(UniqueIdentificator
    Unique identificator of client connection. It is automatically generated in
    constructor.)

@member(WaitingForPassword
    Value of @true means the client is connected but is not logged to the
    server and must send login packet (password) - when different packet is
    received, it is assumed the client does not know right password and is
    immediately disconnected. @False means it has logged successfuly.@br
    When set to different value then it already has, OnStatusChange event is
    called.)

@member(ReadyToWork
    @True means the client is ready to fully communicate with server. It is set
    to @true only after client sends ready packet - meaning client decides
    whether it is ready, not the server.@br
    When set to different value then it already has, OnStatusChange event is
    called.)

@member(AdminRights
    Indicates wheather client has administrative rights.@br
    When set to different value then it already has, OnStatusChange event is
    called.)

@member(SuperAdminRights
    Indicates wheather client has super-administrator rights.@br
    When set to different value then it already has, OnStatusChange event is
    called.)

@member(OnStatusChange
    Event called whenever status of this socket changes (properties
    WaitingForPassword, ReadyToWork, AdminRights and SuperAdminRights).)

}
type
  TTelemetryNetServerClientWinSocket = class(TServerClientWinSocket)
  private
    fCircularPacketsBuffer: TCircularPacketsBuffer;
    fIncomingStream:        TIncomingStream;
    fUniqueIdentificator:   TGUID;
    fWaitingForPassword:    Boolean;
    fReadyToWork:           Boolean;
    fAdminRights:           Boolean;
    fSuperAdminRights:      Boolean;
    fOnStatusChange:        TNotifyEvent;
    procedure SetWaitingForPassword(Value: Boolean);
    procedure SetReadyToWork(Value: Boolean);
    procedure SetAdminRights(Value: Boolean);
    procedure SetSuperAdminRights(Value: Boolean);
  public
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
    destructor Destroy; override;
    Function CanSendPacket(Packet: TPacketBuffer): Boolean; virtual;
    Function CanReceivePacket(Packet: TPacketBuffer): Boolean; virtual;
  published
    property CircularPacketsBuffer: TCircularPacketsBuffer read fCircularPacketsBuffer;
    property IncomingStream: TIncomingStream read fIncomingStream;
    property UniqueIdentificator: TGUID read fUniqueIdentificator;
    property WaitingForPassword: Boolean read fWaitingForPassword write SetWaitingForPassword;
    property ReadyToWork: Boolean read fReadyToWork write SetReadyToWork;
    property AdminRights: Boolean read fAdminRights write SetAdminRights;
    property SuperAdminRights: Boolean read fSuperAdminRights write SetSuperAdminRights;
    property OnStatusChange: TNotifyEvent read fOnStatusChange write fOnStatusChange;
  end;

  // For shortened writing of TTelemetryNetServerClientWinSocket class name.
  TNSCWinSocket = TTelemetryNetServerClientWinSocket;


{==============================================================================}
{------------------------------------------------------------------------------}
{                         TTelemetryNetClientWinSocket                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TTelemetryNetClientWinSocket // Class declaration                         }
{==============================================================================}
{
  @abstract(Class used as connection endpoint in client socket.)
  It replaces original socket of class TClientWinSocket, because there is no way
  to implement needed buffers into in.

@member(fCircularPacketsBuffer See CircularPacketsBuffer property.)

@member(fIncomingStream See IncomingStream property.)



@member(Create
    Object constructor.)

@member(Destroy
    Object destructor.)



@member(CircularPacketsBuffer
    Circular items buffer used to store outgoing packets that cannot be send
    immediately.)

@member(IncomingStream
    Object used to buffer incoming data and process them into usable packets.
    Use this object event OnPacket to actually receive packets.)    
}
  TTelemetryNetClientWinSocket = class(TClientWinSocket)
  private
    fCircularPacketsBuffer: TCircularPacketsBuffer;
    fIncomingStream:        TIncomingStream;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
  published
    property CircularPacketsBuffer: TCircularPacketsBuffer read fCircularPacketsBuffer;
    property IncomingStream: TIncomingStream read fIncomingStream;
  end;

  // For shortened writing of TTelemetryNetClientWinSocket class name.
  TNCWinSocket = TTelemetryNetClientWinSocket;


{==============================================================================}
{------------------------------------------------------------------------------}
{                           TTelemetryNetClientSocket                          }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TTelemetryNetClientSocket // Class declaration                            }
{==============================================================================}
{
  @abstract(Reimplementation of TClientSocket class.)
  This class is implemented in the same way as TClientSocket in Delphi 7, only
  differnce is, it uses TTelemetryNetClientWinSocket as endpoint
  @noAutoLink(socket) instead of TClientWinSocket - because
  TTelemetryNetClientWinSocket implements buffers used during communication.
  There is currently no other method how to force use of
  TTelemetryNetClientWinSocket other than completely reimplementing the client
  @noAutoLink(socket).

  For members description, refer to delphi help/documentation.
}
  TTelemetryNetClientSocket = class(TCustomSocket)
  private
    fClientSocket: TTelemetryNetClientWinSocket;
  protected
    procedure DoActivate(Value: Boolean); override;
    Function GetClientType: TClientType;
    procedure SetClientType(Value: TClientType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Socket: TTelemetryNetClientWinSocket read fClientSocket;
  published
    property Active;
    property Address;
    property ClientType: TClientType read GetClientType write SetClientType;
    property Host;
    property Port;
    property Service;
    property OnLookup;
    property OnConnecting;
    property OnConnect;
    property OnDisconnect;
    property OnRead;
    property OnWrite;
    property OnError;
  end;

  // For shortened writing of TTelemetryNetClientSocket class name.
  TNClientSocket = TTelemetryNetClientSocket;
  

implementation

uses
  SysUtils;

{==============================================================================}
{    TTelemetryNetServerClientWinSocket // Implementation                      }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TTelemetryNetServerClientWinSocket // Private methods                     }
{------------------------------------------------------------------------------}

procedure TTelemetryNetServerClientWinSocket.SetWaitingForPassword(Value: Boolean);
begin
If fWaitingForPassword <> Value then
  begin
    fWaitingForPassword := Value;
    If Assigned(fOnStatusChange) then fOnStatusChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServerClientWinSocket.SetReadyToWork(Value: Boolean);
begin
If fReadyToWork <> Value then
  begin
    fReadyToWork := Value;
    If Assigned(fOnStatusChange) then fOnStatusChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServerClientWinSocket.SetAdminRights(Value: Boolean);
begin
If fAdminRights <> Value then
  begin
    fAdminRights := Value;
    If Assigned(fOnStatusChange) then fOnStatusChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetServerClientWinSocket.SetSuperAdminRights(Value: Boolean);
begin
If fSuperAdminRights <> Value then
  begin
    fSuperAdminRights := Value;
    If Assigned(fOnStatusChange) then fOnStatusChange(Self);
  end;
end;

{------------------------------------------------------------------------------}
{    TTelemetryNetServerClientWinSocket // Public methods                      }
{------------------------------------------------------------------------------}

constructor TTelemetryNetServerClientWinSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
var
  ConnectEvent:  TSocketNotifyEvent;
begin
// The OnClientConnect is called before this procedure is completed (in
// inherited constructor to be precise), but the buffers are not created at that
// time. So the event must be postponed until this procedure ends - it is called
// again manually at the end of the OnGetSocket handler.
ConnectEvent := ServerWinSocket.OnClientConnect;
ServerWinSocket.OnClientConnect := nil;
inherited Create(Socket,ServerWinSocket);
ServerWinSocket.OnClientConnect := ConnectEvent;
fCircularPacketsBuffer := TCircularPacketsBuffer.Create(Self);
fIncomingStream := TIncomingStream.Create(Self);
CreateGUID(fUniqueIdentificator);
WaitingforPassword := True;
ReadyToWork := False;
AdminRights := False;
SuperAdminRights := False;
end;

//------------------------------------------------------------------------------

destructor TTelemetryNetServerClientWinSocket.Destroy;
begin
fIncomingStream.Free;
fCircularPacketsBuffer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServerClientWinSocket.CanSendPacket(Packet: TPacketBuffer): Boolean;
begin
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_PING,
  TN_PACKET_BYE:                Result := True;
  TN_PACKET_VERSION,
  TN_PACKET_TELEMETRY_VERSION:  Result := not WaitingForPassword;
  TN_PREFIX_ADMIN..
  TN_PREFIX_ADMIN + $FFF:       Result := not WaitingForPassword and ReadyToWork and
                                          (AdminRights or SuperAdminRights);
  TN_PREFIX_SUPERADMIN..
  TN_PREFIX_SUPERADMIN + $FFF:      Result := not WaitingForPassword and ReadyToWork and
                                              SuperAdminRights;                                          
else
  Result := not WaitingForPassword and ReadyToWork;
end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetServerClientWinSocket.CanReceivePacket(Packet: TPacketBuffer): Boolean;
begin
case GetPacketHeader(Packet).PacketID of
  TN_PACKET_HI:                     Result := True;
  TN_PACKET_TELEMETRY_VERSION_GET,
  TN_PACKET_READY:                  Result := not WaitingForPassword;
  TN_PREFIX_ADMIN..
  TN_PREFIX_ADMIN + $FFF:           Result := not WaitingForPassword and ReadyToWork and
                                              (AdminRights or SuperAdminRights);
  TN_PREFIX_SUPERADMIN..
  TN_PREFIX_SUPERADMIN + $FFF:      Result := not WaitingForPassword and ReadyToWork and
                                              SuperAdminRights;
else
  Result := not WaitingForPassword and ReadyToWork;
end;
end;


{==============================================================================}
{    TTelemetryNetClientWinSocket // Implementation                            }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TTelemetryNetClientWinSocket // Public methods                            }
{------------------------------------------------------------------------------}

constructor TTelemetryNetClientWinSocket.Create(ASocket: TSocket);
begin
inherited Create(ASocket);
fCircularPacketsBuffer := TCircularPacketsBuffer.Create(Self);
fIncomingStream := TIncomingStream.Create(Self);
end;

//------------------------------------------------------------------------------

destructor TTelemetryNetClientWinSocket.Destroy;
begin
fIncomingStream.Free;
fCircularPacketsBuffer.Free;
inherited;
end;

{==============================================================================}
{    TTelemetryNetClientSocket // Implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TTelemetryNetClientSocket // Protected methods                            }
{------------------------------------------------------------------------------}

procedure TTelemetryNetClientSocket.DoActivate(Value: Boolean);
begin
If (Value <> fClientSocket.Connected) and not (csDesigning in ComponentState) then
  begin
    If fClientSocket.Connected then
      fClientSocket.Disconnect(fClientSocket.SocketHandle)
    else
      fClientSocket.Open(Host,Address,Service,Port,ClientType = ctBlocking);
  end;
end;

//------------------------------------------------------------------------------

Function TTelemetryNetClientSocket.GetClientType: TClientType;
begin
Result := fClientSocket.ClientType;
end;

//------------------------------------------------------------------------------

procedure TTelemetryNetClientSocket.SetClientType(Value: TClientType);
begin
fClientSocket.ClientType := Value;
end;

{------------------------------------------------------------------------------}
{    TTelemetryNetClientSocket // Public methods                               }
{------------------------------------------------------------------------------}

constructor TTelemetryNetClientSocket.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
fClientSocket := TTelemetryNetClientWinSocket.Create(INVALID_SOCKET);
InitSocket(FClientSocket);
end;

//------------------------------------------------------------------------------

destructor TTelemetryNetClientSocket.Destroy;
begin
fClientSocket.Free;
inherited;
end;

end.
