{*******************************************************************************
@abstract(Per-connection buffer for incoming data.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-10)
@lastmod(2013-10-10)

  TelemetryNetIncomingStream

  ©František Milt, all rights reserved.

  This unit contains TIncomingStream class (see declaration of this class for
  details).

  Last change:  2013-10-10

  Change List:@unorderedList(
    @item(2013-10-10  - First stable version.))

*******************************************************************************}
unit TelemetryNetIncomingStream;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  Classes, ScktComp,
  TelemetryNetPackets;

{==============================================================================}
{------------------------------------------------------------------------------}
{                               TIncomingStream                                }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  // Dafault maximum size of stream (for details, see TIncomingStream.MaxSize
  // property).
  def_MaxSize = 16384 {16 KiB};


{==============================================================================}
{    TIncomingStream // Class declaration                                      }
{==============================================================================}
{
  @abstract(TIncomingStream serves as per-connection buffer for incoming data.)
  Server-client communication is realized trought variable size packets, but
  incoming data do not have guaranteed size. That means, any incoming data have
  to be checked if they are complete, and when not, they must be stored and next
  received part must be appended at the end of them and the whole stream must be
  checked again.

  For better undestanding, here is small example:
@unorderedList(
  @item(sender sends packet 120 bytes long)
  @item(receiver receives 100 bytes)
  @item(they are checket for length, but are marked as incomplete (each packet
        has minimal size and actual size minus header size is stored at the
        beginning of each packet - see unit TelemetryNetPackets for details))
  @item(incomplete data are stored to stream)
  @item(receiver receives 24 bytes)
  @item(they are appended and the whole stream is checked)
  @item(as the stream is now evaluated as one complete packet 120 bytes long
        plus 4 additional unknown bytes, the first 120 bytes are passed as valid
        packet for further processing)
  @item(after processing is done, the packet is removed, which means the stream
        now contains 4 bytes (probably first part of next packet))
)

  As this class is direct descendant of TMemoryStream, it contains all of
  its methods and fields and the actual memory stream is used internally.

  
@member(fSocket See Socket property.)

@member(fMaxSize See MaxSize property.)

@member(fOnOverflowClear Holds referece to OnOverflowClear event handler.)

@member(fOnPacket Holds reference to OnPacket event handler.)



@member(DirectBufferProcessing
    When TIncomingStream receive any data, they are first send to this function.
    It will evaluate whether these data can be passed as one whole packet for
    further processing, without any store operation, and if yes, they are
    passed (an OnPacket event is called).@br
    To directly pass the data, internal stream must be empty and the buffer must
    contain just one whole valid packet.
    If the data cannot be directly passed, they are stored (see
    WriteIncomingBuffer method for details).@br

    @param Buffer Incoming data that has to be processed.

    @returns(@True when buffer was processed directly, @false when it was
             stored.))



@member(Create
    Object constructor.@br
    If passed aMaxSize is smaller than def_MaxSize constant, then actual maximum
    size is set to def_MaxSize.

    @param aSocket Socket to which created instance will be bound. Can be @nil.

    @param(aMaxSize Maximum size the stream can growth to (see MaxSize
                    property).))

@member(WriteIncomingBuffer
    When connection receives data, they are passed to this method.
    If the data buffer cannot be directly processed, this function will
    store it in internal memory stream and calls GetPackets to check for
    packets in the whole stored stream. After that, size of the stream is
    evaluated and if it exceeds maximum allowed size, OnOverflowClear event is
    called and the stream is cleared.

    @param Buffer Incoming data.
    @param Count  Size of the buffer on bytes.)

@member(GetPackets
    Searches the internal stream for any packet. If one is found, it is passed
    to OnPacket event. At the end of this method, all packets found in stream
    are deleted along with any data preceding them. Only data after the last
    packet are kept stored.)



@member(Socket
    @noAutoLink(Socket) to which currend instance is bound.@br
    This property is set during creation and thus is not initialized. Can be
    @nil.@br
    This object is passed as @noAutoLink("Socket") parameter in event calls.)

@member(MaxSize
    Maximum size of the stream. If internal stream holding incoming data becames
    larger than this value, then OnOverflowClear event is called and after that,
    the entire stream is cleared (deleted, not destroyed).@br
    This mechanism is there to prevent uncontrolled stream growth when the
    connection produces large number of invalid data.@br
    This value is set during creation and thus is not initialized.)

@member(OnOverflowClear
    Called when the internal stream exceeds maximum allowed size, just before
    it is cleared. If you want to react before any data are deleted, this is
    the right place.)

@member(OnPacket
    Called whenever a valid packet is found in incoming data stream.@br
    You must immediately process this packet, as it will be deleted right after
    the event handler returns.)
}
type
  TIncomingStream = class(TMemoryStream)
  private
    fSocket:          TCustomWinSocket;
    fMaxSize:         LongWord;
    fOnOverflowClear: TSocketNotifyEvent;
    fOnPacket:        TPacketNotifyEvent;
  protected
    Function DirectBufferProcessing(const Buffer; Count: Integer): Boolean; virtual;
  public
    constructor Create(aSocket: TCustomWinSocket = nil; aMaxSize: LongWord = def_MaxSize);
    procedure WriteIncomingBuffer(const Buffer; Count: Integer); virtual;
    procedure GetPackets; virtual;
  published
    property Socket: TCustomWinsocket read fSocket;
    property MaxSize: LongWord read fMaxSize;
    property OnOverflowClear: TSocketNotifyEvent read fOnOverflowClear write fOnOverflowClear;
    property OnPacket: TPacketNotifyEvent read fOnPacket write fOnPacket;
  end;

implementation

uses
  Windows;

{==============================================================================}
{    TIncomingStream // Implementation                                         }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TIncomingStream // Protected methods                                      }
{------------------------------------------------------------------------------}

Function TIncomingStream.DirectBufferProcessing(const Buffer; Count: Integer): Boolean;
var
  PacketHeader: PPacketHeader;
  PacketBuffer: TPacketBuffer;
begin
Result := False;
If (Size <= 0) and (Count >= TN_MIN_PACKET_SIZE) then
  begin
    PacketHeader := @Buffer;
    If (PacketHeader^.Signature = TN_PACKET_SIGNATURE) and
       (PacketHeader^.PayloadSize = (Count - SizeOf(TPacketHeader))) then
      begin
        PacketBuffer.Size := Count;
        PacketBuffer.Data := @Buffer;
        If Assigned(fOnPacket) then fOnPacket(Self,Socket,PacketBuffer);
        Result := True;
      end;
  end;
end;

{------------------------------------------------------------------------------}
{    TIncomingStream // Public methods                                         }
{------------------------------------------------------------------------------}

constructor TIncomingStream.Create(aSocket: TCustomWinSocket = nil; aMaxSize: LongWord = def_MaxSize);
begin
inherited Create;
fSocket := aSocket;
If aMaxSize < def_MaxSize then fMaxSize := def_MaxSize
  else fMaxSize := aMaxSize;
end;

//------------------------------------------------------------------------------

procedure TIncomingStream.WriteIncomingBuffer(const Buffer; Count: Integer);
begin
If not DirectBufferProcessing(Buffer,Count) then
  begin
    Seek(0,soFromEnd);
    WriteBuffer(Buffer,Count);
    GetPackets;
    If Size > MaxSize then
      begin
        If Assigned(fOnOverflowClear) then fOnOverflowClear(Self,Socket);
        Clear;
      end;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DevelopmentHints}
  {$MESSAGE HINT 'Development hint: Review this method later.'}
{$ENDIF}
procedure TIncomingStream.GetPackets;
var
  TempHeader:   PPacketHeader;
  PacketBuffer: TPacketBuffer;
  Offset:       Integer;
  CropTo:       Integer;
begin
If Size >= TN_MIN_PACKET_SIZE then
begin
  Offset := 0;
  CropTo := 0;
  While Offset <= (Size - TN_MIN_PACKET_SIZE) do
    begin
      TempHeader := PPacketHeader(NativeInt(Self.Memory) + Offset);
      If TempHeader^.Signature = TN_PACKET_SIGNATURE then
        begin
          If TempHeader^.PayloadSize + Offset + SizeOf(TPacketHeader) <= Size then
            begin
              If Assigned(fOnPacket) then
                begin
                  PacketBuffer.Size := TempHeader^.PayloadSize + SizeOf(TPacketHeader);
                  PacketBuffer.Data := TempHeader;
                  fOnPacket(Self,Socket,PacketBuffer);
                end;
              Inc(Offset,TempHeader^.PayloadSize + SizeOf(TPacketHeader));
              CropTo := Offset;
            end
          else Break;
        end
      else Inc(Offset);
    end;
  If CropTo > 0 then
    begin
      If CropTo < Size then
        begin
          CopyMemory(Self.Memory,Pointer(NativeInt(Self.Memory) + CropTo),Size - CropTo);
          Size := Size - CropTo;
        end
      else Clear;
    end;
end;
end;

end.
