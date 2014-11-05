{@html(<hr>)
@abstract(Contains class designed allocate and free memory for packets buffers.)
@author(František Milt <fmilt@seznam.cz>)
@created(2014-05-15)
@lastmod(2014-04-07)

  @bold(@NoAutoLink(TelemetryCommPacketsAllocator))

  ©František Milt, all rights reserved.

  This unit contains TTelemetryCommPacketsAllocator class (see class declaration
  for details).

  Last change:  2014-04-07

  Change List:@unorderedList(
    @item(2014-05-15 - First stable version.)
    @item(2014-04-07 - Type of parameters @code(Size) in function
                       TTelemetryCommPacketsAllocator.AllocatePacket changed
                       from signed to unsigned integer.))

@html(<hr>)}
unit TelemetryCommPacketsAllocator;

interface

{$INCLUDE '..\Telemetry_defs.inc'}

uses
  TelemetryCommPackets;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommPacketsAllocator                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommPacketsAllocator // Class declaration                        }
{==============================================================================}
{
  @abstract(Class intended as a helper for allocation and freeing memory for
            packets buffers.)
  Right now, it has only basic implementation, but should be reimplemented
  later (preallocation should be used)).

  @member(FreePacket
    Frees memory used by passed packet.

    @param Packet Packet buffer whose memory should be freed.)
}
type
  TTelemetryCommPacketsAllocator = class(TObject)
  public
  {
    Allocates memory for @code(Packet) (@code(Packet.Data) field). Memory is
    allocated to a size passed in @code(Size) parameter.

    @param Packer Packet to be initialized.
    @param Size   Requiered memory size.
  }
    procedure AllocatePacket(out Packet: TPacketBuffer; Size: LongWord); overload;
  {
    Allocates memory for @code(Packet) (@code(Packet.Data) field). Memory is
    allocated to a size passed in @code(Packet.Size) field.

    @param Packer Packet to be initialized.
    @param Size   Requiered memory size.
  }
    procedure AllocatePacket(var Packet: TPacketBuffer); overload;
    procedure FreePacket(var Packet: TPacketBuffer);
  end;

implementation

uses
  SysUtils;

{==============================================================================}
{------------------------------------------------------------------------------}
{                        TTelemetryCommPacketsAllocator                        }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TTelemetryCommPacketsAllocator // Class implementation                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TTelemetryCommPacketsAllocator  // Public methods                          }
{------------------------------------------------------------------------------}

procedure TTelemetryCommPacketsAllocator.AllocatePacket(out Packet: TPacketBuffer; Size: LongWord);
begin
Packet.Size := Size;
AllocatePacket(Packet);
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommPacketsAllocator.AllocatePacket(var Packet: TPacketBuffer);
begin
Packet.Data := AllocMem(Packet.Size);
Packet.AllocInfo.CanBeFreed := True;
end;

//------------------------------------------------------------------------------

procedure TTelemetryCommPacketsAllocator.FreePacket(var Packet: TPacketBuffer);
begin
FreeMem(Packet.Data,Packet.Size);
end;

end.
