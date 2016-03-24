{
  Following are defines needed for successful compilation of Telemetry Library
  in Delphi and/or Lazarus/FPC.
  Do not touch this part unless you are 100% sure what you are doing.
}
{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
  {$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
    {$DEFINE PurePascal}
  {$IFEND}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}    
  {$DEFINE PurePascal}
{$IFEND}

{$IF defined(FPC) and not defined(PurePascal)}
  {$ASMMODE Intel}
{$IFEND}

//------------------------------------------------------------------------------

{
  When defined, a development hints are shown during compilation.
  Should not be defined directly as it is defined automatically for debug builds
  (see further).

  Default: disabled
}
{.$DEFINE DevHints}

{
  Defined when documentation is being created by PasDoc.
  Do not activate this symbol!

  Default: disabled
}
{.$DEFINE Documentation}

{
  When defined, condensed headers are used in Telemetry Library code instead of
  using individual header files (and vice-versa).
  Note - must be enabled for compilation of example plugins and programs.

  Default: enabled
}
{$DEFINE CondensedHeaders}

{
  When defined, IDs in unit TelemetryIDs are declared as true constants. When
  not defined, those IDs are declared as writeable constants (more-or-less as
  initialized variables).
  Also, when defined, symbol ID_ManualCompute must be disabled (done
  automatically) and you will not be able to recalculate IDs.

  Default: enabled
}
{$DEFINE ID_TrueConstants}

{
  When NOT defined, then IDs in unit TelemetryIDs are automatically calculated
  in the initialization section of said unit. When defined, you have to call
  proper function for the IDs to be calculated. But note that in either case,
  those IDs have initial values.

  Default: disabled  
}
{.$DEFINE ID_ManualCompute}

{
  When defined, multicast events are used where possible, otherwise they are not
  used.

  Default: enabled
}
{$DEFINE MulticastEvents}

{
  Smaller maximum channel index is used when defined.
  See constant TelemetryRecipient.MaxChannelIndex for details.

  Default: enabled
}
{$DEFINE SmallMaxChannelIndex}
{
  Log events that are not providing time are available in log parser(s).

  Default: enabled
}
{$DEFINE NoTimeLogEvents}

//------------------------------------------------------------------------------

{
  Automatic defines. Do not touch!
}
{$IFDEF Documentation}
  {$DEFINE CondensedHeaders}
  {$DEFINE ID_TrueConstants}
  {$DEFINE MulticastEvents}
  {$DEFINE NoTimeLogEvents}
{$ENDIF}

{$IFDEF ID_TrueConstants}
  {$UNDEF ID_ManualCompute}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE DevHints}
{$ELSE}
  {$UNDEF DevHints}
{$ENDIF}
