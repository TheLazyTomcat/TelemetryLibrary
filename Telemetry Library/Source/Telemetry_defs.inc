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

{.$DEFINE DevHints}
{.$DEFINE Documentation}
{.$DEFINE CondensedHeaders}

{.$DEFINE ID_TrueConstants}
{.$DEFINE ID_ManualCompute}

{$DEFINE MulticastEvents}

{$DEFINE SmallMaxChannelIndex}
{$DEFINE NoTimeLogEvents}

//------------------------------------------------------------------------------

{$IFDEF Documentation}
  {$DEFINE CondensedHeaders}
  {$DEFINE ID_TrueConstants}
  {$DEFINE MulticastEvents}
  {$DEFINE NoTimeLogEvents}
{$ELSE}
{$ENDIF}

{$IFDEF ID_TrueConstants}
  {$UNDEF ID_ManualCompute}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE DevHints}
{$ELSE}
  {$UNDEF DevHints}
{$ENDIF}
