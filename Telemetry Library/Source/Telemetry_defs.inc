{.$DEFINE DevelopmentHints}
{.$DEFINE Documentation}
{.$DEFINE DocumentMulticastEvents}
{.$DEFINE IncludeMulticastEventHandlers}
{.$DEFINE UseCondensedHeader}
{$DEFINE MulticastEvents}
{$DEFINE AssumeASCIIString}

{$DEFINE PrecomputedItemID}
{.$DEFINE TrueIDConstants}
{.$DEFINE ManualItemIDCompute}

{$DEFINE SmallMaxChannelIndex}
{$DEFINE NoTimeLogEvents}

{.$DEFINE DeprecatedMessage}

//------------------------------------------------------------------------------

{$IFDEF Documentation}
  {$DEFINE UseCondensedHeader}
  {$DEFINE MulticastEvents}
  {$UNDEF PrecomputedItemID}
  {$DEFINE NoTimeLogEvents}
  {$IFDEF DocumentMulticastEvents}
    {$DEFINE IncludeMulticastEventHandlers}
  {$ELSE}
    {$UNDEF IncludeMulticastEventHandlers}
  {$ENDIF}
  {$DEFINE DeprecatedMessage}
{$ELSE}
  {$IFDEF MulticastEvents}
    {$DEFINE IncludeMulticastEventHandlers}
  {$ELSE}
    {$UNDEF IncludeMulticastEventHandlers}
  {$ENDIF}
  {$IF Defined(FPC) or (CompilerVersion >= 20)}
    {$DEFINE DeprecatedMessage}
  {$ELSE}
    {$UNDEF DeprecatedMessage}
  {$IFEND}
{$ENDIF}

{$IFDEF PrecomputedItemID}
  {$UNDEF ManualItemIDCompute}
  {$DEFINE TrueIDConstants}
{$ELSE}
  {$UNDEF TrueIDConstants}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE DevelopmentHints}
{$ELSE}
  {$UNDEF DevelopmentHints}
{$ENDIF}
