{.$DEFINE DevelopmentHints}
{.$DEFINE Documentation}
{.$DEFINE UseCondensedHeader}
{$DEFINE MulticastEvents}
{$DEFINE AssumeASCIIString}

{$DEFINE PrecomputedItemID}
{.$DEFINE TrueIDConstants}
{.$DEFINE ManualItemIDCompute}

{$DEFINE MaxIndexedChannelCount8}
{$DEFINE NoTimeLogEvents}

{.$DEFINE DeprecatedMessage}

//------------------------------------------------------------------------------

{$IFDEF Documentation}
  {$DEFINE UseCondensedHeader}
  {$DEFINE MulticastEvents}
  {$UNDEF PrecomputedItemID}
  {$DEFINE NoTimeLogEvents}
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

{$IF Defined(FPC) or (CompilerVersion >= 20)}
  {$DEFINE DeprecatedMessage}
{$ELSE}
  {$UNDEF DeprecatedMessage}
{$IFEND}

