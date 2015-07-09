{$IFDEF x64}
  {$DEFINE SCS_ARCHITECTURE_x64}
{$ELSE}
  {$DEFINE SCS_ARCHITECTURE_x86}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE AssertTypeSize}
{$ELSE}
  {$UNDEF AssertTypeSize}
{$ENDIF}

{.$DEFINE AssertTypeSize}
