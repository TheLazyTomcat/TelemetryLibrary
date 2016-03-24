{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE SCS_ARCHITECTURE_x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE SCS_ARCHITECTURE_x86}
{$ELSE}
  {$MESSAGE Fatal 'Unsupported CPU architecture'}
{$IFEND}

{$IFDEF Debug}
  {$DEFINE AssertTypeSize}
{$ELSE}
  {$UNDEF AssertTypeSize}
{$ENDIF}
