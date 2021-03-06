{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SimpleLog

  ©František Milt 2016-07-12

  Version 1.3.7

===============================================================================}
{$IFNDEF SimpleLog_Include}
unit SimpleLog;
{$ENDIF}

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  // Activate symbol BARE_FPC if you want to compile this unit outside of Lazarus.
  {.$DEFINE BARE_FPC}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs, SyncObjs;

type
  TLogEvent = procedure(Sender: TObject; LogText: String) of Object;

{==============================================================================}
{    TSimpleLog // Class declaration                                           }
{==============================================================================}
  TSimpleLog = class(TObject)
  private
    fFormatSettings:          TFormatSettings;
    fTimeFormat:              String;
    fTimeSeparator:           String;
    fTimeOfCreation:          TDateTime;
    fBreaker:                 String;
    fTimeStamp:               String;
    fStartStamp:              String;
    fEndStamp:                String;
    fAppendStamp:             String;
    fHeader:                  String;
    fIndentNewLines:          Boolean;
    fThreadLocked:            Boolean;
    fInternalLog:             Boolean;
    fWriteToConsole:          Boolean;
    fStreamToFile:            Boolean;
    fConsoleBinded:           Boolean;
    fStreamAppend:            Boolean;
    fStreamFileName:          String;
    fStreamFileAccessRights:  Cardinal;
    fForceTime:               Boolean;
    fForcedTime:              TDateTIme;
    fLogCounter:              Integer;
    fThreadLock:              TCriticalSection;
    fInternalLogObj:          TStringList;
    fExternalLogs:            TObjectList;
    fStreamFile:              TFileStream;
    fConsoleBindFlag:         Integer;
    fOnLog:                   TLogEvent;
    procedure SetWriteToConsole(Value: Boolean);    
    procedure SetStreamToFile(Value: Boolean);
    procedure SetStreamFileName(Value: String);
    Function GetInternalLogCount: Integer;
    Function GetExternalLogsCount: Integer;    
    Function GetExternalLog(Index: Integer): TStrings;
  protected
    Function ReserveConsoleBind: Boolean; virtual;
    Function GetCurrentTime: TDateTime; virtual;
    Function GetDefaultStreamFileName: String; virtual;
    Function GetTimeAsStr(Time: TDateTime; const Format: String = '$'): String; virtual;
    procedure DoIndentNewLines(var Str: String; IndentCount: Integer); virtual;    
    procedure ProtectedAddLog(LogText: String; IndentCount: Integer = 0; LineBreak: Boolean = True); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ThreadLock; virtual;
    procedure ThreadUnlock; virtual;
    procedure AddLogNoTime(const Text: String); virtual;
    procedure AddLogTime(const Text: String; Time: TDateTime); virtual;
    procedure AddLog(const Text: String); virtual;
    procedure AddEmpty; virtual;
    procedure AddBreaker; virtual;
    procedure AddTimeStamp; virtual;
    procedure AddStartStamp; virtual;
    procedure AddEndStamp; virtual;
    procedure AddAppendStamp; virtual;
    procedure AddHeader; virtual;
    procedure ForceTimeSet(Time: TDateTime); virtual;
    Function InternalLogGetLog(LogIndex: Integer): String; virtual;
    Function InternalLogGetAsText: String; virtual;
    procedure InternalLogClear; virtual;
    Function InternalLogSaveToFile(const FileName: String; Append: Boolean = False): Boolean; virtual;
    Function InternalLogLoadFromFile(const FileName: String; Append: Boolean = False): Boolean; virtual;
    Function BindConsole: Boolean; virtual;
    procedure UnbindConsole; virtual;
    Function ExternalLogAdd(ExternalLog: TStrings): Integer; virtual;
    Function ExternalLogIndexOf(ExternalLog: TStrings): Integer; virtual;
    Function ExternalLogRemove(ExternalLog: TStrings): Integer; virtual;
    procedure ExternalLogDelete(Index: Integer); virtual;
    property FormatSettings: TFormatSettings read fFormatSettings write fFormatSettings;
    property ExternalLogs[Index: Integer]: TStrings read GetExternalLog; default;
  published
    property TimeFormat: String read fTimeFormat write fTimeFormat;
    property TimeSeparator: String read fTimeSeparator write fTimeSeparator;
    property TimeOfCreation: TDateTime read fTimeOfCreation;
    property Breaker: String read fBreaker write fBreaker;
    property TimeStamp: String read fTimeStamp write fTimeStamp;
    property StartStamp: String read fStartStamp write fStartStamp;
    property EndStamp: String read fEndStamp write fEndStamp;
    property AppendStamp: String read fAppendStamp write fAppendStamp;
    property Header: String read fHeader write fHeader;
    property IndentNewLines: Boolean read fIndentNewLines write fIndentNewLines;
    property ThreadLocked: Boolean read fThreadLocked write fThreadLocked;
    property InternalLog: Boolean read fInternalLog write fInternalLog;
    property WriteToConsole: Boolean read fWriteToConsole write SetWriteToConsole;
    property StreamToFile: Boolean read fStreamToFile write SetStreamToFile;
    property ConsoleBinded: Boolean read fConsoleBinded write fConsoleBinded;
    property StreamAppend: Boolean read fStreamAppend write fStreamAppend;
    property StreamFileName: String read fStreamFileName write SetStreamFileName;
    property StreamFileAccessRights: Cardinal read fStreamFileAccessRights write fStreamFileAccessRights;
    property ForceTime: Boolean read fForceTime write fForceTime;
    property ForcedTime: TDateTIme read fForcedTime write fForcedTime;
    property LogCounter: Integer read fLogCounter;
    property InternalLogCount: Integer read GetInternalLogCount;
    property ExternalLogsCount: Integer read GetExternalLogsCount;
    property OnLog: TLogEvent read fOnLog write fOnLog;
  end;


{$IFDEF SimpleLog_Include}
var
  LogActive:      Boolean = False;
  LogFileName:    String = '';
{$ENDIF}

{$IF not Declared(FPC_FULLVERSION)}
const
(*
  Delphi 7 requires this, otherwise they throw error on comparison in
  {$IF FPC_FULLVERSION < ...} condition.
*)
  FPC_FULLVERSION = Integer(0);
{$IFEND}

implementation

uses
  Windows, StrUtils
  {$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
  (*
    If compiler throws error that LazUTF8 unit cannot be found, you have to
    add LazUtils to required packages (Project > Project Inspector).
  *)
  , LazUTF8
  {$IF (FPC_FULLVERSION < 20701)}
  , LazFileUtils
  {$IFEND}
  {$IFEND};

{==============================================================================}
{    TSimpleLog // Console binding                                             }
{==============================================================================}

type
  IOFunc = Function(var F: TTextRec): Integer;

const
  ERR_SUCCESS                 = 0;
  ERR_UNSUPPORTED_MODE        = 10;
  ERR_WRITE_FAILED            = 11;
  ERR_READ_FAILED             = 12;
  ERR_FLUSH_FUNC_NOT_ASSIGNED = 13;

  UDI_OUTFILE = 1;

  CBF_LOCKED   = 1;
  CBF_UNLOCKED = 0;

//------------------------------------------------------------------------------

Function SLCB_Output(var F: TTextRec): Integer;
var
  BytesWritten: DWord;
  StrBuffer:    String;
begin
If WriteConsole(F.Handle,F.BufPtr,F.BufPos,{%H-}BytesWritten,nil) then
  begin
    SetLength(StrBuffer,F.BufPos);
    Move(F.Buffer,PChar(StrBuffer)^,F.BufPos * SizeOf(Char));
    TSimpleLog(Addr(F.UserData[UDI_OUTFILE])^).ProtectedAddLog(StrBuffer,0,False);
    Result := ERR_SUCCESS;
  end
else Result := ERR_WRITE_FAILED;
F.BufPos := 0;
end;

//------------------------------------------------------------------------------

Function SLCB_Input(var F: TTextRec): Integer;
var
  BytesRead:  DWord;
  StrBuffer:  String;
begin
If ReadConsole(F.Handle,F.BufPtr,F.BufSize,{%H-}BytesRead,nil) then
  begin
    SetLength(StrBuffer,BytesRead);
    Move(F.Buffer,PChar(StrBuffer)^,BytesRead * SizeOf(Char));
    TSimpleLog(Addr(F.UserData[UDI_OUTFILE])^).ProtectedAddLog(StrBuffer,0,False);
    F.bufend := BytesRead;
    Result := ERR_SUCCESS;
  end
else Result := ERR_READ_FAILED;
F.BufPos := 0;
end;

//------------------------------------------------------------------------------

Function SLCB_Flush(var F: TTextRec): Integer;
begin
case F.Mode of
  fmOutput: begin
              If Assigned(F.InOutFunc) then IOFunc(F.InOutFunc)(F);
              Result := ERR_SUCCESS;
            end;
  fmInput:  begin
              F.BufPos := 0;
              F.BufEnd := 0;
              Result := ERR_SUCCESS;
            end;
else
  Result := ERR_UNSUPPORTED_MODE;
end;
end;

//------------------------------------------------------------------------------

Function SLCB_Open(var F: TTextRec): Integer;
begin
case F.Mode of
  fmOutput: begin
              F.Handle := GetStdHandle(STD_OUTPUT_HANDLE);
              F.InOutFunc := @SLCB_Output;
              Result := ERR_SUCCESS;
            end;
  fmInput:  begin
              F.Handle := GetStdHandle(STD_INPUT_HANDLE);
              F.InOutFunc := @SLCB_Input;
              Result := ERR_SUCCESS;
            end;
else
  Result := ERR_UNSUPPORTED_MODE;
end;
end;

//------------------------------------------------------------------------------

Function SLCB_Close(var F: TTextRec): Integer;
begin
If Assigned(F.FlushFunc) then
  Result := IOFunc(F.FlushFunc)(F)
else
  Result := ERR_FLUSH_FUNC_NOT_ASSIGNED;
F.Mode := fmClosed;
end;

//------------------------------------------------------------------------------

procedure AssignSLCB(var T: Text; LogObject: TSimpleLog);
begin
with TTextRec(T) do
  begin
    Mode := fmClosed;
  {$IFDEF FPC}
    LineEnd := sLineBreak;
  {$ELSE}
    Flags := tfCRLF;
  {$ENDIF}
    BufSize := SizeOf(Buffer);
    BufPos := 0;
    BufEnd := 0;
    BufPtr := @Buffer;
    OpenFunc := @SLCB_Open;
    FlushFunc := @SLCB_Flush;
    CloseFunc := @SLCB_Close;
    TSimpleLog(Addr(UserData[UDI_OUTFILE])^) := LogObject;
    Name := '';
  end;
end;

{==============================================================================}
{    TSimpleLog // Class implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TSimpleLog // Constants                                                   }
{------------------------------------------------------------------------------}

const
  HeaderLines = '================================================================================';

//--- default settings ---
  def_TimeFormat             = 'yyyy-mm-dd hh:nn:ss.zzz';
  def_TimeSeparator          = ' //: ';
  def_Breaker                = '--------------------------------------------------------------------------------';
  def_TimeStamp              = def_Breaker + sLineBreak +  'TimeStamp: %s' + sLineBreak + def_Breaker;
  def_StartStamp             = def_Breaker + sLineBreak +  '%s - Starting log' + sLineBreak + def_Breaker;
  def_EndStamp               = def_Breaker + sLineBreak +  '%s - Ending log' + sLineBreak + def_Breaker;
  def_AppendStamp            = def_Breaker + sLineBreak +  '%s - Appending log' + sLineBreak + def_Breaker;
  def_Header                 = HeaderLines + sLineBreak +
                               '              Created by SimpleLog 1.3, (c)2015-2016 Frantisek Milt' +
                               sLineBreak + HeaderLines;
  def_IndentNewLines         = False;
  def_ThreadLocked           = False;
  def_InternalLog            = True;
  def_WriteToConsole         = False;
  def_StreamToFile           = False;
  def_StreamAppend           = False;
  def_StreamFileAccessRights = fmShareDenyWrite;
  def_ForceTime              = False;


{------------------------------------------------------------------------------}
{    TSimpleLog // Private routines                                            }
{------------------------------------------------------------------------------}

procedure TSimpleLog.SetWriteToConsole(Value: Boolean);
begin
If not fConsoleBinded then fWriteToConsole := Value;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetStreamToFile(Value: Boolean);
begin
If fStreamToFile <> Value then
  If fStreamToFile then
    begin
      FreeAndNil(fStreamFile);
      fStreamToFile := Value;
    end
  else
    begin
    {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701) and not Defined(BARE_FPC)}
      If FileExistsUTF8(fStreamFileName) then
    {$ELSE}
      If FileExists(fStreamFileName) then
    {$IFEND}
    {$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
        fStreamFile := TFileStream.Create(UTF8ToSys(fStreamFileName),fmOpenReadWrite or fStreamFileAccessRights)
      else
        fStreamFile := TFileStream.Create(UTF8ToSys(fStreamFileName),fmCreate or fStreamFileAccessRights);
    {$ELSE}
        fStreamFile := TFileStream.Create(fStreamFileName,fmOpenReadWrite or fStreamFileAccessRights)
      else
        fStreamFile := TFileStream.Create(fStreamFileName,fmCreate or fStreamFileAccessRights);
    {$IFEND}
      If fStreamAppend then fStreamFile.Seek(0,soEnd)
        else fStreamFile.Size := 0;
      fStreamToFile := Value;
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetStreamFileName(Value: String);
begin
If Value = '' then Value := GetDefaultStreamFileName;
If not AnsiSameText(fStreamFileName,Value) then
  begin
    If fStreamToFile then
      begin
        fStreamFileName := Value;
        FreeAndNil(fStreamFile);
      {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701) and not Defined(BARE_FPC)}
        If FileExistsUTF8(fStreamFileName) then
      {$ELSE}
        If FileExists(fStreamFileName) then
      {$IFEND}
      {$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
          fStreamFile := TFileStream.Create(UTF8ToSys(fStreamFileName),fmOpenReadWrite or fStreamFileAccessRights)
        else
          fStreamFile := TFileStream.Create(UTF8ToSys(fStreamFileName),fmCreate or fStreamFileAccessRights);
      {$ELSE}
          fStreamFile := TFileStream.Create(fStreamFileName,fmOpenReadWrite or fStreamFileAccessRights)
        else
          fStreamFile := TFileStream.Create(fStreamFileName,fmCreate or fStreamFileAccessRights);
      {$IFEND}
        If fStreamAppend then fStreamFile.Seek(0,soEnd)
          else fStreamFile.Size := 0;
      end
    else fStreamFileName := Value;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetInternalLogCount: Integer;
begin
Result := fInternalLogObj.Count;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetExternalLogsCount: Integer;
begin
Result := fExternalLogs.Count;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetExternalLog(Index: Integer): TStrings;
begin
If (Index >= 0) and (Index < fExternalLogs.Count) then
  Result := TStrings(fExternalLogs[Index])
else
 raise exception.CreateFmt('TSimpleLog.GetExternalLog: Index (%d) out of bounds.',[Index]);
end;

{------------------------------------------------------------------------------}
{    TSimpleLog // Protected routines                                          }
{------------------------------------------------------------------------------}

Function TSimpleLog.ReserveConsoleBind: Boolean;
begin
Result := InterlockedExchange(fConsoleBindFlag,CBF_LOCKED) = CBF_UNLOCKED;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetCurrentTime: TDateTime;
begin
If ForceTime then Result := ForcedTime
  else Result := Now;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetDefaultStreamFileName: String;
begin
{$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
Result := SysToUTF8(ParamStr(0)) + '[' + GetTimeAsStr(fTimeOfCreation,'YYYY-MM-DD-HH-NN-SS') + '].log';
{$ELSE}
Result := ParamStr(0) + '[' + GetTimeAsStr(fTimeOfCreation,'YYYY-MM-DD-HH-NN-SS') + '].log';
{$IFEND}
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTimeAsStr(Time: TDateTime; const Format: String = '$'): String;
begin
If Format <> '$' then DateTimeToString(Result,Format,Time,fFormatSettings)
  else DateTimeToString(Result,fTimeFormat,Time,fFormatSettings);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.DoIndentNewLines(var Str: String; IndentCount: Integer);
begin
If (IndentCount > 0) and AnsiContainsStr(Str,sLineBreak) then
  Str := AnsiReplaceStr(Str,sLineBreak,sLineBreak + StringOfChar(' ',IndentCount));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ProtectedAddLog(LogText: String; IndentCount: Integer = 0; LineBreak: Boolean = True);
var
  i:    Integer;
  Temp: String;
begin
If fIndentNewLines then DoIndentNewLines(LogText,IndentCount);
If fWriteToConsole and System.IsConsole then WriteLn(LogText);
If fInternalLog then fInternalLogObj.Add(LogText);
For i := 0 to Pred(fExternalLogs.Count) do TStrings(fExternalLogs[i]).Add(LogText);
If fStreamToFile then
  begin
    If LineBreak then
      begin
        Temp := LogText + sLineBreak;
        fStreamFile.WriteBuffer(PChar(Temp)^, Length(Temp) * SizeOf(Char));
      end
    else fStreamFile.WriteBuffer(PChar(LogText)^, Length(LogText) * SizeOf(Char));
  end;
Inc(fLogCounter);
If Assigned(fOnLog) then fOnLog(Self,LogText);
end;

{------------------------------------------------------------------------------}
{    TSimpleLog // Public routines                                             }
{------------------------------------------------------------------------------}

constructor TSimpleLog.Create;
begin
inherited Create;
{%H-}GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
fTimeFormat := def_TimeFormat;
fTimeSeparator := def_TimeSeparator;
fTimeOfCreation := Now;
fBreaker := def_Breaker;
fTimeStamp := def_TimeStamp;
fStartStamp := def_StartStamp;
fEndStamp := def_EndStamp;
fAppendStamp := def_AppendStamp;
fHeader := def_Header;
fIndentNewLines := def_IndentNewLines;
fThreadLocked := def_ThreadLocked;
fInternalLog := def_InternalLog;
fWriteToConsole := def_WriteToConsole;
fStreamToFile := def_StreamToFile;
fConsoleBinded := False;
fStreamAppend := def_StreamAppend;
fStreamFileName := GetDefaultStreamFileName;
fStreamFileAccessRights := def_StreamFileAccessRights;
fForceTime := def_ForceTime;
fForcedTime := Now;
fLogCounter := 0;
fThreadLock := SyncObjs.TCriticalSection.Create;
fInternalLogObj := TStringList.Create;
fExternalLogs := TObjectList.Create(False);
fConsoleBindFlag := CBF_UNLOCKED;
fStreamFile := nil;
end;

//------------------------------------------------------------------------------

destructor TSimpleLog.Destroy;
begin
If Assigned(fStreamFile) then FreeAndNil(fStreamFile);
fExternalLogs.Free;
fInternalLogObj.Free;
fThreadLock.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ThreadLock;
begin
fThreadLock.Enter;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ThreadUnlock;
begin
fThreadLock.Leave;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogNoTime(const Text: String);
begin
If fThreadLocked then
  begin
    fThreadLock.Enter;
    try
      ProtectedAddLog(Text);
    finally
      fThreadLock.Leave;
    end;
  end
else ProtectedAddLog(Text);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogTime(const Text: String; Time: TDateTime);
var
  TimeStr:  String;
begin
TimeStr := GetTimeAsStr(Time) + fTimeSeparator;
If fThreadLocked then
  begin
    fThreadLock.Enter;
    try
      ProtectedAddLog(TimeStr + Text,Length(TimeStr));
    finally
      fThreadLock.Leave;
    end;
  end
else ProtectedAddLog(TimeStr + Text,Length(TimeStr));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLog(const Text: String);
begin
AddLogTime(Text,GetCurrentTime);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEmpty;
begin
AddLogNoTime('');
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreaker;
begin
AddLogNoTime(fBreaker);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddTimeStamp;
begin
AddLogNoTime(Format(fTimeStamp,[GetTimeAsStr(GetCurrentTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddStartStamp;
begin
AddLogNoTime(Format(fStartStamp,[GetTimeAsStr(GetCurrentTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEndStamp;
begin
AddLogNoTime(Format(fEndStamp,[GetTimeAsStr(GetCurrentTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddAppendStamp;
begin
AddLogNoTime(Format(fAppendStamp,[GetTimeAsStr(GetCurrentTime)]));
end;
 
//------------------------------------------------------------------------------

procedure TSimpleLog.AddHeader;
begin
AddLogNoTime(fHeader);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ForceTimeSet(Time: TDateTime);
begin
fForcedTime := Time;
fForceTime := True;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.InternalLogGetLog(LogIndex: Integer): String;
begin
If (LogIndex >= 0) and (LogIndex < fInternalLogObj.Count) then
  Result := fInternalLogObj[LogIndex]
else
  Result := '';
end;

//------------------------------------------------------------------------------

Function TSimpleLog.InternalLogGetAsText: String;
begin
Result := fInternalLogObj.Text;
end;
   
//------------------------------------------------------------------------------

procedure TSimpleLog.InternalLogClear;
begin
fInternalLogObj.Clear;
end;
  
//------------------------------------------------------------------------------

Function TSimpleLog.InternalLogSaveToFile(const FileName: String; Append: Boolean = False): Boolean;
var
  FileStream:   TFileStream;
  StringBuffer: String;
begin
try
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701) and not Defined(BARE_FPC)}
  If FileExistsUTF8(FileName) then
{$ELSE}
  If FileExists(FileName) then
{$IFEND}
{$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
    FileStream := TFileStream.Create(UTF8ToSys(FileName),fmOpenReadWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(UTF8ToSys(FileName),fmCreate or fmShareDenyWrite);
{$ELSE}
    FileStream := TFileStream.Create(FileName,fmOpenReadWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
{$IFEND}
  try
    If Append then FileStream.Seek(0,soEnd)
      else FileStream.Size := 0;
    StringBuffer := fInternalLogObj.Text;
    FileStream.WriteBuffer(PChar(StringBuffer)^,Length(StringBuffer) * SizeOf(Char));
  finally
    FileStream.Free;
  end;
  Result := True;
except
  Result := False;
end;
end;
    
//------------------------------------------------------------------------------

Function TSimpleLog.InternalLogLoadFromFile(const FileName: String; Append: Boolean = False): Boolean;
var
  FileStream:   TFileStream;
  StringBuffer: String;
begin
try
{$IF Defined(FPC) and not Defined(Unicode) and not Defined(BARE_FPC)}
  FileStream := TFileStream.Create(UTF8ToSys(FileName),fmOpenRead or fmShareDenyWrite);
{$ELSE}
  FileStream := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
{$IFEND}
  try
    If not Append then fInternalLogObj.Clear;
    FileStream.Position := 0;
    SetLength(StringBuffer,FileStream.Size div SizeOf(Char));
    FileStream.ReadBuffer(PChar(StringBuffer)^,Length(StringBuffer) * SizeOf(Char));
    fInternalLogObj.Text := fInternalLogObj.Text + StringBuffer;
  finally
    FileStream.Free;
  end;
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.BindConsole: Boolean;
begin
If not fConsoleBinded and System.IsConsole and ReserveConsoleBind then
  begin
    fWriteToConsole := False;
    AssignSLCB(ErrOutput,Self);
    Rewrite(ErrOutput);
    AssignSLCB(Output,Self);
    Rewrite(Output);
    AssignSLCB(Input,Self);
    Reset(Input);
    fConsoleBinded := True;
  end;
Result := fConsoleBinded;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.UnbindConsole;
begin
If fConsoleBinded then
  begin
    Close(Input);
    Close(Output);
    Close(ErrOutput);
    fConsoleBinded := False;
    InterlockedExchange(fConsoleBindFlag,CBF_UNLOCKED);
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogAdd(ExternalLog: TStrings): Integer;
begin
Result := fExternalLogs.Add(ExternalLog);
end;
     
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIndexOf(ExternalLog: TStrings): Integer;
begin
Result := fExternalLogs.IndexOf(ExternalLog);
end;
     
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogRemove(ExternalLog: TStrings): Integer;
begin
Result := fExternalLogs.IndexOf(ExternalLog);
If Result >= 0 then ExternalLogDelete(Result);
end;
    
//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogDelete(Index: Integer);
begin
If (Index >= 0) and (Index < fExternalLogs.Count) then
  fExternalLogs.Delete(Index)
else
 raise exception.CreateFmt('TSimpleLog.ExternalLogDelete: Index (%d) out of bounds.',[Index]);
end;

{$IFNDEF SimpleLog_Include}
{$WARNINGS OFF}
end.
{$ENDIF}
