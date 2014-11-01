program Condenser;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  StrUtils;

const
  cMainUnitTemplate = '<unit_name>'           + sLineBreak +
                                                sLineBreak +
                      'interface'             + sLineBreak +
                                                sLineBreak +
                      '<interface_body>'      + sLineBreak +
                                                sLineBreak +
                      'implementation'        + sLineBreak +
                                                sLineBreak +
                      '<implementation_body>' + sLineBreak +
                                                sLineBreak +
                      '<initialization>'      + sLineBreak +
                      'initialization'        + sLineBreak +
                      '<initialization_body>' + sLineBreak +
                      '</initialization>'     + sLineBreak +
                                                sLineBreak +
                      '<finalization>'        + sLineBreak +
                      'finalization'          + sLineBreak +
                      '<finalization_body>'   + sLineBreak +
                      '</finalization>'       + sLineBreak +
                                                sLineBreak +
                      'end.';

var
  Param_InputListFile:  String = '';
  Param_OutputFolder:   String = '';
  Param_OutputUnit:     String = '';
  Param_TemplateFile:   String = '';
  MainUnitTemplate:     String = '';

Function ReadParameters: Boolean;
var
  i:        Integer;
  TempStrs: TStringList;
begin
Result := False;
MainUnitTemplate := cMainUnitTemplate;
If ParamCount > 1 then
  begin
    For i := 0 to (ParamCount - 1) do
      begin
        If AnsiSameStr(ParamStr(i),'-il') then
          Param_InputListFile := ParamStr(i + 1);
        If AnsiSameStr(ParamStr(i),'-of') then
          Param_OutputFolder := ParamStr(i + 1);
        If AnsiSameStr(ParamStr(i),'-un') then
          Param_OutputUnit := ParamStr(i + 1);
        If AnsiSameStr(ParamStr(i),'-ut') then
          Param_TemplateFile := ParamStr(i + 1);
      end;
  end
else Exit;
If not FileExists(Param_InputListFile) then
  begin
    WriteLn;
    WriteLn('Input list file (' + Param_InputListFile + ') does not exists.');
    Exit;
  end;
If Param_OutputFolder = '' then Param_OutputFolder := ExtractFilePath(ParamStr(0));
If not DirectoryExists(ExcludeTrailingPathDelimiter(Param_OutputFolder)) then
  begin
    WriteLn;
    WriteLn('Output folder (' + Param_OutputFolder + ') does not exists.');
    Exit;
  end;
While Param_OutputUnit = '' do
  begin
    WriteLn;
    Write('Output header unit name: '); ReadLn(Param_OutputUnit);
  end;
If Param_TemplateFile <> '' then
  begin
    If not FileExists(Param_TemplateFile) then
      begin
        WriteLn;
        WriteLn('Main unit template file (' + Param_TemplateFile + ') does not exists.');
        Exit;
      end
    else
      begin
        TempStrs := TStringList.Create;
        try
          TempStrs.LoadFromFile(Param_TemplateFile);
          MainUnitTemplate := TempStrs.Text;
        finally
          TempStrs.Free;
        end;
      end;
  end;
Result := True;
end;

procedure ProcessFile(FileName,UnitDescriptor: String; Out_Intf, Out_Impl, Out_Init, Out_Final: TStringList);
const
  cTags: Array[0..9] of String = ('(*<unit>*)','(*</unit>*)','(*<interface>*)',
         '(*</interface>*)','(*<implementation>*)','(*</implementation>*)',
         '(*<initialization>*)','(*</initialization>*)','(*<finalization>*)',
         '(*</finalization>*)');
type
  TTag = (tagNone = -1,tagUnitBegin,tagUnitEnd,tagInterfaceBegin,tagInterfaceEnd,
          tagImplementationBegin,tagImplementationEnd,tagInitializationBegin,
          tagInitializationEnd,tagFinalizationBegin,tagFinalizationEnd);
  TProcessingState = (psUnknown,psBeforeUnit,psUnit,psInterface,psImplementation,
                      psInitialization,psFinalization,psAfterUnit);
var
  i:                Integer;
  WorkStrs:         TStringList;
  ProcessingState:  TProcessingState;
  OldIntfCount,
  OldImplCount,
  OldInitCount,
  OldFinalCount:    Integer;
  TempStr:          String;

  Function GetTag(Line: String): TTag;
  var
    ii: Integer;
  begin
    Result := tagNone;
    For ii := Low(cTags) to High(cTags) do
      If AnsiSameText(cTags[ii],Trim(Line)) then
        begin
          Result := TTag(ii);
          Break;
        end;
  end;

begin
OldIntfCount := Out_Intf.Count;
OldImplCount := Out_Impl.Count;
OldInitCount := Out_Init.Count;
OldFinalCount := Out_Final.Count;
WriteLn;
WriteLn('Processing file:' + sLineBreak + FileName);
WorkStrs := TStringList.Create;
try
  WriteLn('Loading file content...');
  WorkStrs.LoadFromFile(FileName);
  WriteLn(IntToStr(WorkStrs.Count) + ' lines loaded');
  WriteLn('Processing lines...');
  ProcessingState := psBeforeUnit;
  For i := 0 to (WorkStrs.Count - 1) do
    begin
      case GetTag(WorkStrs[i]) of
        tagUnitBegin:           begin
                                  If OldIntfCount < Out_Intf.Count then Out_Intf.Add('');
                                  ProcessingState := psUnit;
                                  Continue;
                                end;
        tagUnitEnd:             begin ProcessingState := psAfterUnit;      Continue; end;
        tagInterfaceBegin:      begin ProcessingState := psInterface;      Continue; end;
        tagInterfaceEnd:        begin ProcessingState := psUnit;           Continue; end;
        tagImplementationBegin: begin ProcessingState := psImplementation; Continue; end;
        tagImplementationEnd:   begin ProcessingState := psUnit;           Continue; end;
        tagInitializationBegin: begin ProcessingState := psInitialization; Continue; end;
        tagInitializationEnd:   begin ProcessingState := psUnit;           Continue; end;
        tagFinalizationBegin:   begin ProcessingState := psFinalization;   Continue; end;
        tagFinalizationEnd:     begin ProcessingState := psUnit;           Continue; end;
      end;
      case ProcessingState of
        psBeforeUnit:     Out_Intf.Add(WorkStrs[i]);
        psUnit:;          // no action
        psInterface:      Out_Intf.Add(WorkStrs[i]);
        psImplementation: Out_Impl.Add(WorkStrs[i]);
        psInitialization: Out_Init.Add(WorkStrs[i]);
        psFinalization:   Out_Final.Add(WorkStrs[i]);
        psAfterUnit:      // no action
      end;
    end;
finally
  WorkStrs.Free;
end;
TempStr := '{=== ' + UnitDescriptor + ' ';
TempStr := TempStr + StringOfChar('=', 80 - Length(TempStr) - 4) + '===}';
If OldIntfCount < Out_Intf.Count then Out_Intf.Insert(OldIntfCount,sLineBreak + TempStr);
If OldImplCount < Out_Impl.Count then Out_Impl.Insert(OldImplCount,sLineBreak + TempStr + sLineBreak);
If OldInitCount < Out_Init.Count then Out_Init.Insert(OldInitCount,sLineBreak + TempStr);
If OldFinalCount < Out_Final.Count then Out_Final.Insert(OldFinalCount,sLineBreak + TempStr);
end;

procedure FillTemplateAndSave(Out_Intf, Out_Impl, Out_Init, Out_Final: TStringList; FileName: String);
const
  cTags: Array[0..15] of String = ('<unit_name>','<info>','<file>','</file>',
         '<interface>','</interface>','<interface_body>','<implementation>',
         '</implementation>','<implementation_body>','<initialization>',
         '</initialization>','<initialization_body>','<finalization>',
         '</finalization>','<finalization_body>');
type
  TTag = (tagNone = -1,tagUnitName,tagInfo,tagFileBegin,tagFileEnd,
          tagInterfaceBegin,tagInterfaceEnd,tagInterfaceBody,
          tagImplementationBegin,tagImplementationEnd,tagImplementationBody,
          tagInitializationBegin,tagInitializationEnd,tagInitializationBody,
          tagFinalizationBegin,tagFinalizationEnd,tagFinalizationBody);
  TProcessingState = (psUnknown,psGeneral,psFile,psInterface,psImplementation,
                      psInitialization,psFinalization);
var
  i:                Integer;
  UnitLines:        TStringList;
  TemplateLines:    TStringList;
  ProcessingState:  TProcessingState;

  Function GetTag(Line: String): TTag;
  var
    ii: Integer;
  begin
    Result := tagNone;
    For ii := Low(cTags) to High(cTags) do
      If AnsiSameText(cTags[ii],Trim(Line)) then
        begin
          Result := TTag(ii);
          Break;
        end;
  end;

  Function GetFileContent(FileName: String): String;
  var
    TempStrs: TStringList;
  begin
  try
    TempStrs := TStringList.Create;
    try
      TempStrs.LoadFromFile(FileName);
      Result := Trim(TempStrs.Text);
    finally
      TempStrs.Free;
    end;
  except
    Result := '';
  end;
  end;

  Function GetCondensingInfo: String;
  var
    TempStr:        String;
    FormatSettings: TFormatSettings;
  begin
    GetLocaleFormatSettings(1033{en-us},FormatSettings);
    FormatSettings.DateSeparator := '-';
    FormatSettings.TimeSeparator := ':';
    FormatSettings.ShortDateFormat := 'dddd yyyy-mm-dd';
    FormatSettings.LongDateFormat := FormatSettings.ShortDateFormat;
    FormatSettings.ShortTimeFormat := 'hh:nn:ss';
    FormatSettings.LongTimeFormat := FormatSettings.ShortTimeFormat;

    Result := '{' + StringOfChar('=',78) + '}' + sLineBreak;

    TempStr := '{  SCS Telemetry API headers condenser, version 1.0a';
    TempStr := TempStr + StringOfChar(' ',79 - Length(TempStr)) + '}' + sLineBreak;
    Result := Result + TempStr;
    
    TempStr := '{  Condensed on: ' + DateTimeToStr(Now,FormatSettings);
    TempStr := TempStr + StringOfChar(' ',79 - Length(TempStr)) + '}' + sLineBreak;
    Result := Result + TempStr;

    Result := Result + '{' + StringOfChar('=',78) + '}';
  end;

begin
TemplateLines := TStringList.Create;
try
  TemplateLines.Text := MainUnitTemplate;
  UnitLines := TStringList.Create;
  try
    ProcessingState := psGeneral;
    // Template preparation
    For i := 0 to (TemplateLines.Count - 1) do
      begin
        case GetTag(TemplateLines[i]) of
          tagUnitName:            begin UnitLines.Add('unit ' + Param_OutputUnit + ';'); Continue; end;
          tagInfo:                begin UnitLines.Add(GetCondensingInfo); Continue; end;
          tagFileBegin:           begin ProcessingState := psFile;            Continue; end;
          tagFileEnd:             begin ProcessingState := psGeneral;         Continue; end;
          tagInterfaceBegin:      begin ProcessingState := psInterface;       Continue; end;
          tagInterfaceEnd:        begin ProcessingState := psGeneral;         Continue; end;
          tagImplementationBegin: begin ProcessingState := psImplementation;  Continue; end;
          tagImplementationEnd:   begin ProcessingState := psGeneral;         Continue; end;
          tagInitializationBegin: begin ProcessingState := psInitialization;  Continue; end;
          tagInitializationEnd:   begin ProcessingState := psGeneral;         Continue; end;
          tagFinalizationBegin:   begin ProcessingState := psFinalization;    Continue; end;
          tagFinalizationEnd:     begin ProcessingState := psGeneral;         Continue; end;
        end;
        case ProcessingState of
          psGeneral:        UnitLines.Add(TemplateLines[i]);
          psFile:           UnitLines.Add(GetFileContent(TemplateLines[i]));
          psInterface:      If Out_Intf.Count > 0 then UnitLines.Add(TemplateLines[i]);
          psImplementation: If Out_Impl.Count > 0 then UnitLines.Add(TemplateLines[i]);
          psInitialization: If Out_Init.Count > 0 then UnitLines.Add(TemplateLines[i]);
          psFinalization:   If Out_Final.Count > 0 then UnitLines.Add(TemplateLines[i]);
        end;
      end;
    // Replace body tags with data
    For i := 0 to (UnitLines.Count - 1) do
      begin
        case GetTag(UnitLines[i]) of
          tagInterfaceBody:       UnitLines[i] := Trim(Out_Intf.Text);
          tagImplementationBody:  UnitLines[i] := Trim(Out_Impl.Text);
          tagInitializationBody:  UnitLines[i] := Trim(Out_Init.Text);
          tagFinalizationBody:    UnitLines[i] := Trim(Out_Final.Text);
        end;
      end;
    WriteLn('Saving resulting unit file...');
    UnitLines.SaveToFile(FileName);
  finally
    UnitLines.Free;
  end;
finally
  TemplateLines.Free
end;
end;

procedure ProcessFiles;
var
  i:              Integer;
  InputFilesList: TStringList;
  Out_Intf,
  Out_Impl,
  Out_Init,
  Out_Final:      TStringList;
  CommonDirLng:   Integer;

  Function RemoveCommonDir(Str: String): String;
  begin
    If CommonDirLng > 0 then
      Result := Copy(Str,CommonDirLng + 1,Length(Str) - CommonDirLng)
    else
      Result := ExtractFileName(Str);
    Result := AnsiReplaceStr(Result,PathDelim,'/');
  end;

  procedure GetCommonDirLength;
  var
    ii:       Integer;
    LastChar: Char;
  begin
    CommonDirLng := 0;
    If InputFilesList.Count > 1 then
      While Length(InputFilesList[0]) > CommonDirLng do
        begin
          LastChar := InputFilesList[0][CommonDirLng + 1];
          For ii := 1 to (InputFilesList.Count - 1) do
            If InputFilesList[ii][CommonDirLng + 1] <> LastChar then
              begin
                Exit;
              end;
          Inc(CommonDirLng);
        end;
  end;

begin
WriteLn;
WriteLn('Processing...');
InputFilesList := TStringList.Create;
try
  WriteLn('Loading input files list...');
  InputFilesList.LoadFromFile(Param_InputListFile);
  WriteLn('Number of input files: ' + IntToStr(InputFilesList.Count));
  GetCommonDirLength;
  Out_Intf := TStringList.Create;
  Out_Impl := TStringList.Create;
  Out_Init := TStringList.Create;
  Out_Final := TStringList.Create;
  try
    For i := 0 to (InputFilesList.Count - 1) do
      ProcessFile(InputFilesList[i],RemoveCommonDir(InputFilesList[i]),Out_Intf,Out_Impl,Out_Init,Out_Final);
    WriteLn;
    WriteLn('Merging files to template...');
    FillTemplateAndSave(Out_Intf,Out_Impl,Out_Init,Out_Final,
                        IncludeTrailingPathDelimiter(Param_OutputFolder) + Param_OutputUnit + '.pas');
  finally
    Out_Intf.Free;
    Out_Impl.Free;
    Out_Init.Free;
    Out_Final.Free;
  end;
finally
  InputFilesList.Free;
end;
WriteLn;
WriteLn('Done.');
end;

begin
try
  WriteLn('-------------------------------------');
  WriteLn(' SCS Telemetry API headers condenser');
  WriteLn('            Version 1.0a');
  WriteLn('-------------------------------------');
  If ReadParameters then ProcessFiles
  else
    begin
      WriteLn;
      WriteLn('Program use:');
      WriteLn;
      WriteLn('condenser.exe -il file_name [-of folder] [-un unit_name] [-ut file_name]');
      WriteLn;
      WriteLn('-il: file containing list of header files to process');
      WriteLn('-of: output folder');
      WriteLn('-un: output header unit name');
      WriteLn('     if not set, then the program will ask for it');
      WriteLn('-ut: output unit template');
      WriteLn('     if not set, the program will use default template');
    end;
except
  WriteLn('Fatal error (0x' + IntToHex(GetLastError,8) + ') occured, stopping program.');
end;
WriteLn;
Write('Press enter to continue...'); ReadLn;
end.
