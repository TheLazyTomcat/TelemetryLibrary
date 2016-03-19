{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program PostProcessor;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  StrUtils;

{$R Data.res}

var
  FilesToProcess:   TStringList;
  WorkList:         TStringList;
  i:                Integer;
  NavTemplate:      String;
  ContentTemplate:  String;
  SearchResultFile: String;
  ActiveFolder:     String;

  procedure ListFiles(Path: String; Strs: TStringList);
  var
    SearchResult: TSearchRec;
  begin
    Strs.Clear;
    If FindFirst(ActiveFolder + '*.html',faAnyFile,SearchResult) = 0 then
      begin
        repeat
          Strs.Add(SearchResult.Name);
        until FindNext(SearchResult) <> 0;
        FindClose(SearchResult);
      end;
  end;

  Function ExtractNavigation(Str: String): String;
  begin
    Str := Copy(Str,PosEx('<',Str,Pos('<td class="navigation">',Str) + 1), Length(Str));
    Str := Copy(Str,1,Pos('</td>',Str) - 1);
    Str := AnsiReplaceStr(Str,'class="navigation"','class="navigation" target="content"');
    Str := AnsiReplaceStr(Str,'action="_tipue_results.html"','action="_tipue_results.html" target="content"');
    Result := AnsiReplaceStr(NavTemplate,'$payload$',Str);
  end;

  Function ProcessFile(Str: String): String;
  var
    TempPos:  Integer;
    j:        Integer;
    Title:    String;
  begin
    Title := Copy(Str,Pos('<title>',Str),Pos('</title>',Str) - Pos('<title>',Str) + 8);
    TempPos := PosEx('<',Str,Pos('<td class="content">',Str) + 1);
    Str := Copy(Str,TempPos, Length(Str));
    TempPos := Pos('</body>',Str);
    For j := TempPos downto 0 do
      begin
        TempPos := PosEx('</td>',Str,j);
        If TempPos > 0 then
          begin
            Str := Copy(Str,1,TempPos - 1);
            break;
          end;
      end;
    Str := Copy(Str,1,TempPos - 1);
    Str := AnsiReplaceStr(Str,'href="http://pasdoc.sourceforge.net/"','href="http://pasdoc.sourceforge.net/" target="_blank"');
    Str := AnsiReplaceStr(ContentTemplate,'$payload$',Str);
    Result := AnsiReplaceStr(Str,'$title$',Title);
  end;

begin
ActiveFolder := ExpandFileName(ExtractFilePath(ParamStr(0)) + ParamStr(1));
DeleteFile(ActiveFolder + 'navigation.html');
FilesToProcess := TStringList.Create;
try
  ListFiles(ActiveFolder,FilesToProcess);
  WorkList := TStringList.Create;
  try
    WorkList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Templates\content_template.txt');
    ContentTemplate := WorkList.Text;
    WorkList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Templates\navigation_template.txt');
    NavTemplate := WorkList.Text;
    WorkList.LoadFromFile(ActiveFolder + '_tipue_results.html');
    SearchResultFile := AnsiReplaceStr(WorkList.Text,'<td class="navigation">','<td class="navigation" style="display: none">');
    If FilesToProcess.Count > 0 then
      begin
        WorkList.LoadFromFile(ActiveFolder + FilesToProcess[0]);
        WorkList.Text := ExtractNavigation(WorkList.Text);
        WorkList.SaveToFile(ActiveFolder + 'navigation.html');
      end;
    For i := 0 to (FilesToProcess.Count - 1) do
      begin
        WorkList.LoadFromFile(ActiveFolder + FilesToProcess[i]);
        WorkList.Text := ProcessFile(WorkList.Text);
        WorkList.SaveToFile(ActiveFolder + FilesToProcess[i]);
      end;
    WorkList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Templates\index.txt');
    WorkList.SaveToFile(ActiveFolder + 'index.html');

    WorkList.LoadFromFile(ActiveFolder + 'GVClasses.dot');
    If not AnsiContainsStr(WorkList.Text,'rankdir="LR"') then
      WorkList.Text := AnsiReplaceStr(WorkList.Text,'{','{' + sLineBreak + 'rankdir="LR"');
    WorkList.SaveToFile(ActiveFolder + 'GVClasses.dot');
    WorkList.LoadFromFile(ActiveFolder + 'GVUses.dot');
    If not AnsiContainsStr(WorkList.Text,'rankdir="LR"') then
      WorkList.Text := AnsiReplaceStr(WorkList.Text,'{','{' + sLineBreak + '  rankdir="LR"');
    WorkList.SaveToFile(ActiveFolder + 'GVUses.dot');
    WorkList.Text := SearchResultFile;
    WorkList.SaveToFile(ActiveFolder + '_tipue_results.html');
  finally
    WorkList.Free;
  end;
finally
  FilesToProcess.Free;
end;

end.
