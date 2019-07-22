unit Utils;

interface

uses
  System.Classes, System.SysUtils;

procedure StringToList(Str: String; Delimiter: char; const strList: TStrings;
    Clear: Boolean = True);

procedure GetFolderContents(Dir: String; OutputList: TStringList;
  AddRoot: Boolean; IncludeDirs, IncludeFiles, IncludeSubFolders: Boolean);

function RemoveTrailingBackSlash(const Path: String): String;

implementation

function RemoveTrailingBackSlash(const Path: String): String;
Begin
  Result := Path;
  if Length(Result) > 0 then
    If Result[Length(Result)] = '\' Then
      Delete(Result, Length(Result), 1);
End;

procedure StringToList(Str: String; Delimiter: char; const strList: TStrings;
    Clear: Boolean = True);
var
  p : Integer;
begin
  if Clear then
    StrList.Clear;
  if Str = '' then exit;
  if (Length(Str) = 1) and (Str[1] = Delimiter) then exit;

  P := Pos(Delimiter, Str);
  while P > 0 do begin
    // add this string to list
    StrList.Add(trim(copy(str, 1, P-1)));
    Delete(Str, 1, P);
    P := Pos(Delimiter, Str);
  end;
  // add last string if it wasn't closed with a Delimiter
  if Str <> '' then
    StrList.Add(trim(Str));
end;

procedure GetFolderContents(Dir: String; OutputList: TStringList;
  AddRoot: Boolean; IncludeDirs, IncludeFiles, IncludeSubFolders: Boolean);

  procedure GetDir(FolderName: string);
  var
    Sr: TSearchRec;
    TempList: TStringList;
    I: Integer;
    procedure AddCurrent;
    begin
      { if current file is a directory }
      if (Sr.Attr and (faDirectory))=faDirectory then
      begin
        if (Sr.Name='..') or (Sr.Name='.') then Exit;
        TempList.Add(FolderName+'\'+Sr.Name);
      end
      else
      { if current file isn't a directory file }
      begin
        If IncludeFiles Then
        Begin
          If AddRoot Then
            OutputList.Add(Dir + FolderName+'\'+Sr.Name)
             // note: FolderName starts with '\'
          Else
            OutputList.Add(FolderName+'\'+Sr.Name);
        End;
        //Inc(Count);
      end;
    end;  {end add current}

  begin
    TempList := TStringList.Create;
    try
      if FindFirst(Dir+FolderName+'\*.*',faanyfile,Sr)=0 then
      begin
        AddCurrent;
        while FindNext(Sr)=0 do
          AddCurrent;
      end;
      For I := 0 to TempList.Count - 1 do Begin
        If IncludeDirs Then
        Begin
          If AddRoot Then
            OutputList.Add(Dir + TempList[I]) // already contains '\'
          Else
            OutputList.Add(TempList[I]);
        End;
        If IncludeSubFolders Then
          GetDir(TempList[I]);
      End;
    finally
      TempList.Free;
      { always close the search variable }
      System.SysUtils.FindClose(Sr);
    end;
  end; {end getdir}

begin  {main}
  Dir := RemoveTrailingBackSlash(Dir);
  OutputList.Clear;
  GetDir('');
End;

end.
