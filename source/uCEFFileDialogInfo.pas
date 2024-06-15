unit uCEFFileDialogInfo;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes;

type
  TCEFFileDialogInfo = class
    protected
      FMode                   : Cardinal;
      FTitle                  : ustring;
      FDefaultFilePath        : ustring;
      FAcceptFilters          : TStrings;
      FAcceptExtensions       : TStrings;
      FAcceptDescriptions     : TStrings;
      FCallback               : ICefFileDialogCallback;
      FDefaultAudioFileDesc   : ustring;
      FDefaultVideoFileDesc   : ustring;
      FDefaultTextFileDesc    : ustring;
      FDefaultImageFileDesc   : ustring;
      FDefaultAllFileDesc     : ustring;
      FDefaultUnknownFileDesc : ustring;

      function  GetDialogFilter : ustring; virtual;
      function  GetDialogType : TCEFDialogType;

      procedure SetAcceptFilters(const aAcceptFilters : TStrings);
      procedure SetAcceptExtensions(const aAcceptExtensions : TStrings);
      procedure SetAcceptDescriptions(const aAcceptDescriptions : TStrings);

      function  ConvertExtensions(aExtensions : ustring): ustring;
      function  CEFAcceptFilterToDialogFilter(const aAcceptFilter, aExtension, aDescription : ustring) : ustring; virtual;
      function  GetDefaultMimeTypeDescription(const aMimeType : ustring) : ustring; virtual;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   Clear;

      property Mode                   : Cardinal                 read FMode                   write FMode;
      property Title                  : ustring                  read FTitle                  write FTitle;
      property DefaultFilePath        : ustring                  read FDefaultFilePath        write FDefaultFilePath;
      property AcceptFilters          : TStrings                                              write SetAcceptFilters;
      property AcceptExtensions       : TStrings                                              write SetAcceptExtensions;
      property AcceptDescriptions     : TStrings                                              write SetAcceptDescriptions;
      property Callback               : ICefFileDialogCallback   read FCallback               write FCallback;
      property DialogFilter           : ustring                  read GetDialogFilter;
      property DialogType             : TCEFDialogType           read GetDialogType;
      property DefaultAudioFileDesc   : ustring                  read FDefaultAudioFileDesc   write FDefaultAudioFileDesc;
      property DefaultVideoFileDesc   : ustring                  read FDefaultVideoFileDesc   write FDefaultVideoFileDesc;
      property DefaultTextFileDesc    : ustring                  read FDefaultTextFileDesc    write FDefaultTextFileDesc;
      property DefaultImageFileDesc   : ustring                  read FDefaultImageFileDesc   write FDefaultImageFileDesc;
      property DefaultAllFileDesc     : ustring                  read FDefaultAllFileDesc     write FDefaultAllFileDesc;
      property DefaultUnknownFileDesc : ustring                  read FDefaultUnknownFileDesc write FDefaultUnknownFileDesc;
  end;

implementation

uses
  uCEFConstants, uCEFMiscFunctions;

constructor TCEFFileDialogInfo.Create;
begin
  inherited Create;

  FMode                   := 0;
  FTitle                  := '';
  FDefaultFilePath        := '';
  FCallback               := nil;
  FAcceptFilters          := nil;
  FAcceptExtensions       := nil;
  FAcceptDescriptions     := nil;
  FDefaultAudioFileDesc   := 'Audio files';
  FDefaultVideoFileDesc   := 'Video files';
  FDefaultTextFileDesc    := 'Text files';
  FDefaultImageFileDesc   := 'Image files';
  FDefaultAllFileDesc     := 'All files';
  FDefaultUnknownFileDesc := 'Unknown files';
end;

destructor TCEFFileDialogInfo.Destroy;
begin
  Clear;

  if assigned(FAcceptFilters) then
    FreeAndNil(FAcceptFilters);

  if assigned(FAcceptExtensions) then
    FreeAndNil(FAcceptExtensions);

  if assigned(FAcceptDescriptions) then
    FreeAndNil(FAcceptDescriptions);

  inherited Destroy;
end;

procedure TCEFFileDialogInfo.Clear;
begin
  FMode                   := 0;
  FTitle                  := '';
  FDefaultFilePath        := '';
  FCallback               := nil;

  if assigned(FAcceptFilters) then
    FAcceptFilters.Clear;
end;

function TCEFFileDialogInfo.GetDialogType : TCEFDialogType;
begin
  case FMode of
    FILE_DIALOG_OPEN          : Result := dtOpen;
    FILE_DIALOG_OPEN_MULTIPLE : Result := dtOpenMultiple;
    FILE_DIALOG_OPEN_FOLDER   : Result := dtOpenFolder;
    FILE_DIALOG_SAVE          : Result := dtSave;
    else                        Result := dtOpen;
  end;
end;

function TCEFFileDialogInfo.GetDefaultMimeTypeDescription(const aMimeType : ustring) : ustring;
begin
  if      (CompareStr(copy(aMimeType, 1, 5), 'audio') = 0) then Result := FDefaultAudioFileDesc
  else if (CompareStr(copy(aMimeType, 1, 5), 'video') = 0) then Result := FDefaultVideoFileDesc
  else if (CompareStr(copy(aMimeType, 1, 4), 'text')  = 0) then Result := FDefaultTextFileDesc
  else if (CompareStr(copy(aMimeType, 1, 5), 'image') = 0) then Result := FDefaultImageFileDesc
  else                                                          Result := FDefaultUnknownFileDesc;
end;

procedure TCEFFileDialogInfo.SetAcceptFilters(const aAcceptFilters : TStrings);
begin
  if assigned(aAcceptFilters) then
    begin
      if assigned(FAcceptFilters) then
        FAcceptFilters.Clear
       else
        FAcceptFilters := TStringList.Create;

      if (aAcceptFilters.Count > 0) then
        FAcceptFilters.AddStrings(aAcceptFilters);
    end;
end;

procedure TCEFFileDialogInfo.SetAcceptExtensions(const aAcceptExtensions : TStrings);
begin
  if assigned(aAcceptExtensions) then
    begin
      if assigned(FAcceptExtensions) then
        FAcceptExtensions.Clear
       else
        FAcceptExtensions := TStringList.Create;

      if (aAcceptExtensions.Count > 0) then
        FAcceptExtensions.AddStrings(aAcceptExtensions);
    end;
end;

procedure TCEFFileDialogInfo.SetAcceptDescriptions(const aAcceptDescriptions : TStrings);
begin
  if assigned(aAcceptDescriptions) then
    begin
      if assigned(FAcceptDescriptions) then
        FAcceptDescriptions.Clear
       else
        FAcceptDescriptions := TStringList.Create;

      if (aAcceptDescriptions.Count > 0) then
        FAcceptDescriptions.AddStrings(aAcceptDescriptions);
    end;
end;

function TCEFFileDialogInfo.GetDialogFilter : ustring;
var
  i : integer;
  TempExtension, TempDescription : ustring;
begin
  Result := '';

  if assigned(FAcceptFilters) and (FAcceptFilters.Count > 0) then
    for i := 0 to pred(FAcceptFilters.Count) do
      begin
        if assigned(FAcceptExtensions) and (FAcceptFilters.Count = FAcceptExtensions.Count) then
          TempExtension := FAcceptExtensions[i]
         else
          TempExtension := '';

        if assigned(FAcceptDescriptions) and (FAcceptFilters.Count = FAcceptDescriptions.Count) then
          TempDescription := FAcceptDescriptions[i]
         else
          TempDescription := '';

        if (i = 0) then
          Result := CEFAcceptFilterToDialogFilter(FAcceptFilters[i], TempExtension, TempDescription)
         else
          Result := Result + '|' + CEFAcceptFilterToDialogFilter(FAcceptFilters[i], TempExtension, TempDescription);
      end;

  if (length(Result) > 0) then
    Result := Result + '|' + FDefaultAllFileDesc + '|*.*'
   else
    Result := FDefaultAllFileDesc + '|*.*';
end;

function TCEFFileDialogInfo.ConvertExtensions(aExtensions : ustring): ustring;
var
  i : integer;
  TempSL : TStringList;
  TempExt : ustring;
begin
  for i := 1 to length(aExtensions) do
    if (aExtensions[i] = ';') then aExtensions[i] := #13;

  TempSL      := TStringList.Create;
  TempSL.Text := aExtensions;
  Result      := '';

  i := 0;
  while (i < TempSL.Count) do
    begin
      TempExt := TempSL[i];
      if (length(TempExt) > 1) and (TempExt[1] = '.') then
        Result := Result + '*' + TempExt + ';';
      inc(i);
    end;

  if (length(Result) > 0) and (Result[length(Result)] = ';') then
    Result := copy(Result, 1, pred(length(Result)));
end;

function TCEFFileDialogInfo.CEFAcceptFilterToDialogFilter(const aAcceptFilter, aExtension, aDescription : ustring) : ustring;
var
  i : integer;
  TempDesc, TempExt, TempExtList : ustring;
  TempSL : TStringList;
begin
  Result := '';

  if (length(aAcceptFilter) = 0) then
    exit;

  if (length(aExtension) > 0) and (length(aDescription) > 0) then
    begin
      TempExtList := ConvertExtensions(aExtension);
      TempDesc    := aDescription;
    end
   else
    begin
      i := pos('|', aAcceptFilter);

      if (i > 0) then
        begin
          if (length(aDescription) > 0) then
            TempDesc := aDescription
           else
            TempDesc := copy(aAcceptFilter, 1, pred(i));

          if (length(aExtension) > 0) then
            TempExtList := ConvertExtensions(aExtension)
           else
            TempExtList := ConvertExtensions(copy(aAcceptFilter, succ(i), length(aAcceptFilter)));
        end
       else
        if (aAcceptFilter[1] = '.') then
          begin
            if (length(aDescription) > 0) then
              TempDesc := aDescription
             else
              begin
                TempDesc := GetFileTypeDescription(aAcceptFilter);

                if (length(TempDesc) = 0) then
                  TempDesc := GetDefaultMimeTypeDescription(CefGetMimeType(aAcceptFilter));
              end;

            TempExtList := ConvertExtensions(aAcceptFilter);
          end
         else
          begin
            TempDesc := GetDefaultMimeTypeDescription(aAcceptFilter);

            if (length(aExtension) > 0) then
              TempExtList := ConvertExtensions(aExtension)
             else
              try
                TempSL := TStringList.Create;
                CefGetExtensionsForMimeType(aAcceptFilter, TempSL);

                if (TempSL.Count > 0) then
                  begin
                    for i := 0 to pred(TempSL.Count) do
                      begin
                        TempExt := TempSL[i];

                        if (length(TempExt) > 0) and (TempExt[1] = '.') then
                          TempExtList := TempExtList + '*' + TempExt + ';'
                         else
                          TempExtList := TempExtList + '*.' + TempExt + ';';
                      end;

                    TempExtList := copy(TempExtList, 1, pred(length(TempExtList)));
                  end
                 else
                  TempExtList := '';
              finally
                if assigned(TempSL) then
                  FreeAndNil(TempSL);
              end;
          end;
    end;

  if (length(TempExtList) > 0) then
    Result := TempDesc + ' (' + TempExtList + ')|' + TempExtList
   else
    Result := TempDesc + '|*.*';
end;

end.
