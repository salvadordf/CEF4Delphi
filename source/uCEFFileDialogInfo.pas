// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

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

      function  CEFAcceptFilterToDialogFilter(const aAcceptFilter : ustring) : ustring; virtual;
      function  GetDefaultMimeTypeDescription(const aMimeType : ustring) : ustring; virtual;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   Clear;

      property Mode                   : Cardinal                 read FMode                   write FMode;
      property Title                  : ustring                  read FTitle                  write FTitle;
      property DefaultFilePath        : ustring                  read FDefaultFilePath        write FDefaultFilePath;
      property AcceptFilters          : TStrings                                              write SetAcceptFilters;
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

function TCEFFileDialogInfo.GetDialogFilter : ustring;
var
  i : integer;
begin
  Result := '';

  if assigned(FAcceptFilters) and (FAcceptFilters.Count > 0) then
    for i := 0 to pred(FAcceptFilters.Count) do
      if (i = 0) then
        Result := CEFAcceptFilterToDialogFilter(FAcceptFilters[i])
       else
        Result := Result + '|' + CEFAcceptFilterToDialogFilter(FAcceptFilters[i]);

  if (length(Result) > 0) then
    Result := Result + '|' + FDefaultAllFileDesc + '|*.*'
   else
    Result := FDefaultAllFileDesc + '|*.*';
end;

function TCEFFileDialogInfo.CEFAcceptFilterToDialogFilter(const aAcceptFilter : ustring) : ustring;
var
  i : integer;
  TempDesc, TempExt, TempString : ustring;
  TempSL : TStringList;
begin
  Result := '';

  if (length(aAcceptFilter) = 0) then exit;

  TempSL := nil;
  i      := pos('|', aAcceptFilter);

  if (i > 0) then
    begin
      TempDesc   := copy(aAcceptFilter, 1, pred(i));
      TempString := copy(aAcceptFilter, succ(i), length(aAcceptFilter));

      for i := 1 to length(TempString) do
        if (TempString[i] = ';') then TempString[i] := #13;

      TempSL      := TStringList.Create;
      TempSL.Text := TempString;
      TempString  := '';

      i := 0;
      while (i < TempSL.Count) do
        begin
          TempExt := TempSL[i];
          if (length(TempExt) > 1) and (TempExt[1] = '.') then
            TempString := TempString + '*' + TempExt + ';';
          inc(i);
        end;

      i := length(TempString);
      if (i > 0) then
        begin
          if (TempString[i] = ';') then TempString := copy(TempString, 1, pred(i));
          Result := TempDesc + '|' + TempString;
        end
       else
        Result := aAcceptFilter;
    end
   else
    if (aAcceptFilter[1] = '.') then
      begin
        TempDesc := GetFileTypeDescription(aAcceptFilter);

        if (length(TempDesc) = 0) then
          TempDesc := GetDefaultMimeTypeDescription(CefGetMimeType(aAcceptFilter));

        Result := TempDesc + ' (*' + aAcceptFilter + ')|*' + aAcceptFilter;
      end
     else
      begin
        TempSL := TStringList.Create;
        CefGetExtensionsForMimeType(aAcceptFilter, TempSL);

        if (TempSL.Count = 0) then
          Result := GetDefaultMimeTypeDescription(aAcceptFilter) + '|*.*'
         else
          begin
            for i := 0 to pred(TempSL.Count) do
              begin
                TempExt := TempSL[i];

                if (length(TempExt) > 0) and (TempExt[1] = '.') then
                  TempString := TempString + '*' + TempExt + ';'
                 else
                  TempString := TempString + '*.' + TempExt + ';';
              end;

            TempString := copy(TempString, 1, pred(length(TempString)));
            TempDesc   := '';
            i          := 0;

            while (length(TempDesc) = 0) and (i < TempSL.Count) do
              begin
                TempDesc := GetFileTypeDescription(TempSL[i]);
                inc(i);
              end;

            if (length(TempDesc) = 0) then
              TempDesc := GetDefaultMimeTypeDescription(CefGetMimeType(aAcceptFilter));

            Result := TempDesc + ' (' + TempString + ')|' + TempString;
          end;
      end;

  if assigned(TempSL) then FreeAndNil(TempSL);
end;

end.
