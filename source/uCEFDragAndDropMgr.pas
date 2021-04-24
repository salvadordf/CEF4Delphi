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
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uCEFDragAndDropMgr;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF FPC}{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.ActiveX, WinApi.ShlObj, WinApi.ShellApi,{$ENDIF}
  System.Classes, System.SysUtils, System.Math, System.StrUtils, System.AnsiStrings,
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows, ActiveX, ShlObj, Shellapi,{$ENDIF}
  Classes, SysUtils, Math, StrUtils, {$IFDEF DELPHI12_UP}AnsiStrings,{$ENDIF}
  {$ENDIF}
  uCEFDragData, uCEFInterfaces, uCEFTypes, uCEFOLEDragAndDrop;

type
  TDragEnterEvent = procedure(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint) of object;
  TDragOverEvent  = procedure(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint) of object;
  TDropEvent      = procedure(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint) of object;

  TCEFDragAndDropMgr = class(TOLEDragAndDropMgr)
    protected
      FCurrentDragData    : ICefDragData;
      FOLEEffect          : integer;
      FMozURLFormat       : cardinal;
      FHTMLFormat         : cardinal;
      FFileDescFormat     : cardinal;
      FFileContentsFormat : cardinal;

      FOnDragEnter        : TDragEnterEvent;
      FOnDragOver         : TDragOverEvent;
      FOnDragLeave        : TNotifyEvent;
      FOnDrop             : TDropEvent;

      function  DragDataToDataObject_Unicode(const aDragData : ICefDragData; var aFormat : TFormatEtc; var aMedium : TStgMedium) : boolean;
      function  DragDataToDataObject_Text(const aDragData : ICefDragData; var aFormat : TFormatEtc;  var aMedium : TStgMedium) : boolean;
      function  DragDataToDataObject_HTML(const aDragData : ICefDragData; var aFormat : TFormatEtc; var aMedium : TStgMedium) : boolean;
      function  DragDataToDataObject_URL(const aDragData : ICefDragData; var aFormat : TFormatEtc; var aMedium : TStgMedium) : boolean;
      function  DragDataToDataObject_FileDesc(const aDragData : ICefDragData; var aFormat : TFormatEtc; var aMedium : TStgMedium) : boolean;
      function  DragDataToDataObject_FileContents(const aDragData : ICefDragData; var aFormat : TFormatEtc; var aMedium : TStgMedium) : boolean;

      procedure DataObjectToDragData(const aDataObject : IDataObject; var aDragData : ICefDragData);
      function  DataObjectToDragData_Unicode(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
      function  DataObjectToDragData_Text(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
      function  DataObjectToDragData_URL(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
      function  DataObjectToDragData_HTML(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
      function  DataObjectToDragData_FileDrop(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;

      function  HtmlToCFHtml(var aHTML, aBaseURL : ustring) : AnsiString;
      procedure CFHtmlToHtml(const cf_html : AnsiString; var html, base_url : string);
      function  ZeroFiller(aNumber, aLength : integer) : AnsiString;
      function  FindStringField(const aString, aFieldName : AnsiString; var aPos : integer) : string;

    public
      constructor Create;
      destructor  Destroy; override;
      function    StartDragging : TCefDragOperation;
      function    CloneDragData(const aDragData : ICefDragData; aAllowedOps : TCefDragOperations) : boolean;

      function    DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; override;
      function    DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; override;
      function    DragLeave: HRESULT; override;
      function    Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; override;

      property    OnDragEnter        : TDragEnterEvent  read FOnDragEnter  write FOnDragEnter;
      property    OnDragOver         : TDragOverEvent   read FOnDragOver   write FOnDragOver;
      property    OnDragLeave        : TNotifyEvent     read FOnDragLeave  write FOnDragLeave;
      property    OnDrop             : TDropEvent       read FOnDrop       write FOnDrop;
  end;


implementation

uses
  uCEFMiscFunctions, uCEFWriteHandler, uCEFStreamWriter, uCEFConstants;

{$IFDEF FPC}
const
  //CFSTR_FILEDESCRIPTORA            = 'FileGroupDescriptor';              // CF_FILEGROUPDESCRIPTORA
  CFSTR_FILEDESCRIPTORW            = 'FileGroupDescriptorW';             // CF_FILEGROUPDESCRIPTORW
  CFSTR_FILEDESCRIPTOR             = CFSTR_FILEDESCRIPTORW;
  CFSTR_FILECONTENTS               = 'FileContents';                     // CF_FILECONTENTS
{$ENDIF}

// *****************************************************
// **************** TCEFDragAndDropMgr *****************
// *****************************************************

constructor TCEFDragAndDropMgr.Create;
begin
  inherited Create;

  FOnDragEnter        := nil;
  FOnDragOver         := nil;
  FOnDragLeave        := nil;
  FOnDrop             := nil;
  FCurrentDragData    := nil;

  FMozURLFormat       := RegisterClipboardFormat('text/x-moz-url');
  FHTMLFormat         := RegisterClipboardFormat('HTML Format');
  FFileDescFormat     := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  FFileContentsFormat := RegisterClipboardFormat(CFSTR_FILECONTENTS);
end;

destructor TCEFDragAndDropMgr.Destroy;
begin
  FCurrentDragData := nil;

  inherited Destroy;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_Unicode(const aDragData : ICefDragData;
                                                         var   aFormat   : TFormatEtc;
                                                         var   aMedium   : TStgMedium) : boolean;
var
  TempText : ustring;
begin
  Result   := False;
  TempText := aDragData.GetFragmentText;

  if (length(TempText) > 0) then
    begin
      aFormat.ptd      := nil;
      aFormat.dwAspect := DVASPECT_CONTENT;
      aFormat.lindex   := -1;
      aFormat.tymed    := TYMED_HGLOBAL;
      aFormat.cfFormat := CF_UNICODETEXT;

      TempText         := TempText + #0;
      Result           := GetStorageForString(aMedium, TempText);
    end;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_Text(const aDragData : ICefDragData;
                                                      var   aFormat   : TFormatEtc;
                                                      var   aMedium   : TStgMedium) : boolean;
var
  TempText : AnsiString;
begin
  Result   := False;
  TempText := UTF8Encode(aDragData.GetFragmentText);

  if (length(TempText) > 0) then
    begin
      aFormat.ptd      := nil;
      aFormat.dwAspect := DVASPECT_CONTENT;
      aFormat.lindex   := -1;
      aFormat.tymed    := TYMED_HGLOBAL;
      aFormat.cfFormat := CF_TEXT;

      TempText         := TempText + #0;
      Result           := GetStorageForString(aMedium, TempText);
    end;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_HTML(const aDragData : ICefDragData;
                                                      var   aFormat   : TFormatEtc;
                                                      var   aMedium   : TStgMedium) : boolean;
var
  TempBaseURL, TempHTML : ustring;
  TempAnsi : AnsiString;
begin
  Result   := False;
  TempHTML := aDragData.GetFragmentHtml;

  if (length(TempHTML) > 0) then
    begin
      aFormat.ptd      := nil;
      aFormat.dwAspect := DVASPECT_CONTENT;
      aFormat.lindex   := -1;
      aFormat.tymed    := TYMED_HGLOBAL;
      aFormat.cfFormat := FHTMLFormat;

      TempBaseURL      := aDragData.GetFragmentBaseURL;
      TempAnsi         := HtmlToCFHtml(TempHTML, TempBaseURL) + #0;
      Result           := GetStorageForString(aMedium, TempAnsi);
    end;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_URL(const aDragData : ICefDragData;
                                                     var   aFormat   : TFormatEtc;
                                                     var   aMedium   : TStgMedium) : boolean;
var
  TempURL, TempTitle : ustring;
begin
  Result := False;

  if aDragData.IsLink then
    begin
      TempURL   := aDragData.GetLinkURL;
      TempTitle := aDragData.GetLinkTitle;

      if (length(TempURL) > 0) then
        begin
          aFormat.ptd      := nil;
          aFormat.dwAspect := DVASPECT_CONTENT;
          aFormat.lindex   := -1;
          aFormat.tymed    := TYMED_HGLOBAL;
          aFormat.cfFormat := FMozURLFormat;

          if (length(TempTitle) > 0) then
            TempURL := TempURL + #13 + TempTitle;

          TempURL := TempURL + #0;
          Result  := GetStorageForString(aMedium, TempURL);
        end;
    end;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_FileDesc(const aDragData : ICefDragData;
                                                          var   aFormat   : TFormatEtc;
                                                          var   aMedium   : TStgMedium) : boolean;
var
  TempFileName : ustring;
  TempSize     : cardinal;
begin
  Result := False;

  if aDragData.IsFile then
    begin
      TempSize     := aDragData.GetFileContents(nil);
      TempFileName := aDragData.GetFileName;

      if (TempSize > 0) and (length(TempFileName) > 0) then
        begin
          aFormat.ptd      := nil;
          aFormat.dwAspect := DVASPECT_CONTENT;
          aFormat.lindex   := -1;
          aFormat.tymed    := TYMED_HGLOBAL;
          aFormat.cfFormat := FFileDescFormat;

          TempFileName     := TempFileName + #0;
          Result           := GetStorageForFileDescriptor(aMedium, TempFileName);
        end;
    end;
end;

function TCEFDragAndDropMgr.DragDataToDataObject_FileContents(const aDragData : ICefDragData;
                                                              var   aFormat   : TFormatEtc;
                                                              var   aMedium   : TStgMedium) : boolean;
var
  TempHandler   : ICefWriteHandler;
  TempWriter    : ICefStreamWriter;
  TempSize      : cardinal;
begin
  Result := False;

  if aDragData.IsFile then
    begin
      TempSize := aDragData.GetFileContents(nil);

      if (TempSize > 0) then
        begin
          aFormat.ptd      := nil;
          aFormat.dwAspect := DVASPECT_CONTENT;
          aFormat.lindex   := -1;
          aFormat.tymed    := TYMED_HGLOBAL;
          aFormat.cfFormat := FFileContentsFormat;

          TempHandler := TCefBytesWriteHandler.Create(TempSize);
          TempWriter  := TCefStreamWriterRef.CreateForHandler(TempHandler);

          aDragData.GetFileContents(TempWriter);

          TempSize    := cardinal(TCefBytesWriteHandler(TempHandler).GetDataSize);
          Result      := GetStorageForBytes(aMedium, TCefBytesWriteHandler(TempHandler).GetData, TempSize);
        end;
    end;
end;

function TCEFDragAndDropMgr.ZeroFiller(aNumber, aLength : integer) : AnsiString;
begin
  Result := AnsiString(IntToStr(aNumber));

  while (length(Result) < aLength) do Result := '0' + Result;
end;

function TCEFDragAndDropMgr.HtmlToCFHtml(var aHTML, aBaseURL : ustring) : AnsiString;
const
  CRLF           : AnsiString = #13+#10;
  HTML_START_TAG : AnsiString = '<html>';
  HTML_END_TAG   : AnsiString = '</html>';
  BODY_START_TAG : AnsiString = '<body>';
  BODY_END_TAG   : AnsiString = '</body>';
  FRAGMENT_START : AnsiString = '<!--StartFragment-->';
  FRAGMENT_END   : AnsiString = '<!--EndFragment-->';
  PATTERN1       : AnsiString = '<<<<<1';
  PATTERN2       : AnsiString = '<<<<<2';
  PATTERN3       : AnsiString = '<<<<<3';
  PATTERN4       : AnsiString = '<<<<<4';
var
  TempString, TempDigits : AnsiString;
  TempPos : integer;
begin
  if (length(aHTML) = 0) then
    begin
      Result := '';
      exit;
    end;

  TempString := 'Version:0.9'                + CRLF +
                'StartHTML:'      + PATTERN1 + CRLF +
                'EndHTML:'        + PATTERN2 + CRLF +
                'StartFragment:'  + PATTERN3 + CRLF +
                'EndFragment:'    + PATTERN4 + CRLF +
                'StartSelection:' + PATTERN3 + CRLF +
                'EndSelection:'   + PATTERN4;

  if (length(aBaseURL) > 0) then
    TempString := TempString + CRLF + 'SourceURL:' + Utf8Encode(aBaseURL);

  TempString := TempString        + CRLF +
                HTML_START_TAG    + CRLF +
                BODY_START_TAG    + CRLF +
                FRAGMENT_START    + CRLF +
                Utf8Encode(aHTML) + CRLF +
                FRAGMENT_END      + CRLF +
                BODY_END_TAG      + CRLF +
                HTML_END_TAG;

  TempPos    := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx(HTML_START_TAG, TempString) + length(HTML_START_TAG);
  TempDigits := ZeroFiller(TempPos, length(PATTERN1));
  TempString := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}StringReplace(TempString, PATTERN1, TempDigits, [rfReplaceAll]);

  TempPos    := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx(HTML_END_TAG, TempString);
  TempDigits := ZeroFiller(TempPos, length(PATTERN2));
  TempString := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}StringReplace(TempString, PATTERN2, TempDigits, [rfReplaceAll]);

  TempPos    := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx(FRAGMENT_START, TempString) + length(FRAGMENT_START);
  TempDigits := ZeroFiller(TempPos, length(PATTERN3));
  TempString := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}StringReplace(TempString, PATTERN3, TempDigits, [rfReplaceAll]);

  TempPos    := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx(FRAGMENT_END, TempString);
  TempDigits := ZeroFiller(TempPos, length(PATTERN4));
  TempString := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}StringReplace(TempString, PATTERN4, TempDigits, [rfReplaceAll]);

  Result := TempString;
end;

function TCEFDragAndDropMgr.FindStringField(const aString, aFieldName : AnsiString; var aPos : integer) : string;
var
  TempLen, i, TempValuePos : integer;
  TempString : AnsiString;
begin
  aPos    := pos(aFieldName, aString);
  TempLen := length(aString);

  if (aPos > 0) then
    begin
      TempValuePos := aPos + length(aFieldName);
      i            := TempValuePos;

      while (i <= TempLen) and
            {$IFDEF DELPHI12_UP}
            not(CharInSet(aString[i], [#13, #10]))
            {$ELSE}
            not(aString[i] in [#13, #10])
            {$ENDIF} do
        inc(i);

      TempString := copy(aString, TempValuePos, i - TempValuePos);

      if (length(TempString) > 0) then
        {$IFDEF DELPHI12_UP}
        Result := UTF8ToString(TempString);
        {$ELSE}
        Result := UTF8Decode(TempString);
        {$ENDIF}
    end
   else
    Result := '';
end;

procedure TCEFDragAndDropMgr.CFHtmlToHtml(const cf_html : AnsiString; var html, base_url : string);
const
  CFHTML_VERSION   : AnsiString = 'Version:';
  CFHTML_STARTHTML : AnsiString = 'StartHTML:';
  CFHTML_ENDHTML   : AnsiString = 'EndHTML:';
  CFHTML_STARTFRAG : AnsiString = 'StartFragment:';
  CFHTML_ENDFRAG   : AnsiString = 'EndFragment:';
  CFHTML_STARSEL   : AnsiString = 'StartSelection:';
  CFHTML_ENDSEL    : AnsiString = 'EndSelection:';
  CFHTML_SOURCEURL : AnsiString = 'SourceURL:';
  FRAGMENT_START   : AnsiString = '<!--StartFragment';
  FRAGMENT_END     : AnsiString = '<!--EndFragment';
var
  TempHTMLStart, TempHTMLEnd : integer;
  TempFragStart, TempFragEnd : integer;
  TempVersionPos, TempSourcePos : integer;
  TempHTMLStartPos, TempHTMLEndPos : integer;
  TempFragStartPos, TempFragEndPos : integer;
  TempFragStartCommentPos, TempFragEndCommentPos : integer;
begin
  html     := '';
  base_url := '';

  if (FindStringField(cf_html, CFHTML_VERSION, TempVersionPos) <> '0.9') then exit;

  TempHTMLStart := StrToIntDef(FindStringField(cf_html, CFHTML_STARTHTML, TempHTMLStartPos), -1);
  TempHTMLEnd   := StrToIntDef(FindStringField(cf_html, CFHTML_ENDHTML,   TempHTMLEndPos),   -1);
  TempFragStart := StrToIntDef(FindStringField(cf_html, CFHTML_STARTFRAG, TempFragStartPos), -1);
  TempFragEnd   := StrToIntDef(FindStringField(cf_html, CFHTML_ENDFRAG,   TempFragEndPos),   -1);

  if (TempVersionPos   < TempHTMLStartPos) and
     (TempHTMLStartPos < TempHTMLEndPos)   and
     (TempHTMLEndPos   < TempFragStartPos) and
     (TempFragStartPos < TempFragEndPos)   then
    begin
      TempFragStartCommentPos := pos(FRAGMENT_START, cf_html);

      if (TempFragStartCommentPos > 0) then
        TempFragStartCommentPos := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx('-->', cf_html, TempFragStartCommentPos + length(FRAGMENT_START));

      if (TempFragStartCommentPos > 0) then
        begin
          TempFragStartCommentPos := TempFragStartCommentPos + 3;
          TempFragEndCommentPos   := {$IFDEF DELPHI12_UP}{$IFDEF DELPHI16_UP}System.{$ENDIF}AnsiStrings.{$ENDIF}PosEx(FRAGMENT_END, cf_html, TempFragStartCommentPos);
        end
       else
        if (TempFragStart > 0) and
           (TempFragEnd   > 0) then
          begin
            TempFragStartCommentPos := TempFragStart;
            TempFragEndCommentPos   := TempFragEnd;
          end
         else
          if (TempHTMLStart > 0) and
             (TempHTMLEnd   > 0) then
            begin
              TempFragStartCommentPos := TempHTMLStart;
              TempFragEndCommentPos   := TempHTMLEnd;
            end
           else
            exit;

      if (TempFragStartCommentPos > 0) and
         (TempFragEndCommentPos   > 0) and
         (TempFragEndCommentPos   > TempFragStartCommentPos) then
        begin
          {$IFDEF DELPHI12_UP}
          html := UTF8ToString(copy(cf_html, TempFragStartCommentPos, TempFragEndCommentPos - TempFragStartCommentPos));
          {$ELSE}
          html := UTF8Decode(copy(cf_html, TempFragStartCommentPos, TempFragEndCommentPos - TempFragStartCommentPos));
          {$ENDIF}

          base_url := FindStringField(cf_html, CFHTML_SOURCEURL, TempSourcePos);
        end;
    end;
end;

function TCEFDragAndDropMgr.DataObjectToDragData_Unicode(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
var
  TempText : string;
  TempPointer : pointer;
begin
  Result := False;

  if (aMedium.hGlobal <> 0) then
    begin
      TempPointer := GlobalLock(aMedium.hGlobal);

      if (TempPointer <> nil) then
        begin
          TempText := PWideChar(TempPointer);
          aDragData.SetFragmentText(TempText);
          GlobalUnlock(aMedium.hGlobal);
          Result   := True;
        end;

      ReleaseStgMedium(aMedium);
    end;
end;

function TCEFDragAndDropMgr.DataObjectToDragData_Text(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
var
  TempText : string;
  TempPointer : pointer;
begin
  Result := False;

  if (aMedium.hGlobal <> 0) then
    begin
      TempPointer := GlobalLock(aMedium.hGlobal);

      if (TempPointer <> nil) then
        begin
          {$IFDEF DELPHI12_UP}
          TempText := UTF8ToString(PAnsiChar(TempPointer));
          {$ELSE}
          TempText := UTF8Decode(PAnsiChar(TempPointer));
          {$ENDIF}

          aDragData.SetFragmentText(TempText);
          GlobalUnlock(aMedium.hGlobal);
          Result   := True;
        end;

      ReleaseStgMedium(aMedium);
    end;
end;

function TCEFDragAndDropMgr.DataObjectToDragData_URL(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
var
  TempText, TempURL, TempTitle : string;
  TempPos : integer;
  TempPointer : pointer;
begin
  Result := False;

  if (aMedium.hGlobal <> 0) then
    begin
      TempPointer := GlobalLock(aMedium.hGlobal);

      if (TempPointer <> nil) then
        begin
          TempText := PWideChar(TempPointer);
          TempPos  := LastDelimiter(#13, TempText);
          if (TempPos <= 0) then TempPos := LastDelimiter(#10, TempText);

          if (TempPos > 0) then
            begin
              TempURL   := copy(TempText, 1, pred(TempPos));
              TempTitle := copy(TempText, succ(TempPos), length(TempText));
            end
           else
            begin
              TempURL   := TempText;
              TempTitle := TempText;
            end;

          aDragData.SetLinkURL(TempURL);
          aDragData.SetLinkTitle(TempTitle);
          GlobalUnlock(aMedium.hGlobal);
          Result := True;
        end;

      ReleaseStgMedium(aMedium);
    end;
end;

function TCEFDragAndDropMgr.DataObjectToDragData_HTML(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
var
  TempAnsi : AnsiString;
  TempHTML, TempBaseURL : string;
  TempPointer : pointer;
begin
  Result := False;

  if (aMedium.hGlobal <> 0) then
    begin
      TempPointer := GlobalLock(aMedium.hGlobal);

      if (TempPointer <> nil) then
        begin
          TempAnsi := PAnsiChar(TempPointer);

          CFHtmlToHtml(TempAnsi, TempHTML, TempBaseURL);

          aDragData.SetFragmentHtml(TempHTML);
          aDragData.SetFragmentBaseURL(TempBaseURL);
          GlobalUnlock(aMedium.hGlobal);
          Result := True;
        end;

      ReleaseStgMedium(aMedium);
    end;
end;

function TCEFDragAndDropMgr.DataObjectToDragData_FileDrop(var aMedium : TStgMedium; var aDragData : ICefDragData) : boolean;
var
  TempHdrop : HDROP;
  TempNumFiles, i, TempLen : integer;
  TempText, TempFilePath, TempFileName : string;
  TempPointer : pointer;
  TempAdded : boolean;
begin
  Result := False;

  if (aMedium.hGlobal <> 0) then
    begin
      TempPointer := GlobalLock(aMedium.hGlobal);

      if (TempPointer <> nil) then
        begin
          TempHdrop    := THandle(TempPointer);
          TempNumFiles := DragQueryFile(TempHdrop, $FFFFFFFF, nil, 0);
          TempAdded    := False;
          i            := 0;

          SetLength(TempText, succ(MAX_PATH));

          while (i < TempNumFiles) do
            begin
              TempLen := DragQueryFile(TempHdrop, i, @TempText[1], succ(MAX_PATH));

              if (TempLen > 0) then
                begin
                  TempFilePath := copy(TempText, 1, TempLen);
                  TempFileName := ExtractFileName(TempFilePath);
                  TempAdded    := True;

                  if (length(TempFileName) > 0) then
                    aDragData.AddFile(TempFilePath, TempFileName)
                   else
                    aDragData.AddFile(TempFilePath, TempFilePath);
                end;

              inc(i);
            end;

          GlobalUnlock(aMedium.hGlobal);
          DragFinish(TempHdrop);
          Result := TempAdded;
        end;
    end;
end;

procedure TCEFDragAndDropMgr.DataObjectToDragData(const aDataObject : IDataObject; var aDragData : ICefDragData);
var
  TempEnumFrmt : IEnumFormatEtc;
  TempFormat   : TFormatEtc;
  TempMedium   : TStgMedium;
  TempUsed     : boolean;
begin
  try
    aDragData := TCefDragDataRef.New;

    if (aDataObject <> nil) and (aDataObject.EnumFormatEtc(DATADIR_GET, TempEnumFrmt) = S_OK) then
      begin
        TempEnumFrmt.Reset;
        TempUsed := False;

        while (TempEnumFrmt.Next(1, TempFormat, nil) = S_OK) and not(TempUsed) do
          begin
            try
              {$IFNDEF FPC}
              TempMedium.unkForRelease := nil;
              {$ELSE}
              TempMedium.PUnkForRelease := nil;
              {$ENDIF}

              if ((TempFormat.tymed and TYMED_HGLOBAL) <> 0) and
                 (aDataObject.GetData(TempFormat, TempMedium) = S_OK) then
                begin
                  if      (TempFormat.cfFormat = CF_UNICODETEXT) then TempUsed := DataObjectToDragData_Unicode(TempMedium, aDragData)
                  else if (TempFormat.cfFormat = CF_TEXT)        then TempUsed := DataObjectToDragData_Text(TempMedium, aDragData)
                  else if (TempFormat.cfFormat = FMozURLFormat)  then TempUsed := DataObjectToDragData_URL(TempMedium, aDragData)
                  else if (TempFormat.cfFormat = FHTMLFormat)    then TempUsed := DataObjectToDragData_HTML(TempMedium, aDragData)
                  else if (TempFormat.cfFormat = CF_HDROP)       then TempUsed := DataObjectToDragData_FileDrop(TempMedium, aDragData)
                  else ReleaseStgMedium(TempMedium);
                end;
            finally
              if (TempFormat.ptd <> nil) then
                begin
                  CoTaskMemFree(TempFormat.ptd);
                  TempFormat.ptd := nil;
                end;
            end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFDragAndDropMgr.DataObjectToDragData', e) then raise;
  end;
end;

function TCEFDragAndDropMgr.StartDragging : TCefDragOperation;
var
  TempDataObject  : IDataObject;
  TempDropSource  : IDropSource;
  TempResEffect   : integer;
  TempResult      : HRESULT;
  TempFormatArray : TOLEFormatArray;
  TempMediumArray : TOLEMediumArray;
  i               : integer;
begin
  Result := DRAG_OPERATION_NONE;

  if (FCurrentDragData <> nil) then
    begin
      i := 0;

      if DragDataToDataObject_Unicode(FCurrentDragData, TempFormatArray[i], TempMediumArray[i])      then inc(i);
      if DragDataToDataObject_Text(FCurrentDragData, TempFormatArray[i], TempMediumArray[i])         then inc(i);
      if DragDataToDataObject_URL(FCurrentDragData, TempFormatArray[i], TempMediumArray[i])          then inc(i);
      if DragDataToDataObject_HTML(FCurrentDragData, TempFormatArray[i], TempMediumArray[i])         then inc(i);
      if DragDataToDataObject_FileDesc(FCurrentDragData, TempFormatArray[i], TempMediumArray[i])     then inc(i);
      if DragDataToDataObject_FileContents(FCurrentDragData, TempFormatArray[i], TempMediumArray[i]) then inc(i);

      if (i > 0) then
        begin
          TempResEffect  := DROPEFFECT_NONE;
          TempDataObject := TOLEDataObject.Create(TempFormatArray, TempMediumArray, i);
          TempDropSource := TOLEDropSource.Create;
          {$IFNDEF FPC}
          TempResult     := DoDragDrop(TempDataObject, TempDropSource, FOLEEffect, TempResEffect);
          {$ELSE}
          TempResult     := DoDragDrop(TempDataObject, TempDropSource, DWORD(FOLEEffect), LPDWORD(TempResEffect));
          {$ENDIF}

          if (TempResult <> DRAGDROP_S_DROP) then TempResEffect := DROPEFFECT_NONE;
          FCurrentDragData := nil;

          DropEffectToDragOperation(TempResEffect, Result);
        end;
    end;
end;

function TCEFDragAndDropMgr.CloneDragData(const aDragData : ICefDragData; aAllowedOps : TCefDragOperations) : boolean;
begin
  if (aDragData <> nil) and
     ((length(aDragData.GetFragmentText) > 0) or
      (length(aDragData.GetFragmentHTML) > 0) or
      aDragData.IsLink or
      aDragData.IsFile) then
    begin
      DragOperationToDropEffect(aAllowedOps, FOLEEffect);

      FCurrentDragData := aDragData.Clone;
      FCurrentDragData.ResetFileContents;

      Result := True;
    end
   else
    Result := False;
end;

function TCEFDragAndDropMgr.DragEnter(const dataObj     : IDataObject;
                                            grfKeyState : Longint;
                                            pt          : TPoint;
                                      var   dwEffect    : Longint): HRESULT;
var
  TempDragData : ICefDragData;
begin
  if assigned(FOnDragEnter) then
    begin
      if (FCurrentDragData <> nil) then
        TempDragData := FCurrentDragData
       else
        DataObjectToDragData(dataObj, TempDragData);

      FOnDragEnter(self, TempDragData, grfKeyState, pt, dwEffect);
      Result := S_OK;
    end
   else
    Result := E_UNEXPECTED;
end;

function TCEFDragAndDropMgr.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  if assigned(FOnDragOver) then
    begin
      FOnDragOver(self, grfKeyState, pt, dwEffect);
      Result := S_OK;
    end
   else
    Result := E_UNEXPECTED;
end;

function TCEFDragAndDropMgr.DragLeave: HResult;
begin
  if assigned(FOnDragLeave) then
    begin
      FOnDragLeave(self);
      Result := S_OK;
    end
   else
    Result := E_UNEXPECTED;
end;

function TCEFDragAndDropMgr.Drop(const dataObj     : IDataObject;
                                       grfKeyState : Longint;
                                       pt          : TPoint;
                                 var   dwEffect    : Longint): HResult;

begin
  if assigned(FOnDrop) then
    begin
      FOnDrop(self, grfKeyState, pt, dwEffect);
      Result := S_OK;
    end
   else
    Result := E_UNEXPECTED;
end;

end.
