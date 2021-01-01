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

unit uCEFOSRIMEHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.Math, WinApi.imm,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, imm, {$IFDEF FPC}imm_dyn,{$ENDIF}{$ENDIF} Classes, Math,
  {$ENDIF}
  uCEFTypes;

const
  DEFAULT_BLINK_UNDERLINE_COLOR  = $FF000000; // Black SkColor
  DEFAULT_BLINK_BACKGROUND_COLOR = $00000000; // White SkColor
  DEFAULT_BLINK_UNDERLINE_STYLE  = CEF_CUS_SOLID;

  KEYBOARD_LAYOUT_EN_US = $0409;

type
  {$IFNDEF MSWINDOWS}
  {$IFDEF FPC}
  // Define some types to compile in Linux with Lazarus
  HWND   = NativeUInt;
  LANGID = Word;
  HIMC   = Integer;
  LPARAM = NativeInt;
  WParam = NativeUInt;
  PWChar = ^WideChar;
  {$ENDIF}
  {$ENDIF}

  TCEFOSRIMEHandler = class
    protected
      FHWND              : HWND;
      FCompositionRange  : TCefRange;
      FCursorIndex       : integer;
      FIMERect           : TCefRect;
      FSystemCaret       : boolean;
      FInputLanguageID   : LANGID;
      FCompositionBounds : TCefRectDynArray;
      FIsComposing       : boolean;
      FIMM32DLL          : THandle;

      function GetPrimaryLangID : word;
      function GetSubLangID : word;
      function GetInitialized : boolean;

      procedure GetCompositionInfo(imc : HIMC; aParam : LPARAM; var composition_text : ustring; var underlines : TCefCompositionUnderlineDynArray; var composition_start : integer);
      function  GetString(imc : HIMC; aParam : WParam; aType : integer; var aResult : ustring) : boolean;
      {$IFDEF MSWINDOWS}
      function  IsSelectionAttribute(aAttribute : byte) : boolean;
      {$ENDIF}
      procedure GetCompositionSelectionRange(imc : HIMC; var target_start, target_end : integer);
      procedure GetCompositionUnderlines(imc : HIMC; target_start, target_end : integer; var underlines : TCefCompositionUnderlineDynArray);

    public
      constructor Create(aHWND : HWND);
      destructor  Destroy; override;

      {$IFDEF MSWINDOWS}
      procedure   SetInputLanguage;
      {$ENDIF}
      procedure   CreateImeWindow;
      procedure   DestroyImeWindow;
      procedure   CleanupComposition;
      procedure   ResetComposition;
      function    GetResult(aParam : LPARAM; var aResult : ustring) : boolean;
      function    GetComposition(aParam : LPARAM; var composition_text : ustring; var underlines : TCefCompositionUnderlineDynArray; var composition_start : integer) : boolean;
      procedure   EnableIME;
      procedure   DisableIME;
      procedure   CancelIME;
      procedure   UpdateCaretPosition(index : integer);
      procedure   ChangeCompositionRange(const selection_range : TCefRange; const character_bounds : TCefRectDynArray);
      procedure   MoveImeWindow;

      property    IsComposing     : boolean   read FIsComposing;
      property    InputLanguageID : LANGID    read FInputLanguageID;
      property    PrimaryLangID   : word      read GetPrimaryLangID;
      property    SubLangID       : word      read GetSubLangID;
      property    Initialized     : boolean   read GetInitialized;

  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions;

constructor TCEFOSRIMEHandler.Create(aHWND : HWND);
begin
  inherited Create;

  FHWND                  := aHWND;
  FCompositionRange.from := 0;
  FCompositionRange.to_  := 0;
  FCursorIndex           := -1;
  FIMERect.x             := -1;
  FIMERect.y             := -1;
  FIMERect.width         := 0;
  FIMERect.height        := 0;
  FSystemCaret           := False;
  FCompositionBounds     := nil;
  FIsComposing           := False;
  FIMM32DLL              := 0;
  {$IFDEF MSWINDOWS}
  FInputLanguageID       := LANG_USER_DEFAULT;
  {$IFNDEF FPC}
  FIMM32DLL              := SafeLoadLibrary('imm32.dll');
  {$ENDIF}
  {$ENDIF}
end;

destructor TCEFOSRIMEHandler.Destroy;
begin
  DestroyImeWindow;

  {$IFDEF MSWINDOWS}
  if (FIMM32DLL <> 0) then
    begin
      FreeLibrary(FIMM32DLL);
      FIMM32DLL := 0;
    end;
  {$ENDIF}

  if (FCompositionBounds <> nil) then
    begin
      Finalize(FCompositionBounds);
      FCompositionBounds := nil;
    end;

  inherited Destroy;
end;

function TCEFOSRIMEHandler.GetPrimaryLangID : word;
begin
  Result := FInputLanguageID and $03FF;
end;

function TCEFOSRIMEHandler.GetSubLangID : word;
begin
  Result := (FInputLanguageID and $FC00) shr 10;
end;

function TCEFOSRIMEHandler.GetInitialized : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    Result := (FIMM32DLL <> 0);
    {$ELSE}
    Result := True;
    {$ENDIF}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.GetCompositionInfo(    imc               : HIMC;
                                                   aParam            : LPARAM;
                                               var composition_text  : ustring;
                                               var underlines        : TCefCompositionUnderlineDynArray;
                                               var composition_start : integer);
var
  TempStart  : integer;
  TempEnd    : integer;
  TempLen    : integer;
  i          : integer;
begin
  if (underlines <> nil) then
    begin
      Finalize(underlines, 0);
      underlines := nil;
    end;

  TempLen   := length(composition_text);
  TempStart := TempLen;
  TempEnd   := TempLen;

  {$IFDEF MSWINDOWS}
  if ((aParam and GCS_COMPATTR) <> 0) then
    GetCompositionSelectionRange(imc, TempStart, TempEnd);

  if ((aParam and CS_NOMOVECARET) = 0) and ((aParam and GCS_CURSORPOS) <> 0) then
    composition_start := ImmGetCompositionString(imc, GCS_CURSORPOS, nil, 0)
   else
    composition_start := 0;

  if ((aParam and GCS_COMPCLAUSE) <> 0) then
    GetCompositionUnderlines(imc, TempStart, TempEnd, underlines);
  {$ENDIF}

  if (underlines = nil) or (length(underlines) = 0) then
    begin
      i := 0;

      if (TempStart > 0)       then inc(i);
      if (TempEnd > TempStart) then inc(i);
      if (TempEnd < TempLen)   then inc(i);

      if (i > 0) then
        begin
          SetLength(underlines, i);
          i := 0;

          if (TempStart > 0) then
            begin
              underlines[i].color            := DEFAULT_BLINK_UNDERLINE_COLOR;
              underlines[i].background_color := DEFAULT_BLINK_BACKGROUND_COLOR;
              underlines[i].range.from       := 0;
              underlines[i].range.to_        := TempStart;
              underlines[i].thick            := 0;
              underlines[i].style            := DEFAULT_BLINK_UNDERLINE_STYLE;

              inc(i);
            end;

          if (TempEnd > TempStart) then
            begin
              underlines[i].color            := DEFAULT_BLINK_UNDERLINE_COLOR;
              underlines[i].background_color := DEFAULT_BLINK_BACKGROUND_COLOR;
              underlines[i].range.from       := TempStart;
              underlines[i].range.to_        := TempEnd;
              underlines[i].thick            := 1;
              underlines[i].style            := DEFAULT_BLINK_UNDERLINE_STYLE;

              inc(i);
            end;

          if (TempEnd < TempLen) then
            begin
              underlines[i].color            := DEFAULT_BLINK_UNDERLINE_COLOR;
              underlines[i].background_color := DEFAULT_BLINK_BACKGROUND_COLOR;
              underlines[i].range.from       := TempEnd;
              underlines[i].range.to_        := TempLen;
              underlines[i].thick            := 0;
              underlines[i].style            := DEFAULT_BLINK_UNDERLINE_STYLE;
            end;
        end;
    end;
end;

function TCEFOSRIMEHandler.GetString(imc : HIMC; aParam : WParam; aType : integer; var aResult : ustring) : boolean;
var
  TempBufferLen : integer;
  TempBuffer    : PWChar;
begin
  Result := False;

  if ((aParam and aType) = 0) then exit;

  {$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  TempBufferLen := ImmGetCompositionStringW(imc, aType, nil, 0);
  {$ELSE}
  TempBufferLen := ImmGetCompositionString(imc, aType, nil, 0);
  {$ENDIF}

  if (GetLastError <> 0) or (TempBufferLen <= 0) then exit;
  {$ENDIF}

  TempBuffer    := nil;
  TempBufferLen := TempBufferLen + SizeOf(wchar);

  try
    try
      GetMem(TempBuffer, TempBufferLen);
      FillChar(TempBuffer^, TempBufferLen, 0);

      {$IFDEF MSWINDOWS}
      {$IFDEF FPC}
      TempBufferLen := ImmGetCompositionStringW(imc, aType, TempBuffer, TempBufferLen);
      {$ELSE}
      TempBufferLen := ImmGetCompositionString(imc, aType, TempBuffer, TempBufferLen);
      {$ENDIF}
      {$ENDIF}

      if (TempBufferLen > 0) then
        begin
          aResult := TempBuffer;
          Result  := True;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFOSRIMEHandler.GetString', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then FreeMem(TempBuffer);
  end;
end;

{$IFDEF MSWINDOWS}
function TCEFOSRIMEHandler.IsSelectionAttribute(aAttribute : byte) : boolean;
begin
  Result := (aAttribute = ATTR_TARGET_CONVERTED) or
            (aAttribute = ATTR_TARGET_NOTCONVERTED);
end;
{$ENDIF}

procedure TCEFOSRIMEHandler.GetCompositionSelectionRange(imc : HIMC; var target_start, target_end : integer);
var
  i             : integer;
  TempBufferLen : integer;
  TempBuffer    : array of byte;
begin
  TempBuffer := nil;
  {$IFDEF MSWINDOWS}
  try
    try
      TempBufferLen := ImmGetCompositionString(imc, GCS_COMPATTR, nil, 0);

      if (GetLastError = 0) and (TempBufferLen > 0) then
        begin
          SetLength(TempBuffer, TempBufferLen);
          for i := 0 to pred(TempBufferLen) do TempBuffer[i] := 0;

          TempBufferLen := ImmGetCompositionString(imc, GCS_COMPATTR, @TempBuffer[0], TempBufferLen);

          if (TempBufferLen > 0) then
            begin
              i := 0;
              while (i < TempBufferLen) do
                if IsSelectionAttribute(TempBuffer[i]) then
                  begin
                    target_start := i;
                    break;
                  end
                 else
                  inc(i);

              while (i < TempBufferLen) do
                if not(IsSelectionAttribute(TempBuffer[i])) then
                  begin
                    target_end := i;
                    break;
                  end
                 else
                  inc(i);
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFOSRIMEHandler.GetCompositionSelectionRange', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then
      begin
        Finalize(TempBuffer);
        TempBuffer := nil;
      end;
  end;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.GetCompositionUnderlines(    imc          : HIMC;
                                                         target_start : integer;
                                                         target_end   : integer;
                                                     var underlines   : TCefCompositionUnderlineDynArray);
var
  i, j, TempSize : integer;
  TempUndLen     : integer;
  TempBufferLen  : integer;
  TempBuffer     : array of cardinal;
begin
  TempBuffer := nil;

  {$IFDEF MSWINDOWS}
  try
    try
      TempBufferLen := ImmGetCompositionString(imc, GCS_COMPCLAUSE, nil, 0);

      if (GetLastError = 0) and (TempBufferLen > 0) then
        begin
          TempSize := TempBufferLen div SizeOf(cardinal);
          SetLength(TempBuffer, TempSize);
          for i := 0 to pred(TempSize) do TempBuffer[i] := 0;

          TempBufferLen := ImmGetCompositionString(imc, GCS_COMPCLAUSE, @TempBuffer[0], TempBufferLen);

          if (TempBufferLen > 0) then
            begin
              TempSize := TempBufferLen div SizeOf(cardinal);

              if (underlines <> nil) then
                TempUndLen := length(underlines)
               else
                TempUndLen := 0;

              SetLength(underlines, TempUndLen + pred(TempSize));
              i := 0;

              while (i < pred(TempSize)) do
                begin
                  j := i + TempUndLen;

                  underlines[j].range.from       := TempBuffer[i];
                  underlines[j].range.to_        := TempBuffer[succ(i)];
                  underlines[j].color            := DEFAULT_BLINK_UNDERLINE_COLOR;
                  underlines[j].background_color := DEFAULT_BLINK_BACKGROUND_COLOR;
                  underlines[j].style            := DEFAULT_BLINK_UNDERLINE_STYLE;

                  if (underlines[j].range.from >= target_start) and
                     (underlines[j].range.to_  <= target_end)   then
                    underlines[j].thick := 1
                   else
                    underlines[j].thick := 0;

                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFOSRIMEHandler.GetCompositionUnderlines', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then
      begin
        Finalize(TempBuffer);
        TempBuffer := nil;
      end;
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TCEFOSRIMEHandler.SetInputLanguage;
var
  TempName   : array[0..KL_NAMELENGTH] of char;
  TempLayout : string;
  i          : integer;
  TempID     : LANGID;
begin
  TempID := KEYBOARD_LAYOUT_EN_US;

  try
    try
      for i := 0 to KL_NAMELENGTH do TempName[i] := #0;

      if GetKeyboardLayoutName(TempName) then
        begin
          TempLayout := trim(copy(TempName, KL_NAMELENGTH div 2, KL_NAMELENGTH));
          if (length(TempLayout) > 0) then TempID := StrToInt('$' + TempLayout);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFOSRIMEHandler.SetInputLanguage', e) then raise;
    end;
  finally
    FInputLanguageID := TempID;
  end;
end;
{$ENDIF}

procedure TCEFOSRIMEHandler.CreateImeWindow;
begin
  {$IFDEF MSWINDOWS}
  if ((PrimaryLangID = LANG_CHINESE) or (PrimaryLangID = LANG_JAPANESE)) and
     not(FSystemCaret) and
     CreateCaret(FHWND, 0, 1, 1) then
    FSystemCaret := True;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.DestroyImeWindow;
begin
  if FSystemCaret then
    begin
      {$IFDEF MSWINDOWS}
      DestroyCaret();
      {$ENDIF}
      FSystemCaret := False;
    end;
end;

procedure TCEFOSRIMEHandler.CleanupComposition;
var
  TempIMC : HIMC;
begin
  if Initialized and FIsComposing then
    begin
      {$IFDEF MSWINDOWS}
      TempIMC := ImmGetContext(FHWND);

      if (TempIMC <> 0) then
        try
          ImmNotifyIME(TempIMC, NI_COMPOSITIONSTR, CPS_COMPLETE, 0);
        finally
          ImmReleaseContext(FHWND, TempIMC);
        end;

      ResetComposition;
      {$ENDIF}
    end;
end;

procedure TCEFOSRIMEHandler.ResetComposition;
begin
  FIsComposing := False;
  FCursorIndex := -1;
end;

function TCEFOSRIMEHandler.GetResult(aParam : LPARAM; var aResult : ustring) : boolean;
var
  TempIMC : HIMC;
begin
  Result := False;
  if not(Initialized) then exit;

  {$IFDEF MSWINDOWS}
  TempIMC := ImmGetContext(FHWND);

  if (TempIMC <> 0) then
    try
      Result := GetString(TempIMC, aParam, GCS_RESULTSTR, aResult);
    finally
      ImmReleaseContext(FHWND, TempIMC);
    end;
  {$ENDIF}
end;

function TCEFOSRIMEHandler.GetComposition(    aParam            : LPARAM;
                                          var composition_text  : ustring;
                                          var underlines        : TCefCompositionUnderlineDynArray;
                                          var composition_start : integer) : boolean;
var
  TempIMC : HIMC;
begin
  Result := False;
  if not(Initialized) then exit;

  {$IFDEF MSWINDOWS}
  TempIMC := ImmGetContext(FHWND);

  if (TempIMC <> 0) then
    try
      if GetString(TempIMC, aParam, GCS_COMPSTR, composition_text) then
        begin
          GetCompositionInfo(TempIMC, aParam, composition_text, underlines, composition_start);
          FIsComposing := True;
          Result       := True;
        end;
    finally
      ImmReleaseContext(FHWND, TempIMC);
    end;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.EnableIME;
begin
  {$IFDEF MSWINDOWS}
  if Initialized then
    begin
      {$IFDEF DELPHI22_UP}
      ImmAssociateContextEx(FHWND, 0, IACE_DEFAULT);
      {$ELSE}
      ImmAssociateContext(FHWND, 0);
      {$ENDIF}
    end;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.DisableIME;
begin
  {$IFDEF MSWINDOWS}
  if Initialized then
    begin
      CleanupComposition();
      {$IFDEF DELPHI22_UP}
      ImmAssociateContextEx(FHWND, 0, 0);
      {$ELSE}
      ImmAssociateContext(FHWND, 0);
      {$ENDIF}
    end;
  {$ENDIF}
end;

procedure TCEFOSRIMEHandler.CancelIME;
var
  TempIMC : HIMC;
begin
  if Initialized and FIsComposing then
    begin
      {$IFDEF MSWINDOWS}
      TempIMC := ImmGetContext(FHWND);

      if (TempIMC <> 0) then
        try
          ImmNotifyIME(TempIMC, NI_COMPOSITIONSTR, CPS_CANCEL, 0);
        finally
          ImmReleaseContext(FHWND, TempIMC);
        end;

      ResetComposition;
      {$ENDIF}
    end;
end;

procedure TCEFOSRIMEHandler.UpdateCaretPosition(index : integer);
begin
  FCursorIndex := index;
  MoveImeWindow();
end;

procedure TCEFOSRIMEHandler.ChangeCompositionRange(const selection_range  : TCefRange;
                                                   const character_bounds : TCefRectDynArray);
var
  i : integer;
begin
  FCompositionRange := selection_range;

  if (character_bounds <> nil) then
    begin
      SetLength(FCompositionBounds, length(character_bounds));
      i := pred(length(character_bounds));

      while (i >= 0) do
        begin
          FCompositionBounds[i] := character_bounds[i];
          dec(i);
        end;
    end
   else
    begin
      Finalize(FCompositionBounds);
      FCompositionBounds := nil;
    end;

  MoveImeWindow();
end;

procedure TCEFOSRIMEHandler.MoveImeWindow;
{$IFDEF MSWINDOWS}
var
  TempRect        : TCefRect;
  TempLocation    : integer;
  TempIMC         : HIMC;
  TempCandidate   : TCandidateForm;
const
  CARET_MARGIN = 1;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if not(Initialized) and (GetFocus <> FHWND) then exit;

  TempRect     := FIMERect;
  TempLocation := FCursorIndex;

  if (TempLocation = -1) then
    TempLocation := FCompositionRange.from;

  if (TempLocation >= FCompositionRange.from) then
    TempLocation := TempLocation - FCompositionRange.from;

  if (FCompositionBounds = nil) then
    exit
   else
    if (TempLocation >= 0) and
       (TempLocation < length(FCompositionBounds)) then
      TempRect := FCompositionBounds[TempLocation]
     else
      if (length(FCompositionBounds) > 0) then
        TempRect := FCompositionBounds[pred(length(FCompositionBounds))]
       else
        exit;

  TempIMC := ImmGetContext(FHWND);

  if (TempIMC <> 0) then
    try
      if (PrimaryLangID = LANG_CHINESE) then
        begin
          TempCandidate.dwIndex        := 0;
          TempCandidate.dwStyle        := CFS_CANDIDATEPOS;
          TempCandidate.ptCurrentPos.X := TempRect.x;
          TempCandidate.ptCurrentPos.Y := TempRect.y;
          TempCandidate.rcArea.Left    := 0;
          TempCandidate.rcArea.Top     := 0;
          TempCandidate.rcArea.Right   := 0;
          TempCandidate.rcArea.Bottom  := 0;

          ImmSetCandidateWindow(TempIMC, @TempCandidate);
        end;

      if FSystemCaret then
        case PrimaryLangID of
          LANG_JAPANESE : SetCaretPos(TempRect.x, TempRect.y + TempRect.height);
          else            SetCaretPos(TempRect.x, TempRect.y);
        end;


      if (PrimaryLangID = LANG_KOREAN) then
        TempRect.y := TempRect.y + CARET_MARGIN;

      TempCandidate.dwIndex        := 0;
      TempCandidate.dwStyle        := CFS_EXCLUDE;
      TempCandidate.ptCurrentPos.X := TempRect.x;
      TempCandidate.ptCurrentPos.Y := TempRect.y;
      TempCandidate.rcArea.Left    := TempRect.x;
      TempCandidate.rcArea.Top     := TempRect.y;
      TempCandidate.rcArea.Right   := TempRect.x + TempRect.width;
      TempCandidate.rcArea.Bottom  := TempRect.y + TempRect.height;

      ImmSetCandidateWindow(TempIMC, @TempCandidate);
    finally
      ImmReleaseContext(FHWND, TempIMC);
    end;
  {$ENDIF}
end;


end.
