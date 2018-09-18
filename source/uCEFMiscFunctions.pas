// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

unit uCEFMiscFunctions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.ActiveX,{$ENDIF} System.IOUtils, System.Classes, System.SysUtils, System.UITypes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX,{$ENDIF} {$IFDEF DELPHI14_UP}IOUtils,{$ENDIF} Classes, SysUtils, Controls, Graphics, Math,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFResourceHandler,
  uCEFRegisterCDMCallback;

const
  Kernel32DLL = 'kernel32.dll';
  SHLWAPIDLL  = 'shlwapi.dll';

function CefColorGetA(color: TCefColor): Byte;
function CefColorGetR(color: TCefColor): byte;
function CefColorGetG(color: TCefColor): Byte;
function CefColorGetB(color: TCefColor): Byte;

function CefColorSetARGB(a, r, g, b: Byte): TCefColor;

function CefInt64Set(int32_low, int32_high: Integer): Int64;

function CefInt64GetLow(const int64_val: Int64): Integer;
function CefInt64GetHigh(const int64_val: Int64): Integer;

function CefGetObject(ptr: Pointer): TObject;  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CefGetData(const i: ICefBaseRefCounted): Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function CefStringAlloc(const str: ustring): TCefString;
function CefStringClearAndGet(var str: TCefString): ustring;

function  CefString(const str: ustring): TCefString; overload;
function  CefString(const str: PCefString): ustring; overload;
function  CefUserFreeString(const str: ustring): PCefStringUserFree;
procedure CefStringFree(const str: PCefString);
function  CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
procedure CefStringSet(const str: PCefString; const value: ustring);

function  CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
procedure CefPostTask(ThreadId: TCefThreadId; const task: ICefTask);
procedure CefPostDelayedTask(ThreadId: TCefThreadId; const task: ICefTask; delayMs: Int64);

function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
function CefTimeToDateTime(const dt: TCefTime): TDateTime;
function DateTimeToCefTime(dt: TDateTime): TCefTime;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;

{$IFDEF MSWINDOWS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : THandle; aRect : TRect; const aWindowName : ustring = '');
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : ustring = '');
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : ustring = '');
{$ENDIF}

{$IFDEF MACOS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; aHidden : boolean = False; const aWindowName : ustring = '');
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aHidden : boolean = False; const aWindowName : ustring = '');
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aHidden : boolean = False; const aWindowName : ustring = '');
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect);
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle);
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle);
{$ENDIF}

{$IFDEF MSWINDOWS}
function ProcessUnderWow64(hProcess: THandle; var Wow64Process: BOOL): BOOL; external Kernel32DLL name 'IsWow64Process';
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external Kernel32DLL;
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation; lpUniversalTime, lpLocalTime: PSystemTime): BOOL; stdcall; external Kernel32DLL;

function PathIsRelativeAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeA';
function PathIsRelativeUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeW';

{$IFNDEF DELPHI12_UP}
  {$IFDEF WIN64}
    function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: int64): int64; stdcall; external user32 name 'SetWindowLongPtrW';
  {$ELSE}
    function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LongInt): LongInt; stdcall; external user32 name 'SetWindowLongW';
  {$ENDIF}
{$ENDIF}

{$ENDIF}

function CustomPathIsRelative(const aPath : string) : boolean;
function GetModulePath : string;

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
function CefIsCertStatusMinorError(Status : TCefCertStatus) : boolean;

function  CefCrashReportingEnabled : boolean;
procedure CefSetCrashKeyValue(const aKey, aValue : ustring);

procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
procedure CefDebugLog(const aMessage : string);
procedure OutputDebugMessage(const aMessage : string);
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;

function CefRegisterSchemeHandlerFactory(const SchemeName, DomainName : ustring; const handler: TCefResourceHandlerClass = nil): Boolean;
function CefClearSchemeHandlerFactories : boolean;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefClearCrossOriginWhitelist: Boolean;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
function  GetExtendedFileVersion(const aFileName : string) : uint64;
function  GetStringFileInfo(const aFileName, aField : string; var aValue : string) : boolean;
function  GetDLLVersion(const aDLLFile : string; var aVersionInfo : TFileVersionInfo) : boolean;

function SplitLongString(aSrcString : string) : string;
function GetAbsoluteDirPath(const aSrcPath : string; var aRsltPath : string) : boolean;
function CheckLocales(const aLocalesDirPath : string; var aMissingFiles : string; const aLocalesRequired : string = '') : boolean;
function CheckResources(const aResourcesDirPath : string; var aMissingFiles : string; aCheckDevResources: boolean = True) : boolean;
function CheckDLLs(const aFrameworkDirPath : string; var aMissingFiles : string) : boolean;
function CheckDLLVersion(const aDLLFile : string; aMajor, aMinor, aRelease, aBuild : uint16) : boolean;
function FileVersionInfoToString(const aVersionInfo : TFileVersionInfo) : string;
function CheckFilesExist(var aList : TStringList; var aMissingFiles : string) : boolean;
function GetDLLHeaderMachine(const aDLLFile : string; var aMachine : integer) : boolean;
function Is32BitProcess : boolean;

function  CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
function  CefCreateUrl(var parts: TUrlParts): ustring;
function  CefFormatUrlForSecurityDisplay(const originUrl: string): string;
function  CefGetMimeType(const extension: ustring): ustring;
procedure CefGetExtensionsForMimeType(const mimeType: ustring; var extensions: TStringList);

function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
function CefBase64Decode(const data: ustring): ICefBinaryValue;
function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
function CefUriDecode(const text: ustring; convertToUtf8: Boolean; unescapeRule: TCefUriUnescapeRule): ustring;

function CefParseJson(const jsonString: ustring; options: TCefJsonParserOptions): ICefValue;
function CefParseJsonAndReturnError(const jsonString   : ustring;
                                          options      : TCefJsonParserOptions;
                                    out   errorCodeOut : TCefJsonParserError;
                                    out   errorMsgOut  : ustring): ICefValue;
function CefWriteJson(const node: ICefValue; options: TCefJsonWriterOptions): ustring;

function CefCreateDirectory(const fullPath: ustring): Boolean;
function CefGetTempDirectory(out tempDir: ustring): Boolean;
function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring; out newDir: ustring): Boolean;
function CefDirectoryExists(const path: ustring): Boolean;
function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
procedure CefLoadCRLSetsFile(const path : ustring);

function CefIsKeyDown(aWparam : WPARAM) : boolean;
function CefIsKeyToggled(aWparam : WPARAM) : boolean;
function GetCefMouseModifiers(awparam : WPARAM) : TCefEventFlags; overload;
function GetCefMouseModifiers : TCefEventFlags; overload;
function GetCefKeyboardModifiers(aWparam : WPARAM; aLparam : LPARAM) : TCefEventFlags;
function GefCursorToWindowsCursor(aCefCursor : TCefCursorType) : TCursor;

procedure DropEffectToDragOperation(aEffect : Longint; var aAllowedOps : TCefDragOperations);
procedure DragOperationToDropEffect(const aDragOperations : TCefDragOperations; var aEffect: Longint);

function  DeviceToLogical(aValue : integer; const aDeviceScaleFactor : double) : integer; overload;
procedure DeviceToLogical(var aEvent : TCEFMouseEvent; const aDeviceScaleFactor : double); overload;
procedure DeviceToLogical(var aPoint : TPoint; const aDeviceScaleFactor : double); overload;
function  LogicalToDevice(aValue : integer; const aDeviceScaleFactor : double) : integer; overload;
procedure LogicalToDevice(var aRect : TCEFRect; const aDeviceScaleFactor : double); overload;

function GetScreenDPI : integer;
function GetDeviceScaleFactor : single;

function DeleteDirContents(const aDirectory : string) : boolean;

implementation

uses
  uCEFConstants, uCEFApplication, uCEFSchemeHandlerFactory, uCEFValue,
  uCEFBinaryValue, uCEFStringList;

function CefColorGetA(color: TCefColor): Byte;
begin
  Result := (color shr 24) and $FF;
end;

function CefColorGetR(color: TCefColor): byte;
begin
  Result := (color shr 16) and $FF;
end;

function CefColorGetG(color: TCefColor): Byte;
begin
  Result := (color shr 8) and $FF;
end;

function CefColorGetB(color: TCefColor): Byte;
begin
  Result := color and $FF;
end;

function CefColorSetARGB(a, r, g, b: Byte): TCefColor;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function CefInt64Set(int32_low, int32_high: Integer): Int64;
begin
  Result := int32_low or (int32_high shl 32);
end;

function CefInt64GetLow(const int64_val: Int64): Integer;
begin
  Result := Integer(int64_val);
end;

function CefInt64GetHigh(const int64_val: Int64): Integer;
begin
  Result := (int64_val shr 32) and $FFFFFFFF;
end;

function CefStringClearAndGet(var str: TCefString): ustring;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result := CefString(@str);
      cef_string_utf16_clear(@str);
    end
   else
    Result := '';
end;

function CefGetObject(ptr: Pointer): TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if (ptr <> nil) then
    begin
      Dec(PByte(ptr), SizeOf(Pointer));
      Result := TObject(PPointer(ptr)^);
    end
   else
    Result := nil;
end;

function CefGetData(const i: ICefBaseRefCounted): Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if (i <> nil) then
    Result := i.Wrap
   else
    Result := nil;
end;

function CefString(const str: PCefString): ustring;
begin
  if (str <> nil) then
    SetString(Result, str^.str, str^.length)
   else
    Result := '';
end;

function CefString(const str: ustring): TCefString;
begin
  Result.str    := PChar16(PWideChar(str));
  Result.length := Length(str);
  Result.dtor   := nil;
end;

procedure CefStringFree(const str: PCefString);
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_utf16_clear(str);
end;

procedure CefStringSet(const str: PCefString; const value: ustring);
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_utf16_set(PWideChar(value), Length(value), str, 1);
end;

function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result := CefString(PCefString(str));
      cef_string_userfree_utf16_free(str);
    end
   else
    Result := '';
end;

function CefStringAlloc(const str: ustring): TCefString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (str <> '') and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_wide_to_utf16(PWideChar(str), Length(str), @Result);
end;

procedure _free_string(str: PChar16); stdcall;
begin
  if (str <> nil) then FreeMem(str);
end;

function CefUserFreeString(const str: ustring): PCefStringUserFree;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result         := cef_string_userfree_utf16_alloc();
      Result^.length := Length(str);
      GetMem(Result^.str, Result^.length * SizeOf(TCefChar));
      Move(PCefChar(str)^, Result^.str^, Result^.length * SizeOf(TCefChar));
      Result^.dtor   := @_free_string;
    end
   else
    Result := nil;
end;

function CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
var
  TempName, TempCode : TCefString;
begin
  if (GlobalCEFApp <> nil)  and
     GlobalCEFApp.LibLoaded and
     (length(name) > 0)     and
     (length(code) > 0)     then
    begin
      TempName := CefString(name);
      TempCode := CefString(code);
      Result   := cef_register_extension(@TempName, @TempCode, CefGetData(handler)) <> 0;
    end
   else
    Result := False;
end;

procedure CefPostTask(ThreadId: TCefThreadId; const task: ICefTask);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_post_task(ThreadId, CefGetData(task));
end;

procedure CefPostDelayedTask(ThreadId: TCefThreadId; const task: ICefTask; delayMs: Int64);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_post_delayed_task(ThreadId, CefGetData(task), delayMs);
end;

function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.wYear          := dt.year;
  Result.wMonth         := dt.month;
  Result.wDayOfWeek     := dt.day_of_week;
  Result.wDay           := dt.day_of_month;
  Result.wHour          := dt.hour;
  Result.wMinute        := dt.minute;
  Result.wSecond        := dt.second;
  Result.wMilliseconds  := dt.millisecond;
end;

function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year         := dt.wYear;
  Result.month        := dt.wMonth;
  Result.day_of_week  := dt.wDayOfWeek;
  Result.day_of_month := dt.wDay;
  Result.hour         := dt.wHour;
  Result.minute       := dt.wMinute;
  Result.second       := dt.wSecond;
  Result.millisecond  := dt.wMilliseconds;
end;

function CefTimeToDateTime(const dt: TCefTime): TDateTime;
var
  TempTime : TSystemTime;
begin
  Result := 0;

  try
    TempTime := CefTimeToSystemTime(dt);
    SystemTimeToTzSpecificLocalTime(nil, @TempTime, @TempTime);
    Result   := SystemTimeToDateTime(TempTime);
  except
    on e : exception do
      if CustomExceptionHandler('CefTimeToDateTime', e) then raise;
  end;
end;

function DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  TempTime : TSystemTime;
begin
  FillChar(Result, SizeOf(TCefTime), 0);

  try
    DateTimeToSystemTime(dt, TempTime);
    TzSpecificLocalTimeToSystemTime(nil, @TempTime, @TempTime);
    Result := SystemTimeToCefTime(TempTime);
  except
    on e : exception do
      if CustomExceptionHandler('DateTimeToCefTime', e) then raise;
  end;
end;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_wide_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf8_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf16_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf16_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

{$IFDEF MSWINDOWS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : THandle; aRect : TRect; const aWindowName : ustring);
begin
  aWindowInfo.ex_style                     := 0;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
  aWindowInfo.x                            := aRect.left;
  aWindowInfo.y                            := aRect.top;
  aWindowInfo.width                        := aRect.right  - aRect.left;
  aWindowInfo.height                       := aRect.bottom - aRect.top;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : ustring);
begin
  aWindowInfo.ex_style                     := 0;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_VISIBLE;
  aWindowInfo.x                            := integer(CW_USEDEFAULT);
  aWindowInfo.y                            := integer(CW_USEDEFAULT);
  aWindowInfo.width                        := integer(CW_USEDEFAULT);
  aWindowInfo.height                       := integer(CW_USEDEFAULT);
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : ustring);
begin
  aWindowInfo.ex_style                     := 0;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := 0;
  aWindowInfo.x                            := 0;
  aWindowInfo.y                            := 0;
  aWindowInfo.width                        := 0;
  aWindowInfo.height                       := 0;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.window                       := 0;
end;
{$ENDIF}

{$IFDEF MACOS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; aHidden : boolean; const aWindowName : ustring);
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.x                            := aRect.left;
  aWindowInfo.y                            := aRect.top;
  aWindowInfo.width                        := aRect.right  - aRect.left;
  aWindowInfo.height                       := aRect.bottom - aRect.top;
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.view                         := 0;
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle; aHidden : boolean; const aWindowName : ustring);
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.x                            := integer(CW_USEDEFAULT);
  aWindowInfo.y                            := integer(CW_USEDEFAULT);
  aWindowInfo.width                        := integer(CW_USEDEFAULT);
  aWindowInfo.height                       := integer(CW_USEDEFAULT);
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.view                         := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; aHidden : boolean; const aWindowName : ustring);
begin

  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.x                            := 0;
  aWindowInfo.y                            := 0;
  aWindowInfo.width                        := 0;
  aWindowInfo.height                       := 0;
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.view                         := 0;
end;
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect);
begin
  aWindowInfo.x                            := aRect.left;
  aWindowInfo.y                            := aRect.top;
  aWindowInfo.width                        := aRect.right  - aRect.left;
  aWindowInfo.height                       := aRect.bottom - aRect.top;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle);
begin
  aWindowInfo.x                            := integer(CW_USEDEFAULT);
  aWindowInfo.y                            := integer(CW_USEDEFAULT);
  aWindowInfo.width                        := integer(CW_USEDEFAULT);
  aWindowInfo.height                       := integer(CW_USEDEFAULT);
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle);
begin
  aWindowInfo.x                            := 0;
  aWindowInfo.y                            := 0;
  aWindowInfo.width                        := 0;
  aWindowInfo.height                       := 0;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;
{$ENDIF}

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_is_cert_status_error(Status) <> 0);
end;

function CefIsCertStatusMinorError(Status : TCefCertStatus) : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_is_cert_status_minor_error(Status) <> 0);
end;

function CefCrashReportingEnabled : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_crash_reporting_enabled() <> 0);
end;

procedure CefSetCrashKeyValue(const aKey, aValue : ustring);
var
  TempKey, TempValue : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempKey   := CefString(aKey);
      TempValue := CefString(aValue);
      cef_set_crash_key_value(@TempKey, @TempValue);
    end;
end;

procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
var
  TempFile, TempMessage : AnsiString;
begin
  if (length(aFile) > 0) and (length(aMessage) > 0) then
    begin
      TempFile    := AnsiString(aFile);
      TempMessage := AnsiString(aMessage);

      cef_log(@TempFile[1], aLine, aSeverity, @TempMessage[1]);
    end;
end;

procedure CefDebugLog(const aMessage : string);
const
  DEFAULT_LINE = 1;
var
  TempString : string;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempString := 'PID: ' + IntToStr(GetCurrentProcessID) + ', TID: ' + IntToStr(GetCurrentThreadID);

      case GlobalCEFApp.ProcessType of
        ptBrowser   : TempString := TempString + ', PT: Browser';
        ptRenderer  : TempString := TempString + ', PT: Renderer';
        ptZygote    : TempString := TempString + ', PT: Zygote';
        ptGPU       : TempString := TempString + ', PT: GPU';
        else          TempString := TempString + ', PT: Other';
      end;

      CefLog('CEF4Delphi', DEFAULT_LINE, CEF_LOG_SEVERITY_ERROR, TempString + ' - ' + aMessage);
    end;
end;

procedure OutputDebugMessage(const aMessage : string);
const
  DEFAULT_LINE = 1;
begin
  {$IFDEF DEBUG}
  OutputDebugString({$IFDEF DELPHI12_UP}PWideChar{$ELSE}PAnsiChar{$ENDIF}(aMessage + chr(0)));

  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    CefLog('CEF4Delphi', DEFAULT_LINE, CEF_LOG_SEVERITY_ERROR, aMessage);
  {$ENDIF}
end;

function CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
begin
  OutputDebugMessage(aFunctionName + ' error : ' + aException.message);

  Result := (GlobalCEFApp <> nil) and GlobalCEFApp.ReRaiseExceptions;
end;

function CefRegisterSchemeHandlerFactory(const SchemeName : ustring;
                                         const DomainName : ustring;
                                         const handler    : TCefResourceHandlerClass) : boolean;
var
  TempScheme, TempDomainName : TCefString;
  TempFactory : ICefSchemeHandlerFactory;
  TempDomainNamePtr : PCefString;
begin
  Result := False;

  try
    if (GlobalCEFApp <> nil)    and
       GlobalCEFApp.LibLoaded   and
       (length(SchemeName) > 0) then
      begin
        if (length(DomainName) > 0) then
          begin
            TempDomainName    := CefString(DomainName);
            TempDomainNamePtr := @TempDomainName;
          end
         else
          TempDomainNamePtr := nil;

        TempScheme  := CefString(SchemeName);
        TempFactory := TCefSchemeHandlerFactoryOwn.Create(handler);
        Result      := cef_register_scheme_handler_factory(@TempScheme, TempDomainNamePtr, TempFactory.Wrap) <> 0;
      end;
  finally
    TempFactory := nil;
  end;
end;

function CefClearSchemeHandlerFactories : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_clear_scheme_handler_factories() <> 0);
end;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                         const TargetProtocol        : ustring;
                                         const TargetDomain          : ustring;
                                               AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSourceOrigin   := CefString(SourceOrigin);
      TempTargetProtocol := CefString(TargetProtocol);
      TempTargetDomain   := CefString(TargetDomain);
      Result             := cef_add_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                                 @TempTargetProtocol,
                                                                 @TempTargetDomain,
                                                                 Ord(AllowTargetSubdomains)) <> 0;
    end
   else
    Result := False;
end;

function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                            const TargetProtocol        : ustring;
                                            const TargetDomain          : ustring;
                                                  AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSourceOrigin   := CefString(SourceOrigin);
      TempTargetProtocol := CefString(TargetProtocol);
      TempTargetDomain   := CefString(TargetDomain);
      Result             := cef_remove_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                                    @TempTargetProtocol,
                                                                    @TempTargetDomain,
                                                                    Ord(AllowTargetSubdomains)) <> 0;
    end
   else
    Result := False;
end;

function CefClearCrossOriginWhitelist: Boolean;
begin
  Result := cef_clear_cross_origin_whitelist() <> 0;
end;

function SplitLongString(aSrcString : string) : string;
const
  MAXLINELENGTH = 50;
begin
  while (length(aSrcString) > 0) do
    begin
      if (length(Result) > 0) then
        Result := Result + CRLF + copy(aSrcString, 1, MAXLINELENGTH)
       else
        Result := Result + copy(aSrcString, 1, MAXLINELENGTH);

      aSrcString := copy(aSrcString, succ(MAXLINELENGTH), length(aSrcString));
    end;
end;

function GetAbsoluteDirPath(const aSrcPath : string; var aRsltPath : string) : boolean;
begin
  Result := True;

  if (length(aSrcPath) > 0) then
    begin
      aRsltPath := IncludeTrailingPathDelimiter(aSrcPath);

      if DirectoryExists(aSrcPath) then
        begin
          if CustomPathIsRelative(aRsltPath) then aRsltPath := GetModulePath + aRsltPath;
        end
       else
        Result := False;
    end
   else
    aRsltPath := '';
end;

function CheckLocales(const aLocalesDirPath : string; var aMissingFiles : string; const aLocalesRequired : string) : boolean;
const
  LOCALES_REQUIRED_DEFAULT =
    'am,' +
    'ar,' +
    'bg,' +
    'bn,' +
    'ca,' +
    'cs,' +
    'da,' +
    'de,' +
    'el,' +
    'en-GB,' +
    'en-US,' +
    'es,' +
    'es-419,' +
    'et,' +
    'fa,' +
    'fi,' +
    'fil,' +
    'fr,' +
    'gu,' +
    'he,' +
    'hi,' +
    'hr,' +
    'hu,' +
    'id,' +
    'it,' +
    'ja,' +
    'kn,' +
    'ko,' +
    'lt,' +
    'lv,' +
    'ml,' +
    'mr,' +
    'ms,' +
    'nb,' +
    'nl,' +
    'pl,' +
    'pt-BR,' +
    'pt-PT,' +
    'ro,' +
    'ru,' +
    'sk,' +
    'sl,' +
    'sr,' +
    'sv,' +
    'sw,' +
    'ta,' +
    'te,' +
    'th,' +
    'tr,' +
    'uk,' +
    'vi,' +
    'zh-CN,' +
    'zh-TW';
var
  i        : integer;
  TempDir  : string;
  TempList : TStringList;
begin
  Result   := False;
  TempList := nil;

  try
    try
      if (length(aLocalesDirPath) > 0) then
        TempDir := IncludeTrailingPathDelimiter(aLocalesDirPath)
       else
        TempDir := 'locales\';

      TempList := TStringList.Create;

      if (length(aLocalesRequired) > 0) then
        TempList.CommaText := aLocalesRequired
       else
        TempList.CommaText := LOCALES_REQUIRED_DEFAULT;

      i := 0;
      while (i < TempList.Count) do
        begin
          TempList[i] := TempDir + TempList[i] + '.pak';
          inc(i);
        end;

      if DirectoryExists(TempDir) then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckLocales', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckResources(const aResourcesDirPath : string; var aMissingFiles : string; aCheckDevResources: boolean) : boolean;
var
  TempDir    : string;
  TempList   : TStringList;
  TempExists : boolean;
begin
  Result := False;

  try
    try
      TempExists := GetAbsoluteDirPath(aResourcesDirPath, TempDir);

      TempList := TStringList.Create;
      TempList.Add(TempDir + 'natives_blob.bin');
      TempList.Add(TempDir + 'snapshot_blob.bin');
      TempList.Add(TempDir + 'v8_context_snapshot.bin');
      TempList.Add(TempDir + 'cef.pak');
      TempList.Add(TempDir + 'cef_100_percent.pak');
      TempList.Add(TempDir + 'cef_200_percent.pak');
      TempList.Add(TempDir + 'cef_extensions.pak');

      if aCheckDevResources then TempList.Add(TempDir + 'devtools_resources.pak');

      if TempExists then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckResources', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckDLLs(const aFrameworkDirPath : string; var aMissingFiles : string) : boolean;
var
  TempDir    : string;
  TempList   : TStringList;
  TempExists : boolean;
begin
  Result   := False;
  TempList := nil;

  try
    try
      TempExists := GetAbsoluteDirPath(aFrameworkDirPath, TempDir);

      // The icudtl.dat file must be placed next to libcef.dll
      // http://www.magpcss.org/ceforum/viewtopic.php?f=6&t=14503#p32263

      TempList := TStringList.Create;
      TempList.Add(TempDir + CHROMEELF_DLL);
      TempList.Add(TempDir + LIBCEF_DLL);
      TempList.Add(TempDir + 'd3dcompiler_43.dll');
      TempList.Add(TempDir + 'd3dcompiler_47.dll');
      TempList.Add(TempDir + 'libEGL.dll');
      TempList.Add(TempDir + 'libGLESv2.dll');
      TempList.Add(TempDir + 'swiftshader\libEGL.dll');
      TempList.Add(TempDir + 'swiftshader\libGLESv2.dll');
      TempList.Add(TempDir + 'icudtl.dat');

      if TempExists then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckDLLs', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckFilesExist(var aList : TStringList; var aMissingFiles : string) : boolean;
var
  i : integer;
begin
  Result := True;

  try
    if (aList <> nil) then
      begin
        i := 0;

        while (i < aList.Count) do
          begin
            if (length(aList[i]) > 0) and not(FileExists(aList[i])) then
              begin
                Result        := False;
                aMissingFiles := aMissingFiles + aList[i] + CRLF;
              end;

            inc(i);
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('CheckFilesExist', e) then raise;
  end;
end;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
begin
  aVersionInfo.MajorVer := uint16(aVersion shr 48);
  aVersionInfo.MinorVer := uint16((aVersion shr 32) and $FFFF);
  aVersionInfo.Release  := uint16((aVersion shr 16) and $FFFF);
  aVersionInfo.Build    := uint16(aVersion and $FFFF);
end;

function GetExtendedFileVersion(const aFileName : string) : uint64;
var
  TempSize   : DWORD;
  TempBuffer : pointer;
  TempLen    : UINT;
  TempHandle : cardinal;
  TempInfo   : PVSFixedFileInfo;
begin
  Result     := 0;
  TempBuffer := nil;

  try
    try
      TempSize := GetFileVersioninfoSize(PChar(aFileName), TempHandle);

      if (TempSize > 0) then
        begin
          GetMem(TempBuffer, TempSize);

          if GetFileVersionInfo(PChar(aFileName), TempHandle, TempSize, TempBuffer) and
             VerQueryValue(TempBuffer, '\', Pointer(TempInfo), TempLen) then
            begin
              Result := TempInfo^.dwFileVersionMS;
              Result := Result shl 32;
              Result := Result or TempInfo^.dwFileVersionLS;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('GetExtendedFileVersion', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then FreeMem(TempBuffer);
  end;
end;

function GetStringFileInfo(const aFileName, aField : string; var aValue : string) : boolean;
type
  PLangAndCodepage = ^TLangAndCodepage;
  TLangAndCodepage = record
    wLanguage : word;
    wCodePage : word;
  end;
var
  TempSize     : DWORD;
  TempBuffer   : pointer;
  TempHandle   : cardinal;
  TempPointer  : pointer;
  TempSubBlock : string;
  TempLang     : PLangAndCodepage;
  TempArray    : array of TLangAndCodepage;
  i, j : DWORD;
begin
  Result     := False;
  TempBuffer := nil;
  TempArray  := nil;
  aValue     := '';

  try
    try
      TempSize := GetFileVersionInfoSize(PChar(aFileName), TempHandle);

      if (TempSize > 0) then
        begin
          GetMem(TempBuffer, TempSize);

          if GetFileVersionInfo(PChar(aFileName), 0, TempSize, TempBuffer) then
            begin
              if VerQueryValue(TempBuffer, '\VarFileInfo\Translation\', Pointer(TempLang), TempSize) then
                begin
                  i := 0;
                  j := TempSize div SizeOf(TLangAndCodepage);

                  SetLength(TempArray, j);

                  while (i < j) do
                    begin
                      TempArray[i].wLanguage := TempLang^.wLanguage;
                      TempArray[i].wCodePage := TempLang^.wCodePage;
                      inc(TempLang);
                      inc(i);
                    end;
                end;

              i := 0;
              j := Length(TempArray);

              while (i < j) and not(Result) do
                begin
                  TempSubBlock := '\StringFileInfo\' +
                                  IntToHex(TempArray[i].wLanguage, 4) + IntToHex(TempArray[i].wCodePage, 4) +
                                  '\' + aField;

                  if VerQueryValue(TempBuffer, PChar(TempSubBlock), TempPointer, TempSize) then
                    begin
                      aValue := trim(PChar(TempPointer));
                      Result := (length(aValue) > 0);
                    end;

                  inc(i);
                end;

              // Adobe's flash player DLL uses a different codepage to store the StringFileInfo fields
              if not(Result) and (j > 0) and (TempArray[0].wCodePage <> 1252) then
                begin
                  TempSubBlock := '\StringFileInfo\' +
                                  IntToHex(TempArray[0].wLanguage, 4) + IntToHex(1252, 4) +
                                  '\' + aField;

                  if VerQueryValue(TempBuffer, PChar(TempSubBlock), TempPointer, TempSize) then
                    begin
                      aValue := trim(PChar(TempPointer));
                      Result := (length(aValue) > 0);
                    end;
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('GetStringFileInfo', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then FreeMem(TempBuffer);
  end;
end;

function GetDLLVersion(const aDLLFile : string; var aVersionInfo : TFileVersionInfo) : boolean;
var
  TempVersion : uint64;
begin
  Result := False;

  try
    if FileExists(aDLLFile) then
      begin
        TempVersion := GetExtendedFileVersion(aDLLFile);
        UInt64ToFileVersionInfo(TempVersion, aVersionInfo);
        Result := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('GetDLLVersion', e) then raise;
  end;
end;

function FileVersionInfoToString(const aVersionInfo : TFileVersionInfo) : string;
begin
  Result := IntToStr(aVersionInfo.MajorVer) + '.' +
            IntToStr(aVersionInfo.MinorVer) + '.' +
            IntToStr(aVersionInfo.Release)  + '.' +
            IntToStr(aVersionInfo.Build);
end;

function CheckDLLVersion(const aDLLFile : string; aMajor, aMinor, aRelease, aBuild : uint16) : boolean;
var
  TempVersionInfo : TFileVersionInfo;
begin
  Result := GetDLLVersion(aDLLFile, TempVersionInfo) and
            (TempVersionInfo.MajorVer = aMajor)      and
            (TempVersionInfo.MinorVer = aMinor)      and
            (TempVersionInfo.Release  = aRelease)    and
            (TempVersionInfo.Build    = aBuild);
end;

// This function is based on the answer given by 'Alex' in StackOverflow
// https://stackoverflow.com/questions/2748474/how-to-determine-if-dll-file-was-compiled-as-x64-or-x86-bit-using-either-delphi
function GetDLLHeaderMachine(const aDLLFile : string; var aMachine : integer) : boolean;
var
  TempHeader         : TImageDosHeader;
  TempImageNtHeaders : TImageNtHeaders;
  TempStream         : TFileStream;
begin
  Result     := False;
  aMachine   := IMAGE_FILE_MACHINE_UNKNOWN;
  TempStream := nil;

  try
    try
      if FileExists(aDLLFile) then
        begin
          TempStream := TFileStream.Create(aDLLFile, fmOpenRead);
          TempStream.seek(0, soFromBeginning);
          TempStream.ReadBuffer(TempHeader, SizeOf(TempHeader));

          if (TempHeader.e_magic = IMAGE_DOS_SIGNATURE) and
             (TempHeader._lfanew <> 0) then
            begin
              TempStream.Position := TempHeader._lfanew;
              TempStream.ReadBuffer(TempImageNtHeaders, SizeOf(TempImageNtHeaders));

              if (TempImageNtHeaders.Signature = IMAGE_NT_SIGNATURE) then
                begin
                  aMachine := TempImageNtHeaders.FileHeader.Machine;
                  Result   := True;
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('GetDLLHeaderMachine', e) then raise;
    end;
  finally
    if (TempStream <> nil) then FreeAndNil(TempStream);
  end;
end;

function Is32BitProcess : boolean;
{$IFDEF MSWINDOWS}
var
  TempResult : BOOL;
{$ENDIF}
begin
  {$IFDEF CPUX32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPU386}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPUi386}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPUPOWERPC32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPUSPARC32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPU32BITS}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF CPUARM32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF WIN32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF IOS32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF MACOS32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF LINUX32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF POSIX32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF ANDROID32}
    Result := True;
    exit;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    Result := ProcessUnderWow64(GetCurrentProcess, TempResult) and TempResult;
    exit;
  {$ENDIF}

  {$IFDEF DELPHI16_UP}
    Result := TOSVersion.Architecture in [arIntelX86, arARM32];
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function CustomPathIsRelative(const aPath : string) : boolean;
begin
  {$IFDEF DELPHI12_UP}
  Result := PathIsRelativeUnicode(PChar(aPath));
  {$ELSE}
  Result := PathIsRelativeAnsi(PChar(aPath));
  {$ENDIF}
end;

function GetModulePath : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HINSTANCE{$IFDEF FPC}(){$ENDIF})));
end;

function CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
var
  TempURL   : TCefString;
  TempParts : TCefUrlParts;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempParts, sizeof(TempParts), 0);
      TempURL := CefString(url);
      Result  := cef_parse_url(@TempURL, TempParts) <> 0;

      if Result then
        begin
          //parts.spec     := CefString(@TempParts.spec);
          parts.scheme   := CefString(@TempParts.scheme);
          parts.username := CefString(@TempParts.username);
          parts.password := CefString(@TempParts.password);
          parts.host     := CefString(@TempParts.host);
          parts.port     := CefString(@TempParts.port);
          parts.origin   := CefString(@TempParts.origin);
          parts.path     := CefString(@TempParts.path);
          parts.query    := CefString(@TempParts.query);
        end;
    end
   else
    Result := False;
end;

function CefCreateUrl(var parts: TUrlParts): ustring;
var
  TempURL   : TCefString;
  TempParts : TCefUrlParts;
begin
  Result := '';

  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempParts, sizeof(TempParts), 0);
      TempParts.spec     := CefString(parts.spec);
      TempParts.scheme   := CefString(parts.scheme);
      TempParts.username := CefString(parts.username);
      TempParts.password := CefString(parts.password);
      TempParts.host     := CefString(parts.host);
      TempParts.port     := CefString(parts.port);
      TempParts.origin   := CefString(parts.origin);
      TempParts.path     := CefString(parts.path);
      TempParts.query    := CefString(parts.query);

      FillChar(TempURL, SizeOf(TempURL), 0);
      if cef_create_url(@TempParts, @TempURL) <> 0 then Result := CefString(@TempURL);
    end;
end;

function CefFormatUrlForSecurityDisplay(const originUrl: string): string;
var
  TempOrigin : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempOrigin := CefString(originUrl);
      Result     := CefStringFreeAndGet(cef_format_url_for_security_display(@TempOrigin));
    end
   else
    Result := '';
end;

function CefGetMimeType(const extension: ustring): ustring;
var
  TempExt : TCefString;
begin
  TempExt := CefString(extension);
  Result  := CefStringFreeAndGet(cef_get_mime_type(@TempExt));
end;

procedure CefGetExtensionsForMimeType(const mimeType: ustring; var extensions: TStringList);
var
  TempSL       : ICefStringList;
  TempMimeType : TCefString;
begin
  if (extensions <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSL       := TCefStringListOwn.Create;
      TempMimeType := CefString(mimeType);
      cef_get_extensions_for_mime_type(@TempMimeType, TempSL.Handle);
      TempSL.CopyToStrings(extensions);
    end;
end;

function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := CefStringFreeAndGet(cef_base64encode(data, dataSize))
   else
    Result := '';
end;

function CefBase64Decode(const data: ustring): ICefBinaryValue;
var
  TempData : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempData := CefString(data);
      Result   := TCefBinaryValueRef.UnWrap(cef_base64decode(@TempData));
    end
   else
    Result := nil;
end;

function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
var
  TempText : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempText := CefString(text);
      Result   := CefStringFreeAndGet(cef_uriencode(@TempText, Ord(usePlus)));
    end
   else
    Result := '';
end;

function CefUriDecode(const text: ustring; convertToUtf8: Boolean; unescapeRule: TCefUriUnescapeRule): ustring;
var
  TempText : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempText := CefString(text);
      Result   := CefStringFreeAndGet(cef_uridecode(@TempText, Ord(convertToUtf8), unescapeRule));
    end
   else
    Result := '';
end;

function CefParseJson(const jsonString: ustring; options: TCefJsonParserOptions): ICefValue;
var
  TempJSON : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempJSON := CefString(jsonString);
      Result   := TCefValueRef.UnWrap(cef_parse_json(@TempJSON, options));
    end
   else
    Result := nil;
end;

function CefParseJsonAndReturnError(const jsonString   : ustring;
                                          options      : TCefJsonParserOptions;
                                    out   errorCodeOut : TCefJsonParserError;
                                    out   errorMsgOut  : ustring): ICefValue;
var
  TempJSON, TempError : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempError, SizeOf(TempError), 0);
      TempJSON    := CefString(jsonString);
      Result      := TCefValueRef.UnWrap(cef_parse_jsonand_return_error(@TempJSON, options, @errorCodeOut, @TempError));
      errorMsgOut := CefString(@TempError);
    end
   else
    begin
      errorCodeOut := JSON_NO_ERROR;
      Result       := nil;
      errorMsgOut  := '';
    end;
end;

function CefWriteJson(const node: ICefValue; options: TCefJsonWriterOptions): ustring;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := CefStringFreeAndGet(cef_write_json(CefGetData(node), options))
   else
    Result := '';
end;

function CefCreateDirectory(const fullPath: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(fullPath);
      Result   := cef_create_directory(@TempPath) <> 0;
    end
   else
    Result := False;
end;

function CefGetTempDirectory(out tempDir: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempPath, SizeOf(TempPath), 0);
      Result  := cef_get_temp_directory(@TempPath) <> 0;
      tempDir := CefString(@TempPath);
    end
   else
    begin
      Result  := False;
      tempDir := '';
    end;
end;

function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
var
  TempPath, TempPref : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempPath, SizeOf(TempPath), 0);
      TempPref    := CefString(prefix);
      Result      := cef_create_new_temp_directory(@TempPref, @TempPath) <> 0;
      newTempPath := CefString(@TempPath);
    end
   else
    begin
      Result      := False;
      newTempPath := '';
    end;
end;

function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring; out newDir: ustring): Boolean;
var
  TempBase, TempPath, TempPref: TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempPath, SizeOf(TempPath), 0);
      TempPref := CefString(prefix);
      TempBase := CefString(baseDir);
      Result   := cef_create_temp_directory_in_directory(@TempBase, @TempPref, @TempPath) <> 0;
      newDir   := CefString(@TempPath);
    end
   else
    begin
      Result   := False;
      newDir   := '';
    end;
end;

function CefDirectoryExists(const path: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      Result   := cef_directory_exists(@TempPath) <> 0;
    end
   else
    Result := False;
end;

function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      Result   := cef_delete_file(@TempPath, Ord(recursive)) <> 0;
    end
   else
    Result := False;
end;

function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
var
  TempSrc, TempDst : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSrc := CefString(srcDir);
      TempDst := CefString(destFile);
      Result  := cef_zip_directory(@TempSrc, @TempDst, Ord(includeHiddenFiles)) <> 0;
    end
   else
    Result := False;
end;

procedure CefLoadCRLSetsFile(const path : ustring);
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      cef_load_crlsets_file(@TempPath);
    end;
end;

function CefIsKeyDown(aWparam : WPARAM) : boolean;
begin
  Result := (GetKeyState(aWparam) < 0);
end;

function CefIsKeyToggled(aWparam : WPARAM) : boolean;
begin
  Result := (GetKeyState(aWparam) and $1) <> 0;
end;

function GetCefMouseModifiers(aWparam : WPARAM) : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if ((aWparam and MK_CONTROL) <> 0) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if ((aWparam and MK_SHIFT)   <> 0) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if ((aWparam and MK_LBUTTON) <> 0) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if ((aWparam and MK_MBUTTON) <> 0) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  if ((aWparam and MK_RBUTTON) <> 0) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if CefIsKeyDown(VK_MENU)           then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK)     then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL)     then Result := Result or EVENTFLAG_CAPS_LOCK_ON;
end;

function GetCefMouseModifiers : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if CefIsKeyDown(MK_CONTROL)    then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if CefIsKeyDown(MK_SHIFT)      then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if CefIsKeyDown(MK_LBUTTON)    then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if CefIsKeyDown(MK_MBUTTON)    then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  if CefIsKeyDown(MK_RBUTTON)    then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if CefIsKeyDown(VK_MENU)       then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK) then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL) then Result := Result or EVENTFLAG_CAPS_LOCK_ON;
end;

function GetCefKeyboardModifiers(aWparam : WPARAM; aLparam : LPARAM) : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if CefIsKeyDown(VK_SHIFT)      then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if CefIsKeyDown(VK_CONTROL)    then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if CefIsKeyDown(VK_MENU)       then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK) then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL) then Result := Result or EVENTFLAG_CAPS_LOCK_ON;


  case aWparam of
    VK_RETURN:
      if (((aLparam shr 16) and KF_EXTENDED) <> 0) then Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_INSERT,
    VK_DELETE,
    VK_HOME,
    VK_END,
    VK_PRIOR,
    VK_NEXT,
    VK_UP,
    VK_DOWN,
    VK_LEFT,
    VK_RIGHT :
      if (((aLparam shr 16) and KF_EXTENDED) = 0) then Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_NUMLOCK,
    VK_NUMPAD0,
    VK_NUMPAD1,
    VK_NUMPAD2,
    VK_NUMPAD3,
    VK_NUMPAD4,
    VK_NUMPAD5,
    VK_NUMPAD6,
    VK_NUMPAD7,
    VK_NUMPAD8,
    VK_NUMPAD9,
    VK_DIVIDE,
    VK_MULTIPLY,
    VK_SUBTRACT,
    VK_ADD,
    VK_DECIMAL,
    VK_CLEAR :
      Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_SHIFT :
      if CefIsKeyDown(VK_LSHIFT) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RSHIFT) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_CONTROL :
      if CefIsKeyDown(VK_LCONTROL) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RCONTROL) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_MENU :
      if CefIsKeyDown(VK_LMENU) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RMENU) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_LWIN :
      Result := Result or EVENTFLAG_IS_LEFT;

    VK_RWIN :
      Result := Result or EVENTFLAG_IS_RIGHT;
  end;
end;

function GefCursorToWindowsCursor(aCefCursor : TCefCursorType) : TCursor;
begin
  case aCefCursor of
    CT_POINTER                  : Result := crArrow;
    CT_CROSS                    : Result := crCross;
    CT_HAND                     : Result := crHandPoint;
    CT_IBEAM                    : Result := crIBeam;
    CT_WAIT                     : Result := crHourGlass;
    CT_HELP                     : Result := crHelp;
    CT_EASTRESIZE               : Result := crSizeWE;
    CT_NORTHRESIZE              : Result := crSizeNS;
    CT_NORTHEASTRESIZE          : Result := crSizeNESW;
    CT_NORTHWESTRESIZE          : Result := crSizeNWSE;
    CT_SOUTHRESIZE              : Result := crSizeNS;
    CT_SOUTHEASTRESIZE          : Result := crSizeNWSE;
    CT_SOUTHWESTRESIZE          : Result := crSizeNESW;
    CT_WESTRESIZE               : Result := crSizeWE;
    CT_NORTHSOUTHRESIZE         : Result := crSizeNS;
    CT_EASTWESTRESIZE           : Result := crSizeWE;
    CT_NORTHEASTSOUTHWESTRESIZE : Result := crSizeNESW;
    CT_NORTHWESTSOUTHEASTRESIZE : Result := crSizeNWSE;
    CT_COLUMNRESIZE             : Result := crHSplit;
    CT_ROWRESIZE                : Result := crVSplit;
    CT_MOVE                     : Result := crSizeAll;
    CT_PROGRESS                 : Result := crAppStart;
    CT_NONE                     : Result := crNone;
    CT_NODROP,
    CT_NOTALLOWED               : Result := crNo;
    CT_GRAB,
    CT_GRABBING                 : Result := crDrag;

    else Result := crDefault;
  end;
end;

procedure DropEffectToDragOperation(aEffect: Longint; var aAllowedOps : TCefDragOperations);
begin
  aAllowedOps := DRAG_OPERATION_NONE;

  if ((aEffect and DROPEFFECT_COPY) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_COPY;
  if ((aEffect and DROPEFFECT_LINK) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_LINK;
  if ((aEffect and DROPEFFECT_MOVE) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_MOVE;
end;

procedure DragOperationToDropEffect(const aDragOperations : TCefDragOperations; var aEffect: Longint);
begin
  aEffect := DROPEFFECT_NONE;

  if ((aDragOperations and DRAG_OPERATION_COPY) <> 0) then aEffect := aEffect or DROPEFFECT_COPY;
  if ((aDragOperations and DRAG_OPERATION_LINK) <> 0) then aEffect := aEffect or DROPEFFECT_LINK;
  if ((aDragOperations and DRAG_OPERATION_MOVE) <> 0) then aEffect := aEffect or DROPEFFECT_MOVE;
end;

function DeviceToLogical(aValue : integer; const aDeviceScaleFactor : double) : integer;
begin
  Result := floor(aValue / aDeviceScaleFactor);
end;

procedure DeviceToLogical(var aEvent : TCEFMouseEvent; const aDeviceScaleFactor : double);
begin
  aEvent.x := DeviceToLogical(aEvent.x, aDeviceScaleFactor);
  aEvent.y := DeviceToLogical(aEvent.y, aDeviceScaleFactor);
end;

procedure DeviceToLogical(var aPoint : TPoint; const aDeviceScaleFactor : double);
begin
  aPoint.x := DeviceToLogical(aPoint.x, aDeviceScaleFactor);
  aPoint.y := DeviceToLogical(aPoint.y, aDeviceScaleFactor);
end;

function LogicalToDevice(aValue : integer; const aDeviceScaleFactor : double) : integer;
begin
  Result := floor(aValue * aDeviceScaleFactor);
end;

procedure LogicalToDevice(var aRect : TCEFRect; const aDeviceScaleFactor : double);
begin
  aRect.x      := LogicalToDevice(aRect.x,      aDeviceScaleFactor);
  aRect.y      := LogicalToDevice(aRect.y,      aDeviceScaleFactor);
  aRect.width  := LogicalToDevice(aRect.width,  aDeviceScaleFactor);
  aRect.height := LogicalToDevice(aRect.height, aDeviceScaleFactor);
end;

function GetScreenDPI : integer;
var
  TempDC : HDC;
begin
  TempDC := GetDC(0);
  Result := GetDeviceCaps(TempDC, LOGPIXELSX);
  ReleaseDC(0, TempDC);
end;

function GetDeviceScaleFactor : single;
begin
  Result := GetScreenDPI / 96;
end;

function DeleteDirContents(const aDirectory : string) : boolean;
var
  TempRec  : TSearchRec;
  TempPath : string;
begin
  Result := True;

  try
    if (length(aDirectory) > 0) and
       DirectoryExists(aDirectory) and
       (FindFirst(aDirectory + '\*', faAnyFile, TempRec) = 0) then
      try
        repeat
          TempPath := aDirectory + PathDelim + TempRec.Name;

          if ((TempRec.Attr and faDirectory) <> 0) then
            begin
              if (TempRec.Name <> '.') and (TempRec.Name <> '..') then
                begin
                  if DeleteDirContents(TempPath) then
                    Result := RemoveDir(TempPath) and Result
                   else
                    Result := False;
                end;
            end
           else
            Result := DeleteFile(TempPath) and Result;
        until (FindNext(TempRec) <> 0) or not(Result);
      finally
        FindClose(TempRec);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('DeleteDirContents', e) then raise;
  end;
end;

end.
