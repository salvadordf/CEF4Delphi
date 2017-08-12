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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils, System.UITypes, WinApi.ActiveX, System.Math,
  {$ELSE}
  Windows, Classes, SysUtils, Controls, ActiveX, Math,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFResourceHandler, uCEFGetGeolocationCallback;

const
  Kernel32DLL = 'kernel32.dll';
  SHLWAPIDLL  = 'shlwapi.dll';

procedure CefStringListToStringList(var aSrcSL : TCefStringList; var aDstSL : TStringList); overload;
procedure CefStringListToStringList(var aSrcSL : TCefStringList; var aDstSL : TStrings); overload;

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

function  CefExecuteProcess(var app : ICefApp; aWindowsSandboxInfo : Pointer = nil) : integer;
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

procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : THandle; aRect : TRect; const aWindowName : string = '');
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : string = '');
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : string = '');

function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external Kernel32DLL;
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation; lpUniversalTime, lpLocalTime: PSystemTime): BOOL; stdcall; external Kernel32DLL;

function PathIsRelativeAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeA';
function PathIsRelativeUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeW';
function CustomPathIsRelative(const aPath : string) : boolean;

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
function CefIsCertStatusMinorError(Status : TCefCertStatus) : boolean;

function  CefCrashReportingEnabled : boolean;
procedure CefSetCrashKeyValue(const aKey, aValue : ustring);

procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
procedure OutputDebugMessage(const aMessage : string);
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;

function CefRegisterSchemeHandlerFactory(const SchemeName, HostName: ustring; const handler: TCefResourceHandlerClass): Boolean;
function CefClearSchemeHandlerFactories : boolean;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefClearCrossOriginWhitelist: Boolean;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
function  GetExtendedFileVersion(const aFileName : string) : uint64;
function  GetDLLVersion(const aDLLFile : string; var aVersionInfo : TFileVersionInfo) : boolean;

function CheckLocales(const aLocalesDirPath : string) : boolean;
function CheckResources(const aResourcesDirPath : string) : boolean;
function CheckDLLs(const aFrameworkDirPath : string) : boolean;
function CheckDLLVersion(const aDLLFile : string; aMajor, aMinor, aRelease, aBuild : uint16) : boolean;

function  CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
function  CefCreateUrl(var parts: TUrlParts): ustring;
function  CefFormatUrlForSecurityDisplay(const originUrl: string): string;
function  CefGetMimeType(const extension: ustring): ustring;
procedure CefGetExtensionsForMimeType(const mimeType: ustring; extensions: TStringList);

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

function CefGetGeolocation(const aCallbackFunction : TOnLocationUpdate) : boolean;

implementation

uses
  uCEFConstants, uCEFApplication, uCEFSchemeHandlerFactory, uCEFValue,
  uCEFBinaryValue;

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

procedure CefStringListToStringList(var aSrcSL : TCefStringList; var aDstSL : TStringList);
begin
  CefStringListToStringList(aSrcSL, TStrings(aDstSL));
end;

procedure CefStringListToStringList(var aSrcSL : TCefStringList; var aDstSL : TStrings);
var
  i, j : NativeUInt;
  TempString : TCefString;
begin
  if (aSrcSL <> nil) and (aDstSL <> nil) then
    begin
      i := 0;
      j := pred(cef_string_list_size(aSrcSL));

      while (i < j) do
        begin
          FillChar(TempString, SizeOf(TempString), 0);
          cef_string_list_value(aSrcSL, i, @TempString);
          aDstSL.Add(CefStringClearAndGet(TempString));
          inc(i);
        end;
    end;
end;

function CefStringClearAndGet(var str: TCefString): ustring;
begin
  Result := CefString(@str);
  cef_string_utf16_clear(@str);
end;

function CefGetObject(ptr: Pointer): TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Dec(PByte(ptr), SizeOf(Pointer));
  Result := TObject(PPointer(ptr)^);
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
    SetString(Result, str.str, str.length)
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
  if (str <> nil) then cef_string_utf16_clear(str);
end;

procedure CefStringSet(const str: PCefString; const value: ustring);
begin
  if (str <> nil) then cef_string_utf16_set(PWideChar(value), Length(value), str, 1);
end;

function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
begin
  if (str <> nil) then
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
  if (str <> '') then cef_string_wide_to_utf16(PWideChar(str), Length(str), @Result);
end;

procedure _free_string(str: PChar16); stdcall;
begin
  if (str <> nil) then FreeMem(str);
end;

function CefUserFreeString(const str: ustring): PCefStringUserFree;
begin
  Result        := cef_string_userfree_utf16_alloc;
  Result.length := Length(str);
  GetMem(Result.str, Result.length * SizeOf(TCefChar));
  Move(PCefChar(str)^, Result.str^, Result.length * SizeOf(TCefChar));
  Result.dtor   := @_free_string;
end;

function CefExecuteProcess(var app : ICefApp; aWindowsSandboxInfo : Pointer) : integer;
begin
  Result := cef_execute_process(@HInstance, CefGetData(app), aWindowsSandboxInfo);
end;

function CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
var
  n, c : TCefString;
begin
  n      := CefString(name);
  c      := CefString(code);
  Result := cef_register_extension(@n, @c, CefGetData(handler)) <> 0;
end;

procedure CefPostTask(ThreadId: TCefThreadId; const task: ICefTask);
begin
  cef_post_task(ThreadId, CefGetData(task));
end;

procedure CefPostDelayedTask(ThreadId: TCefThreadId; const task: ICefTask; delayMs: Int64);
begin
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
  st: TSystemTime;
begin
  st     := CefTimeToSystemTime(dt);
  SystemTimeToTzSpecificLocalTime(nil, @st, @st);
  Result := SystemTimeToDateTime(st);
end;

function DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(dt, st);
  TzSpecificLocalTimeToSystemTime(nil, @st, @st);
  Result := SystemTimeToCefTime(st);
end;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
begin
  Result := cef_string_wide_set(src, src_len, output, ord(True));
end;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
begin
  Result := cef_string_utf8_set(src, src_len, output, ord(True));
end;

function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
begin
  Result := cef_string_utf16_set(src, src_len, output, ord(True));
end;

function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;
begin
  Result := cef_string_utf16_set(src, src_len, output, ord(True));
end;

procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : THandle; aRect : TRect; const aWindowName : string);
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

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : string);
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

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; const aWindowName : string);
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

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
begin
  Result := (cef_is_cert_status_error(Status) <> 0);
end;

function CefIsCertStatusMinorError(Status : TCefCertStatus) : boolean;
begin
  Result := (cef_is_cert_status_minor_error(Status) <> 0);
end;

function CefCrashReportingEnabled : boolean;
begin
  Result := (cef_crash_reporting_enabled <> 0);
end;

procedure CefSetCrashKeyValue(const aKey, aValue : ustring);
var
  TempKey, TempValue : TCefString;
begin
  TempKey   := CefString(aKey);
  TempValue := CefString(aValue);
  cef_set_crash_key_value(@TempKey, @TempValue);
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
                                         const HostName   : ustring;
                                         const handler    : TCefResourceHandlerClass) : boolean;
var
  TempScheme, TempHostName : TCefString;
  TempFactory : ICefSchemeHandlerFactory;
begin
  TempScheme   := CefString(SchemeName);
  TempHostName := CefString(HostName);
  TempFactory  := TCefSchemeHandlerFactoryOwn.Create(handler);
  Result       := cef_register_scheme_handler_factory(@TempScheme, @TempHostName, TempFactory.Wrap) <> 0;
end;

function CefClearSchemeHandlerFactories : boolean;
begin
  Result := cef_clear_scheme_handler_factories <> 0;
end;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                         const TargetProtocol        : ustring;
                                         const TargetDomain          : ustring;
                                               AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  TempSourceOrigin   := CefString(SourceOrigin);
  TempTargetProtocol := CefString(TargetProtocol);
  TempTargetDomain   := CefString(TargetDomain);
  Result             := cef_add_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                             @TempTargetProtocol,
                                                             @TempTargetDomain,
                                                             Ord(AllowTargetSubdomains)) <> 0;
end;

function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                            const TargetProtocol        : ustring;
                                            const TargetDomain          : ustring;
                                                  AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  TempSourceOrigin   := CefString(SourceOrigin);
  TempTargetProtocol := CefString(TargetProtocol);
  TempTargetDomain   := CefString(TargetDomain);
  Result             := cef_remove_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                                @TempTargetProtocol,
                                                                @TempTargetDomain,
                                                                Ord(AllowTargetSubdomains)) <> 0;
end;

function CefClearCrossOriginWhitelist: Boolean;
begin
  Result := cef_clear_cross_origin_whitelist <> 0;
end;

function CheckLocales(const aLocalesDirPath : string) : boolean;
var
  TempDir : string;
begin
  Result := False;

  try
    if (length(aLocalesDirPath) > 0) then
      TempDir := aLocalesDirPath
     else
      TempDir := 'locales';

    if DirectoryExists(TempDir) then
      begin
        TempDir := IncludeTrailingPathDelimiter(TempDir);

        Result := FileExists(TempDir + 'am.pak') and
                  FileExists(TempDir + 'ar.pak') and
                  FileExists(TempDir + 'bg.pak') and
                  FileExists(TempDir + 'bn.pak') and
                  FileExists(TempDir + 'ca.pak') and
                  FileExists(TempDir + 'cs.pak') and
                  FileExists(TempDir + 'da.pak') and
                  FileExists(TempDir + 'de.pak') and
                  FileExists(TempDir + 'el.pak') and
                  FileExists(TempDir + 'en-GB.pak') and
                  FileExists(TempDir + 'en-US.pak') and
                  FileExists(TempDir + 'es.pak') and
                  FileExists(TempDir + 'es-419.pak') and
                  FileExists(TempDir + 'et.pak') and
                  FileExists(TempDir + 'fa.pak') and
                  FileExists(TempDir + 'fi.pak') and
                  FileExists(TempDir + 'fil.pak') and
                  FileExists(TempDir + 'fr.pak') and
                  FileExists(TempDir + 'gu.pak') and
                  FileExists(TempDir + 'he.pak') and
                  FileExists(TempDir + 'hi.pak') and
                  FileExists(TempDir + 'hr.pak') and
                  FileExists(TempDir + 'hu.pak') and
                  FileExists(TempDir + 'id.pak') and
                  FileExists(TempDir + 'it.pak') and
                  FileExists(TempDir + 'ja.pak') and
                  FileExists(TempDir + 'kn.pak') and
                  FileExists(TempDir + 'ko.pak') and
                  FileExists(TempDir + 'lt.pak') and
                  FileExists(TempDir + 'lv.pak') and
                  FileExists(TempDir + 'ml.pak') and
                  FileExists(TempDir + 'mr.pak') and
                  FileExists(TempDir + 'ms.pak') and
                  FileExists(TempDir + 'nb.pak') and
                  FileExists(TempDir + 'nl.pak') and
                  FileExists(TempDir + 'pl.pak') and
                  FileExists(TempDir + 'pt-BR.pak') and
                  FileExists(TempDir + 'pt-PT.pak') and
                  FileExists(TempDir + 'ro.pak') and
                  FileExists(TempDir + 'ru.pak') and
                  FileExists(TempDir + 'sk.pak') and
                  FileExists(TempDir + 'sl.pak') and
                  FileExists(TempDir + 'sr.pak') and
                  FileExists(TempDir + 'sv.pak') and
                  FileExists(TempDir + 'sw.pak') and
                  FileExists(TempDir + 'ta.pak') and
                  FileExists(TempDir + 'te.pak') and
                  FileExists(TempDir + 'th.pak') and
                  FileExists(TempDir + 'tr.pak') and
                  FileExists(TempDir + 'uk.pak') and
                  FileExists(TempDir + 'vi.pak') and
                  FileExists(TempDir + 'zh-CN.pak') and
                  FileExists(TempDir + 'zh-TW.pak');
      end;
  except
    on e : exception do
      if CustomExceptionHandler('CheckLocales', e) then raise;
  end;
end;

function CheckResources(const aResourcesDirPath : string) : boolean;
var
  TempDir : string;
begin
  Result := False;

  try
    if (length(aResourcesDirPath) > 0) then
      begin
        if DirectoryExists(aResourcesDirPath) then
          begin
            TempDir := IncludeTrailingPathDelimiter(aResourcesDirPath);
            if CustomPathIsRelative(TempDir) then TempDir := ExtractFilePath(ParamStr(0)) + TempDir;
          end
         else
          exit;
      end
     else
      TempDir := '';

    Result := FileExists(TempDir + 'natives_blob.bin')       and
              FileExists(TempDir + 'snapshot_blob.bin')      and
              FileExists(TempDir + 'icudtl.dat')             and
              FileExists(TempDir + 'cef.pak')                and
              FileExists(TempDir + 'cef_100_percent.pak')    and
              FileExists(TempDir + 'cef_200_percent.pak')    and
              FileExists(TempDir + 'cef_extensions.pak')     and
              FileExists(TempDir + 'devtools_resources.pak');
  except
    on e : exception do
      if CustomExceptionHandler('CheckResources', e) then raise;
  end;
end;

function CheckDLLs(const aFrameworkDirPath : string) : boolean;
var
  TempDir : string;
begin
  Result := False;

  try
    if (length(aFrameworkDirPath) > 0) then
      begin
        if DirectoryExists(aFrameworkDirPath) then
          begin
            TempDir := IncludeTrailingPathDelimiter(aFrameworkDirPath);
            if CustomPathIsRelative(TempDir) then TempDir := ExtractFilePath(ParamStr(0)) + TempDir;
          end
         else
          exit;
      end
     else
      TempDir := '';

    Result := FileExists(TempDir + CHROMEELF_DLL)            and
              FileExists(TempDir + LIBCEF_DLL)               and
              FileExists(TempDir + 'd3dcompiler_43.dll')     and
              FileExists(TempDir + 'd3dcompiler_47.dll')     and
              FileExists(TempDir + 'libEGL.dll')             and
              FileExists(TempDir + 'libGLESv2.dll')          and
              FileExists(TempDir + 'widevinecdmadapter.dll');
  except
    on e : exception do
      if CustomExceptionHandler('CheckDLLs', e) then raise;
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
              Result := TempInfo.dwFileVersionMS;
              Result := Result shl 32;
              Result := Result or TempInfo.dwFileVersionLS;
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

function CustomPathIsRelative(const aPath : string) : boolean;
begin
  {$IFDEF DELPHI12_UP}
  Result := PathIsRelativeUnicode(PChar(aPath));
  {$ELSE}
  Result := PathIsRelativeAnsi(PChar(aPath));
  {$ENDIF}
end;

function CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
var
  u: TCefString;
  p: TCefUrlParts;
begin
  FillChar(p, sizeof(p), 0);
  u := CefString(url);
  Result := cef_parse_url(@u, p) <> 0;
  if Result then
  begin
    //parts.spec := CefString(@p.spec);
    parts.scheme := CefString(@p.scheme);
    parts.username := CefString(@p.username);
    parts.password := CefString(@p.password);
    parts.host := CefString(@p.host);
    parts.port := CefString(@p.port);
    parts.origin := CefString(@p.origin);
    parts.path := CefString(@p.path);
    parts.query := CefString(@p.query);
  end;
end;

function CefCreateUrl(var parts: TUrlParts): ustring;
var
  p: TCefUrlParts;
  u: TCefString;
begin
  FillChar(p, sizeof(p), 0);
  p.spec := CefString(parts.spec);
  p.scheme := CefString(parts.scheme);
  p.username := CefString(parts.username);
  p.password := CefString(parts.password);
  p.host := CefString(parts.host);
  p.port := CefString(parts.port);
  p.origin := CefString(parts.origin);
  p.path := CefString(parts.path);
  p.query := CefString(parts.query);
  FillChar(u, SizeOf(u), 0);
  if cef_create_url(@p, @u) <> 0 then
    Result := CefString(@u) else
    Result := '';
end;

function CefFormatUrlForSecurityDisplay(const originUrl: string): string;
var
  o: TCefString;
begin
  o := CefString(originUrl);
  Result := CefStringFreeAndGet(cef_format_url_for_security_display(@o));
end;

function CefGetMimeType(const extension: ustring): ustring;
var
  s: TCefString;
begin
  s := CefString(extension);
  Result := CefStringFreeAndGet(cef_get_mime_type(@s));
end;

procedure CefGetExtensionsForMimeType(const mimeType: ustring; extensions: TStringList);
var
  list: TCefStringList;
  s, str: TCefString;
  i: Integer;
begin
  list := cef_string_list_alloc();
  try
    s := CefString(mimeType);
    cef_get_extensions_for_mime_type(@s, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      extensions.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
begin
  Result:= CefStringFreeAndGet(cef_base64encode(data, dataSize));
end;

function CefBase64Decode(const data: ustring): ICefBinaryValue;
var
  s: TCefString;
begin
  s := CefString(data);
  Result := TCefBinaryValueRef.UnWrap(cef_base64decode(@s));
end;

function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
var
  s: TCefString;
begin
  s := CefString(text);
  Result := CefStringFreeAndGet(cef_uriencode(@s, Ord(usePlus)));
end;

function CefUriDecode(const text: ustring; convertToUtf8: Boolean;
  unescapeRule: TCefUriUnescapeRule): ustring;
var
  s: TCefString;
begin
  s := CefString(text);
  Result := CefStringFreeAndGet(cef_uridecode(@s, Ord(convertToUtf8), unescapeRule));
end;

function CefParseJson(const jsonString: ustring; options: TCefJsonParserOptions): ICefValue;
var
  s: TCefString;
begin
  s := CefString(jsonString);
  Result := TCefValueRef.UnWrap(cef_parse_json(@s, options));
end;

function CefParseJsonAndReturnError(const jsonString   : ustring;
                                          options      : TCefJsonParserOptions;
                                    out   errorCodeOut : TCefJsonParserError;
                                    out   errorMsgOut  : ustring): ICefValue;
var
  s, e: TCefString;
begin
  s := CefString(jsonString);
  FillChar(e, SizeOf(e), 0);
  Result := TCefValueRef.UnWrap(cef_parse_jsonand_return_error(@s, options, @errorCodeOut, @e));
  errorMsgOut := CefString(@e);
end;

function CefWriteJson(const node: ICefValue; options: TCefJsonWriterOptions): ustring;
begin
  Result := CefStringFreeAndGet(cef_write_json(CefGetData(node), options));
end;

function CefCreateDirectory(const fullPath: ustring): Boolean;
var
  path: TCefString;
begin
  path := CefString(fullPath);
  Result := cef_create_directory(@path) <> 0;
end;

function CefGetTempDirectory(out tempDir: ustring): Boolean;
var
  path: TCefString;
begin
  FillChar(path, SizeOf(path), 0);
  Result := cef_get_temp_directory(@path) <> 0;
  tempDir := CefString(@path);
end;

function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
var
  path, pref: TCefString;
begin
  FillChar(path, SizeOf(path), 0);
  pref := CefString(prefix);
  Result := cef_create_new_temp_directory(@pref, @path) <> 0;
  newTempPath := CefString(@path);
end;

function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring;
  out newDir: ustring): Boolean;
var
  base, path, pref: TCefString;
begin
  FillChar(path, SizeOf(path), 0);
  pref := CefString(prefix);
  base := CefString(baseDir);
  Result := cef_create_temp_directory_in_directory(@base, @pref, @path) <> 0;
  newDir := CefString(@path);
end;

function CefDirectoryExists(const path: ustring): Boolean;
var
  str: TCefString;
begin
  str := CefString(path);
  Result := cef_directory_exists(@str) <> 0;
end;

function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
var
  str: TCefString;
begin
  str := CefString(path);
  Result := cef_delete_file(@str, Ord(recursive)) <> 0;
end;

function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
var
  src, dst: TCefString;
begin
  src := CefString(srcDir);
  dst := CefString(destFile);
  Result := cef_zip_directory(@src, @dst, Ord(includeHiddenFiles)) <> 0;
end;

procedure CefLoadCRLSetsFile(const path : ustring);
var
  TempPath : TCefString;
begin
  TempPath := CefString(path);
  cef_load_crlsets_file(@TempPath);
end;

function CefGetGeolocation(const aCallbackFunction : TOnLocationUpdate) : boolean;
var
  TempGeoCallBack : ICefGetGeolocationCallback;
begin
  TempGeoCallBack := TCefFastGetGeolocationCallback.Create(aCallbackFunction);
  Result          := (cef_get_geolocation(TempGeoCallBack.Wrap) <> 0);
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

end.
