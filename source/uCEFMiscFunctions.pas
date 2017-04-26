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
  WinApi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
  Windows, Classes, SysUtils,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFResourceHandler;

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

function CefGetObject(ptr: Pointer): TObject;
function CefGetData(const i: ICefBaseRefCounted): Pointer;

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
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; aTransparent : boolean; const aWindowName : string = '');

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

implementation

uses
  uCEFConstants, uCEFApplication, uCEFSchemeHandlerFactory;

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
  aWindowInfo.transparent_painting_enabled := ord(False);
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
  aWindowInfo.transparent_painting_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : THandle; aTransparent : boolean; const aWindowName : string);
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
  aWindowInfo.transparent_painting_enabled := ord(aTransparent);
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

  if (GlobalCEFApp <> nil) and GlobalCEFApp.ReRaiseExceptions then
    raise Exception.Create(aMessage);
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
      OutputDebugMessage('CheckLocales error: ' + e.Message);
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
            if CustomPathIsRelative(PChar(TempDir)) then TempDir := ExtractFilePath(ParamStr(0)) + TempDir;
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
      OutputDebugMessage('CheckResources error: ' + e.Message);
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
            if CustomPathIsRelative(PChar(TempDir)) then TempDir := ExtractFilePath(ParamStr(0)) + TempDir;
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
      OutputDebugMessage('CheckDLLs error: ' + e.Message);
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
        OutputDebugMessage('GetExtendedFileVersion error: ' + e.Message);
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
      OutputDebugMessage('GetDLLVersion error: ' + e.Message);
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

end.
